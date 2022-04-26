
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)
library(data.table)
library(optimx)


load("london.rda")
train = london[london$Year == "Year 1",]
test = london[london$Year == "Year 2",]


require(foreign)
require(MASS)
fit_model = glm.nb(Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + 
                     Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow, data = train)
summary(fit_model)



r.walk = function(){
  runif(1,-1/4,1/4)
}


g.sim = function(u){
  u + r.walk()
}

# yi = train$Bike_count[train$Day == 1]
#X = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow, data = train)
#xi = X[train$Day == 1,]
#betat = coef(fit_model)
#thetat = fit_model$theta

sample.gamma.posterior.i = function(yi, xi, M, maxit, thetat, betat, s2gammat, trace = 0) {
  
  gammai = rep(0,M)
  gammai[1] = rnorm(1, 0, sqrt(s2gammat))
  lambdai = exp(xi %*% betat + gammai[1])
  
  # Random Walk Sampler
  for (i in 1:(M-1)) {
    lambdai0 = lambdai
    
    gammai[i+1] = g.sim(gammai[i])
    lambdai = exp(xi %*% betat + gammai[i+1])
    #  p_i0 = (exp(-thetat)+1)/(exp(-thetat)+2)
    #  r_i0 = lambdai0 * (exp(-thetat)+1)
    #  p_i = (exp(-thetat)+1)/(exp(-thetat)+2)
    #  r_i = lambdai * (exp(-thetat)+1)
    
    L1 = prod(dnbinom(yi,size = thetat, mu = lambdai))*dnorm(gammai[i+1],
                                                             mean = 0,sd=sqrt(s2gammat))
    L2= prod(dnbinom(yi,size=thetat,mu= lambdai0))*dnorm(gammai[i],
                                                         mean = 0,sd=sqrt(s2gammat))
    
    if (L1>0) {
      r = L1 / L2
      if (r<1) {
        randnum = rbinom(1,1,r)
        if (randnum==0){
          gammai[i+1]=gammai[i]
        }
      }
    } else{
      gammai[i+1]=gammai[i]
    }
    
    
    
    if (trace > 0)
      print(gammai[i+1])
    
  }
  
  
  return(list(gammai = gammai))
  
}

sample.gamma.posterior.all = function(data,
                                      M,
                                      maxit,
                                      thetat,
                                      betat,
                                      s2gammat,
                                      trace = 0) {
  
  unique_date = unique(data$Date)
  n=length(unique_date)
  samples = matrix(NA,nrow = n, ncol = M)
  
  X = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + 
                      Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow, data = data)
  
  ## looping over n subjects
  for (i in 1:n) {
    
    if(trace > 0) print(i) 
    
    # draw M samples from the posterior for gamma_i
    samples.i =
      sample.gamma.posterior.i(
        yi = data[data$Date == unique_date[i], Bike_count],
        xi =  X[data$Date == unique_date[i],],
        M = M,
        maxit = maxit,
        thetat = thetat,
        betat = betat,
        s2gammat = s2gammat,
        trace = trace
      )$gammai
    
    # save to matrix
    samples[i,] = samples.i
    
  }
  
  if(trace > 0) print("completed sampling") 
  
  ## return matrix
  return(samples)
  
}


Qi = function(yi,
              xi,
              thetat,
              betat,
              s2gammat,
              gammai,
              burn_in) {
  
  # get M
  M = length(gammai)
  
  x_beta_mat = xi %*% matrix(betat, nrow = length(betat), ncol = M)
  
  # create 5 x M matrix, x_beta_plus_gamma_mat
  # m'th column is xi %*% betat + gammai[m]
  x_beta_plus_gamma_mat = sweep(x_beta_mat, 2 , gammai, "+")
  
  # calculate lambda (5 x M matrix)
  lambdai = exp(x_beta_plus_gamma_mat)  
  
  # calculate Q
  ymat = matrix(yi, nrow = length(yi), ncol = M)
  
  #p_i = (exp(-thetat)+1)/(exp(-thetat)+2)
  #r_i = lambdai * (exp(-thetat)+1)
  
  
  qi = sum(dnbinom(yi,size= thetat, mu=lambdai,log = T)[,-c(1:burn_in)]) + 
    sum(dnorm(gammai, mean = 0,sd = sqrt(s2gammat),log = T)[-c(1:burn_in)])
  
  # divide sum by M
  qi = qi / (M-burn_in)
  
  ## return values
  return(qi)
}

Q = function(data,
             thetat,
             betat,
             s2gammat,
             samples,
             burn_in,
             logs2gammat = F) {
  # backtranform if maximizing s2gammat on log scale
  if (logs2gammat == T) {
    s2gammat = exp(s2gammat)
  }
  
  unique_date = unique(data$Date)
  
  # initialize sum
  Q = 0
  
  ## Obtain Model.Matrix
  X = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + 
                      Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow, data = data)
  
  
  # loop over subjects
  for (i in 1:length(unique_date)) {
    Q = Q + Qi(yi = data[data$Date == unique_date[i], Bike_count],
               xi =  X[data$Date == unique_date[i],],
               thetat = thetat,
               betat = betat,
               s2gammat = s2gammat,
               gammai = samples[i, ],
               burn_in = burn_in)
  }
  
  # return
  return(Q)
}




MCEM_algorithm = function(
  beta_initial = c(7.49, 0.8, 1.06, -0.15, -0.23, -0.3, -0.18, -0.72, -0.01, 0.05, 0, 0, 0.01, -0.48),
  theta_initial = 4.542,
  s2gamma_initial = 0.2,
  M = 1000,
  burn.in = 200,
  tol = 10^-5,
  maxit = 100,
  data = train,
  trace  = 0
) {
  
  tol = tol
  maxit = maxit
  iter = 0
  eps = 10000
  qfunction = -1000000 # using Qfunction for convergence
  
  
  beta = beta_initial
  theta = theta_initial
  s2gamma = s2gamma_initial
  
  # Length of chain
  M = M
  
  # burn in
  burn.in = burn.in
  
  train = data.table(data)
  
  n = length(unique(train$Date))
  
  
  
  while(eps > tol & iter < maxit){
    
    qfunction0 = qfunction
    
    ## Begin E-step
    
    samples = sample.gamma.posterior.all(
      data = train,
      M = M,
      maxit = maxit * M,
      thetat = theta,
      betat = beta,
      s2gammat = s2gamma,
      trace = trace
    )
    
    qfunction = Q(data = train, 
                  thetat = theta,
                  betat = beta, 
                  s2gammat = s2gamma, 
                  samples = samples,
                  burn_in = burn.in)
    
    
    print(qfunction)
    
    
    eps  = abs(qfunction - qfunction0) / abs(qfunction0)
    
    
    ## Start M-step : nelder mead
    fit = optimx(
      # initial values for the parameters
      par = c(beta, theta, log(s2gamma)),
      # Q function wrapper
      fn = function(x, data, samples){
        Q(data = train, 
          betat = x[1:length(beta)], 
          thetat = x[length(beta)+1],
          s2gammat = x[length(beta)+2], 
          samples = samples,
          logs2gammat = T,
          burn_in = burn.in# indicating s2gamma on log scale! 
        )   
      }, 
      method = "Nelder-Mead",
      data = data,
      samples = samples,
      control = list(
        trace = trace, 
        maximize = T, 
        abstol= tol
      )
    )
    
    
    beta = as.numeric(fit[1:length(beta)])
    theta = as.numeric(fit[length(beta)+1])
    s2gamma = as.numeric(fit[length(beta)+2])
    
    s2gamma = exp(s2gamma)
    
    iter = iter + 1
    if(iter == maxit) warning("Iteration limit reached without convergence")
    
    cat(sprintf("Iter: %d Qf: %.3f s2gamma: %f Intercept: %.3f  Hour_Chunks[8,16):%.3f  Hour_chunks[16,24): %.3f  Is_weekend:%.3f Is_holiday:%.3f SeasonSpring:%.3f
                SeasonSummer:%.3f SeasonWinter:%.3f Min_temp:%.3f Max_temp:%.3f Min_humidity:%.3f Max_humidity:%.3f Wind_speed:%.3f Rain_or_snow:%.3f theta:%.3f eps:%f\n",iter, qfunction,s2gamma, beta[1],beta[2],beta[3],beta[4],beta[5],beta[6],
                beta[7],beta[8],beta[9], beta[10], beta[11], beta[12], beta[13], beta[14], theta, eps)
    )
    
    
  }
  
  results = list(
    beta = beta,
    s2gamma = s2gamma,
    theta = theta, 
    eps = eps,
    qfunction = qfunction,
    day_ranef = rowMeans(samples[,burn.in:M]),
    iter = iter
  )
  
  return(results)
  
}


set.seed(1)

## Dummy Example to Make sure algorithm runs
output = MCEM_algorithm( beta_initial = c(8.3, 1.5, 1.5, -0.25, -0.50, 0, 0, -0.25, 0, 0, 0, 0, 0, -0.25),
                         theta_initial = 10,
                         s2gamma_initial = 0.2,
                         M = 1000,
                         burn.in = 200,
                         tol = 10^-4,
                         maxit = 100,
                         data = train
)

save(output, file = "londonoutput_M1000.RData")


