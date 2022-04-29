#' Run MCEM algorithm to estimate NB GLMM bike count parameters
#'
#' This function runs the Monte Carlo EM algorithm as a means to estimate the
#' parameters of a negative binomial general mixed model used to predict bike
#' counts from date, time, and weather data. Hourly chunks, maximum daily
#' temperature, and presence of rain or snow are modeled as fixed effects, and
#' date is modeled as a random effect. The Metropolis Hasting algorithm is used
#' to draw samples of the random effect and the Nelder-Mead algorithm is used
#' to calculate the M-step.
#'
#' @param data pre-processed bike sharing data frame
#'             with columns Bike_count, Hour_chunks,
#'             Max_temp, Rain_or_snow, and Date
#' @param beta_initial initial beta values (vector of 14 numeric values)
#' @param theta_initial initial theta value (single numeric value)
#' @param s2gamma_initial initial value for s2gamma (single numeric value)
#' @param M length of chain (single numeric count value)
#' @param burn.in the number of beginning iteration samples to discard
#'                (single numeric count value)
#' @param tol tolerance for convergence (single numeric value)
#' @param maxit maximum number of iterations to run (single numeric count value)
#' @param trace level of detailed output (single numeric value)
#'
#' @return a list containing beta, theta, s2gamma, qfunction, and date random
#'         effect estimates, as well as epsilon and the iteration count
#'
#' @import data.table
#' @import stats
#' @importFrom optimx optimx
#'
#' @export
MCEM_algorithm = function(
  data,
  beta_initial = c(8.3, 1.5, 1.5, -0.25, -0.50, 0,
                   0, -0.25, 0, 0, 0, 0, 0, -0.25),
  theta_initial = 10,
  s2gamma_initial = 0.2,
  M = 1000,
  burn.in = 200,
  tol = 10^-4,
  maxit = 100,
  trace = 0
) {

  checkBikeData(data)

  checkParamsMCEM(beta_initial = beta_initial, theta_initial = theta_initial,
                  s2gamma_initial = s2gamma_initial, M = M, burn.in = burn.in,
                  tol = tol, maxit = maxit, trace = trace)

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

  data_table = data.table(data)

  n = length(unique(data_table$Date))


  while(eps > tol & iter < maxit){

    qfunction0 = qfunction

    ## Begin E-step

    samples = sample.gamma.posterior.all(
      data = data_table,
      M = M,
      maxit = maxit * M,
      thetat = theta,
      betat = beta,
      s2gammat = s2gamma,
      trace = trace
    )

    qfunction = Q(data = data_table,
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
        Q(data = data_table,
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

    cat(sprintf("Iter: %d Qf: %.3f s2gamma: %f Intercept: %.3f
    Hour_Chunks[8,16):%.3f Hour_chunks[16,24): %.3f Is_weekend:%.3f
    Is_holiday:%.3f SeasonSpring:%.3f SeasonSummer:%.3f SeasonWinter:%.3f
    Min_temp:%.3f Max_temp:%.3f Min_humidity:%.3f Max_humidity:%.3f
    Wind_speed:%.3f Rain_or_snow:%.3f theta:%.3f eps:%f\n",
                iter, qfunction,s2gamma, beta[1], beta[2], beta[3], beta[4],
                beta[5],beta[6], beta[7],beta[8], beta[9], beta[10], beta[11],
                beta[12], beta[13], beta[14], theta, eps)
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

#' Perform random walk
#'
#' This helper function performs a random walk, drawing a
#' sample from a uniform distribution between -1/4 and 1/4
#'
#' @return sampled random value
#'
#' @import stats
#'
r.walk = function(){
  runif(1,-1/4,1/4)
}

#' Add prior u to random walk
#'
#' This helper function simulates a sample from the proposal
#' density g, which is the prior plus the value from a random walk
#'
#' @param u prior
#'
#' @return u + sampled random walk value
#'
g.sim = function(u){
  u + r.walk()
}

#' Run random walk sampler for unique date i
#'
#' This helper function runs the random walk sampler for
#' a single unique date i
#'
#' @param yi a vector containing the bike counts observations for date i
#' @param xi a matrix containing the predictor variable observations for date i
#' @param M chain length
#' @param maxit the maximum number of iterations
#' @param thetat the estimate for theta at EM iteration t
#' @param betat the estimate for beta at EM iteration t
#' @param s2gammat the estimate for s2gammat at EM iteration t
#' @param trace level of detailed output
#'
#' @return gammai, list of random chain results for date i
#'
#' @import stats
#'
sample.gamma.posterior.i = function(yi, xi, M, maxit, thetat, betat, s2gammat,
                                    trace = 0) {

  gammai = rep(0,M)
  gammai[1] = rnorm(1, 0, sqrt(s2gammat))
  lambdai = exp(xi %*% betat + gammai[1])

  # Random Walk Sampler
  for (i in 1:(M-1)) {
    lambdai0 = lambdai

    gammai[i+1] = g.sim(gammai[i])
    lambdai = exp(xi %*% betat + gammai[i+1])

    L1 = prod(dnbinom(yi,size = thetat, mu = lambdai))*dnorm(gammai[i+1],
                                                             mean = 0,
                                                             sd=sqrt(s2gammat))
    L2= prod(dnbinom(yi,size=thetat,mu= lambdai0))*dnorm(gammai[i],
                                                         mean = 0,
                                                         sd=sqrt(s2gammat))

    if (L1>0) {
      r = L1 / L2
      if (r<1) {
        randnum = rbinom(1,1,r)
        if (randnum==0){
          gammai[i+1]=gammai[i]
        }
      }
    } else {
      gammai[i+1]=gammai[i]
    }

    if (trace > 0)
      print(gammai[i+1])
  }

  return(list(gammai = gammai))
}

#' Run random walk sampler for all unique dates
#'
#' This helper function runs the random walk sampler for
#' all unique dates
#'
#' @param data a data table containing bike sharing data
#' @param M chain length
#' @param maxit the maximum number of iterations
#' @param thetat the estimate for theta at EM iteration t
#' @param betat the estimate for beta at EM iteration t
#' @param s2gammat the estimate for s2gammat at EM iteration t
#' @param trace level of detailed output
#'
#' @return list containing random chain results
#'
#' @import stats
#'
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

  X = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday +
                      Season + Min_temp + Max_temp + Min_humidity +
                      Max_humidity + Wind_speed + Rain_or_snow, data = data)

  ## looping over n subjects
  for (i in 1:n) {

    if(trace > 0) print(i)

    Bike_count <- NULL # bind to object to avoid global variable warning

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

#' Calculate the Q-function for the unique date i
#'
#' This is a helper function that calculates the Q-function for
#' the unique date i
#'
#' @param yi a vector containing the bike counts observations for date i
#' @param xi a matrix containing the predictor variable observations for date i
#' @param thetat the estimate for theta at EM iteration t
#' @param betat the estimate for beta at EM iteration t
#' @param s2gammat the estimate for s2gammat at EM iteration t
#' @param gammai list of random chain results for date i
#' @param burn_in iterations to discard for burn_in
#'
#' @return q function values for i
#'
#' @import stats
#'
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

  qi = sum(dnbinom(yi,size= thetat, mu=lambdai,log = T)[,-c(1:burn_in)]) +
    sum(dnorm(gammai, mean = 0,sd = sqrt(s2gammat),log = T)[-c(1:burn_in)])

  # divide sum by M
  qi = qi / (M-burn_in)

  ## return values
  return(qi)
}

#' Calculate the Q-function for all unique dates
#'
#' This is a helper function that calculates the Q-function for
#' all unique dates
#'
#' @param data a data table containing bike sharing data
#' @param thetat the estimate for theta at EM iteration t
#' @param betat the estimate for beta at EM iteration t
#' @param s2gammat the estimate for s2gammat at EM iteration t
#' @param samples samples from random walk sampler
#' @param burn_in iterations to discard for burn_in
#' @param logs2gammat indicates whether s2gammat is initially passed on
#'                    the log scale
#'
#' @return q function values
#'
#' @import stats
#'
Q = function(data,
             thetat,
             betat,
             s2gammat,
             samples,
             burn_in,
             logs2gammat = F) {

  # backtransform if maximizing s2gammat on log scale
  if (logs2gammat == T) {
    s2gammat = exp(s2gammat)
  }

  unique_date = unique(data$Date)

  # initialize sum
  Q = 0

  ## Obtain Model.Matrix
  X = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday +
                      Season + Min_temp + Max_temp + Min_humidity +
                      Max_humidity + Wind_speed + Rain_or_snow, data = data)

  # loop over subjects
  for (i in 1:length(unique_date)) {
    Bike_count <- NULL # bind to object to avoid global variable warning
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
