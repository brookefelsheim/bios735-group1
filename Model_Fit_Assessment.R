## Running Code For Model Assessment for Final Presentation/Project

## Install Package
install.packages("package/bikeSharing_1.0.0.tar.gz", repos = NULL)

## Load Package
library(bikeSharing)
library(dplyr)

## Load Data ## Bound To be A better way to do this
load("package/bikeSharing/data/seoul.rda") 
load("package/bikeSharing/data/london.rda")
load("package/bikeSharing/data/dc.rda")


## Load trained GlMM model
# set.seed(1)
# output = MCEM_algorithm( beta_initial = c(7.5, 0.5, 1.0, -0.25, -0.25, -0.25, -0.25, -0.75, 0, 0, 0, 0, 0, -0.5),
# theta_initial = 4.5,
# s2gamma_initial = 0.2,
# M = 1000,
# burn.in = 200,
# tol = 10^-4,
# maxit = 100,
# data = seoul
# )
load("precomputed_model.Rdata")
model_glmm = output

## Train Random Forest
rf_model = train_random_forest(seoul)

## Apply GLMM Model Fit Assessment
glmm_model_fit(model_glmm, seoul, scale_to_seoul_mean = "no") 
glmm_model_fit(model_glmm, london, scale_to_seoul_mean = "yes") 
glmm_model_fit(model_glmm, dc, scale_to_seoul_mean = "yes") 

rf_model_fit(rf_model, seoul, scale_to_seoul_mean = "no")
rf_model_fit(rf_model, london, scale_to_seoul_mean = "yes")
rf_model_fit(rf_model, dc, scale_to_seoul_mean = "yes")


seoul$city = "Seoul"
london$city = "London - Scaled"
dc$city = "DC - Scaled"
scale_london = mean(london$Bike_count)/
  mean(seoul$Bike_count)
scale_dc = mean(dc$Bike_count)/
  mean(seoul$Bike_count)
seoul$Bike_count2 = seoul$Bike_count
london$Bike_count2 = london$Bike_count / scale_london
dc$Bike_count2 = dc$Bike_count / scale_dc



all_data =rbind(seoul, london, dc)
all_data$city = factor(all_data$city, levels = c("Seoul", "London - Scaled", "DC - Scaled"))

ggplot(all_data, aes(x = Bike_count2))+geom_histogram(fill = "dark blue",aes(y = stat(density)))+facet_wrap(~city)+
  theme_classic()+xlim(0,20000) + labs(title = "Distribution of Bike Counts for Cities", y = "Density", x = "Bike Count")

summary = all_data %>% group_by(city) %>% summarise(
  "Average Number of Holidays" = mean(Is_holiday),
  "Average Min Temp" = mean(Min_temp),
  "Average Max Temp" = mean(Max_temp),
  "Average Min Humidity" = mean(Min_humidity),
  "Average Max Humidity" = mean(Max_humidity),
  "Average Wind Speed" = mean(Wind_speed),
  "Average Rain/Snow Days" = mean(Rain_or_snow)
)

library(kableExtra)
summary1 = t(summary[,2:8])
colnames(summary1) = t(summary)[1,]
summary1 = as.data.frame(summary1)
summary1 %>% round(3) %>%  kbl() %>% 
  kable_styling('hover', full_width = T)


all_data %>% group_by(city) %>% summarise(
  "SD Number of Holidays" = sd(Is_holiday),
  "SD Min Temp" = sd(Min_temp),
  "SD Max Temp" = sd(Max_temp),
  "SD Min Humidity" = sd(Min_humidity),
  "SD Max Humidity" = sd(Max_humidity),
  "SD Wind Speed" = sd(Wind_speed),
  "SD Rain/Snow Days" = sd(Rain_or_snow)
)






glmm_model_fit <- function(model_glmm, data, scale_to_seoul_mean = "no"){
  
  ## Keep only same days as in seoul set
  data = data[data$Date %in% seoul$Date,]
  
  ## Name day ranef from seoul
  names(model_glmm$day_ranef) = unique(seoul$Date)
  
  ## Predict log Mean for each day Values
  model_matrix = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + 
                                 Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow, data = data)
  fixedef = c(model_matrix %*% model_glmm$beta)
  ranef = model_glmm$day_ranef[match(data$Date,names(model_glmm$day_ranef))]
  logmeans = fixedef + ranef
  
  ## Transform to get prediction
  pred = exp(logmeans)
  
  
  ## Scale to seoul bike count mean
  if(scale_to_seoul_mean == "yes"){
    ## Scaling Factor
    scale = mean(data$Bike_count)/
      mean(seoul$Bike_count)
    pred = pred * scale
  }
  
  ## Obtained RMSE
  RMSE = sqrt(mean((data$Bike_count - pred)^2))
  
  ## Obtain MAE
  MAE = mean(abs(data$Bike_count - pred)) 
  
  ## Obtain R^2
  R2 = cor(data$Bike_count, pred, use = "pairwise.complete.obs")^2 ## R squared
  
  
  ## Combine into data frame
  model_fit = data.frame(RMSE = RMSE, MAE = MAE, R2 = R2)
  
  return(model_fit)
}

rf_model_fit <- function(model, data, scale_to_seoul_mean = "no") {
  
  x = subset(data,
             select = c("Hour_chunks", "Is_weekend", "Is_holiday", 
                        "Season", "Min_temp", "Max_temp", "Min_humidity", "Max_humidity",
                        "Wind_speed", "Rain_or_snow", "Date"))
  ## Predict Values
  pred = predict(model, x)
  
  ## Scale to seoul bike count mean
  if(scale_to_seoul_mean == "yes"){
    ## Scaling Factor
    scale = mean(data$Bike_count)/
      mean(seoul$Bike_count)
    pred = pred / scale
  }
  
  ## Obtained RMSE
  RMSE = sqrt(mean((data$Bike_count - pred)^2))
  
  ## Obtain MAE
  MAE = mean(abs(data$Bike_count - pred))
  
  ## Obtain R^2
  R2 = cor(data$Bike_count, pred, use = "pairwise.complete.obs")^2 ## R squared
  
  ## Combine into data frame
  model_fit = data.frame(RMSE = RMSE, MAE = MAE, R2 = R2)
  
  return(model_fit)
}
