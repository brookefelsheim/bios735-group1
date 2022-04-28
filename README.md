# Predicting Bike Sharing System Demand: BIOS 735 Final Project

Jiawen Chen, Brooke Felsheim, Elena Kharitonova, Xinjie Qian, and Jairui Tang (Group 1)

## Introduction
This project is a predictive analysis of publicly available bike sharing demand data. This repository contains the source data, code, and results of the analysis.

## Source data

### London bike sharing data

The London bike sharing demand dataset was downloaded from Kaggle (https://www.kaggle.com/datasets/hmavrodiev/london-bike-sharing-dataset) and provided by Transport for London (https://cycling.data.tfl.gov.uk). This dataset contains hourly bike rental count observations over two years, from Jan 04 2015 - Jan 03 2017. The first full consecutive year of data was used as the training set in the analysis, and the second full consecutive year of data was held out as a test set in the analysis.

### Seoul bike sharing data

The Seoul bike sharing demand dataset was downloaded from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand) and provided by the Seoul Metropolitan Government (https://data.seoul.go.kr). This dataset contains hourly bike rental counts over one year, from Dec 1 2017 - Nov 30 2018. This was used as an independent test set in the analysis.

### Washington, D.C. bike sharing data

The Washington, D.C. bike sharing demand dataset was downloaded from Kaggle (https://www.kaggle.com/datasets/marklvl/bike-sharing-dataset) and provided by Capital Bikeshare (https://ride.capitalbikeshare.com/system-data). This dataset contains hourly bike rental counts over two years, from Jan 01 2011 - Dec 31 2012. This was used as an independent test set in the analysis.

## Analysis

The goal of the analysis is to use time-, date- and weather-related information to predict the number of bikes being rented by a bike sharing system at a particular time. For our analysis, we train two different types of models and assess their performance.

### Models 

For the analysis, we train the following two types of models:     

  1. Negative binomial generalized linear mixed model
  2. Random forest model

### Predictor variables

The variables that are used to predict bike rental count in both models are the following:     

  * Hour chunk (00:00 - 8:00, 8:00-16:00, 16:00-24:00)
  * Weekend status (Yes/No)
  * Holiday status (Yes/No)
  * Season (Winter, Spring, Summer, Autumn)
  * Minimum daily temperature (C)
  * Maximum daily temperature (C)
  * Minimum daily humidity (%)
  * Maximum daily humidity (%)
  * Wind speed (m/s)
  * Presence of any rain or snow (Yes/No)
  * Date (mm-dd)

### R Package

The source data and methods used to train and evaluate the models used in our analysis were included in R package named `bikeSharing`. This contents of this package can be found within the `package/` directory of this repository.

The R package can either be installed via github or locally using in R the zipped package source file.

#### Installation via Github
```
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
library(devtools)
install_github("brookefelsheim/bios731-group1/package/bikeSharing")
```
#### Installation via source file
The `bikeSharing_1.0.0.tar.gz` file can be found under the `package/` directory of this repository. Before running the code below, ensure that `bikeSharing_1.0.0.tar.gz` is in the working directory, or specify the full path to the file.
```
install.packages("bikeSharing_1.0.0.tar.gz", repos = NULL)
```
#### Loading package contents
Once the package is installed, the contents can be loaded into R with the following command:
```
library(bikeSharing)
```
The raw and processed versions of the London, Seoul, and Washington, D.C. datasets are stored within the R package framework. The raw datasets and their pre-processing scripts are stored within the `raw-data/` directory of the package, and the processed datasets are found within the `data/` directory of the package. Once the contents of the package are loaded into R, the processed datasets can be immediately accessed as the variables `london`, `seoul`, and `dc`. For example, to view the structure of each processed dataset, the following commands can be run:     
```
str(london)
str(seoul)
str(dc)
```
#### Package methods
The following methods are included in the `bikeSharing` R package to train and evaluate the predictive models for our analysis:

Model training methods: 
  * `MCEM_algorithm`: This function runs the Monte Carlo EM algorithm as a means to estimate the parameters of a negative binomial general mixed model used to predict bike counts.
  * `train_random_forest`: This function trains a random forest model on bike sharing data.

Model evaluation methods:
  * `plot_rf_importance`: This function plots the variable importance of the random forest model.
  * `rf_model_fit`: This function assesses the fit of the trained random forest model on a given dataset.
  * `glmm_model_fit`: This function assesses the fit of the trained negative binomial mixed model on a given dataset.

## Results
The results of the bike sharing analysis are shown and discussed in the `final_report.pdf` file within this repository.
