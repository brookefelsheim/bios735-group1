# BIOS 735 Final Project

Jiawen Chen, Brooke Felsheim, Elena Kharitonova, Jairui Tang, and Xinjie Qian

## Introduction
This project is a predictive analysis of publicly available bike sharing demand data. This repository contains the source data, code, and results of the analysis.

## Source data

### Seoul bike sharing data

The Seoul bike sharing demand dataset was downloaded from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand) and provided by the Seoul Metropolitan Government (https://data.seoul.go.kr). This was used as the training set in the analysis.

### London bike sharing data

The London bike sharing demand dataset was downloaded from Kaggle (https://www.kaggle.com/datasets/hmavrodiev/london-bike-sharing-dataset) and provided by Transport for London (https://cycling.data.tfl.gov.uk). This was used as a testing set in the analysis.

### Washington, D.C. bike sharing data

The Washington, D.C. bike sharing demand dataset was downloaded from Kaggle (https://www.kaggle.com/datasets/marklvl/bike-sharing-dataset) and provided by Capital Bikeshare (https://ride.capitalbikeshare.com/system-data). This was used as a testing set in the analysis.

## Analysis

The goal of the analysis is to use date- and weather-related information to predict the number of bikes being rented by a bike sharing system at a particular time. For our analysis, we train two different types of models and assess their performance.

### Models 

For the analysis, we train the following two types of models:     

  1. Negative binomial generalized linear mixed model
  2. Random forest model

### Predictor variables

The variables that are used to predict bike rental count are the following:     

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

To allow the negative binomial glmm to run faster, we only use the following variables: hour chunk, maximum daily temperature, presense of rain or snow, and date (random effect). In the random forest model, all 11 predictor variables are used.
