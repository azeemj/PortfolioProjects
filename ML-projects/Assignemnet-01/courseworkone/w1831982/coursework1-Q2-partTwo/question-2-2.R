## load the needed linbraries
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(knitr)
library(e1071) 
#Install Package
#install.packages("hydroGOF")

#Load Library
library(hydroGOF)
## Prepare scatter plot

#Read data from .csv file

#load CSV and mutating the fields 
data_gb <- read_excel("exchangeGBP.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%  ## removed unwanted columns ,first 
  select(date_in_ymd,everything())
head(data_gb)



# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

# All the variables are normalized
normalized_gbp = data_gb %>%
  mutate(across(2:2, ~normalize(.x)))


#Scatter Plot
plot(normalized_gbp, main ="Scatter Plot", pch=16)



#Fit linear model using OLS
linear_model=lm(gbp_eur~date_in_ymd,normalized_gbp)

#Overlay best-fit line on scatter plot
abline(linear_model)

## Scatter plot displaying actual values and predicted values 
# make a prediction for each X
#Scatter Plot
plot (normalized_gbp, pch=16)
predictedY <- predict(linear_model, normalized_gbp)

# display the predictions
points(data_gb$date_in_ymd, predictedY, col = "blue", pch=16)



rgb <- data_gb$gbp_eur
#Calculate RMSE 
RMSE=rmse(predictedY,data_gb$gbp_eur)


#Scatter Plot
plot(normalized_gbp)
#Regression with SVM
modelsvm <- e1071::svm(gbp_eur~date_in_ymd,normalized_gbp)

predictedYSVM <- predict(modelsvm, normalized_gbp)

#Overlay SVM Predictions on Scatter Plot
points(normalized_gbp$date_in_ymd, predictedYSVM, col = "red", pch=4)

##Calculate parameters of the SVR model

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho

## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predictedYSVM,normalized_gbp$gbp_eur)

## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
tuneResult <- tune(svm, gbp_eur ~ date_in_ymd,  data = normalized_gbp,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

#RMSE
sqrt(0.01115691);

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, normalized_gbp) 

#Plot predicted vs desired data






tuneResult <- tune(svm,gbp_eur ~ date_in_ymd,  data = normalized_gbp,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)



tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data_gb) 
#RMSE
sqrt(0.01067864);







tuneResult <- tune(svm,gbp_eur ~ date_in_ymd,  data = normalized_gbp,
                   ranges = list(epsilon = seq(0,0.5,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)
tunedModel <- tuneResult$best.model

predictedYSVM <- predict(tunedModel, normalized_gbp)

#Overlay SVM Predictions on Scatter Plot using best model
plot(normalized_gbp)
points(normalized_gbp$date_in_ymd, predictedYSVM, col = "red", pch=4)


abline(a=0,b=1)
