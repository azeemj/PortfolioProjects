knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

exchangeGBP <- read_excel("exchangeGBP.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())

#all the input is in only one dataframe to be able to preserve the testing and training
#dataset for the two sets of input variables
#Preparing multiple attributes using the exsiitng single atrributes,lag function can help us to do this.
gbp_exchange_full_data <- exchangeGBP
gbp_exchange_full_data <-  mutate(gbp_exchange_full_data,previous_one_day_set_a = lag(exchangeGBP$gbp_eur,1))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data,previous_two_days_set_a = lag(exchangeGBP$gbp_eur,2))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data,previous_three_days_set_a = lag(exchangeGBP$gbp_eur,3))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data,previous_four_days_set_a = lag(exchangeGBP$gbp_eur,4))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data, previous_five_days_set_a = lag(exchangeGBP$gbp_eur,5))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data, previous_six_days_set_a = lag(exchangeGBP$gbp_eur,6))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data, previous_seven_days_set_a = lag(exchangeGBP$gbp_eur,7))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data, previous_eight_days_set_a = lag(exchangeGBP$gbp_eur,8))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data, previous_nine_days_set_a = lag(exchangeGBP$gbp_eur,9))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data, previous_ten_days_set_a = lag(exchangeGBP$gbp_eur,10))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data,five_day_rolling = rollmean(gbp_eur,5, fill = NA))
gbp_exchange_full_data <- mutate(gbp_exchange_full_data,ten_day_rolling = rollmean(gbp_eur,10, fill = NA))

#dropping null records
gbp_exchange_full <- drop_na(gbp_exchange_full_data)
summary(gbp_exchange_full)


gbp_exchange_full %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "First Set of Input Variables") +
  theme(legend.position = "none")


gbp_exchange_full %>%
  pivot_longer(cols =c(3,4,5,6),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Set of Input Variables") +
  theme(legend.position = "none")

gbp_exchange_full %>%
  pivot_longer(cols = 6:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Set of Input Variables") +
  theme(legend.position = "none")

gbp_exchange_full %>%
  pivot_longer(cols = 9:14,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Set of Input Variables") +
  theme(legend.position = "none")


# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# All the variables are normalized
normalized_gbp = gbp_exchange_full %>%
  mutate(across(2:14, ~normalize(.x)))
# Look at the data that has been normalized
summary(normalized_gbp)

set.seed(123)
gbp_train <- normalized_gbp[1:400,]
gbp_test <- normalized_gbp[401:480,]

# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }
# Get the min and max of the original training values
gbp_min_train <- min(gbp_exchange_full[1:400,2])
gbp_max_train <- max(gbp_exchange_full[1:400,2])
# Get the min and max of the original testing values
gbp_min_test <- min(gbp_exchange_full[401:480,2])
gbp_max_test <- max(gbp_exchange_full[401:480,2])
# Check the range of the min and max of the training dataset
gbp_min_test

gbp_min_train

gbp_max_test
gbp_max_train




relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}




set.seed(123)
# function setup that creates 2 layer model
model_two_hidden_layers = function(hidden,sec_hidden) {
  nn_model_true = neuralnet(gbp_eur ~ previous_one_day_set_a+previous_two_days_set_a
                            +previous_three_days_set_a+previous_four_days_set_a
                            +previous_five_days_set_a+previous_six_days_set_a
                            +previous_seven_days_set_a+previous_eight_days_set_a
                            +previous_nine_days_set_a+previous_ten_days_set_a
                            +five_day_rolling+ten_day_rolling
                            , data=gbp_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn_model_true,gbp_test[,2:14])
  truthcol = gbp_exchange_full[401:480,2]$gbp_eur
  predcol = unnormalize(train_results$net.result,gbp_min_train, gbp_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}
# creation of different models with varying number of nodes
results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_two_hidden_layers(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe
set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])


###fit the hidden layer as best results
nn_model_true = neuralnet(gbp_eur ~ gbp_eur ~ previous_one_day_set_a+previous_two_days_set_a
                          +previous_three_days_set_a+previous_four_days_set_a
                          +previous_five_days_set_a+previous_six_days_set_a
                          +previous_seven_days_set_a+previous_eight_days_set_a
                          +previous_nine_days_set_a+previous_ten_days_set_a
                          +five_day_rolling+ten_day_rolling, data=gbp_train, hidden=c(
  7,4), linear.output=TRUE)
train_results = compute(nn_model_true,gbp_test[,2:14])
truthcol = gbp_exchange_full[401:480,2]$gbp_eur
predcol = unnormalize(train_results$net.result,gbp_min_train,
                      gbp_max_train)[,1]
relevant_pred_stat(truthcol,predcol,
                   "Two Hidden Layers") %>%
  mutate(hiddel_layers = paste0(2, " and ",3),
         input_set = "A") %>%
  filter(.metric != "rsq")
plot(nn_model_true)
##truth column vs predict column
plot(predcol,truthcol, col='red',
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

results <- data.frame(actual = truthcol, predicted = predcol)


deviation=((results[,1]-results[,2])/results[,1])
comparison=data.frame(results[,2],results[,1],deviation)
accuracy=1-abs(mean(deviation))
accuracy





