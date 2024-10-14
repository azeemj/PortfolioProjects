
library(tidyverse)
library(tidymodels)
install.packages("corrplot")
library('caret')
#install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
#library(magrittr) # needs to be run every time you start R and want to use %>%
   # alternatively, this also loads %>%
install.packages("RANN")
library('RANN')
install.packages("ggplot2") # Install it again
library(ggplot2)



library(dplyr) 
# Load the dataset
diabetes_data <- read.csv('diabetes.csv') %>% janitor::clean_names()

head(diabetes_data) # # visualize the header of Pima data
summary(diabetes_data)
str(diabetes_data)
#Need to change the class variable to be a factor variable
diabetes_data$outcome <- factor(diabetes_data$outcome, labels = c("No", "Yes"))
diabetes_data_cleaned =diabetes_data
str(diabetes_data_cleaned)
summary(diabetes_data_cleaned) 





##Age waise analysis
db = diabetes_data_cleaned
db$Age_Cat <- ifelse(db$age < 21, "<21", 
                     ifelse((db$age>=21) & (db$age<=25), "21-25", 
                            ifelse((db$age>25) & (db$age<=30), "25-30",
                                   ifelse((db$age>30) & (db$age<=35), "30-35",
                                          ifelse((db$age>35) & (db$age<=40), "35-40",
                                                 ifelse((db$age>40) & (db$Age<=50), "40-50",
                                                        ifelse((db$age>50) & (db$age<=60), "50-60",">60")))))))

library(ggplot2)
ggplot(aes(x = age), data=db) +
  geom_histogram(binwidth=1, color='black', fill = "#F79420") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("No of people by age")

# Barplot by Age_Cat
library(ggplot2)
ggplot(aes(x = Age_Cat), data = db) +
  geom_bar(fill='steelblue')

###result Most of the subjects are in between the ages 21 - 30

# check outliers before normalzing 
# box plot of Age_Cat vs BMI
library(ggplot2)
ggplot(aes(x=Age_Cat, y = bmi), data = db) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,70))




##We use “sapply”" to check the number of missing values in each columns.
sapply(diabetes_data_cleaned, function(x) sum(is.na(x)))

#Let’s produce the matrix of scatterplots
pairs(diabetes_data_cleaned, panel = panel.smooth)

biological_data_investigation <- diabetes_data_cleaned[,setdiff(names(diabetes_data_cleaned), c('outcome', 'pregnancies'))]
features_miss_num <- apply(biological_data_investigation, 2, function(x) sum(x<=0))
features_miss <- names(biological_data_investigation)[ features_miss_num > 0]
features_miss_num

#how many rows are affected
rows_errors <- apply(biological_data_investigation, 1, function(x) sum(x<=0)>1) 
sum(rows_errors)

sum(rows_errors)/nrow(biological_data_investigation)



#We can’t get rid off these rows. We are going to try to impute missing data

#Imputing missing values using median
library('RANN')
library('caret')
preProcValues <- preProcess(biological_data_investigation, method = c("medianImpute","center","scale"))
sum(is.na(preProcValues))

data_processed <- predict(preProcValues, diabetes_data_cleaned)

sum(is.na(data_processed))
summary(data_processed)
str(data_processed)

#proportion of the outcome output
prop.table(table(data_processed$outcome))


##corerelatinos
# calculate correlation matrix
install.packages("corrplot")
library(corrplot)
correlat <- cor(data_processed[, setdiff(names(data_processed), 'outcome')])
print(correlat)
corrplot(correlat)
metric <- "ROC"

library(mlbench)
library(caret)
library(caretEnsemble)
## Univariable analysis
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs = TRUE, summaryFunction = twoClassSummary)
# train the model
str(data_processed);
model <- train(outcome~., data=data_processed, method="lvq",tuneLength=10,
               preProcess = c('center', 'scale'),
               trControl=control, metric=metric,type = "Classification")
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


# Create train and test data sets
trainIndex = createDataPartition(data_processed$outcome, p=0.7, list=FALSE)
train_set = data_processed[trainIndex,]
test_set = data_processed[-trainIndex,]
seed <- 10


levels(data_processed$outcome) <- make.names(levels(factor(data_processed$outcome)))

##repeated cv method
set.seed(seed)
#"Controller
bagcontrol <- trainControl(sampling="rose",method="repeatedcv", number=10,
                           repeats=3, classProbs = TRUE, summaryFunction = twoClassSummary)


library(pROC)
library(caTools)
# Gradient Boosting
set.seed(seed)
fit.gbm <- train(outcome~., data=train_set, method="gbm", metric=metric,
                 preProcess = c('center', 'scale'),
                 trControl=bagcontrol, verbose=FALSE)
pred_gbm <- predict(fit.gbm, test_set)
confusionMatrix(pred_gbm, test_set$outcome)
pred_prob_gbm <- predict(fit.gbm, test_set, type="prob")
roc_gbm <- roc(test_set$outcome, pred_prob_gbm$Yes)
colAUC(pred_prob_gbm$Yes, test_set$outcome, plotROC = TRUE)


set.seed(seed)
fit.c50 <- train(outcome~., data=train_set, method="C5.0", metric=metric, trControl=bagcontrol)

results_boost <- resamples(list( gbm = fit.gbm, c50 = fit.c50))
# Compare models
dotplot(results_boost)




# Bagging Algorithm (Random Forest)
set.seed(seed)
fit.rf <- train(outcome~., data=train_set, method="rf", metric=metric, trControl=bagcontrol)
# evaluate results on test set
test_set$pred <- predict(fit.rf, newdata=test_set)

#test_set$outcome <- as.factor(test_set$outcome)

str(test_set)
confusionMatrix(data = test_set$pred, test_set$outcome)
pred_fit.rf <- predict(fit.rf, test_set, type="prob")
roc_fit.rf <- roc(test_set$outcome, pred_fit.rf$Yes)
colAUC(pred_fit.rf$Yes, test_set$outcome, plotROC = TRUE)


# Bagged CART
set.seed(seed)
fit.treebag <- train(outcome~., data=train_set, method="treebag", metric=metric, trControl=bagcontrol)
pred_fit.treebag <- predict(fit.treebag, newdata=test_set)
confusionMatrix(data = pred_fit.treebag, reference = test_set$outcome)



# evaluate results on test set
test_set$pred <- predict(fit.rf, newdata=test_set)
confusionMatrix(data = test_set$pred, reference = test_set$outcome)




#naive bayes
set.seed(seed)
fit.nb <- train(outcome~., data=train_set, method="nb", metric=metric, trControl=bagcontrol)
pred_fit.nb <- predict(fit.nb, newdata=test_set)
confusionMatrix(data = pred_fit.nb, reference = test_set$outcome)






library(caret)
library(caretEnsemble)

sum(is.na(data_processed))

# Stacking Algorithms
stack_control <- trainControl(sampling="rose",method="repeatedcv", number=10, repeats=3,
                        savePredictions='final', classProbs=TRUE, summaryFunction = twoClassSummary)
algorithmList <- c( 'knn','rpart','nb')
set.seed(seed)
str(train_set);
#levels(train_set$outcome) <- make.names(levels(factor(train_set$outcome)))
stack_models <- caretList(outcome~., data=train_set, trControl=stack_control,
                          methodList=algorithmList, metric = "ROC" )
stacking_results <- resamples(stack_models)
summary(stacking_results)
dotplot(stacking_results)
names(stack_models)
lapply(stack_models,"[[","results")
# Check correlation between models to ensure the results are uncorrelated and can be
modelCor(stacking_results)
splom(stacking_results)


# stack using nb
set.seed(seed)
#start time
start_time <- Sys.time()
stack_nb_model <- caretStack(stack_models, method="nb", metric = metric,
                        trControl=stack_control)
end_time <- Sys.time()

timediffrences <- (end_time - start_time)
print(timediffrences)


print(stack_nb_model)
stack.nb.pred <- predict(stack_nb_model$models$nb, newdata=test_set)

confusionMatrix(data = stack.nb.pred, reference = test_set$outcome)




pred_fit_stack.nb <- predict(stack_nb_model$models$nb, newdata=test_set, type="prob")
roc_fit.stack.nb <- roc(test_set$outcome, pred_fit_stack.nb$Yes)
colAUC(pred_fit_stack.nb$Yes, test_set$outcome, plotROC = TRUE)


# stack using knn
set.seed(seed)
#start time
start_time <- Sys.time()
stack_model.knn <- caretStack(stack_models, method="knn", metric = metric,
                       trControl=stack_control)
end_time <- Sys.time()

timediffrences <- (end_time - start_time)
print(timediffrences)



print(stack.knn)
pred_fit_stack.knn <- predict(stack_nb_model$models$knn, newdata=test_set)
confusionMatrix(data = pred_fit_stack.knn, reference = test_set$outcome)

pred_fit_stack.knn <- predict(stack_nb_model$models$knn, newdata=test_set, type="prob")
roc_fit.stack.nb <- roc(test_set$outcome, pred_fit_stack.knn$Yes)
colAUC(pred_fit_stack.knn$Yes, test_set$outcome, plotROC = TRUE)


# stack using rpart
set.seed(seed)
#start time
start_time <- Sys.time()
stack_rpart_model.rpart <- caretStack(stack_models, method="rpart", metric = metric,
                        trControl=stack_control)
end_time <- Sys.time()

timediffrences <- (end_time - start_time)
print(timediffrences)


print(stack.rpart)
pred_fit_stack.rpart <- predict(stack_rpart_model.rpart, newdata=test_set)
confusionMatrix(data =pred_fit_stack.rpart, reference = test_set$outcome)

pred_fit_stack.rpart <- predict(stack_rpart_model.rpart$models$rpart, newdata=test_set, type="prob")
roc_fit.stack.nb <- roc(test_set$outcome, pred_fit_stack.rpart$Yes)
colAUC(pred_fit_stack.rpart$Yes, test_set$outcome, plotROC = TRUE,alg=c("Wilcoxon","ROC"))







