rm(list=ls())
library(ggplot2)
library(dplyr)
library(ggrepel)
library(plyr)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(ggthemes)
library(caret)
library(grid)
library(MASS)
library(randomForest)
library(party)
library(rpart)
library(rpart.plot)
library(e1071)
library(MLeval)
library(NeuralNetTools)
library(matrixStats)
library(class)
library(sqldf)
library(ipred)
library(mice)
library(ROSE)
library(caretEnsemble)
library(survival)
library(parallel) 
library(doParallel) 
library(randomForest) 
library(fastAdaboost) 

#Setting Directory
setwd("~/Desktop/Quarter 4/Business Consulting/Project Details/After Mid")

#Reading the file
Mortgageloan <- read.csv("loan_mortgage.csv")

#No of Rows
nrow(Mortgageloan)
#No of Col
ncol(Mortgageloan)

head(Mortgageloan)
tail(Mortgageloan)
#------------------------------------------
## Aggregating the data
#------------------------------------------

#Filtering the data by first observation time is equal to first origination time
Mortgage1 <- Mortgageloan %>%
  filter(Mortgageloan$first_ob_time == Mortgageloan$origin_time)

#Filtering the data where time = max
Mortgage2 <- Mortgage1 %>% group_by(MID) %>% slice(which.max(time))

#Creating new variables representing Average and Standard Deviation
Mortgage2$AvgLTV_time <- rowMeans(x = Mortgage2[ ,c("LTV", "LTV_orig_time")])
Mortgage2$SDLTV_time <- rowSds(as.matrix(Mortgage2[ ,c("LTV", "LTV_orig_time")]))

Mortgage2$Avgbalance<- rowMeans(x = Mortgage2[ ,c("balance", "balance_orig_time")])
Mortgage2$SDbalance <- rowSds(as.matrix(Mortgage2[ ,c("balance", "balance_orig_time")]))

Mortgage2$Avginterest_rate_time <- rowMeans(x = Mortgage2[ ,c("interest_rate", 
                                                              "Interest_Rate_orig_time")])
Mortgage2$SDinterest_rate_time <- rowSds(as.matrix(Mortgage2[ ,c("interest_rate", 
                                                                 "Interest_Rate_orig_time")]))

Mortgage2$Avghpi_time <- rowMeans(x = Mortgage2[ ,c("hpi", "hpi_orig_time")])
Mortgage2$SDhpi_time <- rowSds(as.matrix(Mortgage2[ ,c("hpi", "hpi_orig_time")]))

#--------------------------------------------------
MortgageAvg_gdp <- aggregate(x=Mortgageloan$gdp, by = list(Mortgageloan$MID),
                             FUN = "mean")
MortgageAvg_uer <- aggregate(x=Mortgageloan$uer, by = list(Mortgageloan$MID),
                             FUN = "mean")
MortgageSD_gdp <- aggregate(x=Mortgageloan$gdp, by = list(Mortgageloan$MID),
                            FUN = "sd")
MortgageSD_uer <- aggregate(x=Mortgageloan$uer, by = list(Mortgageloan$MID),
                            FUN = "sd")

colnames(MortgageAvg_gdp) <- c("MID", "Avg_gdp_time")
colnames(MortgageSD_uer) <- c("MID", "sd_uer_time")
colnames(MortgageAvg_uer) <- c("MID", "Avg_uer_time")
colnames(MortgageSD_gdp) <- c("MID", "sd_gdp_time")

#Merging different data frames
Mortgage3 <- Mortgage2 %>%
  left_join(MortgageAvg_gdp, by = c("MID" = "MID"))
Mortgage4 <- Mortgage3 %>%
  left_join(MortgageAvg_uer, by = c("MID" = "MID"))
Mortgage5 <- Mortgage4 %>%
  left_join(MortgageSD_gdp, by = c("MID" = "MID"))
Mortgage_New <- Mortgage5 %>%
  left_join(MortgageSD_uer, by = c("MID" = "MID"))

#------------------------------------------
## Data Quality
#------------------------------------------

#Identifying the missing value rows
Mortgage_New[!complete.cases(Mortgage_New), ]
na_rows <- rownames(Mortgage_New)[!complete.cases(Mortgage_New)]
na_rows
colSums(is.na(Mortgage_New))

#Imputing the value from the actual column
Mortgage_New$sd_gdp_time[is.na(Mortgage_New$sd_gdp_time)] <- 
  Mortgage_New$gdp[is.na(Mortgage_New$sd_gdp_time)]
Mortgage_New$sd_uer_time[is.na(Mortgage_New$sd_uer_time)] <- 
  Mortgage_New$uer[is.na(Mortgage_New$sd_uer_time)]
colSums(is.na(Mortgage_New))

#Checking for duplicate values
Mortgage_New[duplicated(x = Mortgage_New), ]
Mortgage_New1 <- Mortgage_New[!duplicated(x = Mortgage_New), ]

#---------------------------------------------------
## Exploratory Data Analysis
##---------------------------------------------------

## Preparing (Y) - Target Variable
Mortgage_New1$status_time <- factor(Mortgage_New1$status_time)

Balance_plot <- ggplot(data = Mortgage_New1, 
                       mapping = aes(x = balance, fill = status_time)) +
  geom_histogram(bins = 100, alpha = 0.9, colour = "black",
                 lwd = 0.25, linetype = 1)
Balance_plot +
  xlim(0,2500000) +
  scale_fill_discrete(labels = c("others", "default", "payoff"))+
  ggtitle("Status visualized over Balance amount") 

HPI_plot <- ggplot(data = Mortgage_New1, 
                   mapping = aes(x = hpi, fill = status_time)) +
  geom_histogram(bins = 25, alpha = 0.9, colour = "black",
                 lwd = 0.25, linetype = 1)
HPI_plot +
  scale_fill_discrete(labels = c("others", "default", "payoff"))+
  ggtitle("Status visualized over House Price Index") 


UER_plot <- ggplot(data = Mortgage_New1, 
                   mapping = aes(x = Avg_uer_time, fill = status_time)) +
  geom_histogram(bins = 50, alpha = 0.9, colour = "black",
                 lwd = 0.25, linetype = 1)
UER_plot +
  scale_fill_discrete(labels = c("others", "default", "payoff"))+
  ggtitle("Status visualized over Unemployement Rate") 


GDP_plot <- ggplot(data = Mortgage_New1, 
                   mapping = aes(x = Avg_gdp_time, fill = status_time)) +
  geom_histogram(bins = 30, alpha = 0.9, colour = "black",
                 lwd = 0.25, linetype = 1)
GDP_plot +
  scale_fill_discrete(labels = c("others", "default", "payoff"))+
  ggtitle("Status visualized over GDP") 

#------------------------------------------
## Nominal, Ordinal & Numerical
#------------------------------------------

#Dropping irrelevant variable
drop <- c("balance","LTV", "interest_rate", "hpi","gdp", "balance_orig_time",
          "LTV_orig_time", "Interest_Rate_orig_time", "hpi_orig_time",
          "uer", "time", "default", "payoff")
Mortgage_New1 <- Mortgage_New[,!(names(Mortgage_New) %in% drop)]
colSums(is.na(Mortgage_New1))

## Preparing (Y) - Target Variable
Mortgage_New1$status_time <- factor(Mortgage_New1$status_time)
plot(Mortgage_New1$status_time, main = "Status Time",
     col = "light blue", 
     names.arg=c("Non Default/Non Payoff","Default","Payoff"))

#Nominal Variable
nominal <- c("RE_Type", "investor")
Mortgage_New1[ ,nominal] = lapply(X = Mortgage_New1[ ,nominal], 
                                  FUN = factor)

#Numerical variable
numerical <- names(Mortgage_New1)[!names(Mortgage_New1) %in% c(nominal,"MID",
                                                               "status_time")]

#Check for Co-relations Matrix                
mortgage_cor <- cor(x = Mortgage_New1[ ,numerical])
corrplot(mortgage_cor)
symnum(x = mortgage_cor,corr = TRUE)
high_corrs <- findCorrelation(x = mortgage_cor, cutoff = 0.75,names = TRUE)
high_corrs

#Removing highly correlation variable
numerical <- numerical[!numerical %in% high_corrs]
numerical

#Creating Vector
vars <- c(nominal, numerical)

#Summary Information
summary(Mortgage_New1[,c(vars, "status_time")])

#---------------------------------------------------
######### Training and Testing #########
#---------------------------------------------------
# Initialize random seed
set.seed(1000) 

# Create list of training indices
sub <- createDataPartition(y = Mortgage_New1$status_time, 
                           p = 0.80, 
                           list = FALSE)

# Subset the transformed data
train <- Mortgage_New1[sub, ] 
test <- Mortgage_New1[-sub, ] 

#------------------------------------------
######### Decision Trees #########
#------------------------------------------

## Analysis

#Basic model
mortgage.rpart <- rpart(formula = status_time ~ ., 
                        data = train[ ,c(vars, "status_time")],
                        method = "class")

#Basic model output and importance variable
mortgage.rpart
mortgage.rpart$variable.importance

#Tree Plots
prp(x = mortgage.rpart,
    extra = 2)

#------------------------------------------
### 2. Hyperparameter Tuning Model
#------------------------------------------

grid <- expand.grid(cp = seq(from = 0,
                              to = 0.05,
                              by = 0.005))
grid

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     search = "grid")

set.seed(1000)

DTFit <- train(form = status_time ~ ., 
               data = train[ ,c(vars, "status_time")], 
               method = "rpart", 
               trControl = ctrl, 
               tuneGrid = grid) 
DTFit

DTFit$results[DTFit$results$cp %in% DTFit$bestTune,] 


# We can plot the cp value vs. Accuracy
plot(DTFit)

# We can view the confusion matrix showing
confusionMatrix(DTFit)

DTFit$finalModel$variable.importance

#------------------------------------------
## Tuned Model Performance
#------------------------------------------
### Training Performance
tune_trainpreds <- predict(object = DTFit,
                        newdata = train)

DT_traintune_conf <- confusionMatrix(data = tune_trainpreds, 
                                  reference = train$status_time, 
                                  positive = "1",
                                  mode = "everything")
DT_traintune_conf

## Testing Performance
tune_testpreds <- predict(object = DTFit,
                        newdata = test)
DT_testtune_conf <- confusionMatrix(data = tune_testpreds, 
                                  reference = test$status_time, 
                                  positive = "1",
                                  mode = "everything")
DT_testtune_conf

## Goodness of Fit
# Overall
cbind(Training = DT_traintune_conf$overall,
      Testing = DT_testtune_conf$overall)

# Class-Level
cbind(Training = DT_traintune_conf$byClass,
      Testing = DT_testtune_conf$byClass)

##----------------------------------------------
#Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
DT_train_up <- upSample(x = train[ ,c(vars, "status_time")], 
                     y = train$status_time, 
                     yname = "status_time") 

par(mfrow = c(1,2))
plot(train$status_time, main = "Original", col = "lavender")
plot(DT_train_up$status_time, main = "ROS", col = "steel blue")
par(mfrow = c(1,1)) 

# Sampling UP
ctrl$sampling <- "up"

#Initialize random seed
set.seed(1000)

DTFit_ROS <- train(form = status_time ~ ., 
                   data = train[ ,c(vars, "status_time")], 
                   method = "rpart",
                   trControl = ctrl,
                   tuneGrid = grid) 

DTFit_ROS


#Variable Importance
DTfit_varimp_ROS <- DTFit_ROS$finalModel$variable.importance
barplot(DTfit_varimp_ROS,horiz = TRUE, las = 1, cex.names=0.40, col = "#C7CEEA",
        font = 4, main = "Variable Importance")

# Training Performance
Tune_train_preds <- predict(object = DTFit_ROS,
                            newdata = train)
DT_ROS_traintune_conf <- confusionMatrix (data = Tune_train_preds, 
                                      reference = train$status_time, 
                                      positive = "1",
                                      mode = "everything")
DT_ROS_traintune_conf

# Testing Performance
tune_test_preds <- predict(object = DTFit_ROS,
                           newdata = test)

DT_ROS_testtune_conf <- confusionMatrix(data = tune_test_preds, 
                                    reference = test$status_time, 
                                    positive = "1",
                                    mode = "everything")
DT_ROS_testtune_conf

## Goodness of Fit
# Overall
cbind(Training = DT_ROS_traintune_conf$overall,
      Testing = DT_ROS_testtune_conf$overall)

# Class-Level
cbind(Training = DT_ROS_traintune_conf$byClass,
      Testing = DT_ROS_testtune_conf$byClass)

#Comparing the Model
# Overall
cbind(Tuned = DT_testtune_conf$overall,
      Oversample = DT_ROS_testtune_conf$overall)

#Class-Level
cbind(Tuned = DT_testtune_conf$byClass,
      Oversample = DT_ROS_testtune_conf$byClass)

##-------------------------------------------------
#KNN Data Preprocessing & Transformation
##-------------------------------------------------

#Rescaling the Numerical Variables
cen_mm <- preProcess(x = Mortgage_New1[ ,numerical],
                     method = "range")
Mortgage_mm <- predict(object = cen_mm,
                       newdata = Mortgage_New1)

##Binarization of Nominal Variables

RE_Type <- dummyVars(formula =  ~ RE_Type,
                     data = Mortgage_mm)
RE_Type_dums <- predict(object = RE_Type, 
                        newdata = Mortgage_mm)

investor <- dummyVars(formula =  ~ investor,
                      data = Mortgage_mm)
investor_dums <- predict(object = investor, 
                         newdata = Mortgage_mm)

Mortgage_KNN_dum <- data.frame(Mortgage_mm[ ,!names(Mortgage_mm) %in% c("RE_Type",
                                                                        "investor")], RE_Type_dums, investor_dums)


#Creating Vector
vars_knn <- names(Mortgage_KNN_dum)[!names(Mortgage_KNN_dum) %in% c("status_time",
                                    "origin_time", "sd_gdp_time")]
vars_knn

# initialize the random seed
set.seed(1000)

subset <- createDataPartition(y = Mortgage_KNN_dum$status_time, 
                              p = 0.80,list = FALSE)

#Training and Testing Set
train_knn <- Mortgage_KNN_dum[subset, ]
test_knn <- Mortgage_KNN_dum[-subset, ]

##-------------------------------------------------
# KNN Analysis
##-------------------------------------------------
#Basic KNN Model
ceiling(sqrt(nrow(train_knn)))

knn_pred <- knn(train = train_knn[ ,vars_knn],
                test = test_knn[ ,vars_knn], 
                cl = train_knn$status_time, 
                k = 81)


conf_basic <- confusionMatrix(data = knn_pred,
                              reference = test_knn$status_time, 
                              positive = "1",
                              mode = "everything")
conf_basic

##-------------------------------------------------
### KNN Hyperparameter Tuning 
##-------------------------------------------------

ctrl_knn <- trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 5)
set.seed(1000)

knnFit  <- train(x = train_knn[ ,vars_knn], 
                 y = train_knn[ ,"status_time"], 
                 preProcess = "range", 
                 method = "knn", 
                 trControl = ctrl_knn,
                 tuneLength = 15) 
knnFit

plot(knnFit)

# Confusion matrix
confusionMatrix(knnFit)

#------------------------------------------
### Model Performance
#------------------------------------------

#Training Performance
knn_preds <- predict(object = knnFit, 
                    newdata = test_knn[ , vars_knn]) 

knn_conf_tuned <- confusionMatrix(data = knn_preds, 
                              reference = test_knn[, "status_time"], 
                              positive = "1",
                              mode = "everything")
knn_conf_tuned

## Overall Model Performance 
knn_conf_tuned$overall[c("Accuracy", "Kappa")]

## Class-Level Model Performance
knn_conf_tuned$byClass

## Comparing Base & Tuned Models

# Overall Model Performance
cbind(Base = conf_basic$overall,
      Tuned = knn_conf_tuned$overall)

# Class-Level Model Performance
cbind(Base = conf_basic$byClass,
      Tuned = knn_conf_tuned$byClass)

##----------------------------------------------
# KNN Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
knn_train_up <- upSample(x = train_knn[ ,c(vars_knn, "status_time")], 
                     y = train_knn$status_time, 
                     yname = "status_time") 

par(mfrow = c(1,2))
plot(train_knn$status_time, main = "Original", col = "lavender")
plot(knn_train_up$status_time, main = "ROS", col = "steel blue")
par(mfrow = c(1,1)) 

# Sampling UP
ctrl_knn$sampling <- "up"

#Initialize random seed
set.seed(1000)

KNN_ROS <- train(x = knn_train_up[ ,vars_knn], 
                 y = knn_train_up[ ,"status_time"], 
                 preProcess = "range", 
                 method = "knn", 
                 trControl = ctrl_knn,
                 tuneLength = 15) 
KNN_ROS

# Variable Importance
plot(KNN_ROS)


### Testing Performance
tune.test.preds <- predict(object = KNN_ROS,
                           newdata = test_knn)

KNN_ROS_testtune_conf <- confusionMatrix(data = tune.test.preds, # predictions
                                         reference = test_knn$status_time, # actual
                                         positive = "1",
                                         mode = "everything")
KNN_ROS_testtune_conf

#Overall
KNN_ROS_testtune_conf$overall
#ByClass
KNN_ROS_testtune_conf$byClass

##Comparing the Model
#Overall
cbind(Tuned = knn_conf_tuned$overall,
      Oversampling = KNN_ROS_testtune_conf$overall)

#By Class
cbind(Tuned = knn_conf_tuned$byClass,
      Oversampling = KNN_ROS_testtune_conf$byClass)

#------------------------------------------
## Naive Bayes Analysis
#------------------------------------------
## Analysis

aggregate(train[ ,c(nominal)],
          by = list(train$status_time),
          FUN = table)


mortgage_nb <- naiveBayes(x = train[ ,vars],
                     y = train$status_time,
                     laplace = 1)
mortgage_nb

#------------------------------------------
### Model Performance & Fit
#------------------------------------------

## Training Performance
nb_train <- predict(object = mortgage_nb, 
                    newdata = train[ ,vars], 
                    type = "class")

head(nb_train)

nb_train_conf <- confusionMatrix(data = nb_train, 
                              reference = train$status_time,
                              positive = "1",
                              mode = "everything")
nb_train_conf

## Testing Performance
nb_test <- predict(object = mortgage_nb, 
                   newdata = test[ ,vars],
                   type = "class")

nb_test_conf <- confusionMatrix(data = nb_test, 
                             reference = test$status_time, 
                             positive = "1",
                             mode = "everything")
nb_test_conf

# Overall Performance
nb_test_conf$overall[c("Accuracy", "Kappa")]

# Class-Level Performance
nb_test_conf$byClass

##Goodness of Fit
# Overall
cbind(Training = nb_train_conf$overall,
      Testing = nb_test_conf$overall)

# Class-Level
cbind(Training = nb_train_conf$byClass,
      Testing = nb_test_conf$byClass)

##---------------------------------------------------------
## Naive Bayes Handling Imbalance
##---------------------------------------------------------
## Oversampling
nb_train_up <- upSample(x = train[ ,vars], 
                     y = train$status_time, 
                     yname = "status_time") 

par(mfrow = c(1,2)) 
plot(train$status_time, main = "Original", col = "lavender")
plot(nb_train_up$status_time, main = "RUS", col = "steel blue")
par(mfrow = c(1,1)) 

mortgage_nb$sampling <- "up"

set.seed(1000)

mortgage_nb_ROS <- naiveBayes(x = nb_train_up[ ,vars],
                         y = nb_train_up$status_time,
                         laplace = 1)

mortgage_nb_ROS
#------------------------------------------
## Model Performance & Fit
#------------------------------------------
# Training Performance

nb_train_ROS <- predict(object = mortgage_nb_ROS, 
                        newdata = nb_train_up[ ,vars], 
                        type = "class")
head(nb_train_ROS)
nb_train_conf_ROS <- confusionMatrix(data = nb_train_ROS, # predictions
                                  reference = nb_train_up$status_time, # actual
                                  positive = "1",
                                  mode = "everything")
nb_train_conf_ROS

# Testing Performance

nb_test_ROS <- predict(object = mortgage_nb_ROS, 
                       newdata = test[ ,vars], 
                       type = "class")

nb_test_conf_ROS <- confusionMatrix(data = nb_test_ROS, 
                                 reference = test$status_time, 
                                 positive = "1",
                                 mode = "everything")
nb_test_conf_ROS

# Overall Performance
nb_test_conf_ROS$overall[c("Accuracy", "Kappa")]

# Class-Level Performance
nb_test_conf_ROS$byClass

## Goodness of Fit
# Overall
cbind(Training = nb_train_conf_ROS$overall,
      Testing = nb_test_conf_ROS$overall)

# Class-Level
cbind(Training = nb_train_conf_ROS$byClass,
      Testing = nb_test_conf_ROS$byClass)

##Compare the Models
#Overall
cbind(Base = nb_test_conf$overall,
      Oversampling = nb_test_conf_ROS$overall)

#By Class
cbind(Base = nb_test_conf$byClass,
      Oversampling = nb_test_conf_ROS$byClass)

##----------------------------------------
# Analysis - SVM
##----------------------------------------

#Base Model - Sigmoid Kernel

set.seed(1000)

sig_mod <- svm(formula = status_time ~ .,
               data = train[ ,c(vars, "status_time")],
               method = "C-classification", 
               kernel = "sigmoid", 
               scale = TRUE)

summary(sig_mod)

## Training Performance
head(sig_mod$fitted)

sig_train_conf <- confusionMatrix(data = sig_mod$fitted, 
                                  reference = train$status_time, 
                                  positive = "1",
                                  mode = "everything")
sig_train_conf

#Base Model - Linear Kernel
set.seed(1000)

lin_mod <- svm(formula = status_time ~ .,
               data = train[ ,c(vars, "status_time")],
               method = "C-classification", 
               kernel = "linear", 
               scale = TRUE)

summary(lin_mod)

## Training Performance
head(lin_mod$fitted)

lin_train_conf <- confusionMatrix(data = lin_mod$fitted, 
                                  reference = train$status_time, 
                                  positive = "1",
                                  mode = "everything")
lin_train_conf

#Base Model - Radial Kernel
set.seed(1000)

rad_mod <- svm(formula = status_time ~ .,
               data = train[ ,c(vars, "status_time")],
               method = "C-classification", 
               kernel = "radial", 
               scale = TRUE)

summary(rad_mod)

## Training Performance
head(rad_mod$fitted)

rad_train_conf <- confusionMatrix(data = rad_mod$fitted, 
                                  reference = train$status_time, 
                                  positive = "1",
                                  mode = "everything")
rad_train_conf

## View training fit for the 3 kernels
#Overall
cbind(sigmoid = sig_train_conf$overall,
      linear = lin_train_conf$overall,
      radial = rad_train_conf$overall)

#By Class
cbind(sigmoid = sig_train_conf$byClass,
      linear = lin_train_conf$byClass,
      radial = rad_train_conf$byClass)

## We are continuing  with radial kernel and tune our hyperparameters
##--------------------------------------------
### SVM Hyperparameter Tuning 
##--------------------------------------------

grid_svm <-  expand.grid(C = seq(from = 1, 
                              to = 5, 
                              by = 1),
                      sigma = seq(from = 0.01,
                                  to = 0.12,
                                  by = 0.02))

grid_svm

control_SVM <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 5,
                            search = "grid",
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary) 
#Intialize seed
set.seed(1000)

#SVM FIT
SVMFit <- svm(form = status_time ~ ., 
              data = train[ ,c(vars, "status_time")], 
              type = "C-classification",
              kernel = "radial",
              preProcess = c("center", "scale"), 
              trControl = control_SVM, 
              tuneGrid = grid_svm, 
              tuneLength = 15,
              metric = "ROC",
              scale = TRUE) 
summary(SVMFit)

## Model Performance
### Training Performance
tune.train.preds <- predict(object = SVMFit,
                         newdata = train)

SVM_traintune_conf <- confusionMatrix(data = tune.train.preds, 
                                   reference = train$status_time, 
                                   positive = "1",
                                   mode = "everything")
SVM_traintune_conf

### Testing Performance
tune.test.preds <- predict(object = SVMFit,
                         newdata = test)

SVM_testtune_conf <- confusionMatrix(data = tune.test.preds, 
                                   reference = test$status_time, 
                                   positive = "1",
                                   mode = "everything")
SVM_testtune_conf

##Goodness of Fit

# Overall
cbind(Training = SVM_traintune_conf$overall,
      Testing = SVM_testtune_conf$overall)

# Class-Level
cbind(Training = SVM_traintune_conf$byClass,
      Testing = SVM_testtune_conf$byClass)

##----------------------------------------------
#Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
svm_train_up <- upSample(x = train[ ,c(vars, "status_time")], 
                     y = train$status_time, 
                     yname = "status_time") 

par(mfrow = c(1,2))
plot(train$status_time, main = "Original", col = "lavender")
plot(svm_train_up$status_time, main = "ROS", col = "steel blue")
par(mfrow = c(1,1)) 

# Sampling UP
control_SVM$sampling <- "up"

#Initialize random seed
set.seed(1000)

SVMFit_ROS <- svm(form = status_time ~ ., 
                  data = svm_train_up[ ,c(vars, "status_time")], 
                  type = "C-classification",
                  kernel = "radial",
                  preProcess = c("center", "scale"), 
                  trControl = control_SVM, 
                  tuneGrid = grid_svm, 
                  tuneLength = 15,
                  metric = "ROC",
                  scale = TRUE) 
SVMFit_ROS

# Training Performance
Tune_train_preds <- predict(object = SVMFit_ROS,
                            newdata = svm_train_up)
SVM_traintune_ROS_conf <- confusionMatrix (data = Tune_train_preds, 
                                       reference = svm_train_up$status_time, 
                                       positive = "1",
                                       mode = "everything")
SVM_traintune_ROS_conf

# Testing Performance
tune_test_preds <- predict(object = SVMFit_ROS,
                           newdata = test)

SVM_testtune_ROS_conf <- confusionMatrix(data = tune_test_preds, 
                                     reference = test$status_time, 
                                     positive = "1",
                                     mode = "everything")
SVM_testtune_ROS_conf

## Goodness of Fit
# Overall
cbind(Training = SVM_traintune_ROS_conf$overall,
      Testing = SVM_testtune_ROS_conf$overall)

# Class-Level
cbind(Training = SVM_traintune_ROS_conf$byClass,
      Testing = SVM_testtune_ROS_conf$byClass)

#Comparing the Model
# Overall
cbind(Tuned = SVM_testtune_conf$overall,
      Oversample = SVM_testtune_ROS_conf$overall)

#Class-Level
cbind(Tuned = SVM_testtune_conf$byClass,
      Oversample = SVM_testtune_ROS_conf$byClass)

#---------------------------------------------------
## Ensemble Model Hyperparameter
#---------------------------------------------------
# Model Hyperparameter:
floor(sqrt(length(vars)))

grid_rf = expand.grid(mtry = seq(from = 2, 
                               to = length(vars), 
                               by = 1))
grid_rf

grid_ctrl <- trainControl(method = "cv",
                          number = 10,
                          search="grid")


#------------------------------------------
## Ensemble Methods using caretEnsemble
#------------------------------------------
ctrl_rf <- trainControl(method = "cv", 
                     number = 10, 
                     search="grid",
                     allowParallel = TRUE, 
                     savePredictions = "final") 

# Calculate the number of cores
no_cores <- detectCores() - 1
# create the cluster for caret to use
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

# initialize random seed
set.seed(1000) 

ensemble_models <- caretList(x = train[ ,vars], 
                             y = train$status_time, 
                             trControl = ctrl_rf, 
                             tuneList = list(bagging = caretModelSpec(method = "treebag"),
                             randforest = caretModelSpec(method = "rf", 
                             tuneGrid = grid_rf,
                             boosting = caretModelSpec(method = "real_adaboost",
                             tuneLength = 15))))

# End parallel processing
stopCluster(cl)
registerDoSEQ()

# Random Forest
ensemble_models[["randforest"]]$results
# Bagging
ensemble_models[["bagging"]]$results


results <- resamples(ensemble_models)
summary(results)

# visualizing the distribution of Accuracy and Kappa across the 3 ensemble models
bwplot(results) 
dotplot(results)

## Variable Importance
par(mfrow = c(1,2))
# Random Forest
plot(varImp(ensemble_models$randforest))
# Boosting
plot(varImp(ensemble_models$bagging))
par(mfrow = c(1,1))

#------------------------------------------
## Model Performance
#------------------------------------------
## Testing Performance 
ens_preds <- data.frame(predict(object = ensemble_models,
                                newdata = test),
                        stringsAsFactors = TRUE)


ph <- list()
for (i in 1:ncol(ens_preds)){
  ph[[i]] <- confusionMatrix(data = ens_preds[ ,i],
                             reference = test$status_time,
                             positive = "1",
                             mode = "everything")
  names(ph)[[i]] <- colnames(ens_preds)[i]
}

## Comparing Performance Across Methods
## Overall
cbind(bagging = ph[["bagging"]]$overall,
      randomforest = ph[["randforest"]]$overall)

## By Class
cbind(bagging = ph[["bagging"]]$byClass,
      randomforest = ph[["randforest"]]$byClass)


## Goodness of Fit
## Training
t(matrix(colMeans(results$values[,-1]), 
         nrow = 2, 
         byrow = TRUE,
         dimnames = list(c("bagging", "randomforest"), 
                         c("Accuracy", "Kappa"))))
## Testing
cbind(bagging = ph[["bagging"]]$overall,
      randomforest = ph[["randforest"]]$overall)[1:2,]

#Moving Ahead with Random Forest
##------------------------------------------
#Random Forest
##------------------------------------------
#Initalizing random seeding
set.seed(1000) 

fit_rf <- train(x = train[ ,vars], 
                y = train$status_time, 
                method = "rf", 
                preProcess = c("YeoJohnson", "center", "scale"),
                trControl = grid_ctrl,
                tuneGrid = grid_rf)

#View Result
fit_rf
confusionMatrix(fit_rf)

# Variable Importance
plot(varImp(fit_rf))


### Testing Performance
rf_tune_test_preds <- predict(object = fit_rf,
                         newdata = test)

RF_testtune_conf <- confusionMatrix(data = rf_tune_test_preds, 
                                  reference = test$status_time, 
                                  positive = "1",
                                  mode = "everything")
RF_testtune_conf

#Overall
RF_testtune_conf$overall

#ByClass
RF_testtune_conf$byClass

##----------------------------------------------
#Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
rf_train_up <- upSample(x = train[ ,c(vars, "status_time")], 
                     y = train$status_time, 
                     yname = "status_time") 

par(mfrow = c(1,2))
plot(train$status_time, main = "Original", col = "lavender")
plot(rf_train_up$status_time, main = "ROS", col = "steel blue")
par(mfrow = c(1,1)) 

# Sampling UP
grid_ctrl$sampling <- "up"

#Initialize random seed
set.seed(1000)

fit_rf_ROS <- train(x = rf_train_up[ ,vars], 
                    y = rf_train_up$status_time,
                    preProcess = c("YeoJohnson", "center", "scale"),
                    method = "rf", 
                    trControl = grid_ctrl,
                    tuneGrid = grid_rf)
fit_rf_ROS

# Variable Importance
plot(varImp(fit_rf_ROS))


### Testing Performance
rf.tune.test.preds <- predict(object = fit_rf_ROS,
                           newdata = test)

RF_ROS_testtune_conf <- confusionMatrix(data = rf.tune.test.preds, 
                                        reference = test$status_time, 
                                        positive = "1",
                                        mode = "everything")
RF_ROS_testtune_conf

#Overall
RF_ROS_testtune_conf$overall
#ByClass
RF_ROS_testtune_conf$byClass

##Comparing the Model
#Overall
cbind(Tuned = RF_testtune_conf$overall,
      Oversampling = RF_ROS_testtune_conf$overall)

#By Class
cbind(Tuned = RF_testtune_conf$byClass,
      Oversampling = RF_ROS_testtune_conf$byClass)


##----------------------------------------------
# Comparing All the Models Together
##----------------------------------------------

#Overall

cbind(DecisionTree = DT_ROS_testtune_conf$overall,
      KNN = KNN_ROS_testtune_conf$overall,
      NaiveBayes = nb_test_conf_ROS$overall,
      SVM = SVM_testtune_ROS_conf$overall,
      RandomForest = RF_ROS_testtune_conf$overall)

#By Class
cbind(DecisionTree = DT_ROS_testtune_conf$byClass,
      KNN = KNN_ROS_testtune_conf$byClass,
      NaiveBayes = nb_test_conf_ROS$byClass,
      SVM = SVM_testtune_ROS_conf$byClass,
      RandomForest = RF_ROS_testtune_conf$byClass)


