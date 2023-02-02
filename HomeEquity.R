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
library(ROSE)
library(caretEnsemble)
library(survival)
library(parallel) 
library(doParallel) 
library(randomForest) 
library(fastAdaboost) 
library(lattice)
library(cluster) 
library(factoextra) 
library(fpc)

#Setting Directory
setwd("~/Desktop/Quarter 4/Business Consulting/Project Details/After Mid")

#Reading the file
Home_equity <- read.csv("loan_home equity.csv")

#------------------------------------------
## Data & Variables
#------------------------------------------
head(Home_equity)
tail(Home_equity)

#No of Rows
nrow(Home_equity)
#No of Col
ncol(Home_equity)

#------------------------------------------
## Data Exploration
#------------------------------------------

#Summary of the data
summary(Home_equity,na.rm=True)

#------------------------------------------
## Data Quality
#------------------------------------------

#Identifying the missing value rows
colSums(is.na(Home_equity) | Home_equity == "" )
Home_equity[Home_equity == ""] <- NA
Home_equity[!complete.cases(Home_equity), ]

#Imputting missing values
#Using Median
Home_equity$Mort_Bal[is.na(Home_equity$Mort_Bal)] <- median(Home_equity$Mort_Bal, 
                                                            na.rm = TRUE)
Home_equity$Home_Val[is.na(Home_equity$Home_Val)] <- median(Home_equity$Home_Val, 
                                                            na.rm = TRUE)
Home_equity$Debt_Inc[is.na(Home_equity$Debt_Inc)] <- median(Home_equity$Debt_Inc, 
                                                            na.rm = TRUE)
Home_equity$YOJ[is.na(Home_equity$YOJ)] <- median(Home_equity$YOJ, 
                                                  na.rm = TRUE)
Home_equity$CL_Age[is.na(Home_equity$CL_Age)] <- median(Home_equity$CL_Age, 
                                                        na.rm = TRUE)
Home_equity$Num_CL[is.na(Home_equity$Num_CL)] <- median(Home_equity$Num_CL, 
                                                        na.rm = TRUE)
Home_equity$Num_Derog[is.na(Home_equity$Num_Derog)] <- median(Home_equity$Num_Derog,
                                                              na.rm = TRUE) 
Home_equity$Num_Delinq[is.na(Home_equity$Num_Delinq)] <- median(Home_equity$Num_Delinq,
                                                                na.rm = TRUE)
Home_equity$Num_Inq[is.na(Home_equity$Num_Inq)] <- median(Home_equity$Num_Inq,
                                                          na.rm = TRUE)
# Using Mode Function

modefun <- function(x){
  if(any(tabulate(match(x, unique(x))) > 1)){
    outp <- unique(x)[which(tabulate(match(x, unique(x))) == max(tabulate(match(x, unique(x)))))]
  } else {
    outp <- "No mode exists!"}
  return(outp)
}

Home_equity$Reason_HE[is.na(Home_equity$Reason_HE)] <- modefun(Home_equity$Reason_HE)
Home_equity$Occupation[is.na(Home_equity$Occupation)] <- modefun(Home_equity$Occupation)
Home_equity$Default[is.na(Home_equity$Default)] <- modefun(Home_equity$Default)


#Re-checking for missing values
colSums(is.na(Home_equity) | Home_equity == "" )

#Checking for duplicate values
Home_equity[duplicated(x = Home_equity), ]
Home_equity_new <- Home_equity[!duplicated(x = Home_equity), ]
#------------------------------------------
## Nominal, Ordinal & Numerical
#------------------------------------------

#structure of the data
str(object = Home_equity_new)

#Preparing the Target Varible
Home_equity_new$Default <- factor(Home_equity_new$Default)

plot(Home_equity_new$Default, main = "Default", col = "light blue", 
     names.arg = c("Non Default", "Default"))

nominal <- c("Reason_HE", "Occupation")

Home_equity_new[ ,nominal] = lapply(X = Home_equity_new[ ,nominal], 
                                FUN = factor)

numerical <- names(Home_equity_new)[!names(Home_equity_new) %in% c(nominal,"Default",
                                                           "HEID")]
vars <- c(nominal, numerical)

#Correlation Matrix
cor_var <- cor(x = Home_equity_new[ ,numerical])
corrplot(cor_var)
symnum(x = cor_var,corr = TRUE)
high_corrs <- findCorrelation(x = cor_var, cutoff = 0.75,names = TRUE)
high_corrs

#Removing highly correlation variable
numerical <- numerical[!numerical %in% high_corrs]
numerical

# Checking for Outliers
outliers <- sapply(Home_equity[,numerical], function(x) which(abs(x) > 3))
outliers

Home_equity[unique(unlist(outliers)),]

#Creating vector
vars <- c(nominal,numerical)
#------------------------------------------
## Data Visualization
#------------------------------------------

## Plots for Numerical Variables
ggplot2 <- ggplot(data = Home_equity_new, 
                  mapping = aes(x = YOJ, fill = Default)) +
  geom_histogram(bins = 15)

ggplot2 +
  scale_fill_discrete(labels = c("No", "Yes"))+
  ggtitle("Loan Default by Years of Present Job") 

## Plots for Categorical Variables

ggplot3 <-gg_plot<-ggplot(data = Home_equity_new, mapping = aes(x = Reason_HE,
                                                            fill = Default)) +
  geom_bar() 

ggplot3 +
  scale_fill_discrete(labels = c("No", "Yes"))+
  ggtitle("Loan Default by Loan Reason")

# Chi squared test
#Null hypothesis: Loan default and loan reason are independent
#Alternative hypothesis: Both variables relate to each other

chisq.test(Home_equity_new$Default, Home_equity_new$Reason_HE, correct=FALSE)
## p-value is 0.003775 < significance level of 0.05, we reject the null hypothesis 
##and conclude that the two variables are in fact dependent.

# t-test 
t.test(Home_equity_new$YOJ ~ Home_equity_new$Default, var.equal = TRUE)
## The results below show a p-value < .01 supporting the alternative hypothesis 
## that “true difference in means is not equal to 0”; 
## essentially it states there is a statistical difference 
## between the two means default and non-default
#------------------------------------------
######### Cluster Analysis #########
#------------------------------------------

#Rescaling the Numerical Variables
cen_mm <- preProcess(x = Home_equity_new[ ,numerical],
                     method = "range")
Home_equity_mm <- predict(object = cen_mm,
                          newdata = Home_equity_new)

## YeoJohnson:
cen_yj <- preProcess(x = Home_equity_new,
                     method = "YeoJohnson")
home_equity_yj <- predict(object = cen_yj,
                          newdata = Home_equity_new)

home_equity_yj <- home_equity_yj[ -c(1) ]

# Create a distance metric using Gower to handle mixed-type data.
load("Cluster_Group1_Final.RData")
dist <- daisy(x = home_equity_yj, metric = "gower")

sil_plot(dist_mat = dist,
         method = "hc", 
         hc.type = "ward.D2", 
         max.k = 15)
## 10
## Possible values are 2, 9 and 10

## kMedoids Clustering (PAM) (method = "pam")
sil_plot(dist_mat = dist, # distance matrix
         method = "pam", # PAM
         max.k = 15, # maximum k value
         seed_no = 1000) # seed value for set.seed()
## 
## k = 2, 7 and 8 are also possible solutions

#------------------------------------------
## Hierarchical Cluster Analysis
#------------------------------------------

# Apply Ward's Clustering
wards <- hclust(d = dist, 
                method = "ward.D2")

# Plot the dendrogram
plot(wards, 
     xlab = NA, sub = NA, 
     main = "Ward's Method")

## Based on the dendrogram, there could be many
## possible values for k. We decided to go with 8 since 
## there is clearly to see.

# Overlay boxes identifying clusters
# for a k = 8  clustering solution
rect.hclust(tree = wards, 
            k = 8, 
            border = hcl.colors(10))

# Create a vector of cluster assignments
wards_clusters <- cutree(tree = wards, 
                         k = 8)
#------------------------------------------

## Describing Cluster Solutions

# Obtain average variable values for each cluster
# for (original) numeric variables
aggregate(x = home_equity_yj[ ,numerical], 
          by = list(wards_clusters),
          FUN = mean)

nominal <- c("Reason_HE", "Occupation","Default")

# Obtain frequency values for each class level
# for categorical variables
aggregate(x = home_equity_yj[ ,c(nominal)], 
          by = list(wards_clusters), 
          FUN = table)

cluster.stats(d = dist,
              clustering = wards_clusters, 
              alt.clustering = as.numeric(home_equity_yj$Default))$corrected.rand 
#------------------------------------------
## PAM (k-Medoids)
#------------------------------------------
set.seed(1000)

pam <- pam(x = dist,
           diss = TRUE,
           k = 8) # 8 cluster solution
pam

## View the Medoids.
home_equity_yj[pam$medoids,]

#------------------------------------------
## Cluster Validation
#------------------------------------------

# Compare the two clustering solutions 
# using (HCA and PAM) based on the Dunn Index,
# the average distance between clusters and
# the average distance within clusters.

stats_PAM <- cluster.stats(d = dist, 
                           clustering = pam$clustering)
stats_HCA <- cluster.stats(d = dist, 
                           clustering = wards_clusters)

c_stats <- c("average.between", "average.within",
             "dunn")

cbind(HCA = stats_HCA[names(stats_HCA) %in% c_stats], 
      PAM = stats_PAM[names(stats_PAM) %in% c_stats])

#------------------------------------------
######### Training and Testing #########
#------------------------------------------
## Training and Testing

# Initialize random seed
set.seed(1000) 

# Create a training and testing split of 80/20
sub <- createDataPartition(y = Home_equity_new$Default, 
                           p = 0.80, 
                           list = FALSE)

# Subset the transformed data
train <- Home_equity_new[sub, ] 
test <- Home_equity_new[-sub, ] 

#------------------------------------------
## Decision Tree Analysis
#------------------------------------------
#Basic model
Homeequity.rpart <- rpart(formula = Default ~., 
                          data = train[ ,c(vars, "Default")],
                          method = "class")

#Basic model output and importance variable
Homeequity.rpart
Homeequity.rpart$variable.importance

#Tree Plots
prp(x = Homeequity.rpart,
    extra = 2)

#------------------------------------------
### 2. Hyperparameter Tuning Model
#------------------------------------------

grids <- expand.grid(cp = seq(from = 0,
                              to = 0.05,
                              by = 0.005))
grids

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     search = "grid")

set.seed(1000)

DTFit <- train(form = Default ~ ., 
               data = train[ ,c(vars, "Default")], 
               method = "rpart", 
               preProcess = c("YeoJohnson", "center", "scale"),
               trControl = ctrl, 
               tuneGrid = grids) 
DTFit

DTFit$results[DTFit$results$cp %in% DTFit$bestTune,] 


# Ploting the cp value vs. Accuracy
plot(DTFit)

# Confusion matrix 
confusionMatrix(DTFit)

DTFit$finalModel$variable.importance

## Tuned Model Performance
### Training Performance
DTtuned_trainpred <- predict(object = DTFit,
                        newdata = train)

DT_traintuned_conf <- confusionMatrix(data = DTtuned_trainpred,
                                  reference = train$Default, 
                                  positive = "1",
                                  mode = "everything")
DT_traintuned_conf

## Testing Performance
DTtuned_testpred <- predict(object = DTFit,
                        newdata = test)
DT_testtuned_conf <- confusionMatrix(data = DTtuned_testpred, 
                                  reference = test$Default,
                                  positive = "1",
                                  mode = "everything")
DT_testtuned_conf

## Goodness of Fit
# Overall
cbind(Training = DT_traintuned_conf$overall,
      Testing = DT_testtuned_conf$overall)

# Class-Level
cbind(Training = DT_traintuned_conf$byClass,
      Testing = DT_testtuned_conf$byClass)

##----------------------------------------------
#Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
DT_train_up <- upSample(x = train[ ,c(vars, "Default")], 
                     y = train$Default, 
                     yname = "Default") 

par(mfrow = c(1,2))
plot(train$Default, main = "Original", col = "lavender")
plot(DT_train_up$Default, main = "ROS", col = "light blue")
par(mfrow = c(1,1)) 

# Sampling UP
ctrl$sampling <- "up"

#Initialize random seed
set.seed(1000)

DTFit_ROS <- train(form = Default ~ ., 
                   data = train[ ,c(vars, "Default")], 
                   method = "rpart",
                   preProcess = c("YeoJohnson", "center", "scale"),
                   trControl = ctrl,
                   tuneGrid = grids) 

DTFit_ROS

#Variable Importance
DTfit_varimp_ROS <- DTFit_ROS$finalModel$variable.importance
barplot(DTfit_varimp_ROS,horiz = TRUE, las = 1, cex.names=0.40, col = "#C7CEEA",
        font = 4, main = "Variable Importance")

# Training Performance
Tune_train_preds <- predict(object = DTFit_ROS,
                            newdata = train)
DT_up_traintune_conf <- confusionMatrix (data = Tune_train_preds, 
                                      reference = train$Default, 
                                      positive = "1",
                                      mode = "everything")
DT_up_traintune_conf

# Testing Performance
Tune_test_preds <- predict(object = DTFit_ROS,
                           newdata = test)

DT_up_testtune_conf <- confusionMatrix(data = Tune_test_preds, 
                                    reference = test$Default, 
                                    positive = "1",
                                    mode = "everything")
DT_up_testtune_conf

## Goodness of Fit
# Overall
cbind(Training = DT_up_traintune_conf$overall,
      Testing = DT_up_testtune_conf$overall)

# Class-Level
cbind(Training = DT_up_traintune_conf$byClass,
      Testing = DT_up_testtune_conf$byClass)

#Comparing the Model
# Overall
cbind(Base = DT_testtuned_conf$overall,
      Oversample = DT_up_testtune_conf$overall)

#Class-Level
cbind(Base = DT_testtuned_conf$byClass,
      Oversample = DT_up_testtune_conf$byClass)

##---------------------------------------------------
# KNN Data Preprocessing & Transformation
##---------------------------------------------------

#Rescaling the Numerical Variables
cen_mm <- preProcess(x = Home_equity_new[ ,numerical],
                     method = "range")
Home_equity_mm <- predict(object = cen_mm,
                          newdata = Home_equity_new)

##Binarization of Nominal Variables

Reason_HE <- dummyVars(formula =  ~ Reason_HE,
                       data = Home_equity_mm)
Reason_HE_dums <- predict(object = Reason_HE, 
                          newdata = Home_equity_mm)

Occupation <- dummyVars(formula =  ~ Occupation,
                        data = Home_equity_mm)
Occupation_dums <- predict(object = Occupation, 
                           newdata = Home_equity_mm)

Home_Equity_KNN_dum <- data.frame(Home_equity_mm[ ,!names(Home_equity_mm) %in% 
                                                    c("Reason_HE", "Occupation")], Reason_HE_dums, Occupation_dums)


#Creating Vector
vars_knn <- names(Home_Equity_KNN_dum)[!names(Home_Equity_KNN_dum) %in% 
                                         c("Default", "Mort_Bal")]
vars_knn

# initialize the random seed
set.seed(1000)

subset <- createDataPartition(y = Home_Equity_KNN_dum$Default, 
                              p = 0.80,list = FALSE)

#Training and Testing Set
train_knn <- Home_Equity_KNN_dum[subset, ]
test_knn <- Home_Equity_KNN_dum[-subset, ]

##-------------------------------------------
### Hyperparameter Tuning 
#------------------------------------------

ctrl_knn <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5)
set.seed(1000)
knnFit <- train(x = train_knn[ ,vars_knn], 
                 y = train_knn[ ,"Default"], 
                 preProcess = "range", 
                 method = "knn", 
                 trControl = ctrl_knn,
                 tuneLength = 15) 

knnFit

plot(knnFit)

# Confusion Matrix
confusionMatrix(knnFit)

#------------------------------------------
### Model Performance
#------------------------------------------

knn_preds <- predict(object = knnFit, 
                    newdata = test_knn[ , vars_knn])

knn_conf_tuned <- confusionMatrix(data = knn_preds, 
                              reference = test_knn[, "Default"], 
                              positive = "1",
                              mode = "everything")
knn_conf_tuned

# We can describe the overall performance 
# based on our accuracy and kappa values.

knn_conf_tuned$overall[c("Accuracy", "Kappa")]


knn_conf_tuned$byClass

##----------------------------------------------
#KNN Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
knn_train_up <- upSample(x = train_knn[ ,c(vars_knn, "Default")], 
                     y = train_knn$Default, 
                     yname = "status_time") 

par(mfrow = c(1,2))
plot(train_knn$Default, main = "Original", col = "lavender")
plot(knn_train_up$Default, main = "ROS", col = "light blue")
par(mfrow = c(1,1)) 

# Sampling UP
ctrl_knn$sampling <- "up"

#Initialize random seed
set.seed(1000)

KNNFit_ROS <- train(x = knn_train_up[ ,vars_knn], 
                 y = knn_train_up[ ,"Default"], 
                 preProcess = "range", 
                 method = "knn", 
                 trControl = ctrl_knn,
                 tuneLength = 15) 
KNNFit_ROS

# Variable Importance
plot(KNNFit_ROS)


### Testing Performance
knn_tune_test_preds <- predict(object = KNNFit_ROS,
                             newdata = test_knn)

KNN_ROS_testtune_conf <- confusionMatrix(data = knn_tune_test_preds, 
                                         reference = test_knn$Default, 
                                         positive = "1",
                                         mode = "everything")
KNN_ROS_testtune_conf

#Overall
KNN_ROS_testtune_conf$overall
#ByClass
KNN_ROS_testtune_conf$byClass

##Comparing the Model
#Overall
cbind(Base = knn_conf_tuned$overall,
      Oversampling = KNN_ROS_testtune_conf$overall)

#By Class
cbind(Base = knn_conf_tuned$byClass,
      Oversampling = KNN_ROS_testtune_conf$byClass)

#------------------------------------------
## Naive Bayes Analysis
#------------------------------------------
## Analysis

aggregate(train[ ,c(nominal)],
          by = list(train$Default),
          FUN = table)

NB_Fit <-  naiveBayes(x = train[ ,vars],
                     y = train$Default,
                     preProcess = c("YeoJohnson", "center", "scale"),
                     laplace = 1)
NB_Fit

#------------------------------------------

### Model Performance & Fit

## Training Performance
nb_train <- predict(object = NB_Fit, 
                    newdata = train[ ,vars], 
                    type = "class")

head(nb_train)

nb_train_conf <- confusionMatrix(data = nb_train, 
                              reference = train$Default, 
                              mode = "everything")
nb_train_conf

## Testing Performance
nb_test <- predict(object = NB_Fit, 
                   newdata = test[ ,vars],
                   type = "class")

nb_test_conf <- confusionMatrix(data = nb_test, 
                             reference = test$Default, 
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

##---------------------------------------------------------##
## Oversampling
nb_train_up <- upSample(x = train[ ,vars], 
                     y = train$Default, 
                     yname = "Default") 

par(mfrow = c(1,2)) 
plot(train$Default, main = "Original", col = "lavender")
plot(nb_train_up$Default, main = "RUS", col = "light blue")
par(mfrow = c(1,1)) 

NB_Fit$sampling <- "up"

set.seed(1000)

nb_fit_ROS <- naiveBayes(x = nb_train_up[ ,vars],
                         y = nb_train_up$Default,
                         preProcess = c("YeoJohnson", "center", "scale"),
                         laplace = 1)

nb_fit_ROS
#------------------------------------------
## Model Performance & Fit
# Training Performance

nb_train_ROS <- predict(object = nb_fit_ROS, 
                        newdata = nb_train_up[ ,vars], 
                        type = "class")
head(nb_train_ROS)
nb_train_conf_ROS <- confusionMatrix(data = nb_train_ROS, 
                                  reference = nb_train_up$Default, 
                                  positive = "1",
                                  mode = "everything")
nb_train_conf_ROS

# Testing Performance

nb_test_ROS <- predict(object = nb_fit_ROS, 
                       newdata = test[ ,vars], 
                       type = "class")

nb_test_conf_ROS <- confusionMatrix(data = nb_test_ROS, 
                                 reference = test$Default,
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

sig_mod <- svm(formula = Default ~ .,
               data = train[ ,c(vars, "Default")],
               method = "C-classification", 
               kernel = "sigmoid", 
               scale = TRUE)

summary(sig_mod)

## Training Performance
head(sig_mod$fitted)

sig_train_conf <- confusionMatrix(data = sig_mod$fitted, 
                                  reference = train$Default, 
                                  positive = "1",
                                  mode = "everything")
sig_train_conf

#Base Model - Linear Kernel
set.seed(1000)

lin_mod <- svm(formula = Default ~ .,
               data = train[ ,c(vars, "Default")],
               method = "C-classification", 
               kernel = "linear", 
               scale = TRUE)

summary(lin_mod)

## Training Performance
head(lin_mod$fitted)

lin_train_conf <- confusionMatrix(data = lin_mod$fitted, 
                                  reference = train$Default, 
                                  positive = "1",
                                  mode = "everything")
lin_train_conf

#Base Model - Radial Kernel
set.seed(1000)

rad_mod <- svm(formula = Default ~ .,
               data = train[ ,c(vars, "Default")],
               method = "C-classification", 
               kernel = "radial", 
               scale = TRUE)

summary(rad_mod)

## Training Performance
head(rad_mod$fitted)

rad_train_conf <- confusionMatrix(data = rad_mod$fitted, 
                                  reference = train$Default, 
                                  positive = "1",
                                  mode = "everything")
rad_train_conf

## View all training fit for the 3 kernels
#Overall
cbind(sigmoid = sig_train_conf$overall,
      linear = lin_train_conf$overall,
      radial = rad_train_conf$overall)

#By Class
cbind(sigmoid = sig_train_conf$byClass,
      linear = lin_train_conf$byClass,
      radial = rad_train_conf$byClass)

## We will be using the radial kernel and tuned our hyper parameters

##--------------------------------------------
### SVM Hyper parameter Tuning 
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
SVMFit <- svm(form =   Default ~ ., 
              data = train[ ,c(vars, "Default")], 
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

svm_tune_train_preds <- predict(object = SVMFit,
                         newdata = train)

SVM_traintune_conf <- confusionMatrix(data = svm_tune_train_preds, 
                                   reference = train$Default, 
                                   positive = "1",
                                   mode = "everything")
SVM_traintune_conf

### Testing Performance
svm_tune_test_preds <- predict(object = SVMFit,
                         newdata = test)

SVM_testtune_conf <- confusionMatrix(data = svm_tune_test_preds, 
                                   reference = test$Default, 
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
svm_train_up <- upSample(x = train[ ,c(vars, "Default")], 
                         y = train$Default, 
                         yname = "Default") 

par(mfrow = c(1,2))
plot(train$Default, main = "Original", col = "lavender")
plot(svm_train_up$Default, main = "ROS", col = "light blue")
par(mfrow = c(1,1)) 

# Sampling UP
control_SVM$sampling <- "up"

#Initialize random seed
set.seed(1000)

SVMFit_ROS <- svm(form = Default ~ ., 
                  data = svm_train_up[ ,c (vars, "Default")], 
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
ROS_tunetrain_preds <- predict(object = SVMFit_ROS,
                            newdata = svm_train_up)
SVM_ROS_traintune_conf <- confusionMatrix (data = ROS_tunetrain_preds, 
                                       reference = svm_train_up$Default, 
                                       positive = "1",
                                       mode = "everything")
SVM_ROS_traintune_conf

# Testing Performance
ROS_tunetest_preds <- predict(object = SVMFit_ROS,
                           newdata = test)

SVM_ROS_testtune_conf <- confusionMatrix(data = ROS_tunetest_preds, 
                                     reference = test$Default, 
                                     positive = "1",
                                     mode = "everything")
SVM_ROS_testtune_conf

## Goodness of Fit
# Overall
cbind(Training = SVM_ROS_traintune_conf$overall,
      Testing = SVM_ROS_testtune_conf$overall)

# Class-Level
cbind(Training = SVM_ROS_traintune_conf$byClass,
      Testing = SVM_ROS_testtune_conf$byClass)

#Comparing the Model
# Overall
cbind(Base = SVM_testtune_conf$overall,
      Oversample = SVM_ROS_testtune_conf$overall)

#Class-Level
cbind(Base = SVM_testtune_conf$byClass,
      Oversample = SVM_ROS_testtune_conf$byClass)

#---------------------------------------------------
##  Random Forest Model Hyperparameter
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
## Ensemble Methods
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
                             y = train$Default, 
                             trControl = ctrl_rf, 
                             tuneList = list(bagging = caretModelSpec(method = "treebag"),
                             randforest = caretModelSpec(method = "rf", 
                             tuneGrid = grid_rf,
                             boosting = caretModelSpec(method = "adaboost",
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

# We can visualize the distribution of
# our Accuracy and Kappa across the 3
# ensemble models
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
## Random Forest Model Performance
#------------------------------------------
## Testing Performance 
ens_preds <- data.frame(predict(object = ensemble_models,
                                newdata = test),
                                stringsAsFactors = TRUE)


ph <- list()
for (i in 1:ncol(ens_preds)){
  ph[[i]] <- confusionMatrix(data = ens_preds[ ,i],
                             reference = test$Default,
                             positive = "1",
                             mode = "everything")
  names(ph)[[i]] <- colnames(ens_preds)[i]
}

## Comparing Performance Across Methods
## Overall
cbind(bag = ph[["bagging"]]$overall,
      rf = ph[["randforest"]]$overall)

## By Class
cbind(bag = ph[["bagging"]]$byClass,
      rf = ph[["randforest"]]$byClass)


## Goodness of Fit
## Training
t(matrix(colMeans(results$values[,-1]), 
         nrow = 2, 
         byrow = TRUE,
         dimnames = list(c("bag", "rf"), 
                         c("Accuracy", "Kappa"))))
## Testing
cbind(bag = ph[["bagging"]]$overall,
      rf = ph[["randforest"]]$overall)[1:2,]

#Moving Ahead with Random Forest
##------------------------------------------
#Random Forest
##------------------------------------------
#Initalizing random seeding
set.seed(1000) 

RF_fit <- train(x = train[ ,vars], 
                y = train$Default, 
                method = "rf", 
                preProcess = c("YeoJohnson", "center", "scale"),
                trControl = grid_ctrl,
                tuneGrid = grid_rf)

#View Result
RF_fit
confusionMatrix(RF_fit)

# Variable Importance

plot(varImp(RF_fit), top=5)


### Testing Performance
RFtune_test_preds <- predict(object = RF_fit,
                             newdata = test)

RF_test_conf <- confusionMatrix(data = RFtune_test_preds, 
                                  reference = test$Default, 
                                  positive = "1",
                                  mode = "everything")
RF_test_conf

#Overall
RF_test_conf$overall

#ByClass
RF_test_conf$byClass

##----------------------------------------------
#Handling Class Imbalance
##----------------------------------------------

## Oversampling
set.seed(1000) 
rf_train_up <- upSample(x = train[ ,c(vars, "Default")], 
                     y = train$Default, 
                     yname = "Default") 

par(mfrow = c(1,2))
plot(train$Default, main = "Original", col = "lavender")
plot(rf_train_up$Default, main = "ROS", col = "light blue")
par(mfrow = c(1,1)) 

# Sampling UP
grid_ctrl$sampling <- "up"

#Initialize random seed
set.seed(1000)

RF_fit_ROS <- train(x = rf_train_up[ ,vars], 
                    y = rf_train_up$Default,
                    method = "rf", 
                    preProcess = c("YeoJohnson", "center", "scale"),
                    trControl = grid_ctrl,
                    tuneGrid = grid_rf)
RF_fit_ROS 

# Variable Importance
plot(varImp(RF_fit_ROS ), main="Home Equity Importance Variable")
plot(varImp(RF_fit_ROS ), main="Home Equity Importance Variable", top=5)


### Testing Performance
tune_test_preds <- predict(object = RF_fit_ROS,
                           newdata = test)

RF_ROS_testtune_conf <- confusionMatrix(data = tune_test_preds, 
                                        reference = test$Default, 
                                        positive = "1",
                                        mode = "everything")
RF_ROS_testtune_conf

#Overall
RF_ROS_testtune_conf$overall
#ByClass
RF_ROS_testtune_conf$byClass

##Comparing the Model
#Overall
cbind(Base = RF_test_conf$overall,
      Oversampling = RF_ROS_testtune_conf$overall)

#By Class
cbind(Base = RF_test_conf$byClass,
      Oversampling = RF_ROS_testtune_conf$byClass)


##--------------------------------------------------
### Comparing all models Together
##--------------------------------------------------

#Overall
cbind(RandomForest = RF_ROS_testtune_conf$overall,
      SVM = SVM_ROS_testtune_conf$overall,
      KNN = KNN_ROS_testtune_conf$overall,
      NaiveBayes = nb_test_conf_ROS$overall,
      DecisionTree = DT_up_testtune_conf$overall)

#By Class
cbind(RandomForest = RF_ROS_testtune_conf$byClass,
      SVM = SVM_ROS_testtune_conf$byClass,
      KNN = KNN_ROS_testtune_conf$byClass,
      NaiveBayes = nb_test_conf_ROS$byClass,
      DecisionTree = DT_up_testtune_conf$byClass)



