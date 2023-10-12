# -------------------------------------------------------------------------
# Description: Code for SuperLearner on Pima Indian Women data from MASS
# 
# Code file:  a_pima_superlearner.R
# Directory:  H:\\BSR\BSR_Ensemble\code
#
# Author:  Kevin Gillespie
#
# Date Created: 11 October, 2023
#
# Email: kgillesp@fredhutch.org
#
# Dependencies:
# -------------
#   Inputs Files:
#
#   Output Files:
#
# -------------------------------------------------------------------------
# Change log
# -------------------------------------------------------------------------
# Date                      Name            Comment
# 11Oct2023                 K. Gillespie    Initialize
#
# -------------------------------------------------------------------------

library(dplyr)
library(MASS)
library(SuperLearner)
library(caret)

source('functions.R')

set.seed(1188)

# dataio ------------------------------------------------------------------
train <- Pima.tr
test <- Pima.te

# data manipulation -------------------------------------------------------
y <- as.numeric(train[,8])-1
ytest <- as.numeric(test[,8])-1

x <- data.frame(train[,1:7])
xtest <- data.frame(test[,1:7])


# SuperLearning -----------------------------------------------------------
# Perform a randomforest single model approach on training data
single.model <- SuperLearner(y,
                             x,
                             family=binomial(),
                             SL.library=list("SL.ranger"))

# Fit the ensemble model
model <- SuperLearner(y,
                      x,
                      family=binomial(),
                      SL.library=list("SL.ranger",
                                      "SL.ksvm",
                                      "SL.glmnet",
                                      "SL.bayesglm"))

# Return the model
model


# Get V-fold cross-validated risk estimate
cv.model <- CV.SuperLearner(y,
                            x,
                            V=5,
                            SL.library=list("SL.ranger",
                                            "SL.ksvm",
                                            "SL.glmnet",
                                            "SL.bayesglm"))


# Model assessment --------------------------------------------------------
# Print out the summary statistics
summary(cv.model)


# Plot graphic of the CV model
plot(cv.model)


# Predictions -------------------------------------------------------------
predictions <- predict.SuperLearner(model, newdata=xtest)

head(predictions$library.predict)

# Recode probabilities
conv.preds <- as.factor(ifelse(predictions$pred>=0.5,1,0))

# Create the confusion matrix
cm <- confusionMatrix(conv.preds, as.factor(ytest))

# Return the confusion matrix
cm

# Get predictors selected from each model ---------------------------------
# Ranger RandomForest result
importance(model[["fitLibrary"]]$SL.ranger_All$object)

# Support Vector Machine
model[["fitLibrary"]]$SL.ksvm_All$object

# LASSO
coef(model[["fitLibrary"]]$SL.glmnet_All$object)[,1]

# Bayes glm
model[["fitLibrary"]]$SL.bayesglm_All$object$coefficients
