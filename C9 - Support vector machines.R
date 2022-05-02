# Chapter 9 - Support vector machines

library(ISLR)     # Data
library(e1071)    # Svm
library(ROCR)     # ROC curves
library(magrittr) # Pipes

##########################################################################

#############################
# Support vector classifier #
#############################

set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y == 1,] = x[y == 1,] + 1

plot(x, col = (3-y))

data <- data.frame(x = x, y = as.factor(y))
svmfit = svm(y~., data = data, kernel = "linear", cost = 10,
             scale = FALSE)

plot(svmfit, data)

svmfit$index # Support vectors

summary(svmfit)

svmfit = svm(y ~ ., data = data, kernel = "linear", cost = 0.1,
             scale = FALSE)

svmfit$index
summary(svmfit)

set.seed(1)
tune_out <- tune(svm, y ~., data = data, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_out)

bestmod <- tune_out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
test_data = data.frame(x = xtest, y = as.factor(ytest))

ypred = predict(bestmod, test_data)

table(predict = ypred,
      truth = test_data$y)

svmfit = svm(y ~., data = data, kernel = "linear", cost = 0.01,
             scale = FALSE)
ypred = predict(svmfit, test_data)
table(predict = ypred,
      truth = test_data$y)

x[y == 1,] = x[y == 1,] + 0.5
plot(x, col = (y+5)/2, pch = 19)

data = data.frame(x = x, y = as.factor(y))
svmfit = svm(y ~., data = data, kernel = "linear", cost = 1e5,
             scale = FALSE)
summary(svmfit)
plot(svmfit, data)

svmfit = svm(y ~., data = data, kernel = "linear", cost = 1,
             scale = FALSE)
summary(svmfit)
plot(svmfit, data)


###########################
# Support vector machines #
###########################

set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2
y = c(rep(1,150), rep(2,50))
data = data.frame(x = x, y = as.factor(y))

plot(x, col = y)

train = sample(200, 100)
svmfit = svm(y ~ ., data = data[train,], kernel = "radial",
             gamma = 1, cost = 1)
plot(svmfit, data[train,])

summary(svmfit)

svmfit = svm(y ~ ., data = data[train,], kernel = "radial",
             gamma = 1, cost = 1e5)
plot(svmfit, data[train,])

# Cross validation for choice of gamma

set.seed(1)
tune_out = tune(svm, y~., data = data[train,], kernel = "radial",
                ranges = list(cost = c(0.1,1,10,100,100),
                              gamma = c(0.5,1,2,3,4)))

best_mod <- tune_out$best.model

predict(best_mod, data[-train,])

table(true = data[-train,"y"],
      pred = predict(best_mod, data[-train,]))

#############
# ROC curve #
#############

rocplot = function(pred, truth,...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

svmfit_opt <- svm(y~., data = data[train,], kernel = "radial",
                  gamma = 0.5, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit_opt, data[train,],
                             decision.values = TRUE))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, data[train, "y"], main = "")


svmfit_flex = svm(y~., data = data[train,], kernel = "radial",
                  gamma = 50, cost = 1, decision.values = T)

fitted <- attributes(predict(svmfit_flex, data[train,],
                     decision.values = TRUE))$decision.values
rocplot(fitted, data[train, "y"], add = T, col = "red")

# Now, on test data

fitted <- attributes(predict(svmfit_opt, data[-train,],
                             decision.values = TRUE))$decision.values
rocplot(fitted, data[-train, "y"], main = "")

fitted <- attributes(predict(svmfit_flex, data[-train,],
                             decision.values = TRUE))$decision.values
rocplot(fitted, data[-train, "y"], add = T, col = "red")

#############################
# SVM with multiple classes #
#############################

set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0,50))
x[y == 0, 2] = x[y == 0, 2] + 2
data =  data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col = (y+1))

svmfit = svm(y~., data = data, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, data)

########################
# Gene expression data #
########################

rm(list = ls())

names(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)

# Since too much predictors, linear kerner

data = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y~., data = data, kernel = "linear", cost = 10)
summary(out)

table(fitted = out$fitted, truth = data$y)

data_test <- data.frame(x = Khan$xtest,
                        y = as.factor(Khan$ytest))
pred_test <- predict(out, newdata = data_test)
table(fitted = pred_test, truth = data_test$y)

rm(list = ls())

##########################################################################

##############
# Excersises #
##############

# E. 3

data = data.frame(x1 = c(3,2,4,1,2,4,4),
                  x2 = c(4,2,4,4,1,3,1),
                  y  = c(rep(1,4), rep(-1,3)))

plot(x = data$x1, y = data$x2, col = (data$y+2))
lines(x = c(1:5), y = -0.5 + 1:5)
lines(x = c(1:5), y = -0.2 + 1:5, col = "green")

## Applied

# E. 4

set.seed(1)
x1 <- rnorm(100)
x2 <- c((x1[1:50]^2 + 3 + rnorm(50)),
        (-x1[51:100]^2 - 1 + rnorm(50)))
y  <- c(rep(1,50),rep(-1,50))

df <- data.frame(x1 = x1,
                 x2 = x2,
                 y = as.factor(y)) 
plot(x1,x2, col = (2-y))

svm_fit <- svm(y ~., data = df, kernel = "polynomial", degree = 2,
               scale = FALSE, cost = 10)
summary(svm_fit)
plot(svm_fit, df)

tune_out <- tune(svm, y ~., data = df, kernel = "polynomial",
                 ranges = list(cost = c(0.01,0.1,1,10,100),
                               degree = c(1,2,3,4)))
summary(tune_out)

best_mod <- tune_out$best.model
plot(best_mod, df)

rm(list = ls())

# E. 5

set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y  <- 1*(x1^2-x2^2 > 0)

df <- data.frame(x1 = x1,
                 x2 = x2,
                 y  = as.factor(y))

plot(x = x1, y = x2, col = y+2)

log_fit <- glm(y ~ ., family = binomial, data = df)

preds   <- predict(log_fit, type = "response")
y_preds <- NULL
y_preds[preds < 0.5]  = 0
y_preds[preds >= 0.5] = 1

plot(x = x1, y = x2, col = y_preds+2)

log_nonlin_fit <- glm(y ~ x1 + x2 + I(x1^2) + I(x1*x2),
                      family = binomial, data = df)
preds_nonlin <- predict(log_nonlin_fit, type = "response")
y_preds_nonlin <- NULL
y_preds_nonlin[preds_nonlin < 0.5] = 0
y_preds_nonlin[preds_nonlin >= 0.5] = 1

plot(x = x1, y = x2, col = y_preds_nonlin+2)

svm_fit <- svm(y ~., data = df, kernel = "linear",
               cost = 1, scale = FALSE)

svm_preds <- as.numeric(predict(svm_fit)) - 1 

plot(x = x1, y = x2, col = svm_preds + 2)
plot(svm_fit, df)

svm_fit_nonlin <- svm(y ~ x1 + x2 + I(x1^2) + I(x1*x2), data = df,
                      kernel = "polynomial", degree = 2, cost = 1) 
svm_preds_nonlin <- as.numeric(predict(svm_fit_nonlin)) - 1
plot(x = x1, y = x2, col = svm_preds_nonlin + 2)

par(mfrow = c(2,2))
plot(x = x1, y = x2, col = y_preds+2)
plot(x = x1, y = x2, col = y_preds_nonlin+2)
plot(x = x1, y = x2, col = svm_preds+2)
plot(x = x1, y = x2, col = svm_preds_nonlin+2)