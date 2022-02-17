# Chapter 5 - Resampling methods
library(ISLR)

# Validation set approach
#########################


set.seed(1)
train <- sample(392,196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)

# Different training set

set.seed(2)
train <- sample(392,196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)

# Printing different MSE estimates for polynomial regression

for (i in 1:7) {
  print(
    mean(
      (Auto$mpg-predict(
        lm(mpg ~ poly(horsepower,i), data = Auto, subset = train),Auto)
       )[-train]^2)
    )
}

# Leave-one-out cross validation
################################

# We'll use GLM without the family argument. This because we can do LOOCV with this function.

glm.fit <- glm(mpg~horsepower, data = Auto) 
coef(glm.fit)

library(boot)

cv.err = cv.glm(Auto,glm.fit)
cv.err$delta # Leave one-out cross validation MSE estimation!

cv.error = rep(0,10)

for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
  
}

  plot(1:10,cv.error,"b", xlab = "Polynomial degree", ylab = "MSE estimate (LOOCV)")

# K-Fold cross validation
#########################

set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10) {
  glm.fit = glm(mpg~poly(horsepower,i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K = 10)$delta[1]
}
cv.error.10

plot(1:10,cv.error.10, "b", xlab = "Polynomial degree", ylab = "MSE estimate (10 fold)")


# The bootstrap
###############

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)


set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace = TRUE))

boot(Portfolio, alpha.fn, R = 1000)

# Accuracy of Linear Regression Model

boot.fn = function(data, index)
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392, replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower, Auto))

# Accuracy of quadratic model

boot.fn = function(data, index)
  return(coef(lm(mpg ~ poly(horsepower,2), data = data, subset = index)))

boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ poly(horsepower,2), Auto))

# Exercises
###########

# E2

f1 <- function(x,n){
  (1-1/x)^x
}
  

plot(1:100,f1(1:100),"l")

store = rep ( NA , 10000)
for ( i in 1:10000) {
  store [ i ]= sum ( sample (1:100 , rep = TRUE ) ==4) >0
}

mean ( store )

## E5 ##

# No validation

glm.fit <- glm(default ~ income + balance, data = Default, family = binomial)

glm.pred <- rep("No",10000)
glm.pred[predict(glm.fit, type = "response") > 0.5] = "Yes"
mean(glm.pred == Default$default)

# Test validation set

set.seed(420)
train <- sample(10000, 5000)
default.test <- Default[-train,]

glm.fit <- glm(default ~ income + balance,
               data = Default, family = binomial,
               subset = train)

glm.pred <- rep("No",5000)
glm.pred[predict(glm.fit, default.test, type = "response") > 0.5] = "Yes"
1 - mean(glm.pred == default.test)

mse_est_tset <- numeric(50)

for (i in 1:50) {
  train <- sample(10000, 5000)
  default.test <- Default[-train,]
  glm.fit <- glm(default ~ income + balance,
                 data = Default, family = binomial,
                 subset = train)
  glm.pred <- rep("No",5000)
  glm.pred[predict(glm.fit, default.test, type = "response") > 0.5] = "Yes"
  mse_est_tset[i] = 1 - mean(glm.pred == default.test)
}


set.seed(24102021)
train <- sample(10000, 5000)
default.test <- Default[-train,]

glm.fit <- glm(default ~ income + balance + student,
               data = Default, family = binomial,
               subset = train)

glm.pred <- rep("No",5000)
glm.pred[predict(glm.fit, default.test, type = "response") > 0.5] = "Yes"
1 - mean(glm.pred == default.test)


## E6 ##

set.seed(24102021)
glm.fit <- glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

boot.fn <- function(data, index){
  glm.fit = glm(default ~ income + balance,
      data = Default, family = binomial,
      subset = index)
  return(coef(glm.fit))
}

boot(Default, boot.fn, R = 1000)
