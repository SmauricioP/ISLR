# Chapter 6 - Linear model selection and regularization

library(ISLR)    #
library(leaps)   # Variable selection
library(glmnet)  # Ridge, LASSO and more
library(pls)     # PCR and PLS

# Best subset selection
#######################

fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

data <- na.omit(Hitters)
dim(data)
sum(is.na(data))

regfit.full <- regsubsets(Salary ~., data)
summary(regfit.full)

regfit.full <- regsubsets(Salary ~., data, nvmax = 19)
reg.summary <- summary(regfit.full)

names(reg.summary)

reg.summary$rsq

par(mfrow = c(2,2))

plot(reg.summary$rss, xlab = "Number of variables",
     ylab = "RSS", type = "l")


plot(reg.summary$adjr2, xlab = "Number of variables",
     ylab = "Adjusted Rsrq", type = "l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], cex = 2, col = "red", pch = 20)


plot(reg.summary$cp, xlab = "Number of variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], cex = 2, col = "red", pch = 20)

plot(reg.summary$bic, xlab = "Number of variables",
     ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6], cex = 2, col = "red", pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

coef(regfit.full, 6)

# Forward and backword stepwise selection
#########################################


regfit.fwd = regsubsets(Salary ~., data = data, nvmax = 19,
                        method = "forward")
summary(regfit.fwd)


regfit.bwd = regsubsets(Salary ~., data = data, nvmax = 19,
                        method = "backward")
summary(regfit.bwd)


for (n in 1:19) {
  if (mean(abs(coef(regfit.full,n) - coef(regfit.fwd,n))) < 0.0000000001) {
    print(paste0("With ",n," variables, same."))
  }
  else{
    print(paste0("With ",n," variables, different."))
  }
}


# Choosing among models using validation set and cross-validation
#################################################################

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(data), rep = TRUE)
test  <- (!train) 

regfit.best <- regsubsets(Salary ~., data = data[train,],
                         nvmax = 19)

test.mat <- model.matrix(Salary~., data = data[test,])

val.errors <- rep(NA,19)

for (i in 1:19) {
  coefi = coef(regfit.best, id = i)
  pred  = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((data$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 7)

par(mfrow = c(1,1))

plot(val.errors, type = "l")
points(7, val.errors[7], col = "red", pch = 20, cex = 2)

predict.regsubsets <- function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat  = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

regfit.best <- regsubsets(Salary ~., data = data, nvmax = 19)
coef(regfit.best, 7)



k <- 10
set.seed(1)
folds <- sample(1:k, nrow(data), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit = regsubsets(Salary ~ ., data = data[folds != j,],
                        nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit,data[folds == j,], id = i)
    cv.errors[j,i] = mean((data$Salary[folds == j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors

plot(mean.cv.errors, type = "b")
which.min(mean.cv.errors)

reg.best <- regsubsets(Salary ~ ., data = data, nvmax = 19)
coef(reg.best, 10) # We chose 10 variables by cross-validation!

##################################
# Ridge regression and the LASSO #
##################################

x <- model.matrix(Salary ~., data)[,-1]
y <- data$Salary

# Ridge regression
##################

# alpha = 0: Ridge
# alpha = 1: LASSO

grid <- 10^seq(10,-2,length = 100)

ridge.mod <- glmnet(x,y,alpha = 0, lambda = grid)
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # No intercept

predict(ridge.mod, s = 50, type = "coefficients")[1:20,] # Coef for new lambda!


set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test  <- (-train) 
y.test <- y[test]


ridge.mod <- glmnet(x[train,],y[train], alpha = 0, lambda = grid,
                    thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)


mean((mean(y[train])-y.test)^2)
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)


ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,])
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, newx = x[test,], type = "coefficients")[1:20,]


set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# The Lasso
###########

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef


# PCR and PLS regression
########################

### PCR ###

set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = data, scale = TRUE, validation = "CV")

summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

pcr.fit <- pcr(Salary ~ ., data = data, scale = TRUE, subset = train,
               validation = "CV")

validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test,], ncomp = 9)
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 9)
summary(pcr.fit)

### PLS ###

set.seed(1)
pls.fit <- plsr(Salary ~ ., data = data, subset = train, scale = TRUE,
                validation = "CV")

summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((y.test - pls.pred)^2)

pls.fit <- plsr(Salary ~ ., data = data, ncomp = 2, scale = TRUE,)
summary(pls.fit)

##############
# Excercises #
##############


## Excercise 8

library(tidyverse)

rm(list = ls())

set.seed(1)
x <- rnorm(100)
e <- rnorm(100)
y <- 2 - x + 2*x^2 + 3* x^3 + e

data <-data.frame(x1 = x, y = y)

for (i in 2:10) {
  data[,i+1] = x^i
}

colnames(data) <- c("x1","y",sprintf("x%s",seq(2,10)))

select <- regsubsets(y ~., data, nvmax = 10)
sel_sum <- summary(select)

plot(sel_sum$adjr2, type = "l")
which.max(sel_sum$adjr2)
points(4, sel_sum$adjr2[4], col = "red", pch = 20)

plot(sel_sum$cp, type = "l")
which.min(sel_sum$cp)
points(4, sel_sum$cp[4], col = "red", pch = 20)

plot(sel_sum$bic, type = "l")
which.min(sel_sum$bic)
points(3, sel_sum$bic[3], col = "red", pch = 20)

coef(select,3)
sel_sum$adjr2[2]

X <- model.matrix(y ~., data = data)

set.seed(1)
blam  <- cv.glmnet(x = X, y = y, alpha = 1)$lambda.min
lamcv <- cv.glmnet(x = X, y = y, alpha = 1)

plot(cv.glmnet(x = X, y = y, alpha = 1))

lasso <- glmnet(x = X, y = y, alpha = 1, lambda = blam)
round(coef(lasso)[-2],5)

plot(lamcv)

# Optional : LASSO vs. OLS

lasso_reg <- function(x){
  n = length(x)
  coefs = coef(lasso)[-2]
  store = rep(0, n)
  for (i in 1:n) {
    store[i] = as.numeric(t(coefs) %*% (x[i])^seq(0,10))
  }
  return(store)
}


ols_reg <- function(x){
  n = length(x)
  coefs = as.numeric(coef(lm(y~., data = data)))
  store = rep(0, n)
  for (i in 1:n) {
    store[i] = as.numeric(t(coefs) %*% (x[i])^seq(0,10))
  }
  return(store)
}

real_fun <- function(x){
  return(2 - x + 2*x^2 + 3* x^3)
}

ggplot(data) +
  geom_point(aes(x = x, y = y)) +
  geom_function(data = data.frame(xd = 0),
                aes (x = xd), fun = lasso_reg, xlim = c(-2.5,2.8)) +
  geom_function(data = data.frame(xd = 0),
                aes (x = xd), fun = ols_reg, xlim = c(-2.5,2.8),
                color = "red") +
  geom_function(data = data.frame(xd = 0),
                aes (x = xd), fun = real_fun, xlim = c(-2.5,2.8),
                color = "blue", linetype = 2, size = 1, alpha = 0.15) +
  theme_light()

rm(list = c("lasso_reg","ols_reg","real_fun"))

########################################################################

y <- 5 + 0.25*x^7 + e

select <- regsubsets(y ~., data, nvmax = 10)
sel_sum <- summary(select)

plot(sel_sum$adjr2, type = "l")
points(which.max(sel_sum$adjr2),
       sel_sum$adjr2[which.max(sel_sum$adjr2)], col = "red", pch = 20)

plot(sel_sum$cp, type = "l")
points(which.min(sel_sum$cp),
       sel_sum$cp[which.min(sel_sum$cp)], col = "red", pch = 20)

plot(sel_sum$bic, type = "l")
points(which.min(sel_sum$bic), sel_sum$bic[which.min(sel_sum$bic)], col = "red", pch = 20)

# Let's try train-test split

set.seed(1)
train <- sample(c(TRUE,FALSE), size = nrow(data), replace = TRUE)
test  <- (!train)

select <- regsubsets(y ~., data[train,], nvmax = 10)
sel_sum <- summary(select)

X <- model.matrix(y ~ ., data = data)

coef(select,id = 1)

mse <- rep(0,10)
for (i in 1:10) {
  coefi  = coef(select, id = i)
  vars   = names(coefi)
  preds  = as.numeric(X[test, vars] %*% coefi)
  mse[i] = mean((preds - data$y[test])^2)
  
}

which.min(mse)
plot(mse, type = "l")
points(which.min(mse),mse[which.min(mse)], pch = 20, col = "red")

# Cross-validation

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(data), replace = TRUE)
cv.errors <- matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

for (j in 1:k) {
  best.fit = regsubsets(y ~ ., data = data[folds != j,],
                        nvmax = 10)
  for (i in 1:10) {
    coefi  = coef(best.fit, id = i)
    vars   = names(coefi)
    X      = model.matrix(y ~ ., data = data[folds == j,])
    preds  = as.numeric(X[,vars] %*% coefi)
    cv.errors[j,i] = mean((data$y[folds == j]-preds)^2)
  }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors

plot(mean.cv.errors, type = "l")
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], pch = 20,
       col = "red")

best.fit <- regsubsets(y ~ ., data = data, nvmax = 9)
coef(best.fit,5)

pred <- predict(lm(y ~ x1 + x2 + x3 + x5 + x6, data = data),
        data[test,])

mean((pred - data$y[test])^2)

# The LASSO:

X <- model.matrix(y ~ ., data = data)
X_train <- model.matrix(y ~ ., data = data[train,])

lasso_reg <- glmnet(x = X, y = y, alpha = 1, lambda = 10^seq(-6,6,length.out = 200))
plot(lasso_reg)

set.seed(1)
blam <- cv.glmnet(X, y, alpha = 1)
plot(blam)
bestlam <- blam$lambda.min

best_lasso <- glmnet(x = X, y = y, alpha = 1, lambda = bestlam)
coef(best_lasso)

lasso_train <- glmnet(x = X_train, y = y[train], alpha = 1, lambda = bestlam)
preds <- predict(lasso_train, X[test,])

mean((preds - data$y[test])^2)

coef(best_lasso)[-2]


# Excercise 9
#############

rm(list = ls())

data <- College

set.seed(20)
train <- sample(c(TRUE,FALSE), size = nrow(data),
                replace = TRUE)
test <- !train

data_train <- data[train,]
data_test  <- data[test,]

# Linear

linear_mod <- lm(Apps ~ ., data = data_train)
summary(linear_mod)
pred <- predict(linear_mod, data_test)
linear_mse <- mean((pred - data_test$Apps)^2)

X <- model.matrix(Apps ~., data = data)
y <- data$Apps

# Lasso

set.seed(20)
blam <- cv.glmnet(x = X[train,], y = y[train], alpha = 1)$lambda.min

lasso_mod <- glmnet(x = X, y = y, alpha = 1, lambda = blam)
plot(glmnet(x = X, y = y, alpha = 1))

coef(lasso_mod)
preds <- predict(lasso_mod, X[test,])

lasso_mse <- mean((y[test] - preds)^2)

# Ridge

set.seed(20)
blam <- cv.glmnet(x = X[train,], y = y[train], alpha = 0)$lambda.min

ridge_mod <- glmnet(x = X, y = y, alpha = 0, lambda = blam)
plot(glmnet(x = X, y = y, alpha = 0))

coef(ridge_mod)
preds <- predict(ridge_mod, X[test,])

ridge_mse <- mean((y[test] - preds)^2)

c(linear_mse, lasso_mse, ridge_mse)

# Principal Component Regression

set.seed(21)
pcr_fit <- pcr(Apps ~ ., data = data_train, scale = TRUE,
               validation = "CV")
validationplot(pcr_fit, val.type = "MSEP")

pcr_pred <- predict(pcr_fit, X[test,], ncomp = 5)
