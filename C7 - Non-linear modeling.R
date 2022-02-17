# Chapter 7 - Non-linear modeling

library(ISLR)    # Data
library(splines) # Splines
library(gam)     # GAMs
library(akima)   # Plots surfaces

#####################################################################################

# Polynomial and step functions
# ### ### ### ### ### ### ### #

# Load data
data <- Wage


fit <- lm(wage ~ poly(age,4), data = data)
coef(summary(fit))

# Function poly() gives matrix whose columns are orthogonal polynomials.
# If we want normal polynomials, then poly(raw = TRUE)

fit2 <- lm(wage ~ poly(age,4, raw = TRUE), data = data)
coef(summary(fit2))

# We can fit raw polynomials too as following:

lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data)
lm(wage ~ cbind(age,age^2,age^3,age^4), data)

# I() and cbind() work as a wrapper.

agelims  <- range(data$age)
age.grid <- seq(from = agelims[1], to = agelims[2]) 
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)

se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), oma = c(0,0,4,0)) 
plot(data$age, data$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Degree 4 polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))

## Which degree to use? ##
 
# We're using ANOVA testing, of course!

for (i in 1:5) {
  assign(paste0("fit.",i),lm(wage ~ poly(age,i), data = data))
}

# Null hypothesis: Model M1 (previous) is sufficient to explain M2

anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# But, if we're using orthogonal polynomials:

coef(summary(fit.5)) # P-values are the same!

# But! Anova can work with raw polynomials or even with other terms! 
# Just remember that models have to be NESTED.

fit.1 <- lm(wage~education + age, data = data )
fit.2 <- lm(wage~education + poly(age ,2), data = data)
fit.3 <- lm(wage~education + poly(age ,3), data = data)
anova ( fit.1 , fit.2 , fit.3)

# Remember: we can use cross-validation for degree.

## Logistic regression ##

fit <- glm(I(wage > 250) ~ poly(age,4), data = data, family = binomial)
# Used I() function to create binary response on the fly

preds <-  predict(fit, newdata = list(age = age.grid), se = T)

# Transform logit to probability

pfit <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit,preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

# If we just used type = "response", conf. intervals would have included neg. probs!

plot(data$age,I(data$wage > 250), xlim = agelims, type = "n", ylim = c(0,.2))
points(jitter(data$age), I((data$wage > 250)/5), cex = .5, pch = "|",
       col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

## Time for step function! ## 

table(cut(data$age,4))

fit <- lm(wage ~ cut(age,4), data) # Treat them as dummies!

#####################################################################################

# Splines
# ### ###

# Function bs() generates matrix of basis functions

fit  <- lm(wage ~ bs(age, knots = c(25,40,60)), data)
pred <- predict(fit, newdata = list(age = age.grid), se = T) 
plot(data$age, data$wage, col = "gray")
lines(age.grid, pred$fit,lwd = 2)
lines(age.grid, pred$fit + 2*pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se.fit, lty = "dashed")

attr(bs(data$age, df = 6),"knots")

fit2 <- lm(wage ~ ns(age, df = 4), data)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

plot(data$age, data$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing spline")
fit <- smooth.spline(data$age, data$wage, df = 16)
fit2 <- smooth.spline(data$age, data$wage, cv = TRUE)

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
lines(fit3, col = "green", lwd = 2)

plot(data$age, data$wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data)
fit2 <- loess(wage ~ age, span = .5, data)

lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red","blue"), lty = 1,lwd = 2, cex = 0.8)

#####################################################################################

# GAMs
# ### 

# We can use fit, since a GAM is a big linear regression

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data)

gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = data)

par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue") # Recognizes class gam and uses plot.gam

plot.Gam(gam1, se = TRUE, col = "red")

gam.m1 <- gam(wage ~ s(age,5) + education, data = data)
gam.m2 <- gam(wage ~ year + s(age,5) + education, data = data)

anova(gam.m1, gam.m2, gam.m3)

summary(gam.m3)

preds <- predict(gam.m2, newdata = data)

gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education,
              data = data)
plot(gam.lo, se = TRUE, col = "green")

gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education,
                data = data)

plot(gam.lo.i)

gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5)+ education,
              family = binomial, data = data)
par(mfrow = c(1,3))
plot(gam.lr, se = TRUE, col = "green")

table(data$education,I(data$wage > 250))

# Since no high earners in <HS category, fit excluding this
gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial,
                data = data, subset = (education !="1. < HS Grad"))
  plot(gam.lr.s, se = TRUE, col = "green")


#######################################################################################

##############
# Excercises #
##############

# E.6  
### #
  

## A ##

data <- Wage
d    <- 10
  
for (deg in 1:5) {
  assign(paste0("fit_deg_",deg), lm(wage ~ poly(age, deg, raw = TRUE), data = data))
}

anova(fit_deg_1, fit_deg_2, fit_deg_3,
      fit_deg_4, fit_deg_5)            # For ANOVA, d = 3 or d = 4  

# Cross-validation with R base

k <- 10
set.seed(123)
folds <- sample(k,nrow(data), replace = TRUE)

cv_errors <- matrix(rep(NA,k*d),k,d,
                    dimnames = list(sprintf("Fold %s",seq(1,k)),
                                    sprintf("Deg %s",seq(1,d)))
                    )

for (i in 1:d) {
  for (j in 1:k) {
    y    <- data[folds == j, "wage"]
    mod  <- lm(wage ~ poly(age, i, raw = TRUE), data = data[folds != j,])
    pred <- predict(mod, data[folds == j,])
    err  <- mean((y-pred)^2)
    cv_errors[j,i] = err
  }
}

plot(1:d,apply(cv_errors, 2, mean), "l", xlab = "Degree",
     ylab = "Estimated MSE (CV)", main = "Degree - MSE")
points(apply(cv_errors, 2, mean), col = "blue", pch = 20,
       cex = 1.25)
points(which.min(apply(cv_errors, 2, mean)),
       apply(cv_errors, 2, mean)[which.min(apply(cv_errors, 2, mean))],
       col = "red", pch = 20,
       cex = 1.5)

rm(list = ls())
par(mfrow = c(1,1))

## B ##

# We're going to manually compute limits for cut function. Else, things won't work,
# since predict function creates other categories again, different from the ones used
# in the training model.

data <- Wage
cuts <- 50

k <- 10
set.seed(123)
folds <- sample(k,nrow(data), replace = TRUE)

cv_errors <- matrix(rep(NA,k*(cuts-1)),k,cuts-1,
                    dimnames = list(sprintf("Fold %s",seq(1,k)),
                                    sprintf("Cuts %s",seq(2,cuts)))
)

for (i in 2:cuts) {
  for (j in 1:k) {
    
    y = data[folds == j, "wage"]
    x_train = data[folds != j,]
    x_test  = data[folds == j,]
    
    lower = min(x_train$age)
    upper = max(x_train$age)
    
    ct    = lower + seq(0,1,1/i)*(upper - lower)
    ct[1] = lower - 0.1
    ct[i+1] = upper + 0.1
    
    mod   <- lm(wage ~ cut(age,ct), data = x_train)
    if ((i != 31 & j != 7) & (i != 35 & j != 3)) {
      pred  <- predict(mod, x_test)
      err   <- mean((y-pred)^2)
      cv_errors[j,i-1] = err
    } else{
      cv_errors[j,i-1] = NA
    }
  }
}

cv_errors_mean <- apply(cv_errors, 2, FUN = mean, na.rm = TRUE)

plot(2:cuts, cv_errors_mean, "l", xlab = "Number of cuts",
     ylab = "Ten fold cross-validation estimate",
     main = "Cross-validation MSE estimation")
points(2:cuts, cv_errors_mean, col = "blue", pch = 20,
       cex = 1.25)
points(which.min(cv_errors_mean)+1,
       cv_errors_mean[which.min(cv_errors_mean)],
       col = "red", pch = 20,
       cex = 1.5)

which.min(cv_errors_mean)

# Best model may be with eleven cuts

age.grid <- seq(min(data$age),max(data$age))

best_step_model <- lm(wage ~ cut(age,11), data)
preds <- predict(best_step_model, data.frame(age = age.grid))

plot(data$age, data$wage, col = "gray",
     xlab = "Age",
     ylab = "Wage",
     main = "Step-function estimate")
lines(age.grid, preds, col = "red")

rm(list = ls())

# E.7  
### #

data <- Wage
names(data)

features <- names(data)[c(1,2,3,5,7)]

data <- data[,c("wage",features)]

plot(data[,features])

# Trying the linear model

linear_mod <- lm(wage ~ ., data)
summary(linear_mod)

plot(data$age,data$wage)
plot(data$year,data$wage)
plot(data$maritl,data$wage)
plot(data$jobclass,data$wage)
plot(data$education,data$wage)

# Possible interactions between martil-age, we can LOESS that and spline the rest.

gam_mod <- gam(wage ~ lo(maritl,age,span = 0.2) + education + s(year,5) + jobclass,
               data = data)

# Cross validation, yay!

k <- 10
set.seed(123)
folds <- sample(k,nrow(data), replace = TRUE)

cv_errors <- matrix(rep(NA,k*2),k,2,
                    dimnames = list(sprintf("Fold %s",seq(1,k)),
                                    c("Linear","GAM")))


for (f in 1:k) {
  y <- data[folds == f, "wage"]
  
  x_train <- data[folds != f,]
  x_test  <- data[folds == f,]
  
  linear_mod <- lm(wage ~ ., x_train)
  
  preds <- predict(linear_mod, x_test)
  
  cv_errors[f,1] = mean((y-preds)^2)
}

for (f in 1:k) {
  y <- data[folds == f, "wage"]
  
  x_train <- data[folds != f,]
  x_test  <- data[folds == f,]
  
  gam_mod <- gam(wage ~ lo(maritl,age,span = 0.2) + education +
                 s(year,5) + jobclass, data = data)
  
  preds <- predict(gam_mod, x_test)
  
  cv_errors[f,2] = mean((y-preds)^2)
}

cv_errors_mean = apply(cv_errors, 2, mean)

cv_errors_mean

# Yeah, we got a better model, one could say! Difference ain't that big, but at least
# this is my first time trying something different than linear! I really am proud.

