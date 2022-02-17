# Chapter 4 - Classification
library(ISLR)

# Stock market data
###################

data <- Smarket

names(data)
summary(data)
pairs(data)
cor(data[,-9])

plot(data$Volume)

## Linear regression

log.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               family = binomial, data = data)

summary(log.fit)
summary(log.fit)$coef
coef(log.fit)

log.fit.probs <- predict(log.fit, type = "response")
contrasts(data$Direction)

log.fit.pred <- rep("Down", 1250)
log.fit.pred[log.fit.probs > 0.5] = "Up"

table(log.fit.pred, data$Direction)
mean(log.fit.pred == data$Direction)

train <- (data$Year < 2005)
data_2005 <- data[!train,]
dim(data_2005)
direction_2005 <- data_2005$Direction

log.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               family = binomial, data = data, subset = train)

log.fit.probs <- predict(log.fit, data_2005, type = "response")

log.fit.pred <- rep("Down", 252)
log.fit.pred[log.fit.probs > 0.5] = "Up"
table(log.fit.pred, direction_2005)

mean(log.fit.pred == direction_2005)
mean(log.fit.pred != direction_2005) # Test error

## Only two predictors

log.fit <- glm(Direction ~ Lag1 + Lag2,
               family = binomial, data = data, subset = train)

log.fit.probs <- predict(log.fit, data_2005, type = "response")

log.fit.pred <- rep("Down", 252)
log.fit.pred[log.fit.probs > 0.5] = "Up"
table(log.fit.pred, direction_2005)

mean(log.fit.pred == direction_2005)
mean(log.fit.pred != direction_2005) # Test error

## Predict specific values
predict(log.fit, newdata = data.frame(Lag1 = c(1.2,1.5),
                                      Lag2 = c(1.1,-0.8)),
        type = "response")

# Linear Discriminant Analysis
##############################

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

lda.pred <- predict(lda.fit, data_2005)

lda.class <- lda.pred$class
table(lda.class, direction_2005)
mean(lda.class == direction_2005)
mean(lda.class != direction_2005)

sum(lda.pred$posterior[,1] >=.5)
sum(lda.pred$posterior[,1] <.5)

sum(lda.pred$posterior[,1] >=.9)

# Quadratic Discriminant Analysis
#################################

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = data,
               subset = train)

qda.class <- predict(qda.fit, data_2005)$class
table(qda.class, direction_2005)
mean(qda.class == direction_2005)

# K-Nearest Neighbors
#####################

library(class)

train.X <- cbind(data$Lag1, data$Lag2)[train,]
test.X  <- cbind(data$Lag1, data$Lag2)[!train,]
train.direction <- data$Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.direction, 3)
table(knn.pred, direction_2005)
mean(knn.pred == direction_2005)


