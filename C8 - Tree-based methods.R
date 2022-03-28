# Chapter 8 - Tree-based methods

library(ISLR)         # Data
library(tree)         # Tree-based models
library(magrittr)     # Pipes
library(MASS)         # Boston data
library(randomForest) # Random forest
library(gbm)          # Boosting
library(ggplot2)      # Cool graphs

# Loading data: Carseats

data <- Carseats

##########################################################################

#################
# Decision trees#
#################

# 1. Fitting classification trees
#################################

# Recoding variables

High <- ifelse(data$Sales <= 8, "No", "Yes") %>% factor() # We need factor.
data <- data.frame(Carseats, High) # Another way of merging.

tree_carseats <- tree(High ~ . - Sales, data = data)
summary(tree_carseats)

plot(tree_carseats)
text(tree_carseats, pretty = 0)

set.seed(2)
train <- sample(1:nrow(data),200)
data_test <- data[-train,]
High_test <- High[-train]

tree_carseats <- tree(High ~ .-Sales, data, subset = train)     # Test data
tree_preds <- predict(tree_carseats, data_test, type = "class") # Test predicts
table(tree_preds, High_test)

(104+50)/200 # Correct classifications in testing data.

set.seed(3)
cv_carseats <- cv.tree(tree_carseats, FUN = prune.misclass)
names(cv_carseats)
cv_carseats # Second lowest: eight terminal nodes

par(mfrow = c(1,2))
plot(cv_carseats$size, cv_carseats$dev, type = "b")
plot(cv_carseats$k, cv_carseats$dev, type = "b")

prune_carseats <- prune.misclass(tree_carseats, best = 8) # Pruning the tree
plot(prune_carseats)
text(prune_carseats, pretty = 0)

tree_preds <- predict(prune_carseats, data_test, type = "class") #
table(tree_preds, High_test)

(89 + 62)/200 # Correct classification for pruned tree. More interpretability.

rm(list = ls())
par(mfrow = c(1,1))

# 2. Fitting regression trees
#############################

data <- Boston

set.seed(1)
train <- sample(1:nrow(data), nrow(data)/2)

tree_boston <- tree(medv ~ ., data, subset = train)
summary(tree_boston)

plot(tree_boston)
text(tree_boston, pretty = 0)

cv_boston <- cv.tree(tree_boston)
plot(cv_boston$size, cv_boston$dev, type = "b")

prune_boston <- prune.tree(tree_boston, best = 5)
plot(prune_boston)
text(prune_boston, pretty = 0)

yhat <- predict(tree_boston, newdata = data[-train,])
boston_test <- data[-train, "medv"]

plot(yhat, boston_test)
abline(0,1)
mse_pruned_tree <- mean((yhat - boston_test)^2)

rm(list = setdiff(ls(),"mse_pruned_tree"))

# 3. Bagging and random forest
##############################

data <- Boston

set.seed(1)
train <- sample(1:nrow(data), nrow(data)/2)
boston_test <- data[-train, "medv"]
bag_boston1 <- randomForest(medv ~., data = data, subset = train,
                           mtry = 13, importance = TRUE); bag_boston1

yhat_bag1 <- predict(bag_boston1, newdata = data[-train,])
plot(yhat_bag1,boston_test)
abline(0,1)
mse_bag1 <- mean((yhat_bag1 - boston_test)^2); mse_bag1

# Different number of trees grown

set.seed(1)
bag_boston2 <- randomForest(medv ~., data = data, subset = train,
                           mtry = 13, ntree = 25, importance = TRUE); bag_boston2

yhat_bag2 <- predict(bag_boston2, newdata = data[-train,])
plot(yhat_bag2,boston_test)
abline(0,1)
mse_bag2 <- mean((yhat_bag2 - boston_test)^2); mse_bag2

# Now, random forest:

set.seed(1)
rf_boston <- randomForest(medv ~., data = data, subset = train,
                                     mtry = 6, importance = TRUE)

yhat_rf <- predict(rf_boston, newdata = data[-train,])
mse_rf <- mean((yhat_rf - boston_test)^2); mse_rf

importance(rf_boston)
varImpPlot(rf_boston)

rm(list = setdiff(ls(),c("mse_pruned_tree","mse_bag1","mse_bag2","mse_rf")))

# 4. Boosting
#############

data <- Boston

set.seed(1)
train <- sample(1:nrow(data), nrow(data)/2)
boston_test <- data[-train, "medv"]

boost_boston1 <- gbm(medv ~ ., data = data[train,], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)
summary(boost_boston1)

par(mfrow = c(1,2))
plot(boost_boston1, i = "rm")
plot(boost_boston1, i = "lstat")

yhat_boost1 <- predict(boost_boston1, newdata = data[-train,])
mse_boost  <- mean((yhat_boost1 - boston_test)^2)

# Different learning rate

boost_boston2 <- gbm(medv ~ ., data = data[train,], distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 4, shrinkage = 0.2,
                     verbose = FALSE)

yhat_boost2 <- predict(boost_boston2, newdata = data[-train,])
mse_boost2  <- mean((yhat_boost2 - boston_test)^2)

results <- cbind(mse_pruned_tree, mse_bag1, mse_bag2,
                 mse_rf, mse_boost, mse_boost2)

dimnames(results) <- list("MSE", c("Pruned tree","Bag(500 trees)",
                                  "Bag(25 trees)","Random forest",
                                  "Boost(0.01 shrink)", "Boost(0.2 shrink)"))

results

rm(list = ls())

###############################################################3

# Excersises #
##############

# E.4
######

p1 <- seq(0.01,1,by = 1/100)
p2 <- 1 - p1

E <- NULL
G <- NULL
D <- NULL

for (i in 1:100) {
  E[i] = 1-max(p1[i],p2[i])
  G[i] = 2*p1[i]*p2[i]
  D[i] = -(p1[i]*log(p1[i]) + p2[i]*log(p2[i]))
}

par(mfrow = c(1,3))

plot(p1,E,"l", col = "blue",
     xlab = "Proportion class 1", ylab = "Error", main = "Classification error")
plot(p1,G,"l", col = "blue",
     xlab = "Proportion class 1", ylab = "Error", main = "Gini index")
plot(p1,D,"l", col = "blue",
     xlab = "Proportion class 1", ylab = "Error", main = "Cross-entropy")

par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(p1,D,"l", col = "black",
     xlab = "Proportion - class 1", ylab = "Error", main = "Cross-entropy")
lines(p1,E,"l", col = "red",
      xlab = "Proportion - class 1", ylab = "Error", main = "Classification error")
lines(p1,G,"l", col = "blue",
      xlab = "Proportion - class 1", ylab = "Error", main = "Gini index")
legend("right", 
       legend = c("Cross-entropy", "Gini index", "Class. error"), 
       col = c("black", 
               "blue",
               "red"), 
       pch = 20, 
       bty = "o", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(-0.3, -0.3))

rm(list = ls()) ; par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

# E. 7
######

data <- Boston

set.seed(1)
train <- sample(1:nrow(data),nrow(data)/2)
test <- data[-(train),"medv"]

terr25   <- NULL
terr500  <- NULL
terrtree <- NULL

# 25 trees
for (i in 1:13) {
  set.seed(1)
  tree_fit = randomForest(medv ~., data = data, subset = train,
                          mtry = i, ntree = 25)
  preds = predict(tree_fit, data[-train,])
  terr25[i] = mean((test - preds)^2)
}

# 500 trees
for (i in 1:13) {
  set.seed(1)
  tree_fit = randomForest(medv ~., data = data, subset = train,
                          mtry = i, importance = TRUE)
  preds = predict(tree_fit, data[-train,])
  terr500[i] = mean((test - preds)^2)
}

# Variable number of trees while mtry = 4

for (i in 1:50) {
  set.seed(2)
  tree_fit = randomForest(medv ~., data = data, subset = train,
                          mtry = 4, ntree = i*10)
  preds = predict(tree_fit, data[-train,])
  terrtree[i] = mean((test - preds)^2)
}


plot(1:13, terr500, col = "black", "l",
     ylab = "Test error", xlab = "Variables chosen",
     main = "Test error for different mtry values")
lines(1:13, terr25, col = "red", "l")
legend("topright",
       legend = c("25 trees","500 trees"),
       col = c("red","black"),
       pch = 20, 
       bty = "o", 
       pt.cex = 1, 
       cex = 0.75, 
       text.col = "black", 
       horiz = F , 
       inset = c(0, 0))

plot(1:50*10, terrtree, col = "black", "l",
     ylab = "Test error", xlab = "Number of trees",
     main = "Test error for different numbers of trees while mtry = 4")
points(which.min(terrtree)*10, terrtree[which.min(terrtree)],
      pch = 20, col = "red", cex = 2)

rm(list = ls())

# E9 
####

data <- OJ
summary(data)
ggplot(data) +
  geom_bar(aes(x = Purchase, y = ..count..), color = "black")

set.seed(1)
train <- sample(1:nrow(data), 800)

data_train <- data[train,]
data_test  <- data[-train,]

# Setting up the tree

treefit <- tree(Purchase ~ . -WeekofPurchase, data)
summary(treefit)
treefit

plot(treefit)
text(treefit, pretty = 0)

# Testing error:
test_vals  <- data_test[,"Purchase"]
test_preds <- predict(treefit, data_test, type = "class")

1 - mean((test_vals == test_preds)) # Benchmark: 0.39 error
table(test_vals, test_preds)

## Optimal tree size

set.seed(1)
cv_tree <- cv.tree(treefit, FUN = prune.misclass) # Cost complexity optimal: 6
plot(cv_tree$size, cv_tree$dev, "l")

pruned_treefit <- prune.misclass(treefit, best = 6)
plot(pruned_treefit)
text(pruned_treefit, pretty = 0)

test_pruned_preds <- predict(pruned_treefit, data_test, type = "class")

1 - mean((test_vals == test_pruned_preds)) # Benchmark: 0.39 error
table(test_vals, test_pruned_preds)

rm(list = ls())

# E. 10
#######

data <- Hitters[!is.na(Hitters$Salary),]
data[,"log_salary"] = log(data[,"Salary"])
data[,"Salary"] = NULL

set.seed(1)
train <- sample(1:nrow(data),200)
data_train <- data[train,]
data_test  <- data[-train,]

errs1 <- NULL
errs2 <- NULL
errs3 <- NULL
errs4 <- NULL

for (d in 1:4) {
  for (k in 1:100) {
    set.seed(1)
    boost = gbm(log_salary ~ ., data = data_train, distribution = "gaussian",
                n.trees = 1000, interaction.depth = d, shrinkage = k/1000)
    preds = predict(boost, newdata = data_test)
    assign(paste0("errs",d),
           append(
             eval(
               parse(
                 text = paste0("errs",d)
                 )
               )
             ,mean((preds - data_test$log_salary)^2))
           )
  }
}

plot(1:100/1000,errs1, "l", col = "red", xlab = "Learning rate",
     ylab = "Test MSE", main = "Log-salary (MSE boosting estimate)")
lines(1:100/1000,errs2, "l", col = "blue")
lines(1:100/1000,errs3, "l", col = "green")
lines(1:100/1000,errs4, "l", col = "orange")
legend("topright",
       legend = c("Depth 1","Depth 2","Depth 3","Depth 4"),
       col = c("red","blue","green","orange"),
       pch = 20, 
       bty = "o", 
       pt.cex = 1, 
       cex = 0.75, 
       text.col = "black", 
       horiz = F , 
       inset = c(0, 0))

errs2[which.min(errs2)]
errs3[which.min(errs3)] # Apparently, this is it.
errs4[which.min(errs4)]

best_boost <- gbm(log_salary ~ ., data = data_train, distribution = "gaussian",
                  n.trees = 1000, interaction.depth = 3, shrinkage = 0.004)
