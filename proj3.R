dat <- read.csv(file="/home/noura/Desktop/stat4DS/project/train.csv", header=TRUE, sep=",")
High = ifelse(dat$loan_amount <= 70000, "No", "Yes")
dat = data.frame(dat,High)
# Split the data set into a training set and a test set.
set.seed(1)
train <- sample(1:nrow(dat), nrow(dat) / 2)
dat.train <- dat[train, ]
dat.test <- dat[-train, ]

#Fit a regression tree to the training set. 
#Plot the tree, and interpret the results. 
#What test error rate do you obtain ?
library(tree)
tree.dat <- tree(funded_yes_no ~ ., data = dat.train)
summary(tree.dat)

plot(tree.dat)
text(tree.dat, pretty = 0)

yhat <- predict(tree.dat, newdata = dat.test)
mean((yhat - dat.test$funded_yes_no)^2)

#Use cross-validation in order to determine the optimal level of tree complexity. 
#Does pruning the tree improve the test error rate

cv.dat<- cv.tree(tree.dat)
plot(cv.dat$size, cv.dat$dev, type = "b")
tree.min <- which.min(cv.dat$dev)
points(tree.min, cv.dat$dev[tree.min], col = "orange", cex = 2, pch = 20)

prune.dat<- prune.tree(tree.dat, best = 8)
plot(prune.dat)
text(prune.dat, pretty = 0)

yhat <- predict(prune.dat, newdata = dat.test)
mean((yhat - dat.test$funded_yes_no)^2)


#--
#Use the bagging approach in order to analyze this data. What test error rate do you obtain ? Use the
#“importance()” function to determine which variables are most important.

install.packages("randomForest")
rfdat <- randomForest:::randomForest(funded_yes_no ~ ., data = dat.train, mtry = 10, ntree = 500, importance = TRUE)
yhat.rfdat <- predict(rfdat, newdata = dat.test)
mean((yhat.rfdat - dat.test$funded_yes_no)^2)

randomForest:::importance(rfdat)
