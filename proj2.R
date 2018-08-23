library(leaps)
library(glmnet)
library(pls)

#Best subset selection
dat <- read.csv(file="/home/noura/Desktop/stat4DS/project/train.csv", header=TRUE, sep=",")
High = ifelse(dat$loan_amount <= 70000, "No", "Yes")
dat = data.frame(dat,High)
#best subset selection.
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k = 10
folds <- sample(1:k, nrow(dat), replace = TRUE)
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit <- regsubsets(funded_yes_no ~ ., data = dat[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred <- predict(best.fit, dat[folds == j, ], id = i)
    cv.errors[j, i] <- mean((dat$crim[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = "b", xlab = "# of variables", ylab = "CV error")

#------------------------------------------------------------------------------
## Lasso 
x <- model.matrix(funded_yes_no ~ ., dat)[, -1]
y <- dat$funded_yes_no
cv.out <- cv.glmnet(x, y, alpha = 1, type.measure = "mse")
plot(cv.out)
#------------------------------------------------------------------------------
#ridge regression
cv.out <- cv.glmnet(x, y, alpha = 0, type.measure = "mse")
plot(cv.out)
#------------------------------------------------------------------------------
#PCR
pcr.fit <- pcr(funded_yes_no ~ ., data = dat, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
#------------------------------------------------------------------------------
#Perform polynomial regression to predict “funded_yes_no” using “loan_amount”. 
#Use cross-validation to select the optimal degree d for the polynomial.
library(ISLR)
library(boot)
set.seed(1)
deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(funded_yes_no ~ poly(loan_amount, i), data = dat)
  deltas[i] <- cv.glm(dat, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "orange", cex = 2, pch = 20)
# The optimal degree for the polynomial is 2 
#anova 
fit1 <- lm(funded_yes_no ~ loan_amount, data = dat)
fit2 <- lm(funded_yes_no ~ poly(loan_amount, 2), data = dat)
fit3 <- lm(funded_yes_no ~ poly(loan_amount, 3), data = dat)
fit4 <- lm(funded_yes_no ~ poly(loan_amount, 4), data = dat)
fit5 <- lm(funded_yes_no ~ poly(loan_amount, 5), data = dat)
anova(fit1, fit2, fit3, fit4, fit5)
# from p-value, 4th and 5th polynomial appear to provide a reasonable fit

#polynomial fitting 
plot(funded_yes_no ~ loan_amount, data = dat, col = "grey")
loan_amount_lims <- range(dat$loan_amount)
loan_amount.grid <- seq(from = loan_amount_lims[1], to = loan_amount_lims[2])
fit <- lm(funded_yes_no ~ poly(loan_amount, 5), data = dat)
preds <- predict(fit, newdata = list(loan_amount = loan_amount.grid))
lines(loan_amount.grid, preds, col = "red", lwd = 2)

#-----------------------------------------------------------------------------

