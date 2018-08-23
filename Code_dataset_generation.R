
set.seed(123)
kiva_loans$funded_yes_no <- kiva_loans$funded_amount >0
kiva_loans$funded_yes_no[kiva_loans$funded_yes_no == TRUE]<- 1
kiva_loans$funded_yes_no[kiva_loans$funded_yes_no == FALSE]<- 0
mask <- sample(dim(kiva_loans)[1],dim(kiva_loans)[1]/5,replace = FALSE)
test <- kiva_loans[mask,]
train <- kiva_loans[-mask,]
test_answer <- test$funded_amount
test <- subset(test, select=-c(funded_amount,funded_yes_no))
write.csv(test, file = "test_without_fundedAmount_fundedYesNo.csv")
write.csv(train, file = "train.csv")
