setwd("~/Downloads/MISC")
credit <- read.csv("credit3.csv")
credit$NPV <- gsub(",","",credit$NPV)
credit$NPV <- as.numeric(credit$NPV)
head(credit$NPV, 20)
credit$CAN <- rep("Yes", nrow(credit))
credit$CAN[credit$NPV <0] <- "No"
credit$CAN <- factor(credit$CAN, levels = c("No", "Yes"))
credit$CAN <- as.numeric(credit$CAN) - 1
credit$OBS. <- NULL
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$JOB <- as.factor(credit$JOB)
credit$TYPE <- as.factor(credit$TYPE)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$AMOUNT_REQUESTED <- gsub(",","",credit$AMOUNT_REQUESTED) # Converts to charecter and takes off','.
credit$AMOUNT_REQUESTED <- as.numeric(credit$AMOUNT_REQUESTED)
credit$CREDIT_EXTENDED <- NULL
credit$CAN <- as.factor(credit$CAN)
str(credit)
set.seed(12345)
x <- sample(nrow(credit),0.7*nrow(credit),replace = FALSE)
train <- credit[x,]
valid <- credit[-x,]
fit2 <- glm(CAN~.-NPV,train,family="binomial")
summary(fit2)
actual <- valid$CAN
train$pred <- predict(fit2,type="response")
predicted.probability <- predict(fit2, newdata=valid, type = "response") 
cutoff <- 0.5
predicted <- ifelse(predicted.probability > cutoff, 1, 0)
confusion <- table(actual, predicted)
confusion
valid$pred <- predicted.probability
X1 <- train[train$pred>0.5,]
X2 <- valid[valid$pred>0.5,]
fit3 <- lm(NPV~.-CAN-pred,data=X1)
summary(fit3)
pred <- predict(fit3)
X1$pred <- pred
X1 <- X1[order(-pred),]
X1$SUM <- cumsum(X1$NPV)
plot(X1$SUM~X1$pred)
z = which.max(X1$SUM)
cat("The optimal cutoff is:",X1$pred[z])
pred <- predict(fit3,newdata=X2)
X2$pred <- pred
X2 <- X2[order(-pred),]
Xnew <- as.numeric(X2$pred>X1$pred[z])
sum(Xnew)
sum(Xnew*X2$NPV)/sum(Xnew)
sum(Xnew*X2$NPV)
