setwd("~/Downloads/MISC")
credit <- read.csv("credit3.csv")
str(credit)
credit$NPV <- gsub(",","",credit$NPV)
credit$NPV <- as.numeric(credit$NPV)
head(credit$NPV, 20)
credit$CAN <- rep("Yes", nrow(credit))
credit$CAN[credit$NPV <0] <- "No"
credit$CAN <- factor(credit$CAN, levels = c("No", "Yes"))
str(credit$CAN)
credit$CAN <- as.numeric(credit$CAN) - 1
credit$CHK_ACCT <- ifelse(credit$CHK_ACCT > 0, credit$CHK_ACCT+1, credit$CHK_ACCT)
credit$CHK_ACCT[credit$CHK_ACCT == 4] <- 1
credit$CHK_ACCT0 <- ifelse(credit$CHK_ACCT == 0, 1, 0)
credit$CHK_ACCT1 <- ifelse(credit$CHK_ACCT == 1, 1, 0)
credit$CHK_ACCT2 <- ifelse(credit$CHK_ACCT == 2, 1, 0)
credit$CHK_ACCT3 <- ifelse(credit$CHK_ACCT == 3, 1, 0)
credit$CHK_ACCT <- NULL
 # credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT0 <- rep(0,nrow(credit))
credit$SAV_ACCT0[credit$SAV_ACCT == 0] <- 1
credit$SAV_ACCT1 <- rep(0,nrow(credit))
credit$SAV_ACCT1[credit$SAV_ACCT == 1] <- 1
credit$SAV_ACCT2 <- rep(0,nrow(credit))
credit$SAV_ACCT2[credit$SAV_ACCT == 2] <- 1
credit$SAV_ACCT3 <- rep(0,nrow(credit))
credit$SAV_ACCT3[credit$SAV_ACCT == 3] <- 1
credit$SAV_ACCT4 <- rep(0,nrow(credit))
credit$SAV_ACCT4[credit$SAV_ACCT == 4] <- 1
credit$SAV_ACCT <- NULL
credit$NUM_CREDITS1 <- rep(0,nrow(credit))
credit$NUM_CREDITS1[credit$NUM_CREDITS == 1] <- 1
credit$NUM_CREDITS2 <- rep(0,nrow(credit))
credit$NUM_CREDITS2[credit$NUM_CREDITS == 2] <- 1
credit$NUM_CREDITS3 <- rep(0,nrow(credit))
credit$NUM_CREDITS3[credit$NUM_CREDITS == 3] <- 1
credit$NUM_CREDITS4 <- rep(0,nrow(credit))
credit$NUM_CREDITS4[credit$NUM_CREDITS == 4] <- 1
credit$NUM_CREDITS <- NULL
credit$HISTORY0 <- rep(0,nrow(credit))
credit$HISTORY0[credit$HISTORY == 0] <- 1
credit$HISTORY1 <- rep(0,nrow(credit))
credit$HISTORY1[credit$HISTORY == 1] <- 1
credit$HISTORY2 <- rep(0,nrow(credit))
credit$HISTORY2[credit$HISTORY == 2] <- 1
credit$HISTORY3 <- rep(0,nrow(credit))
credit$HISTORY3[credit$HISTORY == 3] <- 1
credit$HISTORY4 <- rep(0,nrow(credit))
credit$HISTORY4[credit$HISTORY == 4] <- 1
credit$HISTORY <- NULL
credit$PRESENT_RESIDENT1 <- rep(0,nrow(credit))
credit$PRESENT_RESIDENT1[credit$PRESENT_RESIDENT == 1] <- 1
credit$PRESENT_RESIDENT2 <- rep(0,nrow(credit))
credit$PRESENT_RESIDENT2[credit$PRESENT_RESIDENT == 2] <- 1
credit$PRESENT_RESIDENT3 <- rep(0,nrow(credit))
credit$PRESENT_RESIDENT3[credit$PRESENT_RESIDENT == 3] <- 1
credit$PRESENT_RESIDENT4 <- rep(0,nrow(credit))
credit$PRESENT_RESIDENT4[credit$PRESENT_RESIDENT == 4] <- 1
credit$PRESENT_RESIDENT <- NULL
credit$EMPLOYMENT0 <- rep(0,nrow(credit))
credit$EMPLOYMENT0[credit$EMPLOYMENT == 0] <- 1
credit$EMPLOYMENT1 <- rep(0,nrow(credit))
credit$EMPLOYMENT1[credit$EMPLOYMENT == 1] <- 1
credit$EMPLOYMENT2 <- rep(0,nrow(credit))
credit$EMPLOYMENT2[credit$EMPLOYMENT == 2] <- 1
credit$EMPLOYMENT3 <- rep(0,nrow(credit))
credit$EMPLOYMENT3[credit$EMPLOYMENT == 3] <- 1
credit$EMPLOYMENT4 <- rep(0,nrow(credit))
credit$EMPLOYMENT4[credit$EMPLOYMENT == 4] <- 1
credit$EMPLOYMENT <- NULL
credit$JOB0 <- rep(0,nrow(credit))
credit$JOB0[credit$JOB == 0] <- 1
credit$JOB1 <- rep(0,nrow(credit))
credit$JOB1[credit$JOB == 1] <- 1
credit$JOB2 <- rep(0,nrow(credit))
credit$JOB2[credit$JOB == 2] <- 1
credit$JOB3 <- rep(0,nrow(credit))
credit$JOB3[credit$JOB == 3] <- 1
credit$JOB <- NULL
credit$NUM_DEPENDENTS1 <- rep(0,nrow(credit))
credit$NUM_DEPENDENTS1[credit$NUM_DEPENDENTS == 1] <- 1
credit$NUM_DEPENDENTS2 <- rep(0,nrow(credit))
credit$NUM_DEPENDENTS2[credit$NUM_DEPENDENTS == 2] <- 1
credit$NUM_DEPENDENTS <- NULL
credit$INSTALL_RATE1 <- rep(0,nrow(credit))
credit$INSTALL_RATE1[credit$INSTALL_RATE == 1] <- 1
credit$INSTALL_RATE2 <- rep(0,nrow(credit))
credit$INSTALL_RATE2[credit$INSTALL_RATE == 2] <- 1
credit$INSTALL_RATE3 <- rep(0,nrow(credit))
credit$INSTALL_RATE3[credit$INSTALL_RATE == 3] <- 1
credit$INSTALL_RATE4 <- rep(0,nrow(credit))
credit$INSTALL_RATE4[credit$INSTALL_RATE == 4] <- 1
credit$INSTALL_RATE <- NULL
credit$TYPE0 <- rep(0,nrow(credit))
credit$TYPE0[credit$TYPE == 0] <- 1
credit$TYPE1 <- rep(0,nrow(credit))
credit$TYPE1[credit$TYPE == 1] <- 1
credit$TYPE2 <- rep(0,nrow(credit))
credit$TYPE2[credit$TYPE == 2] <- 1
credit$TYPE3 <- rep(0,nrow(credit))
credit$TYPE3[credit$TYPE == 3] <- 1
credit$TYPE4 <- rep(0,nrow(credit))
credit$TYPE4[credit$TYPE == 4] <- 1
credit$TYPE5 <- rep(0,nrow(credit))
credit$TYPE5[credit$TYPE == 5] <- 1
credit$TYPE6 <- rep(0,nrow(credit))
credit$TYPE6[credit$TYPE == 6] <- 1
credit$TYPE <- NULL
credit$AMOUNT_REQUESTED <- gsub(",","",credit$AMOUNT_REQUESTED) # Converts to charecter and takes off','.
credit$AMOUNT_REQUESTED <- as.numeric(credit$AMOUNT_REQUESTED)
credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED) # Converts to charecter and takes off','.
credit$CREDIT_EXTENDED <- as.numeric(credit$CREDIT_EXTENDED)
str(credit)
# charecter can be coverted into numeric directly unlike factor!!
credit$OBS. <- NULL
credit$NPV <- NULL
#credit$CREDIT_EXTENDED <- NULL
str(credit)
credit[,-12] <- scale(credit[,-12])
set.seed(12345)
x <- sample(nrow(credit),0.7*nrow(credit),replace = FALSE)
train <- credit[x,]
valid <- credit[-x,]
train_output <- as.vector(train$CAN)
valid_output <- as.vector(valid$CAN)
train_input <- train[,-12]
valid_input <- valid[,-12]
kmax <- 15
z <- 1:kmax
library(class)
eff <- sapply(1:kmax, function(cutoff) knn(train_input, train_input,train_output, k=cutoff))
effvalid <- sapply(1:kmax, function(cutoff) knn(train_input, valid_input,train_output, k=cutoff))
str(eff)
head(eff, 10)
eff <- matrix(eff, nrow = 700, ncol = 15)
eff <- apply(eff,2,as.numeric)
head(eff,18)
effvalid <- matrix(effvalid, nrow = nrow(valid_input), ncol = 15)
effvalid <- apply(effvalid,2,as.numeric)
error_train <- sapply(z, function(bags) 1-sum(eff[,bags]==train_output)/length(train_output))
error_train
error_valid <- sapply(z, function(bags) 1-sum(effvalid[,bags]==valid_output)/length(valid_output))
error_valid
plot(c(1,kmax),c(0,0.8),type="n", xlab="k",ylab="Error Rate") 
lines(error_train,col="red")
lines(error_valid,col="blue")
legend(10, 0.75, c("Training","Validation"),lty=c(1,1), col=c("blue","red"))
# Optimal K
e <- which.min(error_valid)
e
pred <- knn(train_input, train_input,train_output, k=e)
predvalid <- knn(train_input, valid_input,train_output, k=e)
CMtrain <- table(pred, train_output)
CMtrain
CMvalid <- table(predvalid, valid_output)
CMvalid
CMvalid[1,2]/(CMvalid[1,1]+CMvalid[1,2])
CMvalid[2,1]/(CMvalid[2,2]+CMvalid[2,1])
predfinal <- knn(train_input, valid_input, train_output, k=e, prob=T)
probs <- attr(predfinal, "prob")
probs <- ifelse(predfinal ==1, probs, 1-probs)
head(probs,10)
fpr <- sapply(seq(0,1,length = 200), function(cut) sum(probs>cut & valid_output ==0)/sum(valid_output ==0))
tpr <- sapply(seq(0,1,length = 200), function(cut) sum(probs>cut & valid_output ==1)/sum(valid_output ==1))
str(fpr)
summary(fpr)
knntable <- data.frame(fpr,tpr)
plot(tpr ~ fpr, data = knntable, type = "s", main="ROC", xlab="1-specificity", ylab="sensitivity",col="red")
library(ROCR)
library(pROC)
plot(roc(valid_output, probs))
k2 <- 10
library(class)
eff2 <- sapply(1:k2, function(cutoff2) knn(train_input, train_input,train_output, k=cutoff2))
effvalid2 <- sapply(1:k2, function(cutoff2) knn(train_input, valid_input,train_output, k=cutoff2))
effvalid2 <- matrix(effvalid2, nrow = nrow(valid_input), ncol = 15)
effvalid2 <- apply(effvalid2,2,as.numeric)
error_valid2 <- sapply(z, function(bags) 1-sum(effvalid2[,bags]==valid_output)/length(valid_output))
error_valid2
e2 <- which.min(error_valid2)
e2
error_valid2[4]
