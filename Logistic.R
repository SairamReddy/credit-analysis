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
 # credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$JOB <- as.factor(credit$JOB)
credit$TYPE <- as.factor(credit$TYPE)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$PRESENT_RESIDENT <- as.factor(credit$PRESENT_RESIDENT)
credit$EMPLOYMENT <- as.factor(credit$EMPLOYMENT)
credit$NUM_CREDITS <- as.factor(credit$NUM_CREDITS)
credit$INSTALL_RATE <- as.factor(credit$INSTALL_RATE)
credit$NUM_DEPENDENTS <- as.factor(credit$NUM_DEPENDENTS)
credit$AMOUNT_REQUESTED <- gsub(",","",credit$AMOUNT_REQUESTED) # Converts to charecter and takes off','.
credit$AMOUNT_REQUESTED <- as.numeric(credit$AMOUNT_REQUESTED) # charecter can be coverted into numeric directly unlike factor!!
str(credit$AMOUNT_REQUESTED)
str(credit)
set.seed(12345)
x <- sample(nrow(credit),0.7*nrow(credit),replace = FALSE)
train <- credit[x,]
test <- credit[-x,]
str(train)
str(test)
test <- test[,-1]
train <- train[,-1]
logreg1 <- glm(CAN~AGE+CHK_ACCT1+CHK_ACCT2+CHK_ACCT3+SAV_ACCT+NUM_CREDITS+DURATION+HISTORY+PRESENT_RESIDENT+EMPLOYMENT+JOB+NUM_DEPENDENTS+RENT+INSTALL_RATE+GUARANTOR+OTHER_INSTALL+OWN_RES+TELEPHONE+FOREIGN+REAL_ESTATE+TYPE+AMOUNT_REQUESTED, data = train, family = binomial)
summary(logreg1)
pred1 <- predict(logreg1,newdata = test,type = 'response')
y <- 0.5
pred2 <- ifelse(pred1 > y, 1, 0)
tab <- table(test$CAN, pred2)
tab
specificity <- tab[1,1]/(tab[1,1]+tab[1,2])
sensitivity <- tab[2,2]/(tab[2,2]+tab[2,1])
sensitivity
specificity
predtrain <- predict(logreg1, newdata = train, type = 'response')
predtrain2 <- ifelse(predtrain > y, 1, 0)
tabtrain <- table(train$CAN, predtrain2)
tabtrain
library(pROC)
library(ROCR)
ROC <- roc(test$CAN, pred1)
plot(ROC, col = "blue")
ROCtrain <- roc(train$CAN, predtrain)
plot(ROCtrain, add = TRUE, col = 'red')
cutoffs <- seq(0,1,by = 0.05)
eff <- sapply(seq(0, 1, by=0.05), function(cutoff) sum((predtrain > cutoff) == train$CAN))/nrow(train)
plot(cutoffs,eff)
which.max(eff)
max(eff)
valideff <- sapply(seq(0, 1, by=0.05), function(cutoff) sum((pred1 > cutoff) == test$CAN))/nrow(test)
plot(cutoffs,valideff)
which.max(valideff)
max(valideff)
#effy <- sapply(seq(0, 1, by=0.05), function(cutoff1) sum((predtrain < cutoff1) == train$CAN[train$CAN == 1]))#/sum(predtrain[predtrain>cutoff1]))
profitmean <- mean(train$NPV[train$NPV>=0])
lossmean <- mean(train$NPV[train$NPV<0])
profitmean
lossmean
predy <- prediction(pred1, test$CAN)
# perf <- performance(predy, "sens")
# ?performance
# perf1 <-performance(predy, "spec")
perfy <- performance(predy, "tpr", "fpr", col = 'red')
plot(perfy)
x <- matrix(c(0,-profitmean, lossmean,0), nrow = 2, ncol = 2)
x
z <- seq(0,1,0.05)
t1 <- vector(mode = "list", length = length(z))
#predtrain <- predict(logreg1, newdata = train, type = 'response')
# Alert: This code is specifically for roc curve generation manually
for(i in 1:length(z)) {
  predtest <- predict(logreg1, newdata = test, type = 'response')
  #        
  for(j in 1:length(predtest)){
    #               
    predtest[j] <- ifelse(predtest[j]>z[i], 1, 0)
  }
  t1[[i]] <- table(test$CAN, predtest)
}
t1
r <- vector(mode = "list", length = length(z))
for(i in 1:length(z)) {
  r[[i]] <- as.data.frame.matrix(t1[[i]])*x
}
r
q <- sapply(1:length(r), function(p) sum(r[[p]]))
q[1] <- -132432.63
q
plot(cutoffs, q, col = 'red')
max(q)
min(q)
which.max(q)
which.min(q)
logreg2 <- glm(CAN~AGE+CHK_ACCT1+CHK_ACCT2+CHK_ACCT3+I(SAV_ACCT == 3)+I(SAV_ACCT == 4)+I(NUM_CREDITS == 2)+DURATION+I(HISTORY == 3)+I(PRESENT_RESIDENT == 2)+I(EMPLOYMENT == 3)+INSTALL_RATE+OTHER_INSTALL+FOREIGN+I(TYPE == 2)+I(TYPE == 5)+AMOUNT_REQUESTED, data = train, family = "binomial")
predfinaltr<- predict(logreg2, newdata = train, type ='response')
effinaltr <- sapply(seq(0, 1, by=0.05), function(cutoff) sum((predfinaltr > cutoff) == train$CAN))/nrow(train)
plot(cutoffs,effinaltr)
which.max(effinaltr)
max(effinaltr)
predfinalte<- predict(logreg2, newdata = test, type ='response')
effinalte <- sapply(seq(0, 1, by=0.05), function(cutoff) sum((predfinalte > cutoff) == test$CAN))/nrow(test)
plot(cutoffs,effinalte)
which.max(effinalte)
max(effinalte)
plot(effinaltr,effinalte)
