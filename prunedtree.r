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
#credit$AMOUNT_REQUESTED <- NULL
credit$NPV <- NULL
#credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED) # Converts to charecter and takes off','.
#credit$CREDIT_EXTENDED <- as.numeric(credit$CREDIT_EXTENDED)
credit$CREDIT_EXTENDED <- NULL
#credit$REAL_ESTATE <- NULL
#credit$FOREIGN <- NULL
#credit$TELEPHONE <- NULL
#credit$OWN_RES <- NULL
#credit$RENT <- NULL
# credit$REAL_ESTATE <- as.factor(credit$REAL_ESTATE)
# credit$FOREIGN <- as.factor(credit$FOREIGN)
# credit$TELEPHONE <- as.factor(credit$TELEPHONE)
# credit$OWN_RES <- as.factor(credit$OWN_RES)
#credit$OTHER_INSTALL <- as.factor(credit$OTHER_INSTALL)
#credit$OTHER_INSTALL <- NULL
#credit$GUARANTOR <- NULL
#credit$GUARANTOR <- as.factor(credit$GUARANTOR)
#credit$RENT <- as.factor(credit$RENT)
credit$CAN <- as.factor(credit$CAN)
names(credit)
str(credit)
set.seed(12345)
x <- sample(nrow(credit),0.7*nrow(credit),replace = FALSE)
train <- credit[x,]
valid <- credit[-x,]
#install.packages('tree')
#install.packages('ISLR')
library('tree')
library('ISLR')
credit.tree <- tree(CAN~., newdata <- train)
summary(credit.tree)
plot(credit.tree)
text(credit.tree, pretty = 0)
credit.tree
valid.out <- valid[,-21]
valid.tree <- predict(credit.tree, valid.out, type = "class")
tab1 <- table (valid.tree, valid$CAN)
tab1
eff <- (tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
eff
cv.credit =cv.tree(credit.tree ,FUN=prune.misclass)
names(cv.credit)
cv.credit
plot(cv.credit$size,cv.credit$dev, type = 'b')
prune.credit <- prune.misclass(credit.tree, best = 9)
plot(prune.credit)
text(prune.credit, pretty = 0)
tree.pred <- predict(prune.credit, newdata = valid, type = 'class')
table(tree.pred, valid$CAN)
X <- data.frame(CHK_ACCT="1",SAV_ACCT="4", JOB="2", TYPE="2", AGE=27, DURATION=12, REAL_ESTATE=0, NUM_CREDITS=1, FOREIGN=0, TELEPHONE=1, HISTORY="1", AMOUNT_REQUESTED=4500, GUARANTOR=0, RENT = 1, PRESENT_RESIDENT=1, EMPLOYMENT=1, NUM_DEPENDENTS=1, OTHER_INSTALL=0, OWN_RES=0,INSTALL_RATE=3, CAN="1")
names(X)
newdatapred <- predict(credit.tree, newdata = X[,-21], type = 'class')
newdatapred
probnb <- predict(credit.tree, newdata = X[,-21], type <- 'vector')
probnb
prune.credit4 <- prune.misclass(credit.tree, best = 4)
plot(prune.credit4)
text(prune.credit4, pretty = 0)
