setwd("~/Downloads/MISC")

# Read the Data
college <- read.csv("College.csv")
rownames(college) <- college[,1]

# view the data in excel
fix(college)

# Remove observation column
college <- college [,-1]

# Summary Stats
summary(college)
plot(college$Private, college$Accept)
plot(college$Private, col = "red")
plot(college$Accept, col = "red")
A <- college[,1:10]
summary(A)

pairs(A)
plot(college$Outstate, college$Private, col = "blue")
plot(college$Private, college$Outstate)
plot(college$Private, college$Outstate, col = "yellow")

# Box Plot
boxplot(college$Outstate, college$Private, col = "yellow")
plot(college$Outstate, college$Private, col = "blue")
str(college$Private)
head(college$Private, 50)

# Categorical Variable for Elite Group
college$Elite <- rep("No", nrow(college))
str(college)
college$Elite [college$Top10perc > 50] <- "Yes"
college$Elite <- as.factor (college$Elite)
str(college)
summary(college)
par (mfrow=c(2,2))
boxplot(college$Outstate, college$Private)
boxplot(college$Outstate, college$Elite)
hist (college$Top10perc, breaks = 5, main = "Breaks = 5")
hist (college$Top10perc, breaks = 20, main = "Breaks = 20")

# Linear Regression
reg <- lm(Grad.Rate~., data = college)
summary(reg)

# Correlation Matrix
cor(college[2:18])
# variable1 <-lm(Grad.Rate~Private, data=college)
# summary(variable1)
# variable2 <- lm(Grad.Rate~Private+Apps, data = college)
# summary(variable2)

# Linear Regression with better fit and lesser overfitting component
Variable6 <- lm(Grad.Rate~Personal+I(Personal^2)+Apps+I(Apps^2)+Top25perc+P.Undergrad+Outstate+Books+Books:Private+Room.Board:Private+Personal+perc.alumni+Expend+I(Expend^2)+Expend:Private+Expend:Elite, data = college)
summary(Variable6)
