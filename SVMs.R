#Clean Rstudio Console, Environment & Plots, Add libraries & Import data from Excel
cat("\014")
rm(list = ls())
graphics.off()

library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(tidyverse)
library(MASS)
library(corrplot)
library(zoo)
library(caret)
library(psych)
library(InformationValue)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)


setwd("C:/Users/Melvin/Documents/William and Mary/Courses/Spring 2021/BUAD 5132 Machine Learning II/Module 1")
data <- read.csv("buad5132-m1-training-data.csv")
test <- read.csv("buad5132-m1-test-data.csv")

### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
data$INDEX <- as.factor(data$INDEX)
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$PARENT1 <- as.factor(data$PARENT1)
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))

data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$MSTATUS <- as.factor(data$MSTATUS)
data$REVOKED <- as.factor(data$REVOKED)
data$RED_CAR <- as.factor(ifelse(data$RED_CAR=="yes", 1, 0))
data$URBANICITY <- ifelse(data$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
data$URBANICITY <- as.factor(data$URBANICITY)
data$JOB <- as.factor(data$JOB)
data$CAR_USE <- as.factor(data$CAR_USE)
data$CAR_TYPE <- as.factor(data$CAR_TYPE)
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 1, 0 ))
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$OLDCLAIM)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))

summary(data)
str(data)


######## Same treatment on test data set ###########################

### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
test$INDEX <- as.factor(test$INDEX)
test$TARGET_FLAG <- as.factor(test$TARGET_FLAG)
test$SEX <- as.factor(test$SEX)
test$EDUCATION <- as.factor(test$EDUCATION)
test$PARENT1 <- as.factor(test$PARENT1)
test$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$INCOME)))
test$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$MSTATUS <- as.factor(test$MSTATUS)
test$REVOKED <- as.factor(test$REVOKED)
test$RED_CAR <- as.factor(ifelse(test$RED_CAR=="yes", 1, 0))
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)
test$JOB <- as.factor(test$JOB)
test$CAR_USE <- as.factor(test$CAR_USE)
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$DO_KIDS_DRIVE <- as.factor(ifelse(test$KIDSDRIV > 0, 1, 0 ))
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$OLDCLAIM)))
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))
summary(test)
str(test)

#################### Part 1: Data Exploration ##############################################
# Histograms for Numeric Variables
par(mfrow=c(2,2))
hist(data$AGE, col = "red", xlab = "Age", main = "AGE Hist")
data0<- subset(data, TARGET_FLAG == 1 )
boxplot(data$AGE, col = "red", main = "AGE BoxPlot")
hist(data0$AGE, col = "yellow", main = "AGE0 Hist")
boxplot(data0$AGE, col = "yellow", main = "AGEO BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(data$TRAVTIME), col = "green", xlab = "SQRT TRAVTIME", main = "SQRT TRAVTIME Hist")
hist(data$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
boxplot(sqrt(data$TRAVTIME), col = "green", main = "SQRT TRAVTIME BoxPlot")
boxplot(data$YOJ, col = "blue", main = "YOJ BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(data$BLUEBOOK), col = "green", xlab = "SQRT BLUEBOOK", main = "SQRT BLUEBOOK Hist")
hist((data$TIF), col = "blue", xlab = "TIF", main = "TIF Hist")
boxplot(sqrt(data$BLUEBOOK), col = "green", main = "SQRT BLUEBOOK BoxPlot")
boxplot(data$TIF, col = "blue", main = "TIF BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$MVR_PTS, col = "red", xlab = "MVR_PTS", main = "MVR_PTS Hist")
hist(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE Hist")
boxplot(data$MVR_PTS, col = "red", main = "MVR_PTS BoxPlot")
boxplot(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE BoxPlot")
par(mfrow=c(1,1))


########### Part 2: Data Transformation ##################
# Fix NA's, note car age has been changed to 10, Income values updated to 
#quantile values of 25, 75 quantile, and switched car age
data$AGE[is.na(data$AGE)] <- mean(data$AGE, na.rm = TRUE)

options(scipen = 4)

#Years on Job Input and Replacements for NA's and outliers
data$FlagYOJ = ifelse(is.na(data$YOJ),1,0)
data$YOJ[is.na(data$YOJ)] = median(data$YOJ, na.rm = TRUE)
upperWhisker = quantile(data$YOJ,c(.75))+1.5*(quantile(data$YOJ,c(.75))-quantile(data$YOJ,c(.25)))
lowerWhisker = quantile(data$YOJ,c(.25))-1.5*(quantile(data$YOJ,c(.75))-quantile(data$YOJ,c(.25)))

data$FlagYOJ = ifelse(data$YOJ <= lowerWhisker, 2, data$FlagYOJ)
data$FlagYOJ = ifelse(data$YOJ >= upperWhisker, 3, data$FlagYOJ)
data$YOJ = ifelse(data$YOJ==2, lowerWhisker, data$YOJ)
data$YOJ = ifelse(data$YOJ==3, upperWhisker, data$YOJ)

boxplot(data$YOJ, main = "Years on Job Update")
plot(data$YOJ, main = "Years on Job Quantile Function")

data$YOJ <- na.aggregate(data$YOJ, data$JOB, mean, na.rm = TRUE)

#Income Input and Replacements for NA's and outliers
data$FlagIncome = ifelse(is.na(data$INCOME),1,0)
data$INCOME[is.na(data$INCOME)] = mean(data$INCOME, na.rm = TRUE)
upperWhisker = quantile(data$INCOME,c(.75))+1.5*(quantile(data$INCOME,c(.75))-quantile(data$INCOME,c(.25)))
lowerWhisker = quantile(data$INCOME,c(.10))-1.5*(quantile(data$INCOME,c(.90))-quantile(data$INCOME,c(.10)))

data$FlagIncome = ifelse(data$INCOME <= lowerWhisker, 2, data$FlagIncome)
data$FlagIncome = ifelse(data$INCOME >= upperWhisker, 3, data$FlagIncome)
data$INCOME = ifelse(data$INCOME==2, lowerWhisker, data$INCOME)
data$INCOME = ifelse(data$INCOME==3, upperWhisker, data$INCOME)

boxplot(data$INCOME, main = "Income Update")
plot(data$INCOME, main = "Income Quantile Function")

data$INCOME <- na.aggregate(data$INCOME, data$JOB, mean, na.rm = TRUE)

#summary(data$INCOME)

#Home_Val Input and Replacements for NA's and outliers
data$FlagHome_Val = ifelse(is.na(data$HOME_VAL),1,0)
data$HOME_VAL[data$HOME_VAL <= 0] = median(data$HOME_VAL)
data$HOME_VAL[is.na(data$HOME_VAL)] = median(data$HOME_VAL, na.rm = TRUE)
upperWhisker = quantile(data$HOME_VAL,c(.75))+1.5*(quantile(data$HOME_VAL,c(.75))-quantile(data$HOME_VAL,c(.25)))
lowerWhisker = quantile(data$HOME_VAL,c(.25))-1.5*(quantile(data$HOME_VAL,c(.75))-quantile(data$HOME_VAL,c(.25)))

data$FlagHome_Val = ifelse(data$HOME_VAL <= lowerWhisker, 2, data$FlagHome_Val)
data$FlagHome_Val = ifelse(data$HOME_VAL >= upperWhisker, 3, data$FlagHome_Val)
data$HOME_VAL = ifelse(data$HOME_VAL==2, lowerWhisker, data$HOME_VAL)
data$HOME_VAL = ifelse(data$HOME_VAL==3, upperWhisker, data$HOME_VAL)

boxplot(data$HOME_VAL, main = "Home Value Update")
plot(data$HOME_VAL, main = "Home Value Quantile Function")

data$HOME_VAL <- na.aggregate(data$HOME_VAL, data$JOB, mean, na.rm = TRUE )

#summary(data$HOME_VAL)


#Car_Age Input and Replacements for NA's and outliers (maybe divide this by 12 because of months per year)
data$FlagCar_Age = ifelse(is.na(data$CAR_AGE),1,0)
data$CAR_AGE[data$CAR_AGE <= 0] = mean(data$CAR_AGE)
data$CAR_AGE[data$CAR_AGE >=20] = mean(data$CAR_AGE)
data$CAR_AGE[is.na(data$CAR_AGE)] = mean(data$CAR_AGE, na.rm = TRUE)
#upperWhisker = quantile(data$CAR_AGE,c(.75))+1.5*(quantile(data$CAR_AGE,c(.75))-quantile(data$CAR_AGE,c(.25)))
#lowerWhisker = quantile(data$CAR_AGE,c(.25))-1.5*(quantile(data$CAR_AGE,c(.75))-quantile(data$CAR_AGE,c(.25)))

#data$FlagCar_Age = ifelse(data$CAR_AGE <= lowerWhisker, 2, data$FlagCar_Age)
#data$FlagCar_Age = ifelse(data$CAR_AGE >= upperWhisker, 3, data$FlagCar_Age)
#data$CAR_AGE = ifelse(data$CAR_AGE==2, lowerWhisker, data$CAR_AGE)
#data$CAR_AGE = ifelse(data$CAR_AGE==3, upperWhisker, data$CAR_AGE)

boxplot(data$CAR_AGE, main = "Car_Age Update")
plot(data$CAR_AGE, main = "Car_Age Quantile Function")
data$CAR_AGE <- na.aggregate(data$CAR_AGE, data$JOB, mean, na.rm = TRUE)

#summary(data$CAR_AGE)

#Old Claim Input and Replacements for NA's and outliers (EVALUATE THIS AREA AGAIN)
data$FlagOldClaim= ifelse(is.na(data$OLDCLAIM),1,0)
data$OLDCLAIM[is.na(data$OLDCLAIM)] = 1/max(data$OLDCLAIM, na.rm = TRUE)
upperWhisker = quantile(data$OLDCLAIM,c(.75))+1.5*(quantile(data$OLDCLAIM,c(.75))-quantile(data$OLDCLAIM,c(.25)))
lowerWhisker = quantile(data$OLDCLAIM,c(.25))-1.5*(quantile(data$OLDCLAIM,c(.75))-quantile(data$OLDCLAIM,c(.25)))

data$FlagOldClaim = ifelse(data$OLDCLAIM <= lowerWhisker, 2, data$FlagOldClaim)
data$FlagOldClaim = ifelse(data$OLDCLAIM >= upperWhisker, 3, data$FlagOldClaim)
data$OLDCLAIM = ifelse(data$OLDCLAIM==2, lowerWhisker, data$OLDCLAIM)
data$OLDCLAIM = ifelse(data$OLDCLAIM==3, upperWhisker, data$OLDCLAIM)

boxplot(data$OLDCLAIM, main = "Old Claim Update")
plot(data$OLDCLAIM, main = "Old Claim Quantile Function")

data$OLDCLAIM <- ifelse(data$CAR_AGE < 11 & !is.na(data$CAR_AGE),0,data$OLDCLAIM)
data$OLDCLAIM <- na.aggregate(data$OLDCLAIM, data$CAR_AGE, mean, na.rm = TRUE )
data$HOME_OWNER <- ifelse(data$HOME_VAL == 0, 0, 1)
data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)

#Bluebook Input and Replacements for NA's and outliers
data$FlagBluebook = ifelse(is.na(data$BLUEBOOK),1,0)
data$BLUEBOOK[is.na(data$BLUEBOOK)] = median(data$BLUEBOOK, na.rm = TRUE)
upperWhisker = quantile(data$BLUEBOOK,c(.75))+1.5*(quantile(data$BLUEBOOK,c(.75))-quantile(data$BLUEBOOK,c(.25)))
lowerWhisker = quantile(data$BLUEBOOK,c(.25))-1.5*(quantile(data$BLUEBOOK,c(.75))-quantile(data$BLUEBOOK,c(.25)))

data$FlagBluebook = ifelse(data$BLUEBOOK <= lowerWhisker, 2, data$FlagBluebook)
data$FlagBluebook = ifelse(data$BLUEBOOK >= upperWhisker, 3, data$FlagBluebook)
data$BLUEBOOK = ifelse(data$BLUEBOOK==2, lowerWhisker, data$BLUEBOOK)
data$BLUEBOOK = ifelse(data$BLUEBOOK==3, upperWhisker, data$BLUEBOOK)

boxplot(data$BLUEBOOK, main = "Bluebook Update")
plot(data$BLUEBOOK, main = "Bluebook Quantile Function")
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)


# Bin Income Change income value (UPDATED INCOME FACTORS)
data$INCOME_bin[is.na(data$INCOME)] <- "NA"
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 12500] <- "Low"
data$INCOME_bin[data$INCOME >= 12500 & data$INCOME < 70000] <- "Medium"
data$INCOME_bin[data$INCOME >= 70000 & data$INCOME < 120000] <- "High"
data$INCOME_bin[data$INCOME >= 120000] <- "Very High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("NA","Zero","Low","Medium","High", "Very High"))

#Bin Home Value (NEWLY ADDED VALUE)
data$HOME_VAL_bin[is.na(data$HOME_VAL)] <- "NA"
data$HOME_VAL_bin[data$HOME_VAL == 0] <- "Zero"
data$HOME_VAL_bin[data$HOME_VAL >= 1 & data$HOME_VAL < 150000] = "Low"
data$HOME_VAL_bin[data$HOME_VAL >= 150000 & data$HOME_VAL < 300000] = "Medium"
data$HOME_VAL_bin[data$HOME_VAL >= 300000 & data$HOME_VAL < 500000] <- "High"
data$HOME_VAL_bin[data$HOME_VAL >= 500000] <- "Very High"
data$HOME_VAL_bin <- factor(data$HOME_VAL_bin)
data$HOME_VAL_bin <- factor(data$HOME_VAL_bin, levels =c("NA", "Zero", "Low", "Medium", "High", "Very High"))

#Bin Job (NEWLY ADDED VALUE)
#data$JOB_bin[is.na(data$JOB)] = "NA"
data$JOB_bin[data$JOB == ""] = "NA"
data$JOB_bin[data$JOB == "Doctor"] = "Doctor"
data$JOB_bin[data$JOB == "Home Maker"] = "Home Maker"
data$JOB_bin[data$JOB == "Professional"] = "Professional"
data$JOB_bin[data$JOB == "Student"] = "Student"
data$JOB_bin[data$JOB == "Lawyer"] = "Lawyer"
data$JOB_bin[data$JOB == "z_Blue Collar"] = "Blue Collar"
data$JOB_bin[data$JOB == "Clerical"] = "Clerical"
data$JOB_bin[data$JOB == "Manager"] = "Manager"
data$JOB_bin = factor(data$JOB_bin)
data$JOB_bin <- factor(data$JOB_bin, levels = c("NA", "Doctor", "Home Maker", "Professional", "Student", "Lawyer", "Blue Collar",
                       "Clerical", "Manager"))

#Bin Education (NEWLY ADDED VALUE)
#summary(data$EDUCATION_bin)
#head(data$EDUCATION_bin)

data$EDUCATION_bin[data$EDUCATION == ""] = "NA"
data$EDUCATION_bin[data$EDUCATION == "<High School"] = "Still in High School"
data$EDUCATION_bin[data$EDUCATION == "z_High School"] = "Finished High School"
data$EDUCATION_bin[data$EDUCATION == "Bachelors"] = "Bachelors"
data$EDUCATION_bin[data$EDUCATION == "Masters"] = "Masters"
data$EDUCATION_bin[data$EDUCATION == "PhD"] = "PhD"
data$EDUCATION_bin = factor(data$EDUCATION_bin)
data$EDUCATION_bin = factor(data$EDUCATION_bin, levels = c("NA", "Still in High School", "Finished High School", "Bachelors",
                                                           "Masters", "PhD" ))

#Bin Sex (NEWLY ADDED VALUE)
data$FlagFemaleSex = as.factor(ifelse(data$SEX == "z_F", 1, 0))
data$FlagMaleSex = as.factor(ifelse(data$SEX == "M", 1, 0))

#summary(data$JOB_bin)

#Create Additional Flags for Model Purposes
data$FlagJobClerical = as.factor(ifelse(data$JOB_bin == "Clerical", 1, 0))
data$FlagJobBlueCollar = as.factor(ifelse(data$JOB_bin == "Blue Collar", 1, 0))
data$FlagJobManager = as.factor(ifelse(data$JOB_bin == "Manager", 1, 0))

data$FlagEduBachelors = as.factor(ifelse(data$EDUCATION_bin == "Bachelors", 1, 0))
data$FlagEduMasters = as.factor(ifelse(data$EDUCATION_bin == "Masters", 1, 0))
data$FlagEduPhD = as.factor(ifelse(data$EDUCATION_bin == "PhD", 1, 0))

data$FlagSingle = as.factor(ifelse(data$MSTATUS == "z_No", 1, 0))
data$FlagMarried = as.factor(ifelse(data$MSTATUS == "Yes", 1, 0))

data$FlagRural = as.factor(ifelse(data$URBANICITY == "Rural", 1, 0))
data$FlagUrban = as.factor(ifelse(data$URBANICITY == "Urban", 1, 0))

data$FlagUnderAge25 = as.factor(ifelse(data$AGE <= 25, 1, 0))
data$FlagOverAge25 = as.factor(ifelse(data$AGE >= 25, 1, 0))

#data$FlagPrivateCarUse = as.factor(ifelse(data$CAR_USE == "Private", 1, 0))
#data$FlagCommericalCaruse = as.factor(ifelse(data$CAR_USE =="Commercial", 1, 0))

#data$FlagDriversRevoked = as.factor(ifelse(data$REVOKED == "Yes", 1, 0))
#data$FlagDriversNotRevoked = as.factor(ifelse(data$REVOKED == "No", 1, 0))

#summary(data)
#view(data$JOB)
#describe(data$JOB)
#summary(data$JOB_bin)
#str(data$JOB_bin)
#head(data$JOB_bin)

# Fix NA's, note car age on Test Data
test$AGE[is.na(test$AGE)] <- mean(test$AGE, na.rm = TRUE)

#Years on Job Test Input and Replacements for NA's and outliers
test$FlagYOJ = ifelse(is.na(test$YOJ),1,0)
test$YOJ[is.na(test$YOJ)] = median(test$YOJ, na.rm = TRUE)
upperWhisker = quantile(test$YOJ,c(.75))+1.5*(quantile(test$YOJ,c(.75))-quantile(test$YOJ,c(.25)))
lowerWhisker = quantile(test$YOJ,c(.25))-1.5*(quantile(test$YOJ,c(.75))-quantile(test$YOJ,c(.25)))

test$FlagYOJ = ifelse(test$YOJ <= lowerWhisker, 2, test$FlagYOJ)
test$FlagYOJ = ifelse(test$YOJ >= upperWhisker, 3, test$FlagYOJ)
test$YOJ = ifelse(test$YOJ==2, lowerWhisker, test$YOJ)
test$YOJ = ifelse(test$YOJ==3, upperWhisker, test$YOJ)

boxplot(test$YOJ, main = "Test Years on Job Update")
plot(test$YOJ, main = "Test Years on Job Quantile Function")

test$YOJ <- na.aggregate(test$YOJ, test$JOB, mean, na.rm = TRUE)

#Income Test Input and Replacements for NA's and outliers
test$FlagIncome = ifelse(is.na(test$INCOME),1,0)
test$INCOME[is.na(test$INCOME)] = median(test$INCOME, na.rm = TRUE)
upperWhisker = quantile(test$INCOME,c(.75))+1.5*(quantile(test$INCOME,c(.75))-quantile(test$INCOME,c(.25)))
lowerWhisker = quantile(test$INCOME,c(.25))-1.5*(quantile(test$INCOME,c(.75))-quantile(test$INCOME,c(.25)))

test$FlagIncome = ifelse(test$INCOME <= lowerWhisker, 2, test$FlagIncome)
test$FlagIncome = ifelse(test$INCOME >= upperWhisker, 3, test$FlagIncome)
test$INCOME = ifelse(test$INCOME==2, lowerWhisker, test$INCOME)
test$INCOME = ifelse(test$INCOME==3, upperWhisker, test$INCOME)

boxplot(test$INCOME, main = "Test Income Update")
plot(data$INCOME, main = "Test Income Quantile Function")
test$INCOME <- na.aggregate(test$INCOME, test$JOB, mean, na.rm = TRUE)

#summary(test$INCOME)

#NEW STUFF ADDED HERE
#Home_Val Test Input and Replacements48 for NA's and outliers
test$FlagHome_Val = ifelse(is.na(test$HOME_VAL),1,0)
test$HOME_VAL[is.na(test$HOME_VAL)] = median(test$HOME_VAL, na.rm = TRUE)
upperWhisker = quantile(test$HOME_VAL,c(.75))+1.5*(quantile(test$HOME_VAL,c(.75))-quantile(test$HOME_VAL,c(.25)))
lowerWhisker = quantile(test$HOME_VAL,c(.25))-1.5*(quantile(test$HOME_VAL,c(.75))-quantile(test$HOME_VAL,c(.25)))

test$FlagHome_Val = ifelse(test$HOME_VAL <= lowerWhisker, 2, test$FlagHome_Val)
test$FlagHome_Val = ifelse(test$HOME_VAL >= upperWhisker, 3, test$FlagHome_Val)
test$HOME_VAL = ifelse(test$HOME_VAL==2, lowerWhisker, test$HOME_VAL)
test$HOME_VAL = ifelse(test$HOME_VAL==3, upperWhisker, test$HOME_VAL)

boxplot(test$HOME_VAL, main = "Test Home Value Update")
plot(test$HOME_VAL, main = "Test Home Value Quantile Function")
test$HOME_VAL <- na.aggregate(test$HOME_VAL, test$JOB, mean, na.rm = TRUE )

#Car_Age Test Input and Replacements for NA's and outliers
test$FlagCar_Age = ifelse(is.na(test$CAR_AGE),1,0)
test$CAR_AGE[test$CAR_AGE <= 0] = mean(test$CAR_AGE)
test$CAR_AGE[is.na(test$CAR_AGE)] = mean(test$CAR_AGE, na.rm = TRUE)

boxplot(test$CAR_AGE, main = "Test Car_Age Update")
plot(test$CAR_AGE, main = "Test Car_Age Quantile Function")
test$CAR_AGE <- na.aggregate(test$CAR_AGE, test$JOB, mean, na.rm = TRUE)

#summary(test$CAR_AGE)

#Old Claim Test Input and Replacements for NA's and outliers (EVALUATE THIS AREA AGAIN)
test$FlagOldClaim= ifelse(is.na(test$OLDCLAIM),1,0)
test$OLDCLAIM[is.na(test$OLDCLAIM)] = 1/max(test$OLDCLAIM, na.rm = TRUE)
upperWhisker = quantile(test$OLDCLAIM,c(.75))+1.5*(quantile(test$OLDCLAIM,c(.75))-quantile(test$OLDCLAIM,c(.25)))
lowerWhisker = quantile(test$OLDCLAIM,c(.25))-1.5*(quantile(test$OLDCLAIM,c(.75))-quantile(test$OLDCLAIM,c(.25)))

test$FlagOldClaim = ifelse(test$OLDCLAIM <= lowerWhisker, 2, test$FlagOldClaim)
test$FlagOldClaim = ifelse(test$OLDCLAIM >= upperWhisker, 3, test$FlagOldClaim)
test$OLDCLAIM = ifelse(test$OLDCLAIM==2, lowerWhisker, test$OLDCLAIM)
test$OLDCLAIM = ifelse(test$OLDCLAIM==3, upperWhisker, test$OLDCLAIM)

boxplot(test$OLDCLAIM, main = "Test Old Claim Update")
plot(test$OLDCLAIM, main = "Test Old Claim Quantile Function")

test$OLDCLAIM <- ifelse(test = test$CAR_AGE < 11 & !is.na(test$CAR_AGE),0,test$OLDCLAIM)
test$OLDCLAIM <- na.aggregate(test$OLDCLAIM, test$CAR_AGE, mean, na.rm = TRUE )

#Bluebook Test Input and Replacements for NA's and outliers
test$FlagBluebook = ifelse(is.na(test$BLUEBOOK),1,0)
test$BLUEBOOK[is.na(test$BLUEBOOK)] = median(test$BLUEBOOK, na.rm = TRUE)
upperWhisker = quantile(test$BLUEBOOK,c(.75))+1.5*(quantile(test$BLUEBOOK,c(.75))-quantile(test$BLUEBOOK,c(.25)))
lowerWhisker = quantile(test$BLUEBOOK,c(.25))-1.5*(quantile(test$BLUEBOOK,c(.75))-quantile(test$BLUEBOOK,c(.25)))

test$FlagBluebook = ifelse(test$BLUEBOOK <= lowerWhisker, 2, test$FlagBluebook)
test$FlagBluebook = ifelse(test$BLUEBOOK >= upperWhisker, 3, test$FlagBluebook)
test$BLUEBOOK = ifelse(test$BLUEBOOK==2, lowerWhisker, test$BLUEBOOK)
test$BLUEBOOK = ifelse(test$BLUEBOOK==3, upperWhisker, test$BLUEBOOK)

boxplot(test$BLUEBOOK, main = "Test Bluebook Update")
plot(test$BLUEBOOK, main = "Test Bluebook Quantile Function")
test$SQRT_BLUEBOOK <- sqrt(test$BLUEBOOK)


#test$CAR_AGE <- na.aggregate(test$CAR_AGE, test$CAR_TYPE, mean, na.rm = TRUE)
#test$CAR_AGE[test$CAR_AGE < 0 ] <- 0 
#test$OLDCLAIM <- ifelse(test$CAR_AGE < 11 & !is.na(test$CAR_AGE),0,test$OLDCLAIM)
#test$OLDCLAIM <- na.aggregate(test$OLDCLAIM, test$CAR_AGE, mean, na.rm = TRUE )
test$HOME_OWNER <- ifelse(test$HOME_VAL == 0, 0, 1)
test$SQRT_TRAVTIME <- sqrt(test$TRAVTIME)
test$SQRT_BLUEBOOK <- sqrt(test$BLUEBOOK)

# Bin Income Test Change income value (UPDATED INCOME FACTORS)
test$INCOME_bin[is.na(test$INCOME)] <- "NA"
test$INCOME_bin[test$INCOME == 0] <- "Zero"
test$INCOME_bin[test$INCOME >= 1 & test$INCOME < 12500] <- "Low"
test$INCOME_bin[test$INCOME >= 12500 & test$INCOME < 70000] <- "Medium"
test$INCOME_bin[test$INCOME >= 70000 & test$INCOME < 120000] <- "High"
test$INCOME_bin[test$INCOME >= 120000] <- "Very High"
test$INCOME_bin <- factor(test$INCOME_bin)
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("NA","Zero","Low","Medium","High", "Very High"))


#Bin Home Value Test (NEWLY ADDED VALUE)
test$HOME_VAL_bin[is.na(test$HOME_VAL)] <- "NA"
test$HOME_VAL_bin[test$HOME_VAL == 0] <- "Zero"
test$HOME_VAL_bin[test$HOME_VAL >= 1 & test$HOME_VAL < 150000] = "Low"
test$HOME_VAL_bin[test$HOME_VAL >= 150000 & test$HOME_VAL < 300000] = "Medium"
test$HOME_VAL_bin[test$HOME_VAL >= 300000 & test$HOME_VAL < 500000] <- "High"
test$HOME_VAL_bin[test$HOME_VAL >= 500000] <- "Very High"
test$HOME_VAL_bin <- factor(test$HOME_VAL_bin)
test$HOME_VAL_bin <- factor(test$HOME_VAL_bin, levels =c("NA", "Zero", "Low", "Medium", "High", "Very High"))

#Bin Job Test (NEWLY ADDED VALUE)
#test$JOB_bin[is.na(test$JOB)] = "NA"
test$JOB_bin[test$JOB == ""] = "NA"
test$JOB_bin[test$JOB == "Doctor"] = "Doctor"
test$JOB_bin[test$JOB == "Home Maker"] = "Home Maker"
test$JOB_bin[test$JOB == "Professional"] = "Professional"
test$JOB_bin[test$JOB == "Student"] = "Student"
test$JOB_bin[test$JOB == "Lawyer"] = "Lawyer"
test$JOB_bin[test$JOB == "z_Blue Collar"] = "Blue Collar"
test$JOB_bin[test$JOB == "Clerical"] = "Clerical"
test$JOB_bin[test$JOB == "Manager"] = "Manager"
test$JOB_bin = factor(test$JOB_bin)
test$JOB_bin <- factor(test$JOB_bin, levels = c("NA", "Doctor", "Home Maker", "Professional", "Student", "Lawyer", "Blue Collar",
                                                "Clerical", "Manager"))

#Bin Education Test (NEWLY ADDED VALUE)
#summary(test$EDUCATION_bin)
#head(test$EDUCATION_bin)

test$EDUCATION_bin[test$EDUCATION == ""] = "NA"
test$EDUCATION_bin[test$EDUCATION == "<High School"] = "Still in High School"
test$EDUCATION_bin[test$EDUCATION == "z_High School"] = "Finished High School"
test$EDUCATION_bin[test$EDUCATION == "Bachelors"] = "Bachelors"
test$EDUCATION_bin[test$EDUCATION == "Masters"] = "Masters"
test$EDUCATION_bin[test$EDUCATION == "PhD"] = "PhD"
test$EDUCATION_bin = factor(test$EDUCATION_bin)
test$EDUCATION_bin = factor(test$EDUCATION_bin, levels = c("NA", "Still in High School", "Finished High School", "Bachelors",
                                                           "Masters", "PhD" ))

#Bin Sex Test (NEWLY ADDED VALUE)
test$FlagFemaleSex = as.factor(ifelse(test$SEX == "z_F", 1, 0))
test$FlagMaleSex = as.factor(ifelse(test$SEX == "M", 1, 0))

#Create Additional Test Flags for Model Purposes
test$FlagJobClerical = as.factor(ifelse(test$JOB_bin == "Clerical", 1, 0))
test$FlagJobBlueCollar = as.factor(ifelse(test$JOB_bin == "Blue Collar", 1, 0))
test$FlagJobManager = as.factor(ifelse(test$JOB_bin == "Manager", 1, 0))

test$FlagEduBachelors = as.factor(ifelse(test$EDUCATION_bin == "Bachelors", 1, 0))
test$FlagEduMasters = as.factor(ifelse(test$EDUCATION_bin == "Masters", 1, 0))
test$FlagEduPhD = as.factor(ifelse(test$EDUCATION_bin == "PhD", 1, 0))

test$FlagSingle = as.factor(ifelse(test$MSTATUS == "z_No", 1, 0))
test$FlagMarried = as.factor(ifelse(test$MSTATUS == "Yes", 1, 0))

test$FlagRural = as.factor(ifelse(test$URBANICITY == "Rural", 1, 0))
test$FlagUrban = as.factor(ifelse(test$URBANICITY == "Urban", 1, 0))

test$FlagUnderAge25 = as.factor(ifelse(test$AGE <= 25, 1, 0))
test$FlagOverAge25 = as.factor(ifelse(test$AGE >= 25, 1, 0))

#test$FlagPrivateCarUse = as.factor(ifelse(test$CAR_USE == "Private", 1, 0))
#test$FlagCommericalCaruse = as.factor(ifelse(test$CAR_USE =="Commercial", 1, 0))

#test$FlagDriversRevoked = as.factor(ifelse(test$REVOKED == "Yes", 1, 0))
#test$FlagDriversNotRevoked = as.factor(ifelse(test$REVOKED == "No", 1, 0))

summary(data)
summary(test)


numeric <- subset(data, select = c(AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
summary(numeric)
c <- cor(numeric)
corrplot(c, method = "square")

#Create SVM Variables for Additional Analysis
data2 = data

#summary(data2)
#str(data2)

#########################################################
### Functions
#########################################################
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}


needed <- c('e1071')      
installIfAbsentAndLoad(needed)     
# Build toy data that is perfectly separable
set.seed(5082)

#Income SVM Analysis
a = matrix(rnorm(data2$INCOME[1:1000] * 2), ncol=2, byrow = TRUE)
b = c(rep(-1, 250), rep(1, 250))
a[b == 1,] = a[b == 1,] + 2.5
plot(a[, 2], a[, 1], col=(4-b))

data_a_b <- data.frame(x=a, y=as.factor(b))
svmfit <- svm(y~., 
              data=data_a_b, 
              kernel="linear", 
              cost=1e5)
summary(svmfit)
plot(svmfit, data_a_b)
#We can see some of the income is being misclassified as we increase the sampling size of the Income

#Set the cost factor to 10 and the scaling factor to false
svmfit <- svm(y ~ ., data=data_a_b, kernel="polynomial", cost=10, scale=TRUE)
plot(svmfit, data_a_b)

svmfit$index

summary(svmfit)

#Change the cost factor to .01 and the scaling factor to false again
svmfit2 <- svm(y ~ ., data=data_a_b, kernel="radial", cost=0.01, scale=FALSE)
plot(svmfit2, data_a_b)
svmfit2$index

#Tune the data to perform ten-fold cross-validation using linear kernels
#by changing the cost parameter in a range of values
set.seed(100)
tune.out <- tune(svm,y ~ ., data=data_a_b, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100)))

# We can easily access the cross-validation errors for each
# of these models using the summary() command:
summary(tune.out)

# The tune() function stores the best model obtained, which
# can be accessed as follows:
bestmod <- tune.out$best.model
summary(bestmod)

#Use the predict() function to find the correct class based on a test observation set
atest <- matrix(rnorm(data2$INCOME[1:100] * 2), ncol=2, byrow = TRUE)
btest <- c(rep(-1, 25), rep(1, 25))
atest[btest==1,]=atest[btest==1,] + 2
testdata_a_b <- data.frame(x=atest, y=as.factor(btest))

# Now we predict the class labels of these test
# observations. Here we use the best model obtained through
# cross-validation in order to make predictions.
ypred <- predict(bestmod, testdata_a_b)
table(truth=testdata_a_b$y, predict=ypred)

# Thus, with this value of cost, 19 of the test observations
# are correctly classified.
#
# What if we had instead used cost=0.01?
svmfit3 <- svm(y~., data=data_a_b, kernel="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit3, data_a_b)
table(truth=data_a_b$y, predict=ypred)

#Re-evaluate the data as a linearly separable amount using a hyperplane
a[b==1, ] <- a[b==1, ] + 0.5
plot(a[, 2], a[, 1], col=(b + 5) / 2, pch=17)

#Increase the cost to ensure no observations are misclassified
data_a_b_2 <- data.frame(x=a, y=as.factor(b))
svmfit4 <- svm(y~., data=data_a_b_2, kernel="radial", cost=1e6)
summary(svmfit4)
plot(svmfit4, data_a_b_2)

#We still have a couple of values misclassified in the dataset

svmfit5 <- svm(y~., data=data_a_b_2, kernel="radial", gamma = 1, cost=1)
summary(svmfit5)
plot(svmfit5,data_a_b_2)


#Sex SVM Analysis
a = matrix(rnorm(data2$SEX[1:5000]), ncol=2, byrow = TRUE)
b = c(rep(-1, 1250), rep(1, 1250))
a[b == 1,] = a[b == 1,] + 3.5
plot(a[, 2], a[, 1], col=(4-b))

data_a_b <- data.frame(x=a, y=as.factor(b))
svmfit <- svm(y~., 
              data=data_a_b, 
              kernel="linear", 
              cost=1e5)
summary(svmfit)
plot(svmfit, data_a_b)
#We can see some of the income is being misclassified as we increase the sampling size of the Income

#Set the cost factor to 10 and the scaling factor to false
svmfit <- svm(y ~ ., data=data_a_b, kernel="linear", cost=10, scale=TRUE)
plot(svmfit, data_a_b)

svmfit$index

summary(svmfit)

#Change the cost factor to .01 and the scaling factor to false again
svmfit2 <- svm(y ~ ., data=data_a_b, kernel="linear", cost=0.01, scale=FALSE)
plot(svmfit2, data_a_b)
svmfit2$index

#Tune the data to perform ten-fold cross-validation using linear kernels
#by changing the cost parameter in a range of values
set.seed(1000)
tune.out <- tune(svm,y ~ ., data=data_a_b, kernel="linear", 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1,5, 10, 100, 1000, 1500)))

# We can easily access the cross-validation errors for each
# of these models using the summary() command:
summary(tune.out)

# The tune() function stores the best model obtained, which
# can be accessed as follows:
bestmod <- tune.out$best.model
summary(bestmod)

#Use the predict() function to find the correct class based on a test observation set
atest <- matrix(rnorm(data2$INCOME[1:100] * 2), ncol=2, byrow = TRUE)
btest <- c(rep(-1, 25), rep(1, 25))
atest[btest==1,]=atest[btest==1,] + 2
testdata_a_b <- data.frame(x=atest, y=as.factor(btest))

# Now we predict the class labels of these test
# observations. Here we use the best model obtained through
# cross-validation in order to make predictions.
ypred <- predict(bestmod, testdata_a_b)
table(truth=testdata_a_b$y, predict=ypred)

# Thus, with this value of cost, 19 of the test observations
# are correctly classified.
#
# What if we had instead used cost=0.01?
svmfit3 <- svm(y~., data=data_a_b, kernel="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit3, data_a_b)
table(truth=data_a_b$y, predict=ypred)
# In this case one additional observation is misclassified.
#
#Re-evaluate the data as a linearly separable amount using a hyperplane
a[b==1, ] <- a[b==1, ] + 0.5
plot(a[, 2], a[, 1], col=(b + 5) / 2, pch=19)

#Increase the cost to ensure no observations are misclassified
data_a_b_2 <- data.frame(x=a, y=as.factor(b))
svmfit4 <- svm(y~., data=data_a_b_2, kernel="linear", cost=1e6)
summary(svmfit4)
plot(svmfit4, data_a_b_2)

############# Part 3: Model Development ######################
#Model Development for TARGET_FLAG

Model1 <- glm(TARGET_FLAG ~ FlagUnderAge25 + sqrt(SQRT_BLUEBOOK) + sqrt(SQRT_TRAVTIME) + KIDSDRIV + URBANICITY +
                sqrt(CLM_FREQ) + REVOKED + MVR_PTS + sqrt(TIF) + MSTATUS + PARENT1 + 
                CAR_USE + CAR_TYPE + FlagJobClerical + FlagJobBlueCollar + FlagJobManager +
                INCOME_bin + FlagEduBachelors + FlagEduMasters + FlagEduPhD + FlagOldClaim, 
              data = data, family = binomial())
summary(Model1)
data$Model1Prediction <- predict(Model1, type = "response")


Model2 <- glm(TARGET_FLAG ~ AGE + TRAVTIME + KIDSDRIV + SEX +  URBANICITY +
                CLM_FREQ + REVOKED + MVR_PTS + TIF + FlagEduBachelors + FlagEduMasters + FlagEduPhD +
              MSTATUS + PARENT1 + CAR_USE + CAR_TYPE + YOJ + JOB + 
                HOME_VAL,
                data = data, family = binomial())
summary(Model2)
data$Model2Prediction <- predict(Model2, type = "response")

Model3 <- glm(TARGET_FLAG ~ AGE + SQRT_TRAVTIME + SQRT_BLUEBOOK + DO_KIDS_DRIVE + URBANICITY +
                CLM_FREQ + REVOKED + MVR_PTS + TIF + EDUCATION + MSTATUS + PARENT1 + CAR_USE + CAR_TYPE + JOB + 
                HOME_OWNER,
              data = data, family = binomial())
summary(Model3)
data$Model3Prediction <- predict(Model3, type = "response")

Model4 = glm(formula = TARGET_FLAG ~ KIDSDRIV + log(INCOME + 1) + PARENT1 + 
              log(HOME_VAL + 1) + MSTATUS + FlagEduBachelors + FlagEduMasters + FlagEduPhD +
               JOB + TRAVTIME + CAR_USE + BLUEBOOK + TIF + CAR_TYPE + OLDCLAIM + CLM_FREQ + 
               REVOKED + MVR_PTS + URBANICITY, family = binomial(), 
               data = data)
summary(Model4)
data$Model4Prediction = predict(Model4, type = "response")


Model5 = glm(formula = TARGET_FLAG ~ sqrt(SQRT_BLUEBOOK) + sqrt(SQRT_TRAVTIME) + KIDSDRIV + URBANICITY +
               sqrt(CLM_FREQ) + REVOKED + MVR_PTS + sqrt(TIF) + MSTATUS + PARENT1 + 
               CAR_USE + CAR_TYPE + INCOME_bin + FlagBluebook + FlagEduPhD + FlagEduMasters +
               FlagEduBachelors + FlagJobManager + FlagJobBlueCollar + FlagJobClerical + FlagMaleSex +
               FlagOldClaim + FlagCar_Age + FlagYOJ, family = binomial(),
               data = data)
summary(Model5)
data$Model5Prediction = predict(Model5, type = "response")


plotROC(actuals = data$TARGET_FLAG, predictedScores = data$Model1Prediction)
plotROC(actuals = data$TARGET_FLAG, predictedScores = data$Model2Prediction)
plotROC(actuals = data$TARGET_FLAG, predictedScores = data$Model3Prediction)
plotROC(actuals = data$TARGET_FLAG, predictedScores = data$Model4Prediction)
plotROC(actuals = data$TARGET_FLAG, predictedScores = data$Model5Prediction)


########## Part 4: Model Selection 
AIC(Model1)
BIC(Model1)
AIC(Model2)
BIC(Model2)
AIC(Model3)
BIC(Model3)
AIC(Model4)
BIC(Model4)
AIC(Model5)
BIC(Model5)
print(-2*logLik(Model1, REML = TRUE))
print(-2*logLik(Model2, REML = TRUE))
print(-2*logLik(Model3, REML = TRUE))
print(-2*logLik(Model4, REML = TRUE))
print(-2*logLik(Model5, REML = TRUE))
ks_stat(actuals=data$TARGET_FLAG, predictedScores=data$Model1Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=data$Model2Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=data$Model3Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=data$Model4Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=data$Model5Prediction)

# We'll choose Model 1, here are its coefficients
coef(Model1)

#### Part 5:  Score Model on Test Data set and output csv file

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(test)

########### STAND ALONE SCORING PROGRAM ###############
test$P_TARGET_FLAG <- predict(Model1, newdata = test, type = "response")
summary(test)

#Prediction File 
prediction <- test[c("INDEX","P_TARGET_FLAG")]
write.csv(prediction, file = "write_final3.csv")
