#Clean Rstudio Environment and Import data from Excel
rm(list = ls())

setwd("C:/Users/Melvin/Documents/William and Mary/Courses/Spring 2021/BUAD 5122 Machine Learning I/Module 1")
moneyballdata <- read.csv("Moneyball_Data.csv")

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lattice)

#Create all numerical categories for the following dataset below:
moneyballdata$TEAM_BATTING_H = as.numeric(moneyballdata$TEAM_BATTING_H)
moneyballdata$TEAM_BATTING_2B = as.numeric(moneyballdata$TEAM_BATTING_2B)
moneyballdata$TEAM_BATTING_3B = as.numeric(moneyballdata$TEAM_BATTING_3B)
moneyballdata$TEAM_BATTING_HR = as.numeric(moneyballdata$TEAM_BATTING_HR)
moneyballdata$TEAM_BATTING_BB = as.numeric(moneyballdata$TEAM_BATTING_BB)
moneyballdata$TEAM_BATTING_HBP = as.numeric(moneyballdata$TEAM_BATTING_HBP)
moneyballdata$TEAM_BATTING_SO = as.numeric(moneyballdata$TEAM_BATTING_SO)
moneyballdata$TEAM_BASERUN_SB = as.numeric(moneyballdata$TEAM_BASERUN_SB)
moneyballdata$TEAM_BASERUN_CS = as.numeric(moneyballdata$TEAM_BASERUN_CS)
moneyballdata$TEAM_FIELDING_E = as.numeric(moneyballdata$TEAM_FIELDING_E)
moneyballdata$TEAM_FIELDING_DP = as.numeric(moneyballdata$TEAM_FIELDING_DP)
moneyballdata$TEAM_PITCHING_BB = as.numeric(moneyballdata$TEAM_PITCHING_BB)
moneyballdata$TEAM_PITCHING_H = as.numeric(moneyballdata$TEAM_PITCHING_H)
moneyballdata$TEAM_PITCHING_HR = as.numeric(moneyballdata$TEAM_PITCHING_HR)
moneyballdata$TEAM_PITCHING_SO = as.numeric(moneyballdata$TEAM_PITCHING_SO)


#Verify the data frame is built correctly based on logical statements above and then view a brief summary
str(moneyballdata)

#The summary of the data will tell us which columns have errors or NA values
summary(moneyballdata)

#Now we can look at the individual data sets with potential errors and analyze them individually

#Batters Hit by Pitch(get a free base) First we found the NA values(Free Base), converted them to zero and then assigned COMPLETE
#the median value from the median batters hit by pitch(HBP) to the NA variables and created another graph
boxplot(moneyballdata$TEAM_BATTING_HBP, col = "blue", main = "Boxplot of Batters hit by Pitch(get a free base)")
moneyballdata$FreeBase <- as.factor(ifelse(is.na(moneyballdata$TEAM_BATTING_HBP), 0, 1))
moneyballdata$TEAM_BATTING_HBP[is.na(moneyballdata$TEAM_BATTING_HBP)] = median(moneyballdata$TEAM_BATTING_HBP, na.rm = TRUE)

#New Chart with NA values showing median value of 58 with max of 95 and min of 29
plot(moneyballdata$TEAM_BATTING_HBP, xlab = "Team Number", ylab = "Batters Hit by Pitch (Free Base)",
     main = "Batters Hit by Pitch(Free Bases)")
abline(h=max(moneyballdata$TEAM_BATTING_HBP),col="green")
text(x = 80, y= (max(moneyballdata$TEAM_BATTING_HBP) - 2), label = "Max Bases", col="green")


#Strikeouts by batters (Bi-Modal distribution) COMPLETE
hist(moneyballdata$TEAM_BATTING_SO, col="pink", xlab= "Number of Strikeouts", main = "Total Number of Strikeouts" )
moneyballdata$FlagStrikeoutsbyBatters = as.factor(ifelse(is.na(moneyballdata$TEAM_BATTING_SO), 0, 1))
moneyballdata$TEAM_BATTING_SO[is.na(moneyballdata$TEAM_BATTING_SO)] = median(moneyballdata$TEAM_BATTING_SO, na.rm = TRUE)

#New Chart still displays the bi-modal distribution but we can see a pattern in the number of strikeouts
hist(moneyballdata$TEAM_BATTING_SO, col="pink", xlab= "Number of Strikeouts",
     main = "Total Number of Strikeouts with Median Average for NA Values")
abline(v=median(moneyballdata$TEAM_BATTING_SO), col="purple", lwd=2)
text(x=median(moneyballdata$TEAM_BATTING_SO), y =330, label = "Median Strikeout Value of 750 ", adj =1, cex=1, col="Purple", font=4 )




#Stolen Bases(Right-Skewed Distribution with Outliers)
hist(moneyballdata$TEAM_BASERUN_SB, col="yellow", xlab="Number of Stolen Bases", main= "Stolen Bases")
moneyballdata$FlagStolenBases = as.factor(ifelse(is.na(moneyballdata$TEAM_BASERUN_SB), 0, 1))
moneyballdata$TEAM_BASERUN_SB[is.na(moneyballdata$TEAM_BASERUN_SB)] = median(moneyballdata$TEAM_BASERUN_SB, na.rm = TRUE)

a = moneyballdata$TEAM_BASERUN_SB
function(a){
  x[(x > quantile(a, 0.25)-1.5*IQR(a)) & (a < quantile(a, 0.75)+1.5*IQR(a))]
}

hist(a)

#Percentile Trimming of Stolen Bases
Percentile_trim = function(a, lb, ub){
  a[(a > quantile(a, lb)) & (a < quantile(a, ub))]
}
#New chart showing a
hist(Percentile_trim(a, 0.01, 0.99),col="yellow", xlab="Number of Stolen Bases", main= "Stolen Bases")

#Caught Stealing (Right-Skewed Distribution with Outliers)
hist(moneyballdata$TEAM_BASERUN_CS, col="red", xlab= "Number of Caught Stealing", main= "Caught Stealing Bases", breaks=15)
moneyballdata$FlagCaughtStealing <- as.factor(ifelse(is.na(moneyballdata$TEAM_BASERUN_CS), 0, 1))
moneyballdata$TEAM_BASERUN_CS[is.na(moneyballdata$TEAM_BASERUN_CS)] = median(moneyballdata$TEAM_BASERUN_CS, na.rm = TRUE)

#Percentile Trimming of Caught Stealing
b = moneyballdata$TEAM_BASERUN_CS
Percentile_trim2 = function(b, lb, ub){
  b[(b > quantile(b, lb)) & (b < quantile(b, ub))]
}
#New chart displaying trimmed version of histogram from the first data set of Caught Stealing
hist(Percentile_trim2(b, 0.01, 0.99),col="red", xlab="Number of Stolen Bases", main= "Stolen Bases")



#Double Plays(Bell-Shape Distribution of Data) COMPLETE
hist(moneyballdata$TEAM_FIELDING_DP, col="blue", xlab = "Number of Double Plays", main= "Total Double Plays", breaks=15)
moneyballdata$FlagDoublePlays = as.factor(ifelse(is.na(moneyballdata$TEAM_FIELDING_DP), 0, 1))
moneyballdata$TEAM_FIELDING_DP[is.na(moneyballdata$TEAM_FIELDING_DP)] = median(moneyballdata$TEAM_FIELDING_DP, na.rm = TRUE)


#Strikeouts by Pitchers (Basic Plot with Outliers Highlighted)
plot(moneyballdata$TEAM_PITCHING_SO, col="black", xlab= "Strikeouts by Pitchers", main= "Total Strikeouts by Pitchers",
     ylab= "Strikeouts by Pitchers")
moneyballdata$FlagStrikeoutsbyPitcher = as.factor(ifelse(is.na(moneyballdata$TEAM_PITCHING_SO), 0, 1))
moneyballdata$TEAM_PITCHING_SO[is.na(moneyballdata$TEAM_PITCHING_SO)] = median(moneyballdata$TEAM_PITCHING_SO, na.rm = TRUE)

c = moneyballdata$TEAM_PITCHING_SO
Percentile_trim3 = function(c, lb, ub){
  c[(c > quantile(c, lb)) & (c < quantile(c, ub))]
}
plot(c)



#Verify summary has no more missing NA values and then create another Excel document with flags and messages
summary(moneyballdata)


# Rewriting data to a new file (as corrected)
write.csv(moneyballdata, file = "moneyballdata_update.csv")



