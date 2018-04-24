#Constant Difficulty Data Analysis

StaticData <- read.csv(file.choose())

#Converts distance & intersections into a time in minutes
#Assumes you're driving at 10mph and
#spend an average of 10 seconds at an intersection
#TimeData is in Minutes.

STimeData  <- data.frame(matrix(nrow = 100000))
STimeData$Alg <- (StaticData$intersections_alg * 10 + StaticData$distancetraveled_alg/14.7)/60
STimeData$Rand <- (StaticData$intersections_rand * 10 + StaticData$distancetraveled_rand/14.7)/60

boxplot(
  STimeData$Alg,
  STimeData$Rand,
  horizontal = TRUE,
  names = c("Alg","Rand"),
  xlab = "Time (Minutes)",
  main = "Simulation Data (p = 0.2%)"
)

outliers_Alg <- boxplot.stats(STimeData$Alg)$out
outliers_Rand <- boxplot.stats(STimeData$Rand)$out

summary(STimeData$Rand - STimeData$Alg)

#Savings Analysis
Differences <- StaticData$distancetraveled_rand - StaticData$distancetraveled_alg
Avg_Diff = mean(Differences)/5280 #Distance Saved
Cost_Savings_Per_Trip = 0.40 * Avg_Diff #40 Cents a mile
Annual_Cost_Savings = Cost_Savings_Per_Trip * 12 * 40 * 100000

#Savings Analysis Using INRIX Data
#107 Hours a Year Spent by Drivers in NYC, 15 Minutes/Trip -> 428 Trips a Year
#Assuming Average Speed of 20MPhs, 100k users
A_Diff = 5 - (mean(StaticData$distancetraveled_rand))/5280
A_Savings = A_Diff * 0.4 * 428 * 100000
