#Aggregated Data Analysis

MyData <- read.csv(file.choose())

#Parking Probability Range to Analyze. 1 = 0.1%, 2 = 0.2%, ..., 999 = 99.9%
lower_bound = 1
upper_bound = 20

#Converts distance & intersections into a time in seconds.
#Assumes you're driving at 10mph and
#spend an average of 10 seconds at an intersection

TimeData  <- data.frame(matrix(nrow = 999))
TimeData$Alg <- (MyData$intersections_alg * 10 + MyData$distancetraveled_alg/14.7)/60
TimeData$Rand <- (MyData$intersections_rand * 10 + MyData$distancetraveled_rand/14.7)/60

Difference <-   TimeData$Rand[lower_bound:upper_bound] - TimeData$Alg[lower_bound:upper_bound]

plot(
  MyData$probability[lower_bound:upper_bound],
  Difference,
  "l",
  xlab = "Parking Difficulty (Hard to Easy)",
  ylab = "Time Saved (Minutes)",
  xlim = c(0.1,1),
  ylim = c(0,6)
)

print(summary(Difference))
