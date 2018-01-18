# =======================================================
# Title: Grades Visualizer Data Preparation
# Description: This homework visualize small data computing and visalizing
# homework scores
# Input(s): 'rawscores.csv'
# Output(s): 'summary-rawscores.txt'
# =======================================================

# sourcing in functions.R and reading in scores dataframe
source('functions.R')
scores <- read.csv('../data/rawdata/rawscores.csv')

# sinking structure and summary statistics of scores
sink(file = '../output/summary-rawscores.txt')
str(scores)
for(i in 1:length(colnames(scores))) {
  print_stats(summary_statistics(scores[, i]))
}
sink()

# setting all NA values in scores to zero
scores[is.na(scores)] <- 0

# rescaling quiz and exam scores
scores$QZ1 <- rescale100(scores$QZ1, xmin = 0, xmax = 12)
scores$QZ2 <- rescale100(scores$QZ2, xmin = 0, xmax = 18)
scores$QZ3 <- rescale100(scores$QZ3, xmin = 0, xmax = 20)
scores$QZ4 <- rescale100(scores$QZ4, xmin = 0, xmax = 20)
scores$Test1 <- rescale100(scores$EX1, xmin = 0, xmax = 80)
scores$Test2 <- rescale100(scores$EX2, xmin = 0, xmax = 90)

# creating Homework, Quiz, and Overall variable
for(i in 1:nrow(scores)) {
  scores$Homework[i] <- score_homework(as.numeric(scores[i, 1:9]))
  scores$Quiz[i] <- score_quiz(as.numeric(scores[i, c("QZ1", "QZ2", "QZ3", "QZ4")]))
  scores$Overall[i] <- 0.1*score_lab(scores$ATT[i]) + 0.3*scores$Homework[i] + 0.15*scores$Quiz[i] + 0.2*scores$Test1[i] + 0.25*scores$Test2[i]
}

# calculating the letter grade for each student
for(j in 1:nrow(scores)) {
  if (scores$Overall[j] < 50) {
    scores$Grade[j] <- "F"
  } else if (scores$Overall[j] < 60) {
    scores$Grade[j] <- "D"
  } else if (scores$Overall[j] < 70) {
    scores$Grade[j] <- "C-"
  } else if (scores$Overall[j] < 77.5) {
    scores$Grade[j] <- "C"
  } else if (scores$Overall[j] < 79.5) {
    scores$Grade[j] <- "C+"
  } else if (scores$Overall[j] < 82) {
    scores$Grade[j] <- "B-"
  } else if (scores$Overall[j] < 86) {
    scores$Grade[j] <- "B"
  } else if (scores$Overall[j] < 88) {
    scores$Grade[j] <- "B+"
  } else if (scores$Overall[j] < 90) {
    scores$Grade[j] <- "A-"
  } else if (scores$Overall[j] < 95) {
    scores$Grade[j] <- "A"
  } else {
    scores$Grade[j] <- "A+"
  }
}

# sinking structure of scores to summary-cleanscores.txt
sink(file = '../output/summary-cleanscores.txt')
str(scores)
sink()

# sinking stats of Lab to Lab-stats.txt
sink(file = '../output/Lab-stats.txt')
print_stats(summary_statistics(scores$ATT))
sink()

# sinking stats of Homework to Homework-stats.txt
sink(file = '../output/Homework-stats.txt')
print_stats(summary_statistics(scores$Homework))
sink()

# sinking stats of Quiz to Quiz-stats.txt
sink(file = '../output/Quiz-stats.txt')
print_stats(summary_statistics(scores$Quiz))
sink()

# sinking stats of Test1 to Test1-stats.txt
sink(file = '../output/Test1-stats.txt')
print_stats(summary_statistics(scores$Test1))
sink()

# sinking stats of Test2 to Test2-stats.txt
sink(file = '../output/Test2-stats.txt')
print_stats(summary_statistics(scores$Test2))
sink()

# sinking stats of Overall to Overall-stats.txt
sink(file = '../output/Overall-stats.txt')
print_stats(summary_statistics(scores$Overall))
sink()

# exporting clean data frame to cleanscores.csv
write.csv(scores, file = '../data/cleandata/cleandata.csv', row.names = FALSE)

colnames(scores)[1:21]

