# =======================================================
# Title: More dplyr and ggplot basics
# Description: This is a lab to help learn more about the 
#              functionalities ofdplyr and ggplot
# Input(s): data file 'nba2017-players.csv'
# Output(s): what are the main outputs (list of outputs)
# Author: Sandeep Tiwari
# Date: 10-06-1997
# =======================================================

# packages
library(readr) # importing data
library(dplyr) # data wrangling
library(ggplot2) # graphics

# Exporting some data tables
nba <- read_csv('data/nba2017-players.csv')
warriors <- arrange(filter(nba, team == "GSW"), salary)
write.csv(warriors, "data/warriors.csv", row.names = FALSE)
lakers <- arrange(filter(nba, team == "LAL"), desc(experience))
write.csv(lakers, "data/lakers.csv", row.names = FALSE)

# Exporting some R output
sink(file = 'output/data-structure.txt')
str(nba)
sink()

sink(file = 'output/summary-warriors.txt')
summary(warriors)
sink()

sink(file = 'output/summary-lakers.txt')
summary(lakers)
sink()

# Exporting some "base" graphs
png(filename = 'images/scatterplot-height-weight.png')
plot(nba$height, nba$weight, pch = 20, xlab = "Height", ylab = "Weight")
dev.off()

jpeg(filename = 'images/histogram-age.jpeg', width = 600, height = 400)
hist(nba$age, main = "Histogram of Ages in the NBA", xlab = "Age")
dev.off()

# FIGURE OUT HOW TO SAVE PDF FILE FOR HISTOGRAM
pdf(filename = "images/histogram-age.jpeg", width = 7, height = 5)
hist(nba$age, main = "Histogram of Ages in the NBA", xlab = "Age")
dev.off()
# FIGURE OUT HOW TO SAVE PDF FILE FOR HISTOGRAM

gg_pts_salary <- ggplot(nba, aes(points, salary)) + geom_point()
help("ggsave")
ggsave("images/points_salary.pdf", gg_pts_salary, width = 7, height = 5, units = "in")

# More "dplyr"

# Displaying player names of Lakers
nba %>% 
  filter(team == "LAL") %>% 
  select(player)

# Displaying name and salary of GSW point guards
nba %>% 
  filter(team == "GSW" & position == "PG") %>% 
  select(player, salary)

# Displaying name, age, and team of players with > 10 years experience,
# making 10 million dollars or less
nba %>% 
  filter(experience < 10 & salary <= 10000000) %>% 
  select(player, age, team)

# Selecting name, team, height, and weight of rookie players, 20 years
# old, displaying only first five occurrences
nba %>%
  filter(experience == 0 & age == 20) %>%
  select(player, team, height, weight) %>%
  slice(1:5)

# Creating data frame of GSW players, that contains variables for player 
# name, experience, and MPG, sorted by MPG in descending order
gsw_map = nba %>%
  filter(team == "GSW") %>%
  mutate(min_per_game = minutes / games) %>%
  select(player, experience, min_per_game) %>%
  arrange(desc(min_per_game))

# Displaying avg triple points by team, in ascending order, of bottom-5
# teams
nba %>%
  group_by(team) %>%
  summarise(
    avg_p3 = mean(points3)
  ) %>%
  arrange(avg_p3) %>%
  slice(1:5)

# Obtaining mean and SD of age, for PFs, with 5 and 10 years experience
nba %>%
  filter(position == 'PF') %>%
  filter(experience == 5 | experience == 10) %>%
  summarise(
    avg_age = mean(age),
    sd_age = sd(age)
  )
