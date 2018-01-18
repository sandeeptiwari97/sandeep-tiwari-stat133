# =======================================================
# Title: Ranking Teams
# Description: This homework aims to rank teams purely based 
# individual player statistics
# Input(s): data files 'nba2017-roster.csv' and 'nba2017-stats.csv'
# Output(s): data files 'nba2017-teams.csv', 'make-teams-tables.R', images of plots
# =======================================================

# Packages
library(readr) # importing data
library(dplyr) # data wrangling
library(ggplot2) # graphics

# Importing tables
roster <- read.csv('data/nba2017-roster.csv')
stats <- read.csv('data/nba2017-stats.csv')

# Adding new variables
stats$missed_fg <- stats$field_goals_atts - stats$field_goals_made
stats$missed_ft <- stats$points1_atts - stats$points1_made
stats$points <- (stats$points3_made * 3) + (stats$points2_made * 2) + 
  stats$points1_made
stats$rebounds <- stats$off_rebounds + stats$def_rebounds
stats$efficiency <- (stats$points + stats$rebounds + stats$assists + 
                       stats$steals + stats$blocks - stats$missed_fg - 
                       stats$missed_ft - stats$turnovers) / stats$games_played

sink(file = 'output/efficiency-summary.txt')
summary(stats$efficiency)
sink()

# Merging tables
total <- merge(roster, stats)
total

# Creating nba2017-teams.csv
exp = as.data.frame(total %>%
  group_by(team) %>%
  summarise(
    experience = round(mean(experience), 2)
  ))

teams = as.data.frame(total %>%
  group_by(team)%>%
  summarise(
    experience = sum(experience),
    salary = round(sum(salary / 1000000), 2),
    points3 = sum(points3_made),
    points2 = sum(points2_made),
    free_throws = sum(points1_made),
    points = sum(points),
    off_rebounds = sum(off_rebounds),
    def_rebounds = sum(def_rebounds),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    turnovers = sum(turnovers),
    fouls = sum(fouls),
    efficiency = sum(efficiency)
  ))
#teams <- merge(exp, tot, by = 'team')
teams[, 'team'] <- sapply(teams[, 'team'], as.character)
summary(teams)

sink(file = 'data/teams-summary.txt')
summary(teams)
sink()

write.csv(teams, "data/nba2017-teams.csv", row.names = FALSE)

# Some graphics
pdf(file = 'images/teams_star_plot.pdf')
stars(teams[ ,-1], labels = teams$team)
dev.off()

pdf(file = 'images/experience_salary.pdf')
ggplot(teams, aes(experience, salary)) + geom_point() + geom_text(aes(label=team))
dev.off()