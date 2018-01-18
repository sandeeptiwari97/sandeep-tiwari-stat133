### Data Dictionary nba2017-stats.csv

This data table consists of statistics of 441 NBA players. There are 441 rows and 27 variables, 1 row for each player and 1 column for each variable. The column labels are:
player, games_played, minutes, field_goals_made, field_goals_atts, field_goals_perc, points3_made, points3_atts, points3_perc, points2_made, points2_atts, points2_perc, 
points1_made, points1_atts, points1_perc, off_rebounds, def_rebounds, assists, steals, blocks, turnovers, fouls, missed_fg, missed_ft, points, rebounds, efficiency.

For each column:
 - player = the name of the player
 - games_played = the # of games played by player
 - minutes = the # of minutes played by player
 - field_goals_made = the # of field goals(shots) made
 - field_goals_atts = the # of field goals(shots) attempted
 - field_goals_perc = the percentage of field goals made ==> field_goals_made / (field_goals_made + field_goals_atts))
 - points3_made = the # of 3-point shots made
 - points3_atts = the # of 3-point shots attempted
 - points3_perc = the percentage of 3-point shots made ==> points3_made / (points3_made + points3_atts)
 - points2_made = the # of 2-point shots made
 - points2_atts = the # of 2-point shots attempted
 - points2_perc = the percentage of 2-point shots made ==> points2_made / (points2_made + points2_atts)
 - points1_made = the # of free throws shots made
 - points1_atts = the # of free throws shots attempted
 - points1_perc = the percentage of free throws shots made ==> points1_made / (points1_made + points1_atts)
 - off_rebounds = the # of offensive rebounds by player
 - def_rebounds = the # of defensive rebounds by player
 - assists = the # of assists by player
 - steals = the # of steals by player
 - blocks = the # of blocks by player
 - turnovers = the # of turnovers by player
 - missed_fg = the # of missed field goals by player
 - missed_ft = the # of missed free throws by player
 - points = the total number of points scored by player
 - rebounds = the total number of rebounds by player ==> off_rebounds + def_rebounds
 - efficiency = index that measures efficiency of player ==> (points + rebounds + assists + steals + blocks - missed_fg - missed_ft - turnovers) / games_played

Main source: www.basketball-reference.com
Sample link: https://www.basketball-reference.com/teams/GSW/2017.html