### Author: Gordon Quach
### Date: 9-5-2022

### The goal of this script is to create Hans Rosling's animated bubble chart per Keith McNulty's tutorial on Medium. 
### Article Source: https://levelup.gitconnected.com/how-to-create-hans-roslings-famous-animated-bubble-chart-in-r-e89607dbf73a

## Setting up directory and loading in useful libraries.

library(DBI)
library(RSQLite)
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyr)
library(dplyr)
library(viridis)
setwd("~/Desktop")

## Loading and transforming data.

# Data source: https://www.kaggle.com/datasets/hugomathien/soccer
# Connecting to SQL db.
futbolDB <- dbConnect(SQLite(), "futbol.sqlite")

# Exploring tables in database.
dbListTables(futbolDB) # All tables

# Exploring data within tables.
dfMatch <- collect(tbl(futbolDB, "Match"))
dfTeam <- collect(tbl(futbolDB, "Team"))
dfTeamA <- collect(tbl(futbolDB, "Team_Attributes")) 
dfCountry <- collect(tbl(futbolDB, "Country")) 

# Subsetting data to review per table prior to join.
keepDFCountry <- dfCountry # No need to further subset.
keepDFMatch <- dfMatch[c("date", "country_id", "match_api_id", "home_team_api_id", "away_team_api_id", "home_team_goal", "away_team_goal")]
keepDFTeam <- dfTeam[c("team_api_id", "team_long_name")]
keepDFTeamA <- dfTeamA[c("team_api_id", "date", "chanceCreationPassing", "chanceCreationShooting")]

# Creating view to store max team attributes.
# Commenting out query since view has been created.
# viewQuery <- dbSendQuery(futbolDB,
#                          "CREATE VIEW Max_Attributes AS 
#                              SELECT
#                                  team_api_id
#                                  , max(ta.chanceCreationPassing) as Max_Rate_of_Created_Chances
#                                  , max(ta.chanceCreationShooting) as Max_Rate_of_Shots
#                              FROM
#                                 Team_Attributes ta
#                              GROUP BY
#                                 team_api_id"
#                         )

# Used for debugging.
# dropQuery <- dbSendQuery(futbolDB,
#                          "DROP VIEW max_attributes")
# testQuery <- dbSendQuery(futbolDB,
#                          "select * from max_attributes")
# testQueryResults <- dbFetch(testQuery)

# Writing query to DB.
reportQuery <- dbSendQuery(futbolDB, 
                           "SELECT 
                               strftime('%Y', m.date) as Match_Year
                               , c.name as Country
                               , t.team_long_name as Home_Team
                               , sum(m.home_team_goal) as Home_Goals_Scored
                               , ma.Max_Rate_of_Created_Chances
                               , ma.Max_Rate_of_Shots
                           FROM 
                                Match m	
                           INNER JOIN
                                Team t on m.home_team_api_id = t.team_api_id
                           INNER JOIN
                                Team_Attributes ta on m.home_team_api_id = ta.team_api_id 
                           INNER JOIN
                                Country c on m.country_id = c.id
                           INNER JOIN
                                Max_Attributes ma on m.home_team_api_id = ma.team_api_id
                           GROUP BY
                                Match_Year
                               , Country
                               , Home_Team"
                           )

queryResults <- dbFetch(reportQuery)

## Creating charts and animation.

# Chart Build Plan:

# X axis is the rate/risk at which the team within a country creates chances.
# Y axis is the rate/risk at which the team within a country takes shots
# Bubble size represents the amount of goals teams within a country scored.
# Color of bubble represents the countries (grouping teams).

# Calculating ranges of log values to better fit viz proportions.
range(log(queryResults$Max_Rate_of_Created_Chances))
range(log(queryResults$Max_Rate_of_Shots))
range(log(queryResults$Home_Goals_Scored))

# Building and reviewing chart.
roslingChart <- queryResults %>% ggplot(aes(x = log(Max_Rate_of_Created_Chances), y = log(Max_Rate_of_Shots), size = Home_Goals_Scored)) +
  geom_point(alpha = 0.5, aes(color = Country)) +
  scale_color_viridis(discrete = TRUE, name = "Countries", option = "plasma") +
  scale_size(range = c(0,25), guide = "none") +
  scale_x_continuous(breaks = seq(3.3, 4.5, by = .1), limits=c(3.3, 4.5)) +                                  
  scale_y_continuous(breaks = seq(3.3, 4.5, by = .1), limits=c(3.3, 4.5)) +
  labs(x = "Log - Rate of Passing Chances Made", y = "Log - Rate of Shots Made") +
  theme_classic() +
  # geom_text(aes(x = 3.4, y = 4.3, label = Match_Year), size = 12, color = "lightgrey") +
  transition_states(Match_Year, transition_length = .5, state_length = .5) +  
  ease_aes('elastic-in-out') +
  labs(title = 'Season: {closest_state}')

roslingChart 

## Saving GIF to working directory location.

anim_save("Rosling Futbol.gif", roslingChart, nframes = 125, height = 800, width = 1500)

