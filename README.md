# tidyCFL
A tidy data package for the Canadian Football League API


## API info
An API key is required and can be requested [here](http://api.cfl.ca/key-request).

Documentation for the CFL API can be found [here](http://api.cfl.ca/docs).

## Config
set the API key with tidyCFL.api_key('YOUR API KEY')

# Usage

```{r}
library(tidyCFL)
library(dplyr)

tidyCFL.api_key('testK3Y')
games2016 <- cfl_games(2016)

# Order games by point spread
games2016 %>%
  mutate(point_diff = abs(away_score-home_score)) %>%
  arrange(desc(point_diff))


# Information for a single player. 
# Takes the CFL Player ID as an argument
player_id <- 138985
player1 <- cfl_players(player_id)

# Play by play
pbp <- cfl_plays(season = 2016, game_id = 2280)


```
