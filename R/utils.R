
# Flatten a single game JSON to a tibble
flatten_single_game <- function(single_game_JSON) {
  result1 <- dplyr::bind_rows(single_game_JSON[c("game_id", "date_start", "game_number", "week", "season", "attendance")])
  home <- ifelse(single_game_JSON$team_1$is_at_home, "team_1", "team_2")
  away <- ifelse(home == 'team_1', 'team_2', 'team_1')
  result2 <- data.frame(
    event_type = single_game_JSON$event_type$name,
    venue = single_game_JSON$venue$name,
    coin_toss = single_game_JSON$coin_toss$coin_toss_winner,
    winner = ifelse(
      single_game_JSON$event_status$name == 'Pre-Game',
      NA,
      ifelse(
        single_game_JSON$team_1$is_winner, 
        single_game_JSON$team_1$abbreviation,
        single_game_JSON$team_2$abbreviation
      )
    ),
    home_team = single_game_JSON[[home]]$abbreviation,
    away_team = single_game_JSON[[away]]$abbreviation,
    home_score = single_game_JSON[[home]]$score,
    away_score = single_game_JSON[[away]]$score
  )
  return(dplyr::tbl_df(cbind(result1, result2)))
}

# flatten a play-by-play JSON object to a tibble
flatten_play_by_play <- function(single_play_JSON) {
  fields1 <- c("play_id", "play_sequence", "quarter", "play_clock_start", "play_clock_start_in_secs", "field_position_start",
               "field_position_end", "down", "yards_to_go", "is_in_red_zone", "team_home_score",
               "team_visitor_score", "play_type_id", "play_result_type_id", "play_result_yards",
               "play_result_points", "play_success_id", "play_change_of_possession_occurred", "team_abbreviation",
               "team_id", "play_summary"
  )
  
  # Player IDs
  quarterback_id <- ifelse(
    single_play_JSON$players$quarterback$cfl_central_id == 0,
    NA,
    single_play_JSON$players$quarterback$cfl_central_id)
  
  ball_carrier_id <- ifelse(
    single_play_JSON$players$ball_carrier$cfl_central_id == 0,
    NA,
    single_play_JSON$players$ball_carrier$cfl_central_id)
  
  primary_defender_id <- ifelse(
    single_play_JSON$players$primary_defender$cfl_central_id == 0,
    NA,
    single_play_JSON$players$primary_defender$cfl_central_id)
  
  
  # Play descriptions/results
  play_type <- subset(play_type_id,
                      play_type_id == single_play_JSON$play_type_id,
                      select=play_type_desc)
  
  play_result <- subset(play_result_type_id,
                        play_result_type_id == single_play_JSON$play_result_type_id,
                        select=play_result_type_desc)
  
  play_success <- subset(play_success_id,
                         play_success_id == single_play_JSON$play_success_id,
                         select=play_success_desc)
  
  # clock Calcs
  seconds_remain_half <- (
    min(single_play_JSON$quarter,4) %% 2) * 900 +
    single_play_JSON$play_clock_start_in_secs
  
  seconds_remain_game <- (
    4 - min(single_play_JSON$quarter,4)) * 900 +
    single_play_JSON$play_clock_start_in_secs
  
  # Other Calcs
  distance_to_goal <- ifelse(
    substr(single_play_JSON$team_abbreviation,1,1)==substr(single_play_JSON$field_position_start,1,1),
    110 - as.integer(substr(single_play_JSON$field_position_start,2,nchar(single_play_JSON$field_position_start))),
    as.integer(substr(single_play_JSON$field_position_start,2,nchar(single_play_JSON$field_position_start)))
  )
  
  # Added player ids. Is a recursive reference best practice? 
  result1 <- dplyr::bind_rows(single_play_JSON[fields1])
  result1 <- cbind(result1,distance_to_goal,seconds_remain_half,
                   seconds_remain_game,quarterback_id,ball_carrier_id,
                   primary_defender_id,play_result,play_type,play_success)
}

# get game metadata for a Play-by_play object
extract_game_data_for_pbp <- function(single_game_JSON) {
  home <- ifelse(single_game_JSON$team_1$is_at_home, "team_1", "team_2")
  away <- ifelse(home == 'team_1', 'team_2', 'team_1')
  result <- dplyr::tbl_df(data.frame(
    game_id = single_game_JSON$game_id,
    home_team = single_game_JSON[[home]]$abbreviation,
    away_team = single_game_JSON[[away]]$abbreviation,
    stringsAsFactors=FALSE))
  return(result)
}

