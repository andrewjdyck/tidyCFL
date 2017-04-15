
flatten_single_game <- function(single_game_JSON) {
  result1 <- dplyr::bind_rows(single_game_JSON[c("game_id", "date_start", "game_number", "week", "season", "attendance")])
  home <- ifelse(single_game_JSON$team_1$is_at_home, "team_1", "team_2")
  away <- ifelse(home == 'team_1', 'team_2', 'team_1')
  result2 <- data.frame(
    event_type = single_game_JSON$event_type$name,
    venue = single_game_JSON$venue$name,
    coin_toss = single_game_JSON$coin_toss$coin_toss_winner,
    winner = ifelse(single_game_JSON$team_1$is_winner, 
                     single_game_JSON$team_1$abbreviation,
                     single_game_JSON$team_2$abbreviation),
    home_team = single_game_JSON[[home]]$abbreviation,
    away_team = single_game_JSON[[away]]$abbreviation,
    home_score = single_game_JSON[[home]]$score,
    away_score = single_game_JSON[[away]]$score
  )
  return(dplyr::tbl_df(cbind(result1, result2)))
}

flatten_play_by_play <- function(single_play_JSON) {
  fields1 <- c("play_id", "play_sequence", "quarter", "play_clock_start", "play_clock_start_in_secs", "field_position_start",
               "field_position_end", "down", "yards_to_go", "is_in_red_zone", "team_home_score",
               "team_visitor_score", "play_type_id", "play_result_type_id", "play_result_yards",
               "play_result_points", "play_success_id", "play_change_of_possession_occurred", "team_abbreviation",
               "team_id", "play_summary"
               )
  # should probably add the players info here in the future.
  result1 <- dplyr::bind_rows(single_play_JSON[fields1])
}

extract_game_data_for_pbp <- function(single_game_JSON) {
  home <- ifelse(single_game_JSON$team_1$is_at_home, "team_1", "team_2")
  away <- ifelse(home == 'team_1', 'team_2', 'team_1')
  result <- tibble(
    game_id = single_game_JSON$game_id,
    home_team = single_game_JSON[[home]]$abbreviation,
    away_team = single_game_JSON[[away]]$abbreviation
  )
  return(result)
}

