
cfl_players <- function(player_id, foptions = list()) {
  if (is.na(player_id)) {
    stop("A cfl central player ID is required", call. = FALSE)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/players/', player_id, 
                  '?include=seasons,game_by_game,current_team')
  }
  
  url <- tidyCFL.build_url(url)
  players_call <- GET(url, foptions)
  stop_for_status(players_call)
  player_data_JSON <- content(players_call)
  player_data <- player_data_JSON$data[[1]][1:15]
  player_seasons <- player_data_JSON$data[[1]]$seasons
  player_game_data <- player_data_JSON$data[[1]]$game_by_game
  
  # if(length(games_call) == 0) {
  #   pbp_data <- dplyr::tbl_df()
  # } else {
  #   pbp_data <- dplyr::bind_rows(
  #     lapply(1:length(pbp_JSON),
  #            function(x) flatten_play_by_play(pbp_JSON[[x]])))
  # }
  # 
  # if (nrow(pbp_data) == 0) {
  #   stop("No data found", call. = FALSE)
  #   NULL
  # } else {
  #   dplyr::tbl_df(cbind(game_data, pbp_data))
  # }
  return(player_data_JSON)
}
