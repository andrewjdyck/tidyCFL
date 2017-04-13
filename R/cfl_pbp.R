
cfl_plays <- function(season = NA, game_id = NA, foptions = list()) {
  if (is.na(season)) {
    stop("A season year is required", call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    # url <- paste0('http://api.cfl.ca/v1', '/games/',
    #               season)
    stop('A game_id is required', call. = FALSE)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season, 
                  '/game/', game_id)
  }
  
  if(!missing(url)) {
    url <- paste0(url, '?key=', api_key, '&include=play_by_play')
  } else {
    stop('There was an error building the url')
  }
  
  games_call <- GET(url, foptions)
  stop_for_status(games_call)
  games_data_JSON <- content(games_call)
  game_data <- extract_game_data_for_pbp(games_data_JSON$data[[1]])
  
  pbp_JSON <- games_data_JSON$data[[1]]$play_by_play
  
  if(length(games_call) == 0) {
    pbp_data <- tbl_df()
  } else {
    pbp_data <- dplyr::bind_rows(
      lapply(1:length(pbp_JSON),
             function(x) flatten_play_by_play(pbp_JSON[[x]])))
  }
  
  if (nrow(pbp_data) == 0) {
    stop("No data found", call. = FALSE)
    NULL
  } else {
    tbl_df(cbind(game_data, pbp_data))
  }
}
