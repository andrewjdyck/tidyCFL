
cfl_games <- function(season = NA, game_id = NA, foptions = list()) {
  if (is.na(season)) {
    stop("A season year is required to find game_id data", call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    url <- paste0('http://api.cfl.ca/v1', '/games/',
                  season)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season, 
                  '/game/', game_id)
  }
  
  if(!missing(url)) {
    url <- paste0(url, '?key=', api_key)
  } else {
    stop('There was an error building the url')
  }
  
  games_call <- GET(url, foptions)
  stop_for_status(games_call)
  games_data_JSON <- content(games_call)
  if(length(games_call) == 0) {
    games_data <- tbl_df()
  } else {
    games_data <- dplyr::bind_rows(
      lapply(1:length(games_data_JSON$data), 
             function(x) flatten_single_game(games_data_JSON$data[[x]])
      )
    )
  }
  
  if (nrow(games_data) == 0) {
    # stop("No data found", call. = FALSE)
    NULL
  } else {
    games_data
  }
}
