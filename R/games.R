
#' Query for CFL game information
#' @param season Required passed parameter for the football season.
#' @param game_id Optional passed parameter for the football game id.
#' @return Returns a tibble with game information
#' @examples \dontrun{
#' cfl_games(season = 2016, game_id = 2280)
#' }
#' @export
#' @importFrom httr stop_for_status GET content
#' @importFrom dplyr bind_rows tbl_df
cfl_games <- function(season = NA, game_id = NA) {
  if (is.na(season)) {
    stop("A season year is required to find game_id data", call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    url <- paste0('http://api.cfl.ca/v1', '/games/',
                  season)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season, 
                  '/game/', game_id)
  }
  
  url <- tidyCFL.build_url(url)
  
  games_call <- httr::GET(url)
  httr::stop_for_status(games_call)
  games_data_JSON <- httr::content(games_call)
  if(length(games_call) == 0) {
    games_data <- dplyr::tbl_df()
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
