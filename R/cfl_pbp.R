
#' Query for CFL Play-by_Play information
#' @param season Required parameter for the football season.
#' @param game_id Required parameter for the football game id.
#' @return Returns a tibble with play information for a game
#' @examples \dontrun{
#' cfl_plays(season = 2016, game_id = 2280)
#' }
#' @export
#' @importFrom httr stop_for_status GET content
#' @importFrom dplyr bind_rows tbl_df
cfl_plays <- function(season = NA, game_id = NA) {
  if (is.na(season)) {
    stop("A season year is required", call. = FALSE)
  } else if (!is.na(season) && is.na(game_id)) {
    stop('A game_id is required', call. = FALSE)
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/games/', season, 
                  '/game/', game_id, '?include=play_by_play')
  }
  
  url <- tidyCFL.build_url(url)
  games_call <- GET(url)
  stop_for_status(games_call)
  games_data_JSON <- content(games_call)
  game_data <- extract_game_data_for_pbp(games_data_JSON$data[[1]])
  
  pbp_JSON <- games_data_JSON$data[[1]]$play_by_play
  
  if(length(games_call) == 0) {
    pbp_data <- dplyr::tbl_df()
  } else {
    pbp_data <- dplyr::bind_rows(
      lapply(1:length(pbp_JSON),
             function(x) flatten_play_by_play(pbp_JSON[[x]])))
  }
  
  if (nrow(pbp_data) == 0) {
    stop("No data found", call. = FALSE)
    NULL
  } else {
    dplyr::tbl_df(cbind(game_data, pbp_data))
  }
}
