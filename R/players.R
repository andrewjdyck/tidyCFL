
#' Query for CFL player information
#' @param player_id Required parameter for the CFL player ID.
#' @param type Future parameter for the type of player lookup (ie. general, game data, season summary)
#' @return Returns a tibble with game information
#' @examples \dontrun{
#' cfl_players(138985)
#' }
#' @export
#' @importFrom httr stop_for_status GET content
#' @importFrom dplyr bind_rows tbl_df
cfl_players <- function(player_id=NA, type='meta') {
  if (is.na(player_id)) {
    stop("A cfl central player ID is required", call. = FALSE)
  } else if (tolower(player_id) == 'all') {
    url <- paste0('http://api.cfl.ca/v1', '/players','/?page[size]=50&page[number]=108')
  } else {
    url <- paste0('http://api.cfl.ca/v1', '/players/', player_id, 
                  '?include=seasons,game_by_game,current_team')
  }
  
  url <- tidyCFL.build_url(url)
  players_call <- GET(url)
  stop_for_status(players_call)
  player_data_JSON <- content(players_call)
  
  if (tolower(player_id) == 'all') {
    outdata <- player_data_JSON$data
  } else if(tolower(type) == 'meta') {
    player_data <- player_data_JSON$data[[1]][1:15]
    outdata <- cbind(
      dplyr::bind_rows(player_data[1:12]),
      school = player_data$school$name,
      offence_defence_or_special = player_data$position$offence_defence_or_special,
      position = player_data$position$description,
      team_curr = player_data$team$abbreviation
    )
    # player_seasons <- player_data_JSON$data[[1]]$seasons
    # player_game_data <- player_data_JSON$data[[1]]$game_by_game
    outdata <- dplyr::tbl_df(outdata)
  } else {
    # In the future, season and game detail data will go here
    outdata <- dplyr::tbl_df()
  }
  
  return(outdata)
}
