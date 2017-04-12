
#' Query or set CFL API key
#' @param api_key Optionally passed parameter to set CFL \code{api_key}.
#' @return Returns invisibly the currently set \code{api_key}.
#' @examples \dontrun{
#' tidyCFL.api_key('foobar')
#' }
#' @export
tidyCFL.api_key <- function(api_key) {
  if (!missing(api_key)) {
    options(Quandl.api_key = api_key)
  }
  invisible(getOption('tidyCFL.api_key'))
}


tidyCFL.api_version <- function(api_version) {
  if (!missing(api_version)) {
    options(tidyCFL.api_version = api_version)
  }
  invisible(getOption('tidyCFL.api_version'))
}

tidyCFL.base_url <- function(base_url) {
  if (!missing(base_url)) {
    options(tidyCFL.base_url = base_url)
  }
  invisible(getOption('tidyCFL.base_url', 'http://api.cfl.ca/api/v1'))
}