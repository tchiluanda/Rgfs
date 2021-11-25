#' Get the development levels used in GFS
#'
#' @return development levels.
#' @examples
#' getDevelopmentLevels()
#' @export

getDevelopmentLevels  <- function(){

  (imf_countries %>%
     distinct(st_development) %>%
     arrange(st_development))$st_development
}
