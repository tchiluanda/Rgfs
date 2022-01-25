#' Get the development levels used in GFS
#'
#' @return development levels.
#' @examples
#' getDevelopmentLevels()
#' @export

getDevelopmentLevels  <- function(){

  (imf_countries %>%
     dplyr::distinct(st_development) %>%
     dplyr::arrange(st_development))$st_development
}
