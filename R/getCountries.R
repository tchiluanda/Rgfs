#' Get all the available countries
#'
#' @return available countries.
#' @examples
#' getCountries()
#' @export


getCountries <- function(){
  (imf_countries %>%
     distinct(Country) %>%
     arrange(Country))$Country
}
