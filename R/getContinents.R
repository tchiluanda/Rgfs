#' Get the continents
#'
#' @return continents.
#' @examples
#' getContinents()
#' @export

getContinents <- function(){

  (imf_countries%>%
     dplyr::arrange(Continent_Region_Name) %>%
     dplyr::distinct(Continent_Region_Name))$Continent_Region_Name
}
