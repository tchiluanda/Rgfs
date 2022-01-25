#' Get all the economic zones used in GFS
#'
#' @return economic zones.
#' @examples
#' getEconomicZones()
#' @export


getEconomicZones <- function(){
  (imf_countries %>%
     dplyr::distinct(Economic_Zone) %>%
     dplyr::filter(!is.na(Economic_Zone)) %>%
     dplyr::arrange(Economic_Zone))$Economic_Zone

}
