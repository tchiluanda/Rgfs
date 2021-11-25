#' Get all the economic zones used in GFS
#'
#' @return economic zones.
#' @examples
#' getEconomicZones()
#' @export


getEconomicZones <- function(){
  (imf_countries %>%
     distinct(Economic_Zone) %>%
     filter(!is.na(Economic_Zone)) %>%
     arrange(Economic_Zone))$Economic_Zone

}
