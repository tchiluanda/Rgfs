#' Get all the economic groups used in GFS
#'
#' @return economic groups.
#' @examples
#' getEconomicGroups()
#' @export


getEconomicGroups <- function(){
  (imf_countries %>%
     distinct(Economic_Group) %>%
     filter(!is.na(Economic_Group)) %>%
     arrange(Economic_Group))$Economic_Group


}
