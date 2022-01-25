#' Get all the economic groups used in GFS
#'
#' @return economic groups.
#' @examples
#' getEconomicGroups()
#' @export


getEconomicGroups <- function(){
  (imf_countries %>%
     dplyr::distinct(Economic_Group) %>%
     dplyr::filter(!is.na(Economic_Group)) %>%
     dplyr::arrange(Economic_Group))$Economic_Group


}
