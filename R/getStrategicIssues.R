#' Get all the strategic issues used in GFS
#'
#' @return strategic issues.
#' @examples
#' getStrategicIssues()
#' @export


getStrategicIssues <- function() {
  (imf_countries %>%
     dplyr::distinct(Strategic_Issue) %>%
     dplyr::filter(!is.na(Strategic_Issue)) %>%
     dplyr::arrange(Strategic_Issue))$Strategic_Issue

}
