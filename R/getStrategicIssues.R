#' Get all the strategic issues used in GFS
#'
#' @return strategic issues.
#' @examples
#' getStrategicIssues()
#' @export


getStrategicIssues <- function() {
  (imf_countries %>%
     distinct(Strategic_Issue) %>%
     filter(!is.na(Strategic_Issue)) %>%
     arrange(Strategic_Issue))$Strategic_Issue

}
