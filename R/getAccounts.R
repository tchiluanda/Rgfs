#' Get all the available accounts
#'
#' @return available accounts.
#' @examples
#' getAccounts()
#' @export


getAccounts<- function(){

  (serie_trabalho_full %>%
     dplyr::distinct(classification_code,translation_stn) %>%
     dplyr::arrange(classification_code))$translation_stn

}
