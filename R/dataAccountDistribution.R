#' Get data about a set of countries used as support for distribution graphs
#'
#' @param year number year to compare countries
#' @param exclude_country string indicating the country name that will be excluded from the result dataset (country name in PT)
#' @return data about a set of countries.
#' @examples
#' dataAccountDistribution(year=2019)
#' @export


dataAccountDistribution<- function(year=2019,exclude_country=""){

  .data<-serie_trabalho_full %>%
    dplyr::filter(ano == year,
                  stringr::str_sub(classification_code,1,2)%in% c("G1","G2"),
           !classification_code %in% c("G2M|_Z","G2G|_Z"))

  if (!is.null(exclude_country)){
    .data<-
      .data %>%
      dplyr::inner_join(imf_countries)%>%
      dplyr::filter(!(Country %in% exclude_country))
  }
  .data
}
