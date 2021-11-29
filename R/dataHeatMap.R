#' Get data about account values for a set of countries used as support for heat map graphs
#'
#' @param account string with account name
#' @param exclude_country string indicating the country name that will be excluded from the result dataset (country name in PT)
#' @return data about account values for a set of countries.
#' @examples
#' dataHeatMap(account="Despesa")
#' @export


dataHeatMap<- function(account="Despesa", exclude_country=""){


  .data<- serie_trabalho_full %>%
    dplyr::mutate(ano = as.numeric(ano),
                  num_value = round(num_value,1)) %>%
    dplyr::filter(translation_stn == account) %>%
    dplyr::inner_join(imf_countries %>%
                        filter(!(Country %in% exclude_country),
                               !(Continent_Region_Name %in% c("América Latina","Sub-Saara" )))) %>%
    dplyr::arrange(ISO_Code) %>%
    dplyr::select(ano, translation_stn, ISO_Code, num_value, st_development, Continent_Code)

  .data


}
