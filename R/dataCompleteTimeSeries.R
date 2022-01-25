#' Get data about account values for a set of countries used as support for time series graphs
#'
#' @param account string with account name
#' @param exclude_country string indicating the country name that will be excluded from the result dataset (country name in PT)
#' @return data about account values for a set of countries.
#' @examples
#' dataCompleteTimeSeries(account="Despesa")
#' @export


dataCompleteTimeSeries<- function(account="Despesa", exclude_country=""){

  .data<-
    serie_trabalho_full %>%
    dplyr::mutate(ano = as.numeric(ano),
                  num_value = round(num_value,1)) %>%
    dplyr::filter(translation_stn == account) %>%
    dplyr::inner_join(imf_countries %>%
                        filter(!(Continent_Region_Name %in% c("América Latina","Sub-Saara" )))) %>%
    dplyr::arrange(ISO_Code) %>%
    dplyr::group_by(ano, translation_stn, ISO_Code, st_development )%>%
    dplyr::summarise(
      num_value = mean(num_value)
    ) %>%
    dplyr::ungroup()

  print("exclude_country")
  print(exclude_country)

  if (!is.null(exclude_country)){


    .data<-

      .data %>%
      dplyr::inner_join(imf_countries)%>%
      dplyr::filter(!(Country %in% exclude_country))

    print("exclusão de países")
    print(unique(.data$Country))
  }

  .data


}
