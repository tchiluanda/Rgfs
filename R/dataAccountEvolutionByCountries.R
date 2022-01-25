#' Get data about the evolution of an account values for selected countries
#'
#' @param account string with account name
#' @param continent string with continent name
#' @param group string with economic group
#' @param level string with development level
#' @param country string with country name in PT
#' @param zone string with economic zone
#' @param strategy string with strategic issue
#' @return data about the evolution of an account values for selected countries.
#' @examples
#' dataAccountEvolutionByCountries(account="Despesa", group = "G20")
#' @export


dataAccountEvolutionByCountries <-  function( account="Despesa", continent ="", group ="", level ="", country ="", zone= "", strategy =""){

  inf_countries_join<-
    imf_countries %>%
    dplyr::mutate(country_code = as.numeric(country_code)) %>%
    dplyr::filter(Continent_Region_Name %in% continent|
                    Economic_Group %in% group|
                    st_development %in% level|
                    Country %in% country|
                    Economic_Zone %in% zone|
                    Strategic_Issue %in% strategy)

  #print(inf_countries_join)

  df_graph<-
    serie_trabalho_full%>%
    dplyr::mutate(ano = as.numeric(ano),
                  num_value = round(num_value,1)) %>%
    dplyr::filter(translation_stn %in% account) %>%
    dplyr::inner_join(inf_countries_join)


  if (NROW(account)>1){

    print("Por conta")
    df_graph <-
      df_graph %>%
      dplyr::arrange(translation_stn) %>%
      dplyr::mutate(tipo= "Por conta") %>%
      dplyr::select(ano,tipo, translation_stn, Country, num_value) %>%
      tidyr::pivot_wider(names_from = translation_stn, values_from =  num_value,values_fn = mean) %>%
      dplyr::arrange(ano)


  } else{

    print("Por país")
    df_graph <-
      df_graph%>%
      dplyr::arrange(ISO_Code) %>%
      dplyr::mutate(tipo= "Por país") %>%
      dplyr::select(ano, tipo, translation_stn, ISO_Code, num_value) %>%
      tidyr::pivot_wider(names_from = ISO_Code, values_from =  num_value,values_fn = mean) %>%
      dplyr::arrange(ano)


  }



  #print(df_graph)
  ano_ini<- min(df_graph$ano, na.rm = TRUE)
  ano_fim <- max(df_graph$ano, na.rm = TRUE)


  df_ano <- tibble::tibble(ano=seq(ano_ini, ano_fim,1))

  df_graph<-
    df_ano %>%
    dplyr::left_join(df_graph)

  df_graph

}
