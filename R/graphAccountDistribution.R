#' Generate a graph showing the distribution of account values for a set of countries
#'
#' @param .data tibble data about the distribution of account values for a set of countries
#' @param selected_coutry vector name of a coutry to be highlighted
#' @param text_max logical displays the names of the top countries
#' @return box plot graph.
#' @examples
#' dataAccountDistribution(year=2019) %>% graphAccountDistribution(selected_country="Brasil", text_max = FALSE )
#' @export


graphAccountDistribution<- function(.data, selected_country="Brasil", text_max = FALSE){


  df_filtro_pais<-
    .data %>%
    dplyr::mutate(translation_stn =  forcats::fct_reorder(translation_stn, .fun=max, classification_code, .desc= FALSE))  %>%
    dplyr::inner_join(imf_countries %>%
                        dplyr::filter(Country %in% selected_country)) %>%
    dplyr::filter(!is.na(Continent_Code)) %>%
    dplyr::distinct(ISO_Code, num_value,translation_stn,Continent_Code)




  df_val_max<-
    .data %>%
    dplyr::mutate(translation_stn =  forcats::fct_reorder(translation_stn, .fun=max, classification_code, .desc= FALSE))%>%
    dplyr::group_by(translation_stn) %>%
    dplyr::summarise(
      num_value = max(num_value, na.rm = TRUE)
    ) %>%
    dplyr::inner_join(.data)%>%
    dplyr::inner_join(imf_countries)%>%
    dplyr::filter(!is.na(Continent_Code)) %>%
    dplyr::distinct(ISO_Code, num_value,translation_stn,Continent_Code)


  print(df_val_max)


  graph<-
    .data %>%
    dplyr::mutate(translation_stn =  forcats::fct_reorder(translation_stn, .fun=max, classification_code, .desc= FALSE)) %>%
    dplyr::inner_join(imf_countries) %>%
    dplyr::filter(!is.na(Continent_Code)) %>%
    ggplot2::ggplot(aes(x= translation_stn, y=num_value)) +
    ggplot2::geom_boxplot(color= "gray", outlier.shape = NA)+
    ggbeeswarm::geom_quasirandom(ggplot2::aes(fill= Continent_Code ),pch=21, color="white", alpha= 0.5, size =2)+
    ggplot2::guides(x = guide_axis(n.dodge = 2)) +
    colorspace::scale_fill_discrete_qualitative() +
    ggplot2::theme(#legend.title = element_blank(),
      legend.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size=16),
      axis.title= ggplot2::element_text(size=14,face="bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()) +
    ggplot2::ylab("(%) do PIB") +
    ggplot2::labs(
      fill = "Continentes"
    )

  if(text_max){
    graph<-
      graph+
      ggplot2::geom_point(data=  df_val_max, aes(fill= Continent_Code ),pch=21, color="white", size = 3 )+
      ggrepel::geom_text_repel(data= df_val_max, aes(label= str_c (ISO_Code,round(num_value),"%",sep = " ")),box.padding = unit(1, "lines"), color = "black", alpha =0.8, size =4)


  }

  if(!is.null(selected_country)){

    graph<-
      graph+
      ggplot2::geom_point(data=  df_filtro_pais, aes(fill= Continent_Code ),pch=21, color="white", size = 4 )+
      ggrepel::geom_text_repel(data= df_filtro_pais, aes(label= str_c (ISO_Code,round(num_value),"%",sep = " ")),box.padding = unit(1, "lines"), color = "black", alpha =0.8, size =4)


  }

  graph


}
