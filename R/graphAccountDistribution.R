#' Generate a graph showing a the distribution of account values for a set of countries
#'
#' @param .data tibble data about the distribution of account values for a set of countries
#' @return box plot graph.
#' @examples
#' dataAccountDistribution(year=2019) %>% graphAccountDistribution(selected_country="Brasil", text_max = FALSE )
#' @export


graphAccountDistribution<- function(.data, selected_country="Brasil", text_max = FALSE){


  df_filtro_pais<-
    .data %>%
    mutate(translation_stn =  fct_reorder(translation_stn, .fun=max, classification_code, .desc= FALSE))  %>%
    inner_join(imf_countries %>%
                 filter(Country %in% selected_country)) %>%
    filter(!is.na(Continent_Code)) %>%
    distinct(ISO_Code, num_value,translation_stn,Continent_Code)




  df_val_max<-
    .data %>%
    mutate(translation_stn =  fct_reorder(translation_stn, .fun=max, classification_code, .desc= FALSE))%>%
    group_by(translation_stn) %>%
    summarise(
      num_value = max(num_value, na.rm = TRUE)
    ) %>%
    inner_join(.data)%>%
    inner_join(imf_countries)%>%
    filter(!is.na(Continent_Code)) %>%
    distinct(ISO_Code, num_value,translation_stn,Continent_Code)


  print(df_val_max)


  graph<-
    .data %>%
    mutate(translation_stn =  fct_reorder(translation_stn, .fun=max, classification_code, .desc= FALSE)) %>%
    inner_join(imf_countries) %>%
    filter(!is.na(Continent_Code)) %>%
    ggplot(aes(x= translation_stn, y=num_value)) +
    geom_boxplot(color= "gray", outlier.shape = NA)+
    geom_quasirandom(aes(fill= Continent_Code ),pch=21, color="white", alpha= 0.5, size =2)+
    guides(x = guide_axis(n.dodge = 2)) +
    scale_fill_discrete_qualitative() +
    theme(#legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.title = element_text(size=16),
      axis.title=element_text(size=14,face="bold"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()) +
    ylab("(%) do PIB") +
    labs(
      fill = "Continentes"
    )

  if(text_max){
    graph<-
      graph+
      geom_point(data=  df_val_max, aes(fill= Continent_Code ),pch=21, color="white", size = 3 )+
      geom_text_repel(data= df_val_max, aes(label= str_c (ISO_Code,round(num_value),"%",sep = " ")),box.padding = unit(1, "lines"), color = "black", alpha =0.8, size =4)


  }

  if(!is.null(selected_country)){

    graph<-
      graph+
      geom_point(data=  df_filtro_pais, aes(fill= Continent_Code ),pch=21, color="white", size = 4 )+
      geom_text_repel(data= df_filtro_pais, aes(label= str_c (ISO_Code,round(num_value),"%",sep = " ")),box.padding = unit(1, "lines"), color = "black", alpha =0.8, size =4)


  }

  graph


}
