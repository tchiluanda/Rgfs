#' Generate a graph showing a time series about the evolution of account values for selected countries
#'
#' @param .data tibble data about the evolution of account values for selected countries
#' @return time series graph.
#' @examples
#' dataCompleteTimeSeries(account="Despesa") %>% graphCompleteTimeSeries(selected_country="Brasil", text_max = FALSE )
#' @export


graphCompleteTimeSeries <- function(.data, selected_country="Brasil", text_max = FALSE ){


  account<- unique(.data$translation_stn)

  df_pais<-
    serie_trabalho_full %>%
    dplyr::mutate(ano = as.numeric(ano),
                  num_value = round(num_value,1)) %>%
    dplyr::filter(translation_stn == account) %>%
    dplyr::inner_join(imf_countries %>%
                        filter(Country == selected_country)) %>%
    dplyr::group_by(ano, Country )%>%
    dplyr::summarise(
      num_value = mean(num_value)
    ) %>%
    dplyr::ungroup()


  df_min_max_med <-
    .data %>%
    dplyr::group_by(st_development, ano) %>%
    dplyr::summarise(min_val = min(num_value, na.rm = TRUE),
                     max_val = max(num_value, na.rm = TRUE),
                     med_val = median(num_value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  df_country_max<-
    .data %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise (
      num_value = max(num_value)
    ) %>%
    dplyr::inner_join(.data) %>%
    dplyr::ungroup()




  graph <-
    .data %>%
    dplyr::inner_join(df_min_max_med) %>%
    dplyr::mutate(nivel = case_when(
      st_development =="Baixo desenvolvimento" ~ 0,
      st_development =="Emergente" ~ 1,
      st_development =="Avançado" ~ 2,
    )) %>%
    dplyr::mutate(st_development = fct_reorder(st_development, nivel, .desc= TRUE)) %>%
    ggplot2::ggplot( aes(x=factor(ano),y= num_value, color= st_development)) +
    ggplot2::geom_pointrange(aes(ymin = min_val, ymax=max_val, fill = st_development, group = ano))+
    ggplot2::geom_line(aes(y= med_val,group = st_development), size = 1)+
    ggplot2::geom_line(data= df_pais,aes( group= Country, color= Country, fill= Country), size = 1)+
    ggplot2::geom_point(data= df_pais,aes( group= Country, color= Country, fill= Country), size = 3, shape=23, alpha=0.5) +
    colorspace::scale_fill_discrete_qualitative() +
    colorspace::scale_color_discrete_qualitative() +
    ggplot2::theme(legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   plot.title = element_text(size=16),
                   axis.title=element_text(size=14,face="bold"),
                   #axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   #axis.title.y = element_blank(),
                   panel.grid = element_line(),
                   panel.background = element_blank(),
                   panel.border = element_blank()) +
    ggplot2::ylab("(%) do PIB") +
    ggplot2::labs(title =  paste0("Série temporal - ",account),
                  subtitle = "Linhas representam mediana anual dos valores por estágio de desenvolvimento e valor observado no país selecionado")

  if (text_max){
    graph <- graph +
      ggplot2::geom_point(data=  df_country_max, aes(fill=st_development),pch=21, color="white", size = 4 )+
      ggrepel::geom_text_repel(data= df_country_max, aes(label= str_c (ISO_Code,round(num_value),"%",sep = " ")),box.padding = unit(1, "lines"), color = "black", alpha =0.8, size =4)
  }

  graph


}
