#' Generate a graph showing a heat map about the evolution of account values for selected countries
#'
#' @param .data tibble data about the evolution of account values for selected countries
#' @return heat map.
#' @examples
#' dataHeatMap(account="Despesa") %>% graphHeatMap()
#' @export


graphHeatMap  <- function(.data, grouping="Estágio de desenvolvimento"){

  #df_graph_chart3<- .data

  mediana<<- median(.data$num_value, na.rm = TRUE)
  ano_min<- min(.data$ano)
  ano_max<- max(.data$ano)

  print(mediana)




  heat_map<-
    .data %>%
    mutate(nivel = case_when(
      st_development =="Baixo desenvolvimento" ~ 0,
      st_development =="Emergente" ~ 1,
      st_development =="Avançado" ~ 2,
    )) %>%
    mutate(st_development = fct_reorder(st_development, nivel, .desc= TRUE)) %>%
    ggplot( aes(x=factor(ano),y= fct_reorder(ISO_Code, num_value,.fun = mean,.desc= FALSE))) +
    geom_tile(aes(fill = num_value), color = "#333333")+
    #geom_text(aes(label = num_value), size= 2.5)+
    scale_x_discrete(limits = factor(seq(ano_min,ano_max,1)) ) +
    #scale_fill_gradient2(low = "steelblue", high = "red", midpoint = mediana) + # "steelblue"
    #scale_fill_continuous_diverging (palette= "blue-yellow 3", mid =mediana, l2=50) +
    scale_fill_continuous_divergingx(palette= "PuOr", mid= mediana, p3=.2, p4=.6, l3=60, rev = TRUE)+ #
    guides(y = guide_axis(n.dodge = 2), colour=FALSE) +
    ylab("Países ") +
    xlab("Ano") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 12),
          plot.title = element_text(size=16),
          axis.title=element_text(size=14,face="bold"),
          #axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "#333333")) +
    labs(fill = "(%) do PIB")

  if(grouping == "Estágio de desenvolvimento"){
    heat_map<-
      heat_map +
      facet_grid(st_development~., scales="free_y", space = "free_y")

  } else if(grouping == "Continente") {
    heat_map<-
      heat_map +
      facet_grid(Continent_Code~., scales="free_y", space = "free_y")
  }

  heat_map


}
