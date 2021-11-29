#' Generate a graph showing the evolution of account values for selected countries
#'
#' @param .data tibble data about the evolution of account values for selected countries
#' @return dynamic graph the evolution of account values for selected countries.
#' @examples
#' dataAccountEvolutionByCountries(account="Despesa", group = "G20") %>% graphAccountEvolutionByCountries()
#' @export


graphAccountEvolutionByCountries <-  function(.data){

  df_graph<- .data

  ano_ini<- min(df_graph$ano, na.rm = TRUE)
  ano_fim <- max(df_graph$ano, na.rm = TRUE)

  title<- "Governo Geral - "

  if (length(unique(df_graph$translation_stn))==1){
    conta<- unique(df_graph$translation_stn)

    title<- paste(title, conta)


  }

  if (length(unique(df_graph$Country))==1) {

    pais<- unique(df_graph$Country)

    title<- paste(title, pais)


  }


  df_graph <- df_graph[,-2]
  ts_gfs<-
    ts(df_graph[,-c(1:2)],
       start = ano_ini,
       frequency = 1
    )






  dygraphs::dygraph(ts_gfs,main= title, width="100%") %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyAxis(name= 'y', label = "% do PIB",
                     valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}', axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}') %>% #,valueRange = c(y_min,y_max*1.05)
    dygraphs::dyLegend(show = 'always', hideOnMouseOut = TRUE)%>%
    dygraphs::dyOptions(connectSeparatedPoints = TRUE, maxNumberWidth = 30)%>%
    dygraphs::dyOptions( drawGrid = FALSE) %>%
    dygraphs::dyHighlight(highlightCircleSize = 3,
                          highlightSeriesBackgroundAlpha = 0.2,
                          hideOnMouseOut = FALSE,
                          highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dygraphs::dyCSS(textConnection( "
     .dygraph-legend > span.highlight { background-color: #B0B0B0; }
     .dygraph-ylabel {text-align: right;}
  " ))

}
