% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphHeatMap.R
\name{graphHeatMap}
\alias{graphHeatMap}
\title{Generate a graph showing a heat map about the evolution of account values for selected countries}
\usage{
graphHeatMap(.data, grouping = "Estágio de desenvolvimento")
}
\arguments{
\item{.data}{tibble data about the evolution of account values for selected countries}

\item{grouping}{char grouping option c("Estágio de desenvolvimento","Continente")}
}
\value{
heat map.
}
\description{
Generate a graph showing a heat map about the evolution of account values for selected countries
}
\examples{
dataHeatMap(account="Despesa") \%>\% graphHeatMap()
}
