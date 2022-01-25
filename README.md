
# Rgfs

<!-- badges: start -->
<!-- badges: end -->

The goal of Rgfs is to retrieve Financial Statistics of General Governments

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tchiluanda/Rgfs")
```

## Example

Here you can see how to draw a time series graphic with GFS data:

``` r
library(Rgfs)
dataCompleteTimeSeries(account="Despesa") %>% graphCompleteTimeSeries(selected_country="Brasil", text_max = FALSE )
```

