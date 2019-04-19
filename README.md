
# koffing <a src = "https://rpollution.com"> <img src="man/figures/logo.png" align="right" width = "16%"> </a>

The goal of `koffing` is to assemble R functions to catch (scrape) air
pollution data.

## Installation

You can install `koffing` from github with:

``` r
# install.packages("devtools")
devtools::install_github("atmoschem/koffing")
```

## CETESB scraper

To scrape data from the CETESB qualar system, use the function
`scraper_cetesb()`.

``` r
library(koffing)

koffing::scraper_cetesb(
  parameter = 99, 
  station = 17, 
  start = "01/01/2018", 
  end = "31/01/2018", 
  login = "login", 
  password = "password")
```

If you substitute the values `login` and `password` by your login and
password from the Qualar system, this example will return the hourly NO
concentrations from the Pinheiros station for January 2018.
