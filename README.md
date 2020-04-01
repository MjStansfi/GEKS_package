<!-- README.md is generated from README.Rmd. Please edit that file -->
GEKS
====

This package is a modification of the [FEWS
package](https://github.com/MjStansfi/FEWS_package). Where the
individual fixed effects calculation occurs, it has been replaced by the
GEKS calculation from [IndexNumR: A Package for Index Number
Calculation](https://cran.r-project.org/web/packages/IndexNumR/vignettes/indexnumr.html#the-geks-method).

-   To see the original FEWS package written by Donal Lynch [go
    here](https://github.com/Donal-lynch/FEWS_package)

-   To see the complete IndexNumR package written by Graham White [go
    here](https://github.com/grahamjwhite/IndexNumR)

Installation
============

GEKS is still in development. For now it can be installed from GitHub
using the following code

``` r

devtools::install_github("MjStansfi/GEKS_package")
devtools::install_github("grahamjwhite/IndexNumR")

#Other suggestions
# devtools::install_github("MjStansfi/FEWS_package")
# devtools::install_github("MjStansfi/TPDdecomp")

# Once installed, the package can be loaded as usual
library(GEKS)
library(IndexNumR)
```

Usage
=====

The primary function provided by the GEKS package is the `GEKS()`
function. Running `?GEKS()` should give all the required information on
how using the function. An example of running the `GEKS()` the function
is shown below.

Example
-------

As part of the package, a couple of datasets are provided, including the
Turvery dataset as found in the [Consumer Price Index
Manual](https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/presentation/wcms_331153.pdf).

``` r
ggplot(turvey, aes(x = month, y = price)) + 
  geom_line(aes(color = commodity)) + 
  geom_point(aes(color = commodity))+
  theme_bw() +
  ggtitle("Artificial prices of seasonal products Data created by R. Turvey")
```

![](README-data_viz-1.png)

The GEKS is calculated below with a mean splice and a window length of
13 months.

``` r
turvey_GEKS <- GEKS(times = turvey$month,
                    price = turvey$price,
                    id = turvey$commodity,
                    window_length = 13,
                    weight = turvey$price * turvey$quantity,
                    splice_pos = "mean",
                    index_method = "tornqvist",
                    num_cores = NULL)
```

The resulting index is displayed below

``` r

ggplot(turvey_GEKS$geks, aes(x = price_date, y = fe_indexes)) + 
  geom_line() + 
  theme_bw() +
  ggtitle("GEKS with mean splice for Turvey data")+
  ylab("Price Index") + 
  xlab("Date")
```

![](README-geks_result-1.png)

We can compare the FEWS and GEKS as follows

``` r
turvey_FEWS <-FEWS(times = turvey$month,
                    logprice = log(turvey$price),
                    id = turvey$commodity,
                    window_length = 13,
                    weight = turvey$price * turvey$quantity,
                    splice_pos = "mean",
                    num_cores = NULL)
```

The resulting index is displayed below

``` r
ggplot(turvey_FEWS$fews, aes(x = price_date, y = fe_indexes, colour = "FEWS")) +
  geom_line() +
  geom_line(aes(x =turvey_GEKS$geks$price_date, y= turvey_GEKS$geks$fe_indexes, colour = "GEKS-tornqvist"))+
  theme_bw() +
  ggtitle("Different multilateral, with mean splice for Turvey data")+
  ylab("Price Index") +
  xlab("Date")
```

![](README-geks_fews_plot-1.png)
