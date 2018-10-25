Visualizing OMXS30
================

Sources:
========

<https://github.com/thomasp85/gganimate> <https://twitter.com/JustTheSpring/status/1049468485334523906> <http://www.nasdaqomxnordic.com/index/historiska_kurser?languageId=3&Instrument=SE0000337842>

Installing gganimate with devtools:

``` r
#install.packages('devtools')
#devtools::install_github('thomasp85/gganimate')
```

Set working directory and loading libraries:

``` r
setwd("C:/Users/gsz/Documents/R")
library(readr)
library(tidyverse); 
```

    ## -- Attaching packages ----------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v ggplot2 3.0.0     v forcats 0.3.0

    ## -- Conflicts -------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate); 
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(gganimate);
```

Importing CSV into {sample}

``` r
sample <- read_csv2("OMXS30_16okt.csv", col_names = TRUE)
```

    ## Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   value = col_integer()
    ## )

``` r
sample  
```

    ## # A tibble: 1,020 x 2
    ##    date       value
    ##    <date>     <int>
    ##  1 2016-01-01  1394
    ##  2 2016-01-02  1394
    ##  3 2016-01-03  1394
    ##  4 2016-01-04  1394
    ##  5 2016-01-05  1387
    ##  6 2016-01-06  1387
    ##  7 2016-01-07  1357
    ##  8 2016-01-08  1349
    ##  9 2016-01-09  1349
    ## 10 2016-01-10  1349
    ## # ... with 1,010 more rows

The magic happens:

``` r
frame_count <- (max(sample$date) -min( sample$date)) / lubridate::ddays(1)
cycle_length <- 365

sample3 <- map_df(seq_len(frame_count), ~sample, .id ="id") %>%
mutate(id = as.integer(id)) %>%
#view date is the "camera" date. We'll only keep frames up to the camera date.
mutate (view_date = min(sample$date) + id -1) %>%
filter(date <= view_date) %>%

mutate(days_ago  = (view_date-date)/ ddays(1),
       phase_dif = (days_ago %% cycle_length) / cycle_length,
       x_pos = -sin(2*pi*phase_dif),
       nearness = cos(2*pi*phase_dif))

b <- ggplot(sample3) +
geom_path(aes(x_pos, value, alpha = nearness,
              color = days_ago, size = -days_ago)) +
scale_size(range = c(0,2)) +
#gganimate using 'id' to define the frame
transition_manual(id) + theme_void()+
guides(size = "none", alpha = "none", color = "none")

#Change fps to 2+ for smoother motion; low fps for quick render
animate(b, fps = 40, duration = 15, width = 300, height = 150)  
```

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](omxs30_files/figure-markdown_github/unnamed-chunk-4-1.gif)
