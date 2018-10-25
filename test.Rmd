---
title: "Visualizing OMXS30"
output: html_notebook
---
https://github.com/thomasp85/gganimate
https://twitter.com/JustTheSpring/status/1049468485334523906
http://www.nasdaqomxnordic.com/index/historiska_kurser?languageId=3&Instrument=SE0000337842


Installing gganimate with devtools:
```{r}
install.packages('devtools')
devtools::install_github('thomasp85/gganimate')
```


Set working directory and loading libraries:
```{r}
setwd("C:/Users/gsz/Documents/R")
library(readr)
library(tidyverse); 
library(lubridate); 
library(gganimate);
```


Importing CSV into {sample}
```{r}
sample <- read_csv2("OMXS30_16okt.csv", col_names = TRUE)

sample  
  
```


The magic happens:
```{r}
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
animate(b, fps = 40, duration = 15, width = 600, height = 314)  
```

originalen:
  
```{r}

library(tidyverse); 
library(lubridate); 
library(gganimate);

#Fake data
set.seed(4.2)
sample <- tibble(
date = seq.Date(from = ymd(20160101), to = ymd( 20181001), by = "day"),
chg = runif(length(date), min = -10, max = 10),
value = cumsum(chg)
) %>% select(-chg)

#framecount=1004
frame_count <- (max(sample$date) -min( sample$date)) / lubridate::ddays(1)
cycle_length <- 365

#From https://stackoverflow.com/questions/8753531/repeat-data-frame-n-times
# This copies the dataframe frame_count' times, w/ one id number for each set

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
labs(title = 'Year: {date}') +  
guides(size = "none", alpha = "none", color = "none")

#Change fps to 2+ for smoother motion; low fps for quick render
animate(b, fps = 20, duration = 15, width = 100, height = 100)
```

gapminder exempel

```{r}
library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  #facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
```


