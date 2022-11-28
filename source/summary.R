library(dplyr)
library(tidyverse)
library(stringr)
library(maps)

jail_info<- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

number_obs <- nrow(jail_info)
number_features <- ncol(jail_info)


jail_info <- df %>% select(year, county_name,state, black_jail_pop, white_jail_pop, fips)

black_white_jail_max <-jail_info %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  summarise(black_jail_pop, white_jail_pop)

black_white_jail_min <- jail_info %>% 
  filter(black_jail_pop == min(black_jail_pop, na.rm = TRUE)) %>% 
  summarize(black_jail_pop, white_jail_pop, state)

black_white_jail_total <- jail_info  %>% 
  filter(state == "MD") %>% 
  filter(year == max(year)) %>% 
  mutate(total_black = sum(black_jail_pop, na.rm = TRUE)) %>% 
  mutate(total_white = sum(white_jail_pop, na.rm = TRUE)) %>% 
  select(total_black, total_white) 
head(black_white_jail_total, 1)

black_white_jail_ave <- jail_info %>%
  filter(state == "GA") %>% 
  filter(year == max(year)) %>% 
  mutate(total_black = sum(black_jail_pop, na.rm = TRUE)) %>% 
  mutate(total_white = sum(white_jail_pop, na.rm = TRUE)) %>% 
  summarize(total_black, total_white) 
head(black_white_jail_ave, 1)

black_white_jail_GA_max <- jail_info  %>% 
  filter(state == "GA") %>% 
  filter(year == max(year)) %>% 
  filter(black_jail_pop_rate == max(black_jail_pop, na.rm = TRUE)) %>% 
  summarise(black_jail_pop_rate, white_jail_pop)

