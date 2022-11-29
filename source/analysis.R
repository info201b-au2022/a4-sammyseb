library("tidyverse")
library("scales")
library("ggplot2")
library("maps")
library("mapproj")
library("patchwork")
# The functions might be useful for A4
source("../source/a4-helpers.R")

jail_info <- read.csv ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
get_year_jail_pop <- function() {
  view_jail <- jail_info %>% 
    select(year, total_jail_pop)%>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarize(jail_all = sum(total_jail_pop)) %>% 
  return(view_jail)
} 

plot_jail_pop_for_us <- function() { 
  data_jail <- get_year_jail_pop()
  p <- ggplot(data = data_jail) +
    geom_col(mapping = aes(x = year, y= jail_all)) +
    labs(x= "year", y= "Amount of people in Jail")
  
  return(p)
}  
results <- plot_jail_pop_for_us () 

#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
#get_year_jail_pop <- function() {
  # TODO: Implement this function 
#return()   
#}

# This function ... <todo:  update comment>
#plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
 # return()   
#} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
states <- c("WA", "OR", "CA")
  get_jail_pop_by_states <- function(states) { 
    states <- c("WA", "OR", "CA")
    Target <- jail_info %>%
      filter(state %in% states)%>%
      select(total_jail_pop, year, state) %>%
      drop_na() %>% 


    return  (data.frame(Target))
  }
  
plot_jail_pop_by_states <- function (states) {
  get_jail <- get_jail_pop_by_states (states)
  p <- ggplot(data = get_jail) +
  geom_col(mapping = aes(x = state , y=total_jail_pop)) +
  labs(x= "year", y = "Amount of people in jail") +
  ggtitle(" Growth of prison population in Washington from 1970-2018")
  return(p)
}
  task <- plot_jail_pop_by_states()
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

inequality_data <-  function () {
 results <-  jail_info %>% 
    group_by(year) %>% 
    summarize(black_all = sum(black_jail_pop, na.rm = TRUE), white_all = sum(white_jail_pop, na.rm = TRUE)) %>% 
    return(results)
}

  
Over_time <- function (){
  injust_rate <- inequality_data()
 p <-  ggplot(data =  injust_rate) +
    geom_line(mapping = aes(x = year, y= black_all, color = "red")) +
    geom_line(mapping = aes(x = year, y= white_all, color = "blue")) +
    scale_color_manual(name = "race",
                       values = c("red", "blue"),
                       labels = c("black", "white")) +
    labs(x= "year", y= "Amount of people in Jail") +                 
    ggtitle("Incarceration Comparison Chart between White & Black")
    return(p)
}

determine <- Over_time()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
TX_Map <- proj %>% 
  filter(year == max(year))

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>% 
  left_join(county.fips, by = "polyname")

# map_data <- county_shapes %>% 
#  left_join(TX_Map, by ="fips") %>% 
#   filter(state == "TX")

#clean map

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank() 
  )

#create map
cases_Map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x= long, y= lat, group = group, fill = black_jail_pop),
    color="gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop_rate)), na.value= "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Black Jail Population in Alabama")

#----------------------------------------------------------------------------#

## Load data frame ---- 


