library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)
library(zoo)
library(usmap)

setwd("G:\My Drive\Work\University\Year 2\Tribune\Electoral Article\Code\R code")
getwd()

# import data
ev_data = read.csv('Electoral_College.csv') %>% filter(Year >= 1960)
pop_data = read.csv('fullpopdata.csv')




### COMBINE THE TWO

ev_pop_data = inner_join(ev_data, pop_data, by=c('State', 'Year')) %>% 
  .[order(.$State, .$Year), ]

# export
write.csv(ev_pop_data, paste(getwd(), "\\pop_EV.csv", sep = ""), row.names = FALSE)

