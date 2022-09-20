library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)

# import data
setwd("G:\My Drive\Work\University\Year 2\Tribune\Electoral Article\Code\R code")
getwd()

# import
election_data_raw <- read.csv("election_data_raw.csv") %>% 
  select(year, state, state_po, candidate, candidatevotes, totalvotes, party_simplified) %>%
  mutate(voteshare = candidatevotes / totalvotes)



# get ranks of each candidate within each state and year
election_data_ranked <- 
  election_data_raw %>%
  group_by(year, state) %>%
  #mutate(winner = if_else(candidatevotes == max(candidatevotes), 1, 0)) %>% 
  
  mutate(statewiderank = order(order(candidatevotes, decreasing = TRUE)))


election_data_ranked %>% filter(!(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) & statewiderank == 1)
# interesting: no party other than democrats or republicans have won a state, ever. 

# simplify a bit: only keep REP/DEM, turn statewiderank into a dummy for winning
election_data_simplified <- 
  election_data_ranked %>%
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  mutate(winner = if_else(statewiderank == 1, 1, 0)) %>%
  select(-statewiderank)


# only contains the top 2
write.csv(election_data_simplified, paste(getwd(), "\\election_data.csv", sep = ""), row.names = FALSE)
  
  
  
  
  
  
  











