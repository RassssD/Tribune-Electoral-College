library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)

# setup
setwd("G:\My Drive\Work\University\Year 2\Tribune\Electoral Article\Code\R code")
getwd()

# import two data files 
election_data <- read.csv("election_data.csv") %>%
  rename("Year" = "year", "State" = "state")

pop_ev_data <- read.csv("pop_EV.csv") %>%
  rename("EC.Votes" = "Votes") %>%
  mutate(State = toupper(State))


# join the two
final_data <- full_join(election_data, pop_ev_data, by = c("Year", "State"))

write.csv(final_data, paste(getwd(), "\\final_data.csv", sep = ""), row.names = FALSE)
