library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)
library(zoo)

setwd("G:\My Drive\Work\University\Year 2\Tribune\Electoral Article\Code\R code")
getwd()



# import population data files

pop_data_raw = read.csv('Census_Data.csv')

pop_data_raw_2 = read.csv('Pop_data_all_years.csv', check.names = FALSE)
colnames(pop_data_raw_2)[1] <- "State"


# wrangle census data: filter out years, non-states, and make variables numeric
pop_data <- pop_data_raw %>% filter(Year >= 1960) %>% 
  rename(State = Name, "Population_Census" = "Resident.Population") %>%
  filter(Geography.Type == "State") %>%
  mutate(Population_Census = as.numeric(gsub(",", "", Population_Census))) %>%
  select(State, Year, Population_Census)

# same
pop_data_2 = pop_data_raw_2 %>% 
  pivot_longer(-State, names_to = "Year", values_to = "Population_Yearly") %>%
  mutate(Population_Yearly = as.integer(gsub(",", "", Population_Yearly)), 
         Year = as.integer(Year))



## COMBINE

full_pop_data <- full_join(pop_data_2, pop_data, by=c('State', 'Year')) %>% .[order(.$State, .$Year), ]

pop_data_final <- full_pop_data %>% 
  mutate(Population = ifelse(is.na(Population_Census), Population_Yearly, Population_Census)) %>%
  select(State, Year, Population)


## export

write.csv(pop_data_final, paste(getwd(), "\\fullpopdata.csv", sep = ""), row.names = FALSE)


pop_data_final %>% ggplot(aes(Year, Population)) + 
  geom_line(aes(group = State, color = State)) + 
  geom_text(data = subset(pop_data_final, Year == "2020"), aes(label = State, color = State))



