library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)


# import relevant files
EV_data <- read.csv("pop_EV.csv")

census_data <- read.csv("Census_Data.csv") %>% 
  filter(Geography.Type == "State") %>%
  select(Year, Name, Resident.Population.Density) %>%
  rename("State" = "Name", "Density" = "Resident.Population.Density")


## join them on density
ev_density <- inner_join(census_data, EV_data, by = c("State", "Year")) %>%
  mutate(Density = as.numeric(gsub(",", "", Density))) %>%
  mutate(Pop.Per.Vote.Per.Density = Pop.Per.Vote / Density) %>% 
  
  # get share of votes
  group_by(Year) %>%
  mutate(Vote.Share = Votes / sum(Votes)) %>%
  mutate(Vote.Share.Per.Pop = Vote.Share / Population)





year_choice = 1980

## PLOTS
p1 <- ev_density %>% filter(Year == year_choice) %>%
  ggplot() + 
  geom_point(aes(Population, Pop.Per.Vote))
p2 <- ev_density %>% filter(Year == year_choice) %>%
  ggplot() + 
  geom_point(aes(Population, log(Pop.Per.Vote.Per.Density)))

cowplot::plot_grid(p1, p2)

## raw votes
# voting power (inverse of) vs population: decreasing in population
ev_density %>% ggplot() + 
  geom_point(aes(log(Population), Pop.Per.Vote, size = Votes)) + 
  facet_grid(. ~ Year)

# inverse voting power normalised by density, vs population: rural states get relatively weaker when normalising by density
ev_density %>% filter(State != "Alaska") %>% ggplot() + 
  geom_point(aes(log(Population), Pop.Per.Vote.Per.Density, size = Votes, color = Pop.Per.Vote)) + 
  facet_grid(. ~ Year)


## vote share
# voting share vs population: higher states have higher absolute power
ev_density %>% ggplot() + 
  geom_point(aes(log(Population), Vote.Share, size = Votes)) + 
  facet_grid(. ~ Year)

# voting share normalised by population vs population: 
ev_density %>% ggplot() + 
  geom_point(aes(log(Population), Vote.Share.Per.Pop, size = Votes)) + 
  facet_grid(. ~ Year)


## 



ev_density %>% filter(Year == year_choice) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(State, Pop.Per.Vote), y = Pop.Per.Vote, fill = Population), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ev_density %>% filter(Year == year_choice) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(State, Pop.Per.Vote.Per.Density), y = Pop.Per.Vote.Per.Density, fill = Pop.Per.Vote), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
