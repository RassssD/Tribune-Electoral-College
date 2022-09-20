library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(ggthemes)
library(usmap)
library(directlabels)
library(scales)





setwd("G:\My Drive\Work\University\Year 2\Tribune\Electoral Article\Code\R code")
getwd()

dt <- read.csv("final_data.csv") %>%
  rename("Party" = "party_simplified") %>%
  filter(State != "DISTRICT OF COLUMBIA", Year >= 1976, candidate != "") %>%
  mutate(Won.Votes = winner * EC.Votes,
         Pop.Per.EV = Population / EC.Votes) %>%
  rename(State.Abb = state_po) %>%
  group_by(Year) %>%
  mutate(Pop.Per.EV.Relative = Population * 0.5*sum(EC.Votes) / EC.Votes)


party_colors <- c("blue", "red")
election_years <- unique(dt$Year)


#----------------------------------------------------------------------------------------------------------#
## FIGURE 1

# First simple graph: Popular votes vs EC votes for all years
# NB: Top is popular vote, bottom is electoral vote
dt  %>%
  filter(Year > 1) %>%
  group_by(Year, candidate) %>%
  summarise(Year = Year, 
            Party = unique(Party), 
            EV.Total = sum(Won.Votes),
            EV.Share = EV.Total / sum(EC.Votes),
            Pop.Vote.Total = sum(candidatevotes),
            Pop.Vote.Share = Pop.Vote.Total / sum(totalvotes)) %>%
  mutate(Pres.Winner = if_else(EV.Share > 0.5, 1, 0)) %>% 
  gather(key = "Metric", value = "Values", EV.Share, Pop.Vote.Share) %>%
  filter(EV.Total > 0) %>% unique() %>%
  
  ggplot(aes(y = Metric, x = Values, fill = Party)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.8) + 
  
  #geom_vline(xintercept = 0.5, color = "white", size = 1, alpha = 0.8) +
  
  geom_text(aes(label = sprintf("%s%%", 100*round(Values, 2))), size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  
  scale_x_continuous(limits = c(0,1)) +
  scale_y_discrete(breaks = c("Popular", "")) + 
  
  
  scale_fill_manual(values = party_colors) + 
  theme_economist_white() +
  facet_wrap(. ~ Year, ncol = 4) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size = 14, hjust = 0),
        legend.position="none") + 
  labs(title = "Popular and electoral vote shares in past US elections\n", subtitle = "Percentage of the popular vote (top) and electoral vote (bottom) achieved by each party. \n")

  ggsave("Fig1.png")



#----------------------------------------------------------------------------------------------------------#
## FIGURE 2: Same but line graph instead (needs some more refinement)


dt  %>%
  filter(Year > 1) %>%
  group_by(Year, candidate) %>%
  summarise(Year = Year, 
            Party = unique(Party), 
            EV.Total = sum(Won.Votes),
            EV.Share = EV.Total / sum(EC.Votes),
            Pop.Vote.Total = sum(candidatevotes),
            Pop.Vote.Share = Pop.Vote.Total / sum(totalvotes)) %>%
  mutate(Pres.Winner = if_else(EV.Share > 0.5, 1, 0)) %>% 
  filter(EV.Total > 0, Pres.Winner == 1) %>% unique() %>%
  
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = EV.Share), color = "#000000", size = 1.5) + 
  geom_point(aes(y = EV.Share, color = Party), size = 3) + 
  geom_text(aes(y = EV.Share, label = 100*round(EV.Share, 3)), 
            position = position_nudge(x = 1.5, y = 0.02),
            size = 5) + 
  
  
  geom_line(aes(y = Pop.Vote.Share), alpha = 0.5, size = 1.5, color = "#000000") + 
  geom_point(aes(y = Pop.Vote.Share, color = Party), size = 3) + 
  geom_text(aes(y = Pop.Vote.Share, label = 100*round(Pop.Vote.Share, 3)),
            position = position_nudge(x = 2, y = -0.01),
            size = 5) + 
  
  annotate("text", x = 1993, y = 0.9, label = "Electoral vote share", size = 6) +
  annotate("text", x = 1990, y = 0.62, label = "Popular vote share", size = 6, alpha = 0.5) +
  
  scale_color_manual(values = party_colors) + 
  scale_x_continuous(breaks = election_years, labels = election_years) +
  scale_y_continuous(breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                     labels = c("40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  theme_economist_white() + 
  theme(#axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") + 
  labs(title = "Popular and electoral vote shares in past US elections\n", subtitle = "Evolution over time clearly shows that electoral votes can strongly diverge from popular votes.")

ggsave("Fig2.png")





#----------------------------------------------------------------------------------------------------------#
## FIGURE 3: ECs and population over time

dt %>% filter(winner == 1) %>% 
  unique() %>%
  
  ggplot(aes(x = Population, y = EC.Votes)) + 
  geom_line(aes(color = as.character(Year)), size = 1) + 
  geom_point(aes(color = as.character(Year)), size = 2, alpha = 0.5) + 
  
  scale_x_continuous(limits = c(0,40000000), 
                     labels = label_number(suffix = "M", scale = 1e-6)) +
  scale_y_continuous(limits = c(0,60)) +
    
  
  labs(title = "Electoral Votes and Population since 1976",
       subtitle = "The fall in electoral power experienced by each state is due to population increasing across the US, \nwhile the number of total electoral votes is fixed.") + 
  theme_minimal() + 
  xlab("Population") + ylab("Electoral Votes") + 
  scale_color_discrete(name = "Years") + 
  theme(legend.position = c(0.9, 0.3),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.subtitle = element_text(size = 12, hjust = 0)) +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("Fig3.png")

# clear upwards linear relationship
# seems to be declining in number of electoral votes per population: Curve shifts down



#----------------------------------------------------------------------------------------------------------#
## FIGURE 4: Yearly aggregate evolution in voting power per party
## YEARLY EVOLUTion
dt %>% filter(Year > 1) %>% group_by(Year, Party) %>%
  filter(winner == 1) %>%
  summarise(Year = Year, 
            Population = Population,
            Party = Party,
            Winner = winner,
            Pop.Per.EV = Pop.Per.EV,
            Average.Pop.Per.EV = sum(Pop.Per.EV) / sum(winner),
            Median.Pop.Per.EV = median(Pop.Per.EV)) %>% 
  select(Year, Party, Average.Pop.Per.EV, Median.Pop.Per.EV) %>% unique() %>%
  
  ggplot(aes(x = Year)) + 
  geom_line(aes(y = Average.Pop.Per.EV, color = Party), size = 2) + 
  geom_line(aes(y = Median.Pop.Per.EV, color = Party), linetype = "dashed", size = 1) + 
  
  ylab("Citizens per electoral vote") + 
  
  scale_color_manual(values = party_colors) + 
  theme_economist_white() + 
  
  scale_x_continuous(breaks = election_years, labels = election_years) +
  scale_y_reverse(labels = label_number(suffix = "K", scale = 1e-3)) +
  
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12, hjust = 0),
        legend.position="none") + 
  labs(title = "Evolution of average voting power by party (inverted scale)", subtitle = "\nAcross states won by each party, average (full) and median (dashed) citizens per electoral vote.\n")

  ggsave("Fig4.png")

# three things are clear:
  # firstly confirms what was seen before: On average, states are losing power
  # secondly: Republicans get more powerful states on average and median, in all cases but 2016 and 2000 (the two closest elections)
  # however, democrats have on average/median lost less power (vertical distance between first and last)


#----------------------------------------------------------------------------------------------------------#

## FIGURE 5: SAME FOR RELATIVE POWER THIS IS USELESS< SIMPLY DIVIDES ALL BY THE SAME NUMBEr


#----------------------------------------------------------------------------------------------------------#
## FIGURE 5: Voting power individual evolution

dt %>%
  group_by(Year) %>%
  filter(winner == 1) %>%
  
  ggplot(aes(x = Year, y = log(Pop.Per.EV))) + 
  geom_line(aes(group = State, color = Party)) + 
  geom_point(aes(color = Party)) + 
  theme_economist() +
  geom_dl(aes(label = State.Abb), method = list(dl.combine("last.points")), cex = 0.8)
  facet_grid(Party ~ .)












#----------------------------------------------------------------------------------------------------------#

## without logs
dt %>% group_by(Year, Party) %>%
  filter(winner == 1) %>%
  summarise(Year = Year, 
            Population = Population,
            Party = Party,
            Winner = winner,
            Pop.Per.EV = Pop.Per.EV,
            #Average.Votes = sum(Won.Votes) / sum(winner), 
            Average.Pop.Per.EV = sum(Pop.Per.EV) / sum(winner)) %>% 
  unique() %>% 
  
  
  ggplot(aes(x=Population, Pop.Per.EV)) +
  
  geom_point(aes(color = Party)) +
  geom_hline(aes(yintercept = Average.Pop.Per.EV, color = Party)) +
  
  scale_color_manual(values = party_colors) + 
  theme(legend.position="none") + 
  
  facet_grid(. ~ Year)


## with logs
dt %>% filter(Year > 1) %>% group_by(Year, Party) %>%
  filter(winner == 1) %>%
  summarise(Year = Year, 
            Population = Population,
            Party = Party,
            Winner = winner,
            Pop.Per.EV = Pop.Per.EV,
            Average.Pop.Per.EV = sum(Pop.Per.EV) / sum(winner),
            Median.Pop.Per.EV = median(Pop.Per.EV)) %>% 
  unique() %>% 
  

  ggplot(aes(x=log(Population, 10), log(Pop.Per.EV, 10))) +
  
  geom_point(aes(color = Party)) +
  geom_hline(aes(yintercept = log(Average.Pop.Per.EV, 10), color = Party)) + 
  geom_hline(aes(yintercept = log(Median.Pop.Per.EV, 10), color = Party), linetype = "dashed") + 
  scale_color_manual(values = party_colors) + 
  theme(legend.position="none") + 
  
  
  facet_grid(. ~ Year)
# it is clear that on average, republicans win stronger states than the democrats by a significant amount
# also a clear downwards trend on average



#----------------------------------------------------------------------------------------------------------#

## Proportional representation stuff
dt %>%
  mutate(Prop.Votes = EC.Votes * voteshare) %>% 
  filter(Year > 1) %>%
  group_by(Year, candidate) %>%
  summarise(Year = Year, 
            Party = unique(Party), 
            EV.Total = sum(Won.Votes),
            EV.Share = EV.Total / sum(EC.Votes),
            
            Prop.Vote.Total = sum(Prop.Votes),
            Prop.Vote.Share = Prop.Vote.Total / sum(EC.Votes),
            
            Pop.Vote.Total = sum(candidatevotes),
            Pop.Vote.Share = Pop.Vote.Total / sum(totalvotes)) %>%

  gather(key = "Metric", value = "Values", EV.Share, Prop.Vote.Share, Pop.Vote.Share) %>%
  filter(EV.Total > 0) %>% unique() %>%
  
  ggplot(aes(y = Metric, x = Values, fill = Party)) + 
  geom_bar(stat = "identity", position = "fill", width = 0.8) + 
  
  #geom_vline(xintercept = 0.5, color = "white", size = 1, alpha = 0.8) +
  
  geom_text(aes(label = sprintf("%s%%", 100*round(Values, 3))), size = 4, color = "white", position = position_stack(vjust = 0.5)) +
  
  scale_x_continuous(limits = c(0,1)) +
  scale_y_discrete(breaks = c("Popular", "")) + 
  
  
  scale_fill_manual(values = party_colors) + 
  theme_economist_white() +
  facet_wrap(. ~ Year, ncol = 4) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size = 14, hjust = 0),
        legend.position="none") + 
  labs(title = "Popular and electoral vote shares in past US elections\n", subtitle = "Percentage of the popular vote (middle) and electoral vote (bottom) achieved by each party, with \ntheoretical results based on proportional electoral votes (top). \n")

ggsave("Fig5.png")




