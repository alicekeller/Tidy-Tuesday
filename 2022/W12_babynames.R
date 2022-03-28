library(tidyverse)
library(tidytuesdayR)
library(showtext)

#load data and look at format
tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames
head(babynames)

#load fonts
font_add(family = "GW Friends", regular = "GABRWFFR.TTF")
font_add_google("Galdeano", family = "galdeano")
font_add_google("Bangers", family = "bangers")
showtext_auto()

#create vector with friends' names 
friends_names <- c('Phoebe', 'Ross', 'Rachel', 'Joey', 'Chandler', 'Monica')

#get total proportion of each name by year since 1880
the_friends <- babynames %>% 
  filter(name %in% friends_names) %>% 
  group_by(year, name) %>% 
  summarise(total_prop = sum(prop))

#create plot
the_friends %>% 
  ggplot(aes(year, total_prop)) +
  geom_area(aes(fill = the_friends$name)) +
  facet_wrap(~name, ncol = 2) +
  scale_y_continuous(labels = scales::percent, breaks = c(0.001, 0.002, 0.004)) +
  scale_x_continuous((limits = c(1885, 2020))) +
  theme_minimal() +
  scale_fill_manual(values=c("#FF4238", "#FFDC00", "#42A2D6", "#9A0006", "#fff580", "#00009E")) +
  labs(title = "THE ONE WITH ALL THE BABY NAMES",
       subtitle = "Popularity of the Friends names over time \nby percentage of total babies named",
       caption = "Graphic: @alicemkeller | Data: {babynames} R package") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        axis.title = element_blank(),
        strip.text = element_text(hjust = 0, color = "white"),
        text = element_text(size = 20, family = "galdeano", color = "white"),
        plot.title = element_text(hjust = 0.3, size = 40, family = "GW Friends"),
        plot.subtitle = element_text(hjust = 0.4, size = 22, family = "GW Friends"),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        plot.margin = margin(t = 40),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        strip.placement = "inside",
        plot.caption = element_text(size = 14, color = "grey"))

