library(tidyverse)
library(tidytuesdayR)
library(maps)
library(mapdata)
library(ggmap)
library(usmap)
library(showtext)

#adding new fonts for plot
font_add_google("Smooch Sans", family = "smooch")
font_add_google("Permanent Marker", family = "permanent")
showtext_auto()

#load data
tuesdata <- tidytuesdayR::tt_load('2022-03-01')
stations <- tuesdata$stations

#exploring - plot something with electric charging stations?
stations %>%
  count(FUEL_TYPE_CODE)

stations%>%
  ggplot() +
  geom_bar(aes(x = FUEL_TYPE_CODE))

#use usmap package for map data as has alaksa and hawaii included
usa_ha <- us_map(regions = "states")
head(usa_ha)

#make plot of usa with states and outline in white
us_plot <- usa_ha %>%
  ggplot(aes(x = x, y = y, group = group)) +
  coord_fixed(1) +
  geom_polygon(color = "white", fill = "gray")
us_plot

#create smaller data frame with only electric stations and count per state
elec <- stations %>%
  filter(FUEL_TYPE_CODE == "ELEC") %>%
  group_by(STATE) %>%
  summarise(count = n())

#rename abbr column to state so can inner join
usa_ha <- usa_ha %>%
  rename(STATE = abbr)

#inner join on state key
elec_usa <- inner_join(
  usa_ha,
  elec,
  by = "STATE"
)

#rename count column to electric stations
elec_usa <- elec_usa %>%
  rename(e_stations = count)

#make ggplot theme with legend but no axes or ticks
clean_theme <- theme(
  axis.text = element_blank(), 
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

#plot number of electric stations per state with gradient
elec_plot1 <- us_plot +
  geom_polygon(data = elec_usa, aes(fill = e_stations)) +
  geom_polygon(color = "white", fill = NA) +
  theme_bw() +
  clean_theme + 
  labs(fill = "Number of \nelectric stations")
elec_plot1

#need to log transform to get better gradient
#took log base 10 of electric stations per state
#instead of new column, just applied the transformation within gradient
elec_plot2 <- elec_plot1 + scale_fill_gradient(low = "blue", high = "green3",
                                               trans = "log10")

elec_plot2 + labs(title = "It's Electric!",
                  subtitle = "Electric automobile charging stations across the US",
                  caption = "Plot: @alicemkeller | Data: US DOT") + 
  theme(plot.title = element_text(hjust = 0.7, vjust = 2, size = 68, family = "permanent", colour = "green3"),
        plot.subtitle = element_text(hjust = 0.9, vjust = 1.5, size = 30),
        plot.caption = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22))
