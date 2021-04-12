library(tidyverse)
library(ggplot2)
library(maps)
library(gganimate)
library(transformr)
library(scales)
library(glue)
library(sqldf)


df <- read_csv('country_data.csv')

df2 <- df %>% 
  mutate(fill=case_when(
    regime == 3 ~ 1,
    regime == 4 ~ 2,
    regime == 5 ~ 3,
    TRUE ~ 4)) %>% 
  select(ctryname, year, fill, un_continent_name, regime)

#df2 <- df %>% 
#  mutate(fill=if (regime==3) {
#    1
#  } else if (regime==4) {
#    2
#  } else if (regime==4) {
#    3
#  } else {0}) %>% 
#  select(ctryname, year, fill)

color_vector <- c("coral1", "darkgoldenrod1", "blueviolet", "cadetblue1", "darkgray")


world_map <- map_data('world')
world_map$region[world_map$region == "Greenland"] <- "Denmark"


include_countries <- c('Argentina', 'Bolivia', 'Brazil', 'Chile', 'Colombia', 
                      'Ecuador', 'French Guiana', 'Guyana', 'Paraguay', 
                       'Peru', 'Suriname', 'Uruguay', 'Venezuela')
  
map_df <- crossing(world_map %>% distinct(region) %>% filter(region %in% include_countries),
                  df %>% distinct(year) %>% filter(!is.na(year), year<1965)) %>% 
  left_join(world_map) %>% 
  left_join(df2, by=c('region'='ctryname', 'year' = 'year')) %>% 
  mutate(fill=if_else(is.na(fill), 5, fill))


map_df[,2] <- sapply(map_df[,2],as.integer)

world <- ggplot(map_df, 
       aes(x=long, y=lat, group=group, fill=factor(fill))) +
  geom_polygon(color='white', show.legend = TRUE) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=8),
        title = element_text(size=32), axis.title = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank()) +
  scale_fill_manual(values = color_vector, labels=c('Civilian Dictatorship', 
          'Military  Dictatorship', 'Royal Dictatorship', 'Democracy', 'NA')) + 
  transition_time(year) +
  ease_aes('linear') +
  labs(title = 'Dictatorships of the World, Year: {frame_time}', fill='Gov Type')
  

anim <- animate(world, nframes = 100, fps = 10, end_pause = 40, renderer = gifski_renderer())

# anim_save("all_countries4.gif", anim)

anim


#BAR GRAPH

ag_df <- map_df %>% group_by(un_continent_name, year) %>% filter(!is.na(un_continent_name)) %>% 
  summarize(perc_dict = sum(regime %in% c(3, 4, 5)) / sum(!is.na(regime)))

dict_bar <- ggplot(ag_df, aes(x=un_continent_name, y=perc_dict, label=perc_dict)) + 
  geom_bar(stat='identity') + 
  geom_text(vjust = -.5) +
  transition_time(year) +
  labs(title = 'Year: {frame_time}')

anim_b <- animate(dict_bar, nframes = 100, fps = 10, renderer = gifski_renderer())

anim_b




#Color code and have a count by continent
#How do we make sure any NA data is labeled something different
#also na data needs to be inserted for nations without data during the earlier years
#Finally we could consider a second graph using the more recnet democracy index
#Extra: use long/lat points to determine if countries neighbor each other, or
#are just nearby one another



world_map %>% filter(region %in% c('Mexico', 'United States')) %>% View

x <- sqldf('select a.region as regionA, b.region as regionB from world_map as a inner join world_map as b 
      where a.long = b.long and a.lat = b.lat and a.region != b.region')

x %>% distinct(regionA, regionB)











# NEW MAP WITH DEM INDEX


dfa <- read_csv('country_data2.csv')

dfb <- dfa %>% 
  mutate(fill=case_when(
    `Democracy Index` >= 60 ~ 1,
    `Democracy Index` < 60 & `Democracy Index` >= 40 ~ 2,
    `Democracy Index` < 40 ~ 3)) %>% 
  select(name, year, fill, `Democracy Index`)

color_vector2 <- c("cadetblue1", "coral1", "brown4", "darkgray")


world_map2 <- map_data('world')
world_map2$region[world_map2$region == "Greenland"] <- "Denmark"


map_df2 <- crossing(world_map2 %>% distinct(region), #%>% filter(region %in% include_countries),
                   dfb %>% distinct(year) %>% filter(!is.na(year))) %>% 
  left_join(world_map2) %>% 
  left_join(dfb, by=c('region'='name', 'year' = 'year')) %>% 
  mutate(fill=if_else(is.na(fill), 4, fill))


map_df2[,2] <- sapply(map_df2[,2],as.integer)

world2 <- ggplot(map_df2, 
                aes(x=long, y=lat, group=group, fill=factor(fill))) +
  geom_polygon(color='white', show.legend = TRUE) +
  scale_fill_manual(values = color_vector2, labels=c('Democracy', 
                                                    'Hybrid Regime', 'Authoritarian Regime', 'NA')) + 
  transition_time(year) +
  ease_aes('linear') +
  labs(title = 'Dictatorships of the World, Year: {frame_time}', fill='Gov Type')


anim2 <- animate(world2, nframes = 200, fps = 20, end_pause = 60, renderer = gifski_renderer())

anim_save("all_countries_b.gif", anim2)

anim2


