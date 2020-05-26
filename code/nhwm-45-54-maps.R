library(tidyverse)
# library(urbnmapr)
# library(urbnthemes)
    
# set_urbn_defaults(style = "map")

library(maps)
library(ggmap)
library(ggthemes)
library(patchwork)
us_states <- map_data("state")
    
df1 <- read_csv(here("data", "nhw-all-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=1,
         region = tolower(`State`)) 

df2 <- read_csv(here("data", "nhw-suicide-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=2,
         region = tolower(`State`)) 

df3 <- read_csv(here("data", "nhw-etoh-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=3,
         region = tolower(`State`)) 

df4 <- read_csv(here("data", "nhw-drugs-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=4,
         region = tolower(`State`)) 

all4 <- bind_rows(df1,df2,df3,df4) %>%
  mutate(cause=recode(cause, `1` = "All", `2` = "Suicide", 
                `3` = "Alcohol-related", `4` = "Drug-related")) %>%
  left_join(us_states)

p <- ggplot(data = df3e,
            aes(x = long, y = lat,
                group = group, fill = cr))

etoh <- e + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_map()


a0 <- ggplot(data = subset(all4, cause="All"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

a1 <- a0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

a <- a1 + labs(fill = "Rate") + 
  scale_fill_gradient2() +  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("All causes")


s0 <- ggplot(data = subset(all4, cause=="Suicide"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

s1 <- s0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

s <- s1 + labs(fill = "Rate") + 
  scale_fill_gradient2() +  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("Suicides")


e0 <- ggplot(data = subset(all4, cause=="Alcohol-related"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

e1 <- e0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

e <- e1 + labs(fill = "Rate") + 
  scale_fill_gradient2() +  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("Alcohol-related")


d0 <- ggplot(data = subset(all4, cause=="Drug-related"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

d1 <- d0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

d <- d1 + labs(fill = "Rate") + 
  scale_fill_gradient2() +  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("Drug-related")

p4 <- (a + s) / (e + d)
p4

# export to file
ggsave(here("figures", "nhwm-45-54-maps.png"), plot=p4, width=8, height=6.5)





+  theme(legend.position = "bottom",
          strip.background = element_blank()) +
    labs(fill = "Death rate per 100,000 population ",
         title = "Opiate Related Deaths by State, 2000-2014")  

