#  program:  nhwm-45-54-cause-map.R
#  task:     US maps by cause for NHW men
#  input:    nhw-all-45-54-state.txt; nhw-suicide-45-54-state.txt
#            nhw-etoh-45-54-state.txt; nhw-poison-45-54-state.txt
#  output:   nhwm-45-54-maps.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-25

##### 0  #####
##### load libraries

library(tidyverse)
library(maps)
library(ggmap)
library(ggthemes)
library(datasets)
library(patchwork)

##### 1  #####
##### load the map data for US states

us_states <- map_data("state")


##### 2 #####
##### Bring in death data

# All causes
df <- read_tsv(here("data/cdc-wonder", "nhw-all-45-54-state.txt"), skip=1, 
  col_names=c("notes", "state", "stcode", "deaths", "pop", "rate"), n_max=51,  
  col_types = "ccdddd")
    
df1 <- df %>%
  mutate(cause=1, region = tolower(`state`)) 



# Suicides
df <- read_tsv(here("data/cdc-wonder", "nhw-suicide-45-54-state.txt"), skip=1, 
  col_names=c("notes", "state", "stcode", "deaths", "pop", "rate"), n_max=51,  
  col_types = "ccdddd")

df2 <- df %>%
  mutate(cause=2, region = tolower(`state`)) 


# Alcohol-induced causes
df <- read_tsv(here("data/cdc-wonder", "nhw-etoh-45-54-state.txt"), skip=1, 
  col_names=c("notes", "state", "stcode", "deaths", "pop", "rate"), n_max=51,  
  col_types = "ccdddd")

df3 <- df %>%
  mutate(cause=3, region = tolower(`state`)) 

# Unintentional overdoses
df <- read_tsv(here("data/cdc-wonder", "nhw-poison-45-54-state.txt"), skip=1, 
  col_names=c("notes", "state", "stcode", "deaths", "pop", "rate"), n_max=51,  
  col_types = "ccdddd")

df4 <- df %>%
  mutate(cause=4, region = tolower(`state`)) 

all4 <- bind_rows(df1,df2,df3,df4) %>%
  mutate(cause=recode(cause, `1` = "All", `2` = "Suicide", 
                `3` = "Alcohol-related", `4` = "Unintentional poisoning")) %>%
  left_join(us_states)


##### 3 #####
##### Draw the maps


# Map for all causes
a0 <- ggplot(data = subset(all4, cause="All"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

a1 <- a0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

a <- a1 + labs(fill = "Rate") + 
  scale_fill_gradient(low = "#eff3ff", high="#08519c") +  
  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("All causes")


# Map for suicides
s0 <- ggplot(data = subset(all4, cause=="Suicide"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

s1 <- s0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

s <- s1 + labs(fill = "Rate") + 
  scale_fill_gradient(low = "#eff3ff", high="#08519c") + #, limits=c(0,90)) +
  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("Suicides")


# Map for alcohol-induced
e0 <- ggplot(data = subset(all4, cause=="Alcohol-related"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

e1 <- e0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

e <- e1 + labs(fill = "Rate") + 
  scale_fill_gradient(low = "#eff3ff", high="#08519c") + #, limits=c(0,90)) +
  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("Alcohol-related")

# Unintentional poisonings
d0 <- ggplot(data = subset(all4, cause=="Unintentional poisoning"),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = rate))

d1 <- d0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

d <- d1 + labs(fill = "Rate") + 
  scale_fill_gradient(low = "#eff3ff", high="#08519c") + #, limits=c(0,90)) +
  theme_map() + 
  theme(legend.key.size = unit(0.5, "line"), 
        legend.title = element_text(size=8)) +
  ggtitle("Unintentional poisoning")


# Put them all together
p4 <- (a + s) / (e + d)
p5 <- p4 + plot_annotation(
  title = 'Age-adjusted death rates per 100,000 for non-Hispanic white men aged 45-54, 2010-2018')
p5

# export to file
ggsave(here("figures", "nhwm-45-54-maps.png"), plot=p5, width=8, height=6.5)

