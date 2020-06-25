#  program:  nhwm-45-54-maps.R
#  task:     maps of mortality for NH White men 45-54
#  input:    liverca-age-1999-2018.txt, liverca-age-hisp-1999-2018.txt
#  output:   liverca-race-trends.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-25

# 0
# load libraries
library(tidyverse)
library(maps)
library(ggmap)
library(ggthemes)
library(here)
library(ggrepel)
library(patchwork)

here::here()

us_states <- map_data("state")
    
df1 <- read_csv(here("data", "nhw-all-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=1,
         region = tolower(`State`)) 

df1a <- read_tsv(here("data", "nhw-all-45-54-state.txt"),
                 col_names=TRUE, n_max=51) %>%
  mutate(rate = `Crude Rate`, cause=1,
         region = tolower(`State`)) 

df2 <- read_csv(here("data", "nhw-suicide-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=2,
         region = tolower(`State`)) 

df2a <- read_tsv(here("data", "nhw-suicide-45-54-state.txt"),
                 col_names=TRUE, n_max=51) %>%
  mutate(rate = `Crude Rate`, cause=1,
         region = tolower(`State`)) 

df3 <- read_csv(here("data", "nhw-etoh-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=3,
         region = tolower(`State`)) 

df3a <- read_tsv(here("data", "nhw-etoh-45-54-state.txt"),
                 col_names=TRUE, n_max=51) %>%
  mutate(rate = `Crude Rate`, cause=1,
         region = tolower(`State`)) 

df4 <- read_csv(here("data", "nhw-poison-45-54-state.csv")) %>%
  mutate(rate = `Crude Rate`, cause=4,
         region = tolower(`State`)) 

df4a <- read_tsv(here("data", "nhw-poison-45-54-state.txt"),
                 col_names=TRUE, n_max=51) %>%
  mutate(rate = `Crude Rate`, cause=1,
         region = tolower(`State`)) 

all4 <- bind_rows(df1,df2,df3,df4) %>%
  mutate(cause=recode(cause, `1` = "All", `2` = "Suicide", 
                `3` = "Alcohol-related", `4` = "Drug-related")) %>%
  left_join(us_states)

all4a <- bind_rows(df1a,df2a,df3a,df4a) %>%
  mutate(cause=recode(cause, `1` = "All", `2` = "Suicide", 
                `3` = "Alcohol-related", `4` = "Drug poisoning")) %>%
  left_join(us_states)


# map for all causes
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


