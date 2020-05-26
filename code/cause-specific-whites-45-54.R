#  program:  cause-specific-whites-45-54.R
#  task:     trends on alcohol-related mortality
#  input:    
#  output:   
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-05-21

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### Deaths for cirrhosis and chronic liver dx ICD10 K70
nhw <- read_tsv(here("data", "nhw-45-54-113-causes.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "cause", "ccode", 
              "year", "ycode", "deaths", "pop", "crate"), n_max=4838,
  col_types = "cccccddddc")

nhw1 <- nhw %>%
  mutate(gcode=recode(gcode, F = "Women", M = "Men") )%>%
  mutate(cod = parse_number(str_sub(ccode, 7, 9))) %>%
  mutate(cod14 = case_when(
    cod %in% 16 ~ "HIV", 
    cod %in% 19 ~ "Cancers", 
    cod %in% 46 ~ "Diabetes",
    cod %in% 52 ~ "Alzheimer's", 
    cod %in% 53 ~ "CVDs", 
    cod %in% 76 ~ "Flu/pneumonia",
    cod %in% 82 ~ "Chronic Resp dx",
    cod %in% 93 ~ "Liver dx", 
    cod %in% 97 ~ "Kidney dx",
    cod %in% 114 ~ "MV crashes", 
    cod %in% 122 ~ "Poisoning", 
    cod %in% 124 ~ "Suicide", 
    cod %in% 127 ~ "Homicide",
    cod %in% c(1:15, 17, 18, 44, 45, 47, 50, 51, 79, 87:92, 96, 102:105, 108:111, 115, 116, 118:121, 123, 130, 131, 134:136) ~ "Residual",
    cod %in% c(20:43, 48, 49, 54:75, 77, 78, 80, 81, 83:86, 94, 95, 98:101, 106, 107, 112, 113, 117, 125, 126, 128, 129, 132, 133) ~ "Subtotal") )
    
all <- nhw1 %>% filter(cod14!="Subtotal") %>%
  select(gcode, year, deaths, pop) %>%
  group_by(gcode, year) %>%
  summarise(deaths = sum(deaths), pop = mean(pop)) %>%
  mutate(rate = deaths / pop * 100000)

ggplot(all, aes(x=year, y=rate)) + geom_line() + facet_wrap(~gcode)

bc <- nhw1 %>% filter(cod14!="Subtotal") %>%
  select(gcode, year, deaths, pop, cod, cod14) %>%
  group_by(gcode, year, cod14) %>%
  summarise(deaths = sum(deaths), pop = mean(pop)) %>%
  mutate(rate = deaths / pop * 100000)

p <- ggplot(bc, aes(x=year, y=rate, colour=cod14)) + geom_line() + facet_wrap(~gcode)

r <- nhw1 %>% filter(cod14=="Residual") %>%
  select(gcode, year, deaths, pop, cause) %>%
  group_by(gcode, year, cause) %>%
  summarise(deaths = sum(deaths), pop = mean(pop)) %>%
  mutate(rate = deaths / pop * 100000)

rp <- ggplot(r, aes(x=year, y=rate, colour=cause)) + geom_line(show.legend = FALSE) + facet_wrap(~gcode) 
