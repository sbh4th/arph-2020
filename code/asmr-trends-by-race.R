#  program:  asmr-trends-by-race.R
#  task:     age-specific mortality trends by race
#  input:    asmr-sex-race-nhisp-1999-2018.txt, asmr-sex-race-hisp-1999-2018.txt
#  output:   asmr-trends-by-race.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2021-03-28

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### Age-specific death rate data from CDC WONDER

# non-Hispanics by race
a <- read_tsv(here("data/cdc-wonder", "asmr-sex-race-nhisp-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode",  
              "year", "ycode", "deaths", "pop", "crate"), n_max=1760,
  col_types = "cccfccfddddd")

# keep demographics, deaths, population, create crude rates
a2 <- select(a, gender, race, acode, year, deaths, pop) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

# Hispanics
ah <- read_tsv(here("data/cdc-wonder", "asmr-sex-race-hisp-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "age", "acode", "year", "ycode", 
              "deaths", "pop", "crate"), n_max=440,
  col_types = "ccccfddddd")

# keep demographics, deaths, population, create crude rates
ah2 <- select(ah, gender, acode, year, deaths, pop) %>%
  mutate(racen = 5, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
ar <- bind_rows(a2, ah2) %>%
  select(-race, -rate) %>%
  mutate(racen = recode_factor(racen, `1`= "Non-Hispanic AIAN", 
        `2`= "Non-Hispanic API", `3`= "Non-Hispanic Black", 
        `4`= "Non-Hispanic White", `5`= "Hispanic"),
        age4=recode_factor(acode, `1`="<15 yrs", `1-4`="<15 yrs", 
  `5-14`="<15 yrs", `15-24`="15-44 yrs", `25-34`="15-44 yrs",
  `35-44`="15-44 yrs", `45-54`="45-64 yrs", `55-64`="45-64 yrs",
  `65-74`="65+ yrs", `75-84`="65+ yrs", `85+`="65+ yrs")) %>%
  group_by(gender, racen, age4, year) %>% 
  select(-acode) %>%
  summarise_all(list(sum)) %>%
  mutate(rate = deaths / pop * 100000) %>%
  filter(racen != "Non-Hispanic AIAN")




##### 2  #####
##### Make the plot

## set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

# men
m <- ggplot(subset(ar, gender == "Male"), 
       aes(x = year, y = rate, colour = racen)) + 
  geom_line(show.legend=T, size=1.5) + facet_wrap(~ age4, nrow=1, scales="free") +
  scale_color_manual(name="Race-Ethnicity", 
    values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + labs(y = "", x = "") +
  ggtitle("Men") + scale_x_continuous(breaks=c(2005,2015)) +
  stheme 

# women
w <- ggplot(subset(ar, gender == "Female"), 
       aes(x = year, y = rate, colour = racen)) + 
  geom_line(show.legend=T, size=1.5) + facet_wrap(~ age4, nrow=1, scales="free") +
  scale_color_manual(name="Race-Ethnicity", 
    values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + labs(y = "", x = "") +
  ggtitle("Women") + scale_x_continuous(breaks=c(2005,2015)) + 
  stheme 

# men and women together
p <- w / m + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.text = element_text(size=12))

# add titles
p2 <- p + plot_annotation(
  title = 'Age-specific death rates per 100,000, by gender and race-ethnicity', theme = theme(plot.title = element_text(size = 18)))

ggsave(here("figures", "asmr-trends-by-race.png"), plot=p2, width=11, height=8.5)
