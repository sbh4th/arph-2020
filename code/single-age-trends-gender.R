#  program:  single-age-trends-gender.R
#  task:     analyses of mortality by single ages
#  input:    single-age-25-64-1999-2018.txt, single-age-hisp-25-64-1999-2018.txt
#  output:   single-ages-women.png, single-ages-men.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-24

# 0
# load libraries
library(tidyverse)
library(here)
library(patchwork)

here::here()

##### 1  #####
##### Bring in mortality by single ages and build dataset

# Non-Hispanics
sa <- read_tsv(here("data/cdc-wonder", 
                    "single-age-25-64-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=4800,
  col_types = "cccfccddcddc")

# keep demographics, deaths, population, create rates
sa2 <- select(sa, gender, race, acode, year, deaths, pop) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

# Hispanics
sah <- read_tsv(here("data/cdc-wonder", 
                     "single-age-hisp-25-64-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "hisp", "hcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=3200,
  col_types = "cccfccddcddc")

# keep demographics, deaths, population, create rates
sah2 <- select(sah, gender, hisp, acode, year, deaths, pop) %>%
  filter(hisp == "Hispanic or Latino") %>%
  mutate(racen = 4, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
sar <- bind_rows(sa2, sah2)

# create new race grouping
sar$raceabb <- recode_factor(sar$racen, 
  `1` = "NH API", `2` = "NH Black", `3` = "NH White", `4`= "Hispanic")

# format age variable
sar$age <- paste(sar$acode, "yrs", sep=" ")


##### 2  #####
##### Plot for women

w <- ggplot(subset(sar, gender=="Female"), 
            aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=T) + facet_wrap(~age, scales="free", ncol=5) + 
  scale_color_manual(name="", values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + xlab("") + ylab("") + 
  ggtitle("Death rates per 100,000 by single years of age for women, 1999-2018") + 
  theme_classic() + 
  theme(axis.text.x = element_text(colour = "grey60"), 
        axis.text.y = element_text( colour="grey60"), 
        panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), 
        panel.grid.major.x = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white")) + 
  theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.background = element_rect(colour="white")) + theme(legend.position="top")

ggsave(here("figures", "single-ages-women.png"), plot=w, height=11, width=8.5)


##### 3  #####
##### Plot for men

m <- ggplot(subset(sar, gender=="Male"), 
            aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=T) + facet_wrap(~age, scales="free", ncol=5) + 
  scale_color_manual(name="", values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  xlab("") + ylab("") + 
  ggtitle("Death rates per 100,000 by single years of age for men, 1999-2018") + 
  theme_classic() + 
    theme(axis.text.x = element_text(colour = "grey60"), 
          axis.text.y = element_text( colour="grey60"), 
          panel.grid.major.y = element_line(linetype="dotted", colour="grey60"),
          panel.grid.major.x = element_line(colour="white"), 
          panel.grid.minor = element_line(colour="white")) + 
    theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.background = element_rect(colour="white")) + theme(legend.position="top")

ggsave(here("figures", "single-ages-men.png"), plot=m, height=11, width=8.5)

