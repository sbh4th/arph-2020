#  program:  cod-plot-use.R
#  task:     analyses of LE in OECD countries
#  input:    none
#  output:   aadr-sex-race-cod-1990-2017.txt
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-03-10

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### Overall age-adjusted mortality by gender and race
s <- read_tsv(here("data", "suicide-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=960, 
  col_types = "cccfccfdcddc")


# keep demographics, deaths, population, create rates
s2 <- select(s, gender, race, acode, year, deaths, pop) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

# Hispanics
sh <- read_tsv(here("data", "suicide-hisp-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "hisp", "hcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=640,
  col_types = "cccfccfdcddc")

# keep demographics, deaths, population, create rates
sh2 <- select(sh, gender, hisp, acode, year, deaths, pop) %>%
  filter(hisp == "Hispanic or Latino") %>%
  mutate(racen = 4, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
sr <- bind_rows(s2, sh2)

# standard population for age-adjustment
std <- tibble(acode = levels(s$acode), 
        std = c(64529, 71044, 80762, 81851, 72118, 62716, 48454, 38793))
std$acode <- factor(std$acode)
  
std2 <- mutate(std,
               stdwt = std / sum(std))
 
# add standard population to dataframe

suic <- left_join(sr, std2)

# create new race group
suic$raceabb <- recode_factor(suic$racen, 
  `1` = "NHAPI", `2` = "NHB", `3` = "NHW", `4`= "Hispanic")

suic$age <- paste(suic$acode, "yrs", sep=" ")

# s1 <- s %>% select(gender, race, acode, year, deaths, pop) %>% 
#   mutate(age=recode_factor(acode, `25-29`="25-29 yrs", 
#     `30-34`="30-34 yrs", `35-39`="35-39 yrs", `40-49`="45-49 yrs", 
#     `50-54`="50-54 yrs", `55-59`="55-59 yrs", `60-64`="60-64 yrs"))

stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

w <- ggplot(subset(suic, gender == "Female"), 
            aes(x=year, y=rate, colour=raceabb)) +
  geom_line(show.legend=F, size=1.25) + 
  facet_wrap(vars(age), ncol=4) + xlab("") + ylab("") + 
  ggtitle("Age-specific suicide death rates per 100,000", subtitle="Women") + 
  stheme

m <- ggplot(subset(suic, gender == "Male"), 
            aes(x=year, y=rate, colour=raceabb)) + 
  geom_line(show.legend=T, size=1.25) + facet_wrap(vars(age), ncol=4) +
  scale_x_continuous(breaks=c(2000, 2010)) +
  xlab("") + ylab("") + ggtitle("", subtitle="Men") + 
  stheme + theme(legend.position = "bottom", 
                 legend.title = element_text(size=16),
                 legend.text = element_text(size=14)) + 
  scale_colour_discrete(name = "Race-ethnicity")

p <- w / m

ggsave(here("figures", "age-suic-trends.png"), plot=p, height=11, width=9)

