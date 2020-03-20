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
library(patchwork)

here::here()

##### 1  #####
##### Overall age-adjusted mortality by gender and race
aadr <- read_tsv(here("data", "aadr-sex-race-cod-1990-2017.txt"), skip = 1,
                 col_names=c("cod", "sex", "raceeth", "year", "aadr", "count", "pop"), col_types = "ddddddd")

# rescale year
aadr$year <- aadr$year + 1990

# value labels for factors

cods <- c(0:23)
cat(paste(shQuote(cods, type="cmd"), collapse=", "))
aadr$codf <- recode_factor(aadr$cod, `0`= "heart diseases", `1`= "hypertension", `2`= "stroke",
  `3`= "other cvd", `4`= "colorectal", `5`= "lung", `6`= "breast", `7`= "prostate",
  `8`= "other cancer", `9`= "flu/pneumonia", `10`= "septicemia", `11`= "hiv", 
  `12`= "other infectious", `13`= "alzheimers", `14`= "clrd", `15`= "diabetes",
  `16`= "nephritis", `17`= "cirrhosis", `18`= "homicide", `19`= "suicide", 
  `20`= "congenital anomalies", `21`= "perinatal death", `22`= "accidents", 
  `23`= "residual")

# race-ethnicity
aadr$raceethf <- recode_factor(aadr$raceeth, `0`= "Non-Hispanic White", `1`= "Non-Hispanic Black", `2`= "Non-Hispanic AI/AN", `3`= "Non-Hispanic API", `4`= "Hispanic")

# Men
ggplot(subset(aadr, sex == 0 & cod != 6), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_smooth(aes(colour = as.factor(raceethf))) + facet_wrap(~ codf, scales="free") +
  scale_color_discrete(name="Race-Ethnicity") + labs(y = "Age-adjusted rate per 100,000", x = "")

# Women
ggplot(subset(aadr, sex == 1 & cod != 7), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_line(aes(colour = as.factor(raceethf))) + facet_wrap(~ codf, scales="free") +
  scale_color_discrete(name="Race-Ethnicity")

# Relative changes since year 2015
rel <- subset(aadr, year >= 2010)
rr <- rel %>%
  group_by(sex, raceeth, codf) %>%
    mutate(rrate = aadr /  aadr[year==2015] * 100)
  
ggplot(subset(rr, sex == 0), 
       aes(x = year, y = rrate, colour = as.factor(raceethf))) + 
  geom_line(aes(colour = as.factor(raceethf))) + facet_wrap(~ codf)  

ggplot(subset(rr, sex == 0 & cod==19), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_line(aes(colour = as.factor(raceethf)))

ggplotly(p)

# export to figures folder
ggsave(here("figures", "us-le-lagging.png"), plot=p)


# single figure with both plots
t <- p + pc
ggsave(here("figures", "us-le-trends.png"), plot=t, width=11, height=6.5)

