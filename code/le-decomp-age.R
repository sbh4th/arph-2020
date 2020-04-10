#  program:  le-decomp-age.R
#  task:     le trends with joinpoint analysis
#  input:    jp-le-age-sex-race.txt
#  output:   jp-le0.png, jp-le25.png, jp-le65.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-04-07

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
## Read in raw data and label it

# Raw data
raw <- read_csv(here("data", "le-age-decomp.csv"))

# race-ethnicity as factor
raw$raceeth <- recode_factor(raw$race, `1`= "Non-Hispanic AIAN", 
  `2`= "Non-Hispanic API", `3`= "Non-Hispanic Black", 
  `4`= "Non-Hispanic White", `5`= "Hispanic")

# age as factor
raw$agef <- recode_factor(raw$age, `1` = "00-01 yrs", `2` = "01-04 yrs", 
  `3` = "05-14 yrs", `4` = "15-24 yrs", `5` = "24-34 yrs",
  `6` = "35-44 yrs", `7` = "45-54 yrs", `8` = "55-64 yrs", 
  `9` = "65-74 yrs", `10` = "75-84 yrs", `11` = "85+ yrs", 
  `12` = "Total change")

##### 2  #####
## Plot options and style

stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.x=element_text(size=16, colour="grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="white"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16, colour="grey60"), strip.background = element_rect(colour="white"))

##### 3  #####
## Graph for all 11 age groups

# women
w <- ggplot(subset(raw, sex==1 & race!=1)) + geom_bar(aes(y=agef, weight=te)) + 
  geom_text(aes(y=agef, x=te, label = round(te, 2)), hjust=2) +
  facet_wrap(~raceeth, nrow=1) + facet_wrap()
  scale_y_discrete(limits = rev(levels(raw$agef))) +
  scale_x_continuous(limits=c(-2, 0.5), breaks=c(-2, -1, 0)) +
  labs(y = "", x = "") + 
  ggtitle("Age group contribution to le change", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines"))
  
# men
m <- ggplot(subset(raw, sex==2 & race!=1)) + geom_bar(aes(y=agef, weight=te)) + 
  geom_text(aes(y=agef, x=te, label = round(te, 2)), hjust=2) +
  facet_wrap(~raceeth, nrow=1) + 
  scale_y_discrete(limits = rev(levels(raw$agef))) +
  scale_x_continuous(limits=c(-2, 0.5), breaks=c(-2, -1, 0)) +
  labs(y = "", x = "") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines"))

# put both plots together
p <- w / m 
p

# export to file
# ggsave(here("figures", "le-age-decomp.png"), plot=p, width=11, height=8.5)

##### 4  #####
## Collapse to 4 age groups and sum contributions for cleaner plot

raw$age4f <- recode_factor(raw$age, `1` = "<1 yrs", `2` = " 1-15 yrs", 
  `3` = " 1-15 yrs", `4` = "15-44 yrs", `5` = "15-44 yrs",
  `6` = "15-44 yrs", `7` = "45-64 yrs", `8` = "45-64 yrs", 
  `9` = "65+ yrs", `10` = "65+ yrs", `11` = "65+ yrs", 
  `12` = "Total change")

raw4 <- raw %>%
  group_by(sex, race, age4f) %>%
  summarise(total = sum(te) * -1 )

# race-ethnicity as factor
raw4$raceeth <- recode_factor(raw4$race, `1`= "Non-Hispanic AIAN", 
  `2`= "Non-Hispanic API", `3`= "Non-Hispanic Black", 
  `4`= "Non-Hispanic White", `5`= "Hispanic")

# women
w <- ggplot(subset(raw4, sex==1 & race!=1)) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=age4f, weight=total), width=0.5, 
           colour = "#377eb8", fill = "#377eb8") + 
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 2.5), breaks=c(0, 1, 2)) +
  labs(y = "", x = "") + 
  ggtitle("Age group contribution to change in life expectancy at birth, 2010-2018", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines"))
  
# men
m <- ggplot(subset(raw4, sex==2 & race!=1)) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=age4f, weight=total), width=0.5, 
           colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 2.5), breaks=c(0, 1, 2)) +
  labs(y = "", x = "Years") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines"))

# put both plots together
p <- w / m 
p

# export to file
ggsave(here("figures", "le-age-decomp.png"), plot=p, width=11, height=8.5)

