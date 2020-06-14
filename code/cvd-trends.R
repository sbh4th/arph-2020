#  program:  etoh-trends.R
#  task:     trends on alcohol-related mortality
#  input:    etoh-k70-1999-2018.txt, etoh-f10-1999-2018.txt
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
##### Deaths from CVD (age-adjusted within broad age groups)
##### from SEER*Stat

# Raw data
raw <- read_tsv(here("data", "aaasdr-cvd-1990-2017.txt"),
                 col_names=c("sex", "raceeth", "age4", "year", "aadr", "count", "pop"), col_types = "ddddddd")

# rescale year
raw$year <- raw$year + 1990

# race-ethnicity as factor
raw$raceethf <- recode_factor(raw$raceeth, `0`= "Non-Hispanic White", `1`= "Non-Hispanic Black", `2`= "Non-Hispanic AI/AN", `3`= "Non-Hispanic API", `4`= "Hispanic")

# age-group
raw$age4f <- recode_factor(raw$age4, `0`= "15-34yrs", `1`= "35-54yrs", `2`= "55-64yrs", `3`= "65+yrs")

# theme modifications
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

m <- ggplot(subset(raw, sex == 0 & age4>0 & raceeth!=2), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_line(show.legend=T, size=1.5) + facet_wrap(~ age4f, nrow=1, scales="free") +
  scale_color_manual(name="Race-Ethnicity", 
    values=c("#4daf4a","#377eb8", "#e41a1c", "#984ea3")) + labs(y = "", x = "") +
  ggtitle("Men") + stheme 

w <- ggplot(subset(raw, sex == 1 & age4>0 & raceeth!=2), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_line(show.legend=F, size=1.5) + facet_wrap(~ age4f, nrow=1 , scales="free") +
  scale_color_manual(name="Race-Ethnicity", 
    values=c("#4daf4a","#377eb8", "#e41a1c", "#984ea3")) + 
  labs(y = "", x = "") +
  ggtitle("Women") + stheme 

p <- w / m + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.text = element_text(size=12)) 
p2 <- p + plot_annotation(
  title = 'Age-adjusted cardiovascular disease death rates per 100,000, by gender and race-ethnicity', theme = theme(plot.title = element_text(size = 18)))

ggsave(here("figures", "age-cvd-trends-by-race.png"), plot=p2, width=11, height=8.5)
