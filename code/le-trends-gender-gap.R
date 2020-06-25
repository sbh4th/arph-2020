#  program:  le-trends-gender-gap.R
#  task:     le trends with joinpoint analysis
#  input:    jp-le-age-sex-race.txt
#  output:   le-gender-gap.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-25

# 0
# load libraries
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### Life expectancy and joinpoint trends
##### from SEER*Stat

# Raw data
raw <- read_tsv(here("data/seer-stat", "jp-le-age-sex-race.txt"))
  
raw <-rename(raw, exm = Model, ex_se = `Standard Error`) %>%
  select(age3, sex, race, year0, ex)

raw2 <- pivot_wider(raw, names_from = sex, values_from = ex) %>%
  mutate(gap = `1` - `2`)


# rescale year
raw2$year <- raw2$year0 + 1999

# age as factor
raw2$age <- recode_factor(raw2$age3, `1`= "At birth", 
  `2`= "At age 25", `3`= "At age 65") 

# race-ethnicity as factor
raw2$raceeth <- recode_factor(raw2$race, `1`= "Non-Hispanic AIAN", 
  `2`= "Non-Hispanic API", `3`= "Non-Hispanic Black", 
  `4`= "Non-Hispanic White", `5`= "Hispanic")


##### 2  #####
# first some theme modifications for all graphs
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))



##### 3  #####
##### Make plots of life expectancy trends at birth

# at birth
b <- ggplot(subset(raw2, age3==1 & race!=1),  
            aes(x = year0, y = gap, colour = raceeth)) + 
  geom_line(size=1) + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(0,7.5)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("", subtitle="At birth") + 
  annotate("text", label = "Non-Hispanic\nAsian/Pacific\nIslander", 
           x = 2008, y = 3.5, size = 5, colour = "#e41a1c", hjust=0) +
  annotate("text", label = "Hispanic", 
           x = 1999, y = 6.2, size = 5, colour = "#984ea3", hjust=0) +
  annotate("text", label = "Non-Hispanic\nWhite", 
           x = 1999, y = 4.3, size = 5, colour = "#4daf4a", hjust=0) +
  annotate("text", label = "Non-Hispanic Black", 
           x = 2000, y = 7.2, size = 5, colour = "#377eb8", hjust=0) +
  stheme 

# at age 25
a <- ggplot(subset(raw2, age3==2 & race!=1),  
            aes(x = year0, y = gap, colour = raceeth)) + 
  geom_line(size=1) + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(0,7.5)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("", subtitle="At age 25") + 
  stheme 

s <- ggplot(subset(raw2, age3==3 & race!=1),  
            aes(x = year0, y = gap, colour = raceeth)) + 
  geom_line(size=1) + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(0,7.5)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("", subtitle="At age 65") + 
  stheme 

# side by side
p <- b + a + s + 
  plot_annotation(title = 'Gender gap (women - men) in life expectancy (years)',
                  theme = theme(plot.title = element_text(size = 18)))
p

# export to file
ggsave(here("figures", "le-gender-gap.png"), plot=p, width=11, height=6.5)

