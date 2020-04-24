#  program:  le-trends-joinpoint.R
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
##### Life expectancy and joinpoint trends
##### from SEER*Stat

# Raw data
raw <- read_tsv(here("data", "jp-le-age-sex-race.txt"))
  
raw <-rename(raw, exm = Model, ex_se = `Standard Error`)

# rescale year
raw$year <- raw$year0 + 1999

# age as factor
raw$age <- recode_factor(raw$age3, `1`= "At birth", 
  `2`= "At age 25", `3`= "At age 65") 

# race-ethnicity as factor
raw$raceeth <- recode_factor(raw$race, `1`= "Non-Hispanic AIAN", 
  `2`= "Non-Hispanic API", `3`= "Non-Hispanic Black", 
  `4`= "Non-Hispanic White", `5`= "Hispanic")

# age-group
raw$gender <- recode_factor(raw$sex, `1`= "Women", `2`= "Men")

##### 2  #####
# first some theme modifications for all graphs
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

##### 3  #####
##### Make plots of life expectancy trends at birth

# women
w <- ggplot(subset(raw, age3==1 & sex==1 & race!=1),
  aes(x = year0, y = ex, colour = raceeth)) + 
  geom_point(alpha=0.3, size=2) + geom_line(aes(x=year0, y=exm, 
  colour = raceeth), size=1)  + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(65,91)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("Life expectancy at birth (years)", subtitle="Women") + 
  annotate("text", label = "Non-Hispanic\nAsian/Pacific Islander", 
           x = 1999, y = 90, size = 5, colour = "#e41a1c", hjust=0) +
  annotate("text", label = "Hispanic", 
           x = 2013, y = 85, size = 5, colour = "#984ea3", hjust=0) +
  annotate("text", label = "Non-Hispanic White", 
           x = 2005, y = 82, size = 5, colour = "#4daf4a", hjust=0) +
  annotate("text", label = "Non-Hispanic\nBlack", 
           x = 1999, y = 77, size = 5, colour = "#377eb8", hjust=0) +
  stheme 

# men
m <- ggplot(subset(raw, age3==1 & sex==2 & race!=1), 
  aes(x = year0, y = ex, colour = raceeth)) + 
  geom_point(alpha=0.3, size=2) + geom_line(aes(x=year0, y=exm, 
  colour = raceeth), size=1)  + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(65,91)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("", subtitle="Men") + 
  stheme 

# side by side
p <- w + m 
p

# export to file
ggsave(here("figures", "le-jp0.png"), plot=p, width=11, height=6.5)


##### 4  #####
##### Make plots of life expectancy trends at age 25

# women
w <- ggplot(subset(raw, age3==2 & sex==1 & race!=1),
  aes(x = year0, y = ex, colour = raceeth)) + 
  geom_point(alpha=0.3, size=2) + geom_line(aes(x=year0, y=exm, 
  colour = raceeth), size=1) + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(40,70)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("Life expectancy at age 25 (years)", subtitle="Women") + 
  annotate("text", label = "Non-Hispanic\nAsian/Pacific Islander", 
           x = 1999, y = 66, size = 5, colour = "#e41a1c", hjust=0) +
  annotate("text", label = "Hispanic", 
           x = 2001, y = 58, size = 5, colour = "#984ea3", hjust=0) +
  annotate("text", label = "Non-Hispanic White", 
           x = 2010, y = 58, size = 5, colour = "#4daf4a", hjust=0) +
  annotate("text", label = "Non-Hispanic\nBlack", 
           x = 1999, y = 50, size = 5, colour = "#377eb8", hjust=0) +
  stheme 

# men
m <- ggplot(subset(raw, age3==2 & sex==2 & race!=1), 
  aes(x = year0, y = ex, colour = raceeth)) + 
  geom_point(alpha=0.3, size=2) + geom_line(aes(x=year0, y=exm, 
  colour = raceeth), size=1)  + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(40,70)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("", subtitle="Men") + 
  stheme 

# side by side
p <- w + m 
p

# export to file
ggsave(here("figures", "le-jp25.png"), plot=p, width=11, height=6.5)


##### 4  #####
##### Make plots of life expectancy trends at age 25

# women
w <- ggplot(subset(raw, age3==3 & sex==1 & race!=1),
  aes(x = year0, y = ex, colour = raceeth)) + 
  geom_point(alpha=0.3, size=2) + geom_line(aes(x=year0, y=exm, 
  colour = raceeth), size=1) + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(10,30)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("Life expectancy at age 65 (years)", subtitle="Women") + 
  annotate("text", label = "Non-Hispanic\nAsian/Pacific Islander", 
           x = 1999, y = 27, size = 5, colour = "#e41a1c", hjust=0) +
  annotate("text", label = "Hispanic", 
           x = 2015, y = 25.5, size = 5, colour = "#984ea3", hjust=0) +
  annotate("text", label = "Non-Hispanic White", 
           x = 2010, y = 21.5, size = 5, colour = "#4daf4a", hjust=0) +
  annotate("text", label = "Non-Hispanic\nBlack", 
           x = 1999, y = 16.5, size = 5, colour = "#377eb8", hjust=0) +
  stheme 

# men
m <- ggplot(subset(raw, age3==3 & sex==1 & race!=1), 
  aes(x = year0, y = ex, colour = raceeth)) + 
  geom_point(alpha=0.3, size=2) + geom_line(aes(x=year0, y=exm, 
  colour = raceeth), size=1) + labs(y = "", x = "") + 
  scale_y_continuous(limits=c(10,30)) + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
  "#984ea3")) + ggtitle("", subtitle="Men") + 
  stheme 

# side by side
p <- w + m 
p

# export to file
ggsave(here("figures", "le-jp65.png"), plot=p, width=11, height=6.5)

