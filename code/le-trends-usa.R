#  program:  le-trends-usa.R
#  task:     analyses of LE in OECD countries
#  input:    e0-e65-sex-race-usa.csv
#  output:   
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 28feb2020

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(patchwork)

here::here()

##### 1  #####
##### import NCHS data, transform for analysis

# Read in life expectancy data from NCHS, downloaded from:
# https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/Health_US/hus18figures/fig01.xlsx

e <- read_csv(here("data", "e0-e65-sex-race-usa.csv"))

# reshape from wide to long

ed <- e %>% pivot_longer(
  cols = nhw_both_e0:hisp_female_e65,
  names_to = c("race", "gender", "age"),
  names_pattern = "(.*)_(.*)_(.*)",
  values_to = "le"
)

# subset to drop pooled gender estimates
eds <- subset(ed, gender!="both")



##### 2  #####
##### plot trends in life expectancy at birth
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank())

# Women
wb <- ggplot(subset(eds, age=="e0" & gender=="female"), aes(x=year, y=le, group=race)) + geom_line(aes(color=race), size=1.5, show.legend = FALSE) + theme_classic() + scale_x_continuous(breaks=c(2006, 2010, 2014, 2018), expand = c(0,2)) + scale_y_continuous(limits=c(69, 85), breaks=c(70, 75, 80, 85)) + scale_color_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) + annotate("text", label = "Hispanic", x = 2015, y = 83.5, size = 5, colour = "#1b9e77") + annotate("text", label = "Non-Hispanic\nWhite", x = 2011, y = 80, size = 5, colour = "#7570b3", hjust=0) + annotate("text", label = "Non-Hispanic\nBlack", x = 2007, y = 75, size = 5, colour = "#d95f02", hjust=0) + ylab("") + xlab("") + ggtitle("Life expectancy at birth", subtitle="Women") + stheme

# Men
mb <- ggplot(subset(eds, age=="e0" & gender=="male"), aes(x=year, y=le, group=race)) + geom_line(aes(color=race), size=1.5, show.legend = FALSE) + theme_classic() + scale_x_continuous(breaks=c(2006, 2010, 2014, 2018), expand = c(0,2)) + scale_y_continuous(limits=c(69, 85), breaks=c(70, 75, 80, 85)) + scale_color_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) + ylab("") + xlab("") + ggtitle("", subtitle="Men") + stheme

# Both plots together  
eb <- wb + mb
eb

# Save as figure
ggsave(here("figures", "us-le-trends-race-e0.png"), plot=eb, width=11, height=6.5)


##### 3  #####
##### plot trends in life expectancy at age 65

# Women
wo <- ggplot(subset(eds, age=="e65" & gender=="female"), aes(x=year, y=le, group=race)) + geom_line(aes(color=race), size=1.5, show.legend = FALSE) + theme_classic() + scale_x_continuous(breaks=c(2006, 2010, 2014, 2018), expand = c(0,2)) + scale_y_continuous(limits=c(15, 23), breaks=c(16, 18, 20, 22)) + scale_color_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) + annotate("text", label = "Hispanic", x = 2006, y = 22.5, size = 5, colour = "#1b9e77", hjust=0) + annotate("text", label = "Non-Hispanic White", x = 2011, y = 21, size = 5, colour = "#7570b3", hjust=0) + annotate("text", label = "Non-Hispanic\nBlack", x = 2007, y = 17.8, size = 5, colour = "#d95f02", hjust=0) + ylab("") + xlab("") + ggtitle("Life expectancy at age 65", subtitle="Women") + stheme
  
mo <- ggplot(subset(eds, age=="e65" & gender=="male"), aes(x=year, y=le, group=race)) + geom_line(aes(color=race), size=1.5, show.legend = FALSE) + theme_classic() + scale_x_continuous(breaks=c(2006, 2010, 2014, 2018), expand = c(0,2)) + scale_y_continuous(limits=c(15, 23), breaks=c(16, 18, 20, 22)) + scale_color_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) + ylab("") + xlab("") + ggtitle("", subtitle="Men") + stheme

eo <- wo +  mo
eo
ggsave(here("figures", "us-le-trends-race-e65.png"), plot=eo, width=11, height=6.5)

both <- eb / eo
both

ggsave(here("figures", "us-le-trends-race.png"), plot=both, width=11, height=10)

e0d <- e0 %>% pivot_longer(
  cols = all_both:black_female,
  names_to = c("race", "gender"),
  names_pattern = "(.*)_(.*)",
  values_to = "le"
)

le <- subset(e0d, gender!="both" & race!="all")
                    
ggplot(le, aes(x=year, y=le, group=race)) + geom_line(aes(color=race)) + facet_wrap(~gender)   
