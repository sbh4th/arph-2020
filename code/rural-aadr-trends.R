#  program:  rural-aadr-trends.R
#  task:     urban-rural trends
#  input:    seer-ur-2013.txt
#  output:   rural-aadr-trends.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-22

##### 0  #####
##### load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)
library(directlabels)

here::here()

##### 1  #####
##### Read in data

# 2013 rural definitions
d <- read_tsv(here("data", "seer-ur-2013.txt"), 
                     col_names=c("ur","year","gender","aadr","deaths","pop"))

# urban-rural definition
d$ur <- factor(d$ur, levels = c(0,1,2,3,4,5,6,7,8,9,10), 
  labels = c("Metro", "Metro >1m", "Metro >250k-1m", 
  "Metro <250k", "Non-metro", "Large urban, metro-adjacent", 
  "Large urban, metro-non-adjacent", "Small urban, metro-adjacent", 
  "Small urban, metro-non-adjacent", "Rural, metro-adjacent",
  "Rural, metro-non-adjacent"))

# create year scale
d$yearc <- 1970 + 3*d$year
d$yearc <- ifelse(d$year==13, 2010, d$yearc)
d$yearc <- ifelse(d$year==14, 2015, d$yearc)


# subset to both genders, exclude summary levels
dur <- subset(d, (ur != "Metro" & ur != "Non-metro") & gender==0 & yearc>=1970) 


##### 2  #####
##### Make the plot

# theme properties
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

                  
b <- 
ggplot(dur, aes(x=yearc, y=aadr, colour=ur, label=ur)) + 
  geom_segment(aes(x = 1970, y = 600, xend = 2015, yend = 600), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_segment(aes(x = 1970, y = 800, xend = 2015, yend = 800), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_segment(aes(x = 1970, y = 1000, xend = 2015, yend = 1000), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_segment(aes(x = 1970, y = 1200, xend = 2015, yend = 1200), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_line(aes(colour=ur), size=1.5, show.legend=FALSE) + 
  geom_text_repel(data=subset(dur, yearc==2015), xlim = 2016, 
                  hjust = 1, point.padding = 0.2) +
  scale_x_continuous(limits=c(1970, 2030), 
                     breaks=c(1970,1980,1990,2000,2010)) + 
  scale_y_continuous(limits=c(600,1300)) +
  ylab("") + xlab("") + 
  ggtitle("Age-adjusted death rate per 100,000") + stheme + 
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour="white")) +
  scale_color_manual(values=c("#3f007d", "#6a51a3", "#9e9ac8", 
    "#00441b", "#006d2c", "#238b45", "#41ab5d", "#74c476", "#a1d99b")) 

# write to file
ggsave(here("figures", "rural-aadr-trends.png"), 
       plot=b, width=11, height=7)

