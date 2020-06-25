#  program:  rural-upoison-trends.R
#  task:     urban-rural poisoning trends
#  input:    unint-poison-ur-1999-2018-allages.txt
#  output:   rural-upoison-trends.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-25

##### 0 #####
##### load libraries
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)
library(directlabels)

here::here()

##### 1  #####
##### Read in data

upur <- read_tsv(here("data", "unint-poison-ur-1999-2018-allages.txt"), skip=1, 
  col_names=c("notes", "ur", "urcode", "year", "ycode", 
              "deaths", "pop", "crate", "arate"), n_max=120,
  col_types = "ccddddddd")


##### 2  #####
##### Make the plot

# theme properties
stheme <- theme_classic() + theme(plot.title = element_text(size = 16, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))



## plot it
p <- ggplot(upur, aes(x=year, y=arate, colour=ur, label=ur)) +
  geom_segment(aes(x = 1999, y = 10, xend = 2018, yend = 10), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_segment(aes(x = 1999, y = 20, xend = 2018, yend = 20), 
               linetype="dotted", color='grey60', show.legend=FALSE) +
  geom_line(aes(colour=ur), size=1.5, show.legend=FALSE) + 
  geom_text_repel(data=subset(upur, year==2018), xlim  = 2019, 
                  hjust = 1, point.padding = 0.5) +
  geom_segment(aes(x = 2000, y = 15, xend = 2010, yend = 15), 
               color='grey60', show.legend=FALSE) + 
  geom_segment(aes(x = 2010, y = 19, xend = 2015, yend = 19), 
               color='grey60', show.legend=FALSE) + 
  geom_segment(aes(x = 2013, y = 23, xend = 2019, yend = 23), 
               arrow = arrow(length = unit(0.25, "cm")), color='grey60', 
               show.legend=FALSE) + 
  scale_y_continuous(limits=c(0,25), breaks=c(0,10,20)) +
  scale_x_continuous(limits=c(1998,2025),breaks=c(2000,2005,2010,2015)) + 
  ylab("") + xlab("") + 
  ggtitle("Age-adjusted unintentional poisoning death rate per 100,000") + stheme + 
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour="white")) +
  scale_color_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1','#bdbdbd')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1','#bdbdbd')) + 
  annotate(geom="text", x=2005, y=15.7, label="Prescription painkiller phase", 
           color='grey60', cex=5) + 
  annotate(geom="text", x=2012.5, y=19.7, label="Heroin phase", 
           color='grey60', cex=5) + 
  annotate(geom="text", x=2016, y=23.7, label="Synthetic opioid phase", 
           color='grey60', cex=5)

# export to file
ggsave(here("figures", "rural-upoison-trends.png"), 
       plot=p, width=8, height=6.5)
