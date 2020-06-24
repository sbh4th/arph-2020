#  program:  rural-mortality-graphs.R
#  task:     urban-rural trends
#  input:    seer-ur-1974.txt; seer-ur-2013.txt
#  output:   rural-mort.png; urban-rural-gradient.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-05-28

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)
library(directlabels)

here::here()

# 1
# read in data

# 1974 rural definitions
d74 <- read_tsv(here("data", "seer-ur-1974.txt"), 
                     col_names=c("ur","year","gender","aadr","deaths","pop"))
d74$urdef <- 0

# 2013 rural definitions
d13 <- read_tsv(here("data", "seer-ur-2013.txt"), 
                     col_names=c("ur","year","gender","aadr","deaths","pop"))
d13$urdef <- 4

d <- bind_rows(d74,d13)

d$ur <- factor(d$ur, levels = c(0,1,2,3,4,5,6,7,8,9,10), 
  labels = c("Metro", "Metro >1m", "Metro >250k-1m", 
  "Metro <250k", "Non-metro", "Large urban, metro-adjacent", 
  "Large urban, metro-non-adjacent", "Small urban, metro-adjacent", 
  "Small urban, metro-non-adjacent", "Rural, metro-adjacent",
  "Rural, metro-non-adjacent"))

d$urdef <- factor(d$urdef, levels = c(0,4), labels = c("1974 rural definition", "2013 rural definition"))

d$yearc <- 1970 + 3*d$year
d$yearc <- ifelse(d$year==13, 2010, d$yearc)
d$yearc <- ifelse(d$year==14, 2015, d$yearc)

dur <- subset(d, (ur=="Metro" | ur == "Non-metro") & urdef=="2013 rural definition" & gender!=0) 
# "#1b9e77", "#d95f02", "#7570b3"

stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

p <- ggplot(dur, aes(x=yearc, y=aadr)) + 
  geom_line(data=subset(dur, ur=="Metro" & gender==1), 
            colour="#e41a1c", size=2) +
  geom_line(data=subset(dur, ur=="Non-metro" & gender==1), 
            colour="#984ea3", size=2) + 
  geom_line(data=subset(dur, ur=="Metro" & gender==2), 
            colour="#4daf4a", size=2) +
  geom_line(data=subset(dur, ur=="Non-metro" & gender==2), 
            colour="#377eb8", size=2) + 
  annotate("text", label = "Metro men", 
           x = 2016, y = 840, size = 5, colour = "#e41a1c", hjust=0) +
  annotate("text", label = "Non-metro men", 
           x = 2000, y = 1200, size = 5, colour = "#984ea3", hjust=0) +
  annotate("text", label = "Metro\nwomen", 
           x = 2000, y = 550, size = 5, colour = "#4daf4a", hjust=0) +
  annotate("text", label = "Non-metro\nwomen", 
           x = 2016, y = 660, size = 5, colour = "#377eb8", hjust=0) +
  scale_x_continuous(limits=c(1970,2023), breaks=c(1970,1980,1990,2000,2010)) +
  scale_y_continuous(limits=c(0,1600)) + labs(y = "", x = "") +
  stheme
  
# export to file
ggsave(here("figures", "aadr-rural-gender.png"), plot=p, width=8, height=6.5)

  

## Drug overdoses (unintentional)

## read in  data
upur <- read_tsv(here("data", "unint-poison-ur-1999-2018-allages.txt"), skip=1, 
  col_names=c("notes", "ur", "urcode", "year", "ycode", 
              "deaths", "pop", "crate", "arate"), n_max=120,
  col_types = "ccddddddd")

# dt$urb <- factor(dt$Rural, levels=c(0,1,2,3,4,5), labels=c("Large Central Metro", # "Large Fringe Metro", "Medium Metro", "Small Metro", "Micropolitan (Nonmetro)", # "Non-Core (Nonmetro)"))

p2 <- ggplot(upur, aes(x=year, y=arate, colour=ur, label=ur)) + 
  geom_line(aes(colour=ur), size=1.5, show.legend=FALSE) + 
  geom_text_repel(data=subset(upur, year==2018), xlim  = 2019, 
                  hjust = 1, point.padding = 0.5) +
  geom_segment(aes(x = 2000, y = 15, xend = 2010, yend = 15), color='grey60', show.legend=FALSE) + 
  geom_segment(aes(x = 2010, y = 19, xend = 2015, yend = 19), color='grey60', show.legend=FALSE) + 
  geom_segment(aes(x = 2013, y = 23, xend = 2019, yend = 23), 
               arrow = arrow(length = unit(0.25, "cm")), color='grey60', 
               show.legend=FALSE) + 
  scale_y_continuous(limits=c(0,36)) +
  scale_x_continuous(limits=c(1998,2025),breaks=c(2000,2005,2010,2015)) + 
  ylab("Rate per 100,000 population") + xlab("") + 
  stheme + theme(legend.position = "none") + 
  scale_color_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1','#bdbdbd')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1','#bdbdbd')) + 
  annotate(geom="text", x=2005, y=15.5, label="Prescription painkiller phase", color='grey60', cex=5) + 
  annotate(geom="text", x=2012.5, y=19.5, label="Heroin phase", color='grey60', cex=5) + 
  annotate(geom="text", x=2016, y=23.5, label="Synthetic opioid phase", color='grey60', cex=5)

# export to file
ggsave(here("figures", "rural-poisoining-all-ages.png"), 
       plot=p2, width=8, height=6.5)

p3 <- p + p2
p3

ggsave(here("figures", "rural-mort-poison.png"), 
       plot=p3, width=11, height=6.5)




dur <- subset(d, (ur != "Metro" & ur != "Non-metro") & urdef=="2013 rural definition" & gender==0 & yearc>=1970) 

dur6 <- dur %>%
  mutate(recode_factor(ur, `Metro >1m` = `Metro >1m`, 
        `Metro >250k-1m` = `Metro >250k-1m`, `Metro <250k` = `Metro <250k`,
        `Large urban, metro-adjacent` = `Non-metro >20k`,
        `Large urban, metro-non-adjacent` = `Non-metro >20k`,
        `Small urban, metro-adjacent` = `Non-metro >2500-20k`,
        `Small urban, metro-non-adjacent` = `Non-metro >2500-20k`)
        
        
                       
                       
                       
d$ur6 <- factor(dur$ur, levels = c(0,1,2,3,4,5,6,7,8,9,10), 
  labels = c("Metro", "Metro >1m", "Metro >250k-1m", 
  "Metro <250k", "Non-metro", "Large urban, metro-adjacent", 
  "Large urban, metro-non-adjacent", "Small urban, metro-adjacent", 
  "Small urban, metro-non-adjacent", "Rural, metro-adjacent",
  "Rural, metro-non-adjacent"))

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
  ylab("") + xlab("") + ggtitle("Age-adjusted death rate per 100,000") + stheme + 
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour="white")) +
  scale_color_manual(values=c("#3f007d", "#6a51a3", "#9e9ac8", 
    "#00441b", "#006d2c", "#238b45", "#41ab5d", "#74c476", "#a1d99b")) 

ggsave(here("figures", "rural-mort-trends.png"), 
       plot=b, width=11, height=7)

+ scale_color_brewer(type="seq", palette = "BuGn")
