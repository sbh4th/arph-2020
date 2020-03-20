#  program:  urban-rural-plot.R
#  task:     analyses of LE in OECD countries
#  input:    seer-ur-1974.txt, seer-ur-2013.txt
#  output:   us-ur-trends.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-03-19

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(directlabels)
library(patchwork)

here::here()

d74 <- read_tsv(here("data", "seer-ur-1974.txt"), col_names=c("ur","year","aadr","deaths","pop"))
d74$urdef <- 0

d13 <- read_tsv(here("data", "seer-ur-2013.txt"), col_names=c("ur","year","aadr","deaths","pop"))
d13$urdef <- 4

d <- bind_rows(d74,d13)

d$ur <- factor(d$ur, levels = c(0,1,2,3,4,5,6,7,8,9,10), labels = c("Metro", "Metro >1m", 
  "Metro >250k-1m", "Metro <250k", "Non-metro", 
  "Large urban, metro-adjacent", "Large urban, metro-non-adjacent", 
  "Small urban, metro-adjacent", "Small urban, metro-non-adjacent", "Rural, metro-adjacent",
  "Rural, metro-non-adjacent"))

d$urdef <- factor(d$urdef, levels = c(0,4), labels = c("1974 rural definition", "2013 rural definition"))

d$yearc <- 1970 + 3*d$year
d$yearc <- ifelse(d$year==13, 2010, d$yearc)
d$yearc <- ifelse(d$year==14, 2015, d$yearc)

dur <- subset(d, ur=="Metro" | ur == "Non-metro") 

p <- ggplot(dur, aes(x=yearc, y=aadr, color=ur)) + geom_line(size=2, show.legend = F) + geom_dl(aes(label = ur), method=list("last.points", vjust = -2,hjust = .5, cex=1.2)) + facet_wrap(~urdef) + scale_x_continuous(limits=c(1970,2020), breaks=c(1970,1980,1990,2000,2010)) + scale_y_continuous(limits = c(650, 1300), breaks = seq(600, 1300, by = 100)) + theme_classic() + ylab("Death rate per 100,000 population") + xlab("") + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="grey60"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

ggsave(here("figures", "mort-def.png"), p, width=11, height=6.5)
