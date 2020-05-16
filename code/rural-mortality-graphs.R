#  program:  rural-mortality-graphs.R
#  task:     urban-rural trends
#  input:    jp-le-age-sex-race.txt
#  output:   le-gender-gap.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-04-30

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)
library(directlabels)

here::here()



d74 <- read_tsv("seer-ur-1974.txt", col_names=c("ur","year","aadr","deaths","pop"))
d74$urdef <- 0

d13 <- read_tsv("seer-ur-2013.txt", col_names=c("ur","year","aadr","deaths","pop"))
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

p <- ggplot(dur, aes(x=yearc, y=aadr, color=ur)) + geom_line(size=2, show.legend = F) + 
  geom_dl(aes(label = ur), method=list("last.points", vjust = -2,hjust = .5, cex=1.2)) + facet_wrap(~urdef) + 
  scale_x_continuous(limits=c(1970,2020), breaks=c(1970,1980,1990,2000,2010)) + 
  scale_y_continuous(limits = c(650, 1300), breaks = seq(600, 1300, by = 100)) +
  theme_classic() + ylab("Death rate per 100,000 population") + xlab("") + 
  theme(axis.text.x = element_text(size = 18), axis.title.y=element_text(size=16, angle=90), 
        axis.text.y = element_text(size = 20), panel.grid.major = element_line(colour="white"),
        strip.text = element_text(size = 16), strip.background = element_rect(colour="white"),
        panel.grid.major.y = element_line(colour="gray"), panel.spacing = unit(2, "lines"),
        panel.grid.major.x = element_blank())

ggsave("mort-def.png", p, dpi=600, width=11, height=6.5)

dp <- dur %>%
  group_by(ur, urdef) %>%
  mutate(pop, cpop = (pop / pop[1])*100)

ggplot(dp, aes(x=yearc, y=cpop, color=ur)) + geom_line(size=2) + facet_wrap(~urdef) 

dm <- subset(dur, ur=="Non-metro")

ggplot(dm, aes(x=year, y=aadr, color=urdef)) + geom_line(size=2, sh)


ggplot(d, aes(x=Year, y=Age.Adjusted.Rate, colour=X2013.Urbanization)) + geom_line(aes(colour=X2013.Urbanization), size=1.5, show.legend=FALSE) + geom_dl(aes(label = X2013.Urbanization), method=list("last.points", hjust = -.05,cex=1.2)) + scale_x_continuous(limits=c(1998,2022),breaks=c(2000,2005,2010,2015)) + theme_classic() + ylab("Rate per 100,000 population") + xlab("") + theme(axis.text.x = element_text(size = 20), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 20), panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + scale_color_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#de2d26','#a50f15','#9ecae1'))

dr$reg <- as.numeric(dr$Census.Region.Code)
dr$reg <- factor(dr$reg, levels=c(1,2,3,4), labels=c("Northeast", "Midwest", "South", "West"))

dr$urb <- factor(dr$X2013.Urbanization.Code, levels=c(1,2,3,4,5,6), labels=c("Large Central Metro", "Large Fringe Metro", "Medium Metro", "Small Metro", "Micropolitan (Nonmetro)", "Non-Core (Nonmetro)"))

ggplot(dr, aes(x=Year, y=Age.Adjusted.Rate, colour=urb)) + geom_line(aes(colour=urb), size=1.5, show.legend=TRUE) + scale_x_continuous(limits=c(1998,2018),breaks=c(2000,2005,2010,2015)) + facet_wrap(~ reg, nrow=1) + theme_classic() + ylab("Rate per 100,000 population") + xlab("") + theme(axis.text.x = element_text(size = 20), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 20), panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + scale_color_manual(name="2013 Urbanization scheme", values=c('#08519c','#3182bd','#6baed6', '#9ecae1', '#de2d26','#a50f15')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#9ecae1','#de2d26','#a50f15')) + theme(strip.text = element_text(size=14)) + theme(strip.background = element_blank()) + theme(legend.position="top")
  
du$reg <- as.numeric(du$Census.Region.Code)
du$reg <- factor(du$reg, levels=c(1,2,3,4), labels=c("Northeast", "Midwest", "South", "West"))

du$urb <- factor(du$X2013.Urbanization.Code, levels=c(1,2,3,4,5,6), labels=c("Large Central Metro", "Large Fringe Metro", "Medium Metro", "Small Metro", "Micropolitan (Nonmetro)", "Non-Core (Nonmetro)"))

ggplot(du, aes(x=Year, y=Age.Adjusted.Rate, colour=urb)) + geom_line(aes(colour=urb), size=1.5, show.legend=TRUE) + scale_x_continuous(limits=c(1998,2018),breaks=c(2000,2005,2010,2015)) + facet_wrap(~ reg) + theme_classic() + ylab("Rate per 100,000 population") + xlab("") + theme(axis.text.x = element_text(size = 20), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 20), panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + scale_color_manual(name="2013 Urbanization scheme", values=c('#08519c','#3182bd','#6baed6', '#9ecae1', '#de2d26','#a50f15')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#9ecae1','#de2d26','#a50f15')) + theme(strip.text = element_text(size=14)) + theme(strip.background = element_blank())


dt$reg <- factor(dt$Region, levels=c(0,1,2,3), labels=c("Northeast", "Midwest", "South", "West"))

dt$urb <- factor(dt$Rural, levels=c(0,1,2,3,4,5), labels=c("Large Central Metro", "Large Fringe Metro", "Medium Metro", "Small Metro", "Micropolitan (Nonmetro)", "Non-Core (Nonmetro)"))

dt$yr <- factor(dt$Year, levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14), labels=c("1969-1971", "1972-1974", "1975-1977", "1978-1980", "1981-1983", "1984-1986", "1987-1989", "1990-1992", "1993-1995", "1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2011", "2012-2016"))

ggplot(dt, aes(x=Year, y=Rate, colour=urb)) + geom_line(aes(colour=urb), size=1.5, show.legend=TRUE) + scale_x_continuous(limits=c(0,15), breaks=c(2,7,13), labels=c("1975", "1990","2010")) + facet_wrap(~ reg) + theme_classic() + ylab("Rate per 100,000 population") + xlab("") + theme(axis.text.x = element_text(size = 20), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 20), panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + scale_color_manual(name="2013 Urbanization scheme", values=c('#08519c','#3182bd','#6baed6', '#9ecae1', '#de2d26','#a50f15')) + scale_fill_manual(values=c('#08519c','#3182bd','#6baed6','#9ecae1','#de2d26','#a50f15')) + theme(strip.text = element_text(size=14)) + theme(strip.background = element_blank())

scale_x_continuous(limits=c(0,15),breaks=c("1970",3,7,11,15)) +
  
  
ggplot(dt, aes(x=yr, y=Rate)) + geom_line(aes(colour=urb), size=1.5, show.legend=TRUE) + facet_wrap(~ reg) + theme_classic()
