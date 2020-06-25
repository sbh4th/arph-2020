#  program:  liverca-trends.R
#  task:     trends in liver cancer by race and gender
#  input:    liverca-age-1999-2018.txt, liverca-age-hisp-1999-2018.txt
#  output:   liverca-race-trends.png
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
##### Deaths for liver cancer

# non-Hispanics by race
lca <- read_tsv(here("data/cdc-wonder", "liverca-aadr-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=160,
  col_types = "cccfcdcdddd")

# keep demographics, deaths, population, create rates
lca2 <- select(lca, gender, race, year, deaths, pop, aadr) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

# Hispanics
lcah <- read_tsv(here("data/cdc-wonder", "liverca-aadr-hisp-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=40,
  col_types = "cccdcdddd")

# keep demographics, deaths, population, create rates
lcah2 <- select(lcah, gender, year, deaths, pop, aadr) %>%
  mutate(racen = 5, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
l <- bind_rows(lca2, lcah2) %>%
  select(-race) %>%
  mutate(racen = recode_factor(racen, `1`= "Non-Hispanic\nAIAN", 
        `2`= "Non-Hispanic\nAPI", `3`= "Non-Hispanic\nBlack", 
        `4`= "Non-Hispanic\nWhite", `5`= "Hispanic")) %>%
  filter(racen != "Non-Hispanic\nAIAN")


##### 2  #####
##### set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))



##### 3  #####
##### Plot for men

# Non-Hispanic API men
nhac <- "#e41a1c"
amp <- ggplot(subset(l, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Male" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(l, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nAPI"), shape=21, colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,21), breaks=c(0,10,20)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Men", subtitle="Non-Hispanic API") +
  theme(plot.subtitle = element_text(colour=nhac))

# Non-Hispanic Black men
nhbc <- "#377eb8"
bmp <- ggplot(subset(l, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Male" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(l, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nBlack"), shape=21, colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,21), breaks=c(0,10,20)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic Black") +
  theme(plot.subtitle = element_text(colour=nhbc),
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Non-Hispanic White men
nhwc <- "#4daf4a"
wmp <- ggplot(subset(l, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Male" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(l, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nWhite"), shape=21, colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,21), breaks=c(0,10,20)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic White") +
  theme(plot.subtitle = element_text(colour=nhwc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Hispanic men
hc <- "#984ea3"
hmp <- ggplot(subset(l, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Male" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(l, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Hispanic"), shape=21, colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,21), breaks=c(0,10,20)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Hispanic") +
  theme(plot.subtitle = element_text(colour=hc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# put all plots together for men
mp <- (amp | bmp | wmp | hmp) + plot_annotation(subtitle = 'Men')
mp


##### 4  #####
##### Plot for women

# Non-Hispanic API women
nhac <- "#e41a1c"
awp <- ggplot(subset(l, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Female" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(l, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nAPI"), shape=21, colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Women", subtitle="Non-Hispanic API") +
  theme(plot.subtitle = element_text(colour=nhac))

# Non-Hispanic Black women
nhbc <- "#377eb8"
bwp <- ggplot(subset(l, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Female" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(l, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nBlack"), shape=21, colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic Black") +
  theme(plot.subtitle = element_text(colour=nhbc),
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Non-Hispanic White women
nhwc <- "#4daf4a"
wwp <- ggplot(subset(l, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Female" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(l, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nWhite"), shape=21, colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic White") +
  theme(plot.subtitle = element_text(colour=nhwc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Hispanic women
hc <- "#984ea3"
hwp <- ggplot(subset(l, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(l, gender=="Female" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(l, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Hispanic"), shape=21, colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Hispanic") +
  theme(plot.subtitle = element_text(colour=hc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# put all plots together for women
wp <- (awp | bwp | wwp | hwp)
wp

# put men and women's plots together
p <- mp / wp + plot_annotation(
  title = 'Age-adjusted death rates per 100,000 population for liver cancer')
p

# export to file
ggsave(here("figures", "liverca-race-trends.png"), plot=p, width=11, height=7)

