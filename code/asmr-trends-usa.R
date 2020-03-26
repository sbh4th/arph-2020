#  program:  cod-plot-use.R
#  task:     analyses of LE in OECD countries
#  input:    none
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
##### Overall age-adjusted mortality by gender and race
asmr <- read_tsv(here("data", "asmr-1968-1978.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=858, 
  col_types = "ccccccfdcddd")

asmr1 <- asmr %>% select(gender, race, acode, year, deaths, pop) %>% 
  mutate(age=recode_factor(acode, `1`="<1yrs", `1-4`="1-4yrs", 
  `5-9`="5-14yrs", `10-14`="5-14yrs", `15-19`="15-24yrs",
  `20-24`="15-24yrs", `25-34`="25-34yrs", `35-44`="35-44yrs",
  `45-54`="45-54yrs", `55-64`="55-64yrs", `65-74`="65-74yrs",
  `75-84`="75-84yrs",`85+`="85+yrs"))

asmr <- read_tsv(here("data", "asmr-1979-1998.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=1560,
  col_types = "ccccccfdcddd")
  
asmr2 <- asmr %>% select(gender, race, acode, year, deaths, pop) %>% 
  mutate(age=recode_factor(acode, `1`="<1yrs", `1-4`="1-4yrs", 
  `5-9`="5-14yrs", `10-14`="5-14yrs", `15-19`="15-24yrs",
  `20-24`="15-24yrs", `25-34`="25-34yrs", `35-44`="35-44yrs",
  `45-54`="45-54yrs", `55-64`="55-64yrs", `65-74`="65-74yrs",
  `75-84`="75-84yrs",`85+`="85+yrs"))

asmr <- bind_rows(asmr1, asmr2) 

asmr3 <- select(asmr, -acode) %>%
  group_by(gender, race, age, year) %>% 
  summarise_all(list(sum)) %>%
  mutate(rate = deaths / pop * 100000)


asmr <- read_tsv(here("data", "asmr-1999-2017.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=1760,
  col_types = "ccccccfdcddd")

asmr4 <- asmr %>% select(gender, race, acode, year, deaths, pop) %>%
  mutate(age=recode_factor(acode, `1`="<1yrs", `1-4`="1-4yrs", 
  `5-14`="5-14yrs", `15-24`="15-24yrs", `25-34`="25-34yrs", 
  `35-44`="35-44yrs", `45-54`="45-54yrs", `55-64`="55-64yrs", 
  `65-74`="65-74yrs", `75-84`="75-84yrs", `85+`="85+yrs"))

asmr5 <- select(asmr4, -acode) %>%
  mutate(rate = deaths / pop * 100000)

asmrd <- bind_rows(asmr3, asmr5)

stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank())

ggplot(subset(asmrd, gender=="Male"), aes(x=year, y=rate, colour=race)) + 
  geom_line(show.legend=F) + facet_wrap(~age, scales="free", ncol=3)

ggplot(subset(asmr5, gender=="Male"), aes(x=year, y=rate, colour=race)) + 
  geom_line(show.legend=F) + facet_wrap(~age, scales="free", ncol=3)

ggplot(subset(asmr5, gender=="Female"), aes(x=year, y=rate, colour=race)) + 
  geom_line(show.legend=T) + facet_wrap(~age, scales="free", ncol=3)

# generate relative values of mortality, indexed to 2010
asmrr <- asmr5 %>%
  group_by(gender, race, age) %>%
  mutate(rrate = rate / rate[12] * 100) %>%
  mutate(ag = ifelse(rrate[20] > 100, 1, 0))

ggplot(subset(asmrr, gender=="Female"), aes(x=year, y=rrate, colour=race)) + 
  geom_line(show.legend=T) + facet_wrap(~age, ncol=3)

af <- ggplot(subset(asmrr, gender=="Female" & year>=2010), aes(x=year, y=rrate, colour=age, label=age)) + geom_line(datashow.legend=F) + geom_text_repel(data=subset(asmrr, gender=="Female" & year==2018), hjust=1, nudge_x=3) + scale_x_continuous(limits=c(2010, 2022), breaks=c(2010, 2014, 2018)) + facet_wrap(~race) + stheme + theme(strip.text = element_text(size = 16), strip.background = element_rect(colour="white")) + theme(panel.spacing = unit(2, "lines")) + xlab("") + ylab("") + ggtitle("Death rates per 100,000 population")

ggplot(subset(asmrr, gender=="Female" & year>=2010), aes(x=year, y=rrate, colour=age, label=age)) + geom_line(data=subset(asmrr, gender=="Female" & year>=2010 & ag==0), aes(x=year, y=rrate, group=age), show.legend=F, colour="grey") + geom_line(data=subset(asmrr, gender=="Female" & year>=2010 & ag==1), aes(x=year, y=rrate, colour=age), show.legend=F) + geom_text_repel(data=subset(asmrr, gender=="Female" & year==2018 & ag==1), hjust=0) + scale_x_continuous(limits=c(2010, 2022), breaks=c(2010, 2014, 2018))  + facet_wrap(~race) + stheme + theme(strip.text = element_text(size = 16), strip.background = element_rect(colour="white")) + theme(panel.spacing = unit(2, "lines")) + xlab("") + ylab("") + ggtitle("Death rates per 100,000 population") + scale_colour_brewer()
                                                                                                              
ggplot(subset(asmrr, gender=="Male" & year>=2010 & race=="White"), aes(x=year, y=rrate, colour=age, label=age)) + geom_line(show.legend=F) + geom_text_repel(data=subset(asmrr, gender=="Female" & year==2018), hjust=1, nudge_x=3) + scale_x_continuous(limits=c(2010, 2022), breaks=c(2010, 2014, 2018)) + facet_wrap(~age) + stheme + theme(strip.text = element_text(size = 16), strip.background = element_rect(colour="white")) + theme(panel.spacing = unit(2, "lines")) + xlab("") + ylab("") + ggtitle("Death rates per 100,000 population")

ggplot(subset(asmrr, gender=="Male" & year>=2010 & race=="White"), aes(x=year, y=rrate, group=age)) + geom_line(show.legend=F, colour="grey") + geom_line(data=subset(asmrr, gender=="Male" & year>=2010 & race=="White" & age=="<1yrs"), colour="#d95f02", size=1.5) + scale_x_continuous(limits=c(2010, 2018), breaks=c(2010, 2014, 2018)) + stheme + xlab("") + ylab("") + ggtitle("<1 year") + theme(plot.title = element_text(colour="#d95f02")) + scale_y_continuous(limits=c(70,130))

# create list of age groups in data to loop over 
age_list <- unique(asmrr$age)
  
# create for loop to produce ggplot2 graphs 
for (i in seq_along(age_list)) { 
  assign(paste0("p",i), ggplot(subset(asmrr, gender=="Male" & year>=2010 & race=="White"), aes(x=year, y=rrate, group=age)) + geom_hline(yintercept=100, linetype="dashed", color = "black") + geom_line(show.legend=F, colour="grey") + geom_line(data=subset(asmrr, gender=="Male" & year>=2010 & race=="White" & age==age_list[i]), colour="#d95f02", size=1.5) + geom_point(data=subset(asmrr, gender=="Male" & year==2018 & race=="White" & age==age_list[i]), shape=21, colour="white", fill="#d95f02", size=3, stroke=2)  + annotate("text", label = age_list[i], x = 2010, y = 135, size = 5, hjust=0, colour = "#d95f02") + scale_x_continuous(limits=c(2009, 2019), breaks=c(2010, 2014, 2018)) + scale_y_continuous(limits=c(70,140)) + stheme + xlab("") + ylab("") + theme(axis.text.x = element_text(size = 12, margin=margin(-20,0,0,0))) )
}

agep <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + 
  p9 + p10 + p11 + plot_layout(ncol=4)

ggsave(here("figures", "age-mort-trends-2010-wm.png"), plot=agep, width=11, height=8.5)

ggplot(subset(asmrr, gender=="Male" & year>=2010 & race=="White"), aes(x=year, y=rrate, group=age)) + geom_line(show.legend=F, colour="grey") + geom_line(data=subset(asmrr, gender=="Male" & year>=2010 & race=="White" & age=="25-34yrs"), colour="#d95f02", size=1.5) + geom_point(data=subset(asmrr, gender=="Male" & year==2018 & race=="White" & age=="25-34yrs"), shape=21, colour="white", fill="#d95f02",size=3, stroke=3)  + annotate("text", label = "25-34yrs", x = 2010, y = 125, size = 5, hjust=0, colour = "#d95f02") + scale_x_continuous(limits=c(2010, 2019), breaks=c(2010, 2014, 2018)) + scale_y_continuous(limits=c(70,140)) + stheme + xlab("") + ylab("") + theme(axis.text.x = element_text(size = 14))
