#  program:  suicide-trends.R
#  task:     trends in suicide rates
#  input:    suicide-1999-2018.txt; suicide-state-gender-1999-2018.txt
#  output:   
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-05-26

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
s <- read_tsv(here("data", "suicide-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=960, 
  col_types = "cccfccfdcddc")


# keep demographics, deaths, population, create rates
s2 <- select(s, gender, race, acode, year, deaths, pop) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

# Hispanics
sh <- read_tsv(here("data", "suicide-hisp-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "hisp", "hcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=640,
  col_types = "cccfccfdcddc")

# keep demographics, deaths, population, create rates
sh2 <- select(sh, gender, hisp, acode, year, deaths, pop) %>%
  filter(hisp == "Hispanic or Latino") %>%
  mutate(racen = 4, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
sr <- bind_rows(s2, sh2)

# standard population for age-adjustment
std <- tibble(acode = levels(s$acode), 
        std = c(64529, 71044, 80762, 81851, 72118, 62716, 48454, 38793))
std$acode <- factor(std$acode)
  
std2 <- mutate(std,
               stdwt = std / sum(std))
 
# add standard population to dataframe

suic <- left_join(sr, std2)

# create new race group
suic$raceabb <- recode_factor(suic$racen, 
  `1` = "NHAPI", `2` = "NHB", `3` = "NHW", `4`= "Hispanic")

suic$age <- paste(suic$acode, "yrs", sep=" ")

# s1 <- s %>% select(gender, race, acode, year, deaths, pop) %>% 
#   mutate(age=recode_factor(acode, `25-29`="25-29 yrs", 
#     `30-34`="30-34 yrs", `35-39`="35-39 yrs", `40-49`="45-49 yrs", 
#     `50-54`="50-54 yrs", `55-59`="55-59 yrs", `60-64`="60-64 yrs"))

stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

w <- ggplot(subset(suic, gender == "Female"), 
            aes(x=year, y=rate, colour=raceabb)) +
  geom_line(show.legend=F, size=1.25) + 
  facet_wrap(vars(age), ncol=4) + xlab("") + ylab("") + 
  ggtitle("Age-specific suicide death rates per 100,000", subtitle="Women") + 
  stheme

m <- ggplot(subset(suic, gender == "Male"), 
            aes(x=year, y=rate, colour=raceabb)) + 
  geom_line(show.legend=T, size=1.25) + facet_wrap(vars(age), ncol=4) +
  scale_x_continuous(breaks=c(2000, 2010)) +
  xlab("") + ylab("") + ggtitle("", subtitle="Men") + 
  stheme + theme(legend.position = "bottom", 
                 legend.title = element_text(size=16),
                 legend.text = element_text(size=14)) + 
  scale_colour_discrete(name = "Race-ethnicity")

p <- w / m

ggsave(here("figures", "age-suic-trends.png"), plot=p, height=11, width=9)


## data by state and gender
sg <- read_tsv(here("data", "suic-state-gender-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "state", "stcode", "year", 
    "ycode", "deaths", "pop", "crate", "arate"), n_max=2020,  
  col_types = "ccccdddddcc")

library(datasets)
st.reg <- data.frame(state = state.name, stabb = state.abb, 
                     reg = state.region, div = state.division)
st.dc <- data.frame(state="District of Columbia", stabb = "DC", 
                    reg="South", div="South Atlantic")
st.div <- bind_rows(st.reg, st.dc)

sg1 <- sg %>% select(gender, state, stcode, year, deaths, pop, arate) %>%
  left_join(st.div, by="state") %>%
  mutate(rate = deaths / pop * 100000) %>%
  arrange(state, gender, year) %>%
  group_by(state, gender) %>%
  mutate(rchng = rate / rate[1] * 100,
         achng = rate - rate[1])
  
p <- ggplot(subset(sg1, year==2018), aes(x=achng, y=rchng, 
                                         colour=stabb)) + 
  geom_point() + facet_wrap(~ gender) + theme(legend.position = "none")

p <- ggplot(subset(sg1, gender=="Male"), aes(x=year, y=rate, 
                                        group=state, colour=div)) + 
  geom_line() + 
  facet_wrap(~div, nrow=3) + theme_bw() +
  theme(legend.position = "none")

+ geom_label_repel(data=subset(sg1, gender=="Male" & year==2018), aes(x=year, y=rate, label=state)) 

arrange(subset(sg1, year==2018 & gender=="Female"), -achng)
arrange(subset(sg1, year==2018 & gender=="Male"), -achng)

# data by urban-rural
## data by state and gender
sur <- read_tsv(here("data", "suic-ur-gender-1999-2018.txt"), skip=1, 
  col_names=c("notes", "ur", "urcode", "gender", "gcode", "year", 
    "ycode", "deaths", "pop", "crate", "arate"),  n_max=240, 
  col_types = "cccccddddcd")

ggplot(sur, aes(x=year, y=arate, colour=ur)) + 
  geom_line() + 
  facet_wrap(~gender) + theme_bw() 
+
  theme(legend.position = "none")





##### 1  #####
##### Deaths for suicide (age-adjusted)
s <- read_tsv(here("data", "suic-race-aadr-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=160,
  col_types = "cccfcdcdddd")

# keep demographics, deaths, population, create rates
s2 <- select(s, gender, race, year, deaths, pop, aadr) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

sh <- read_tsv(here("data", "suic-hisp-aadr-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=40,
  col_types = "cccdcdddd")

sh2 <- select(sh, gender, year, deaths, pop, aadr) %>%
  mutate(racen = 5, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
sr <- bind_rows(s2, sh2) %>%
  select(-race) %>%
  mutate(racen = recode_factor(racen, `1`= "Non-Hispanic\nAIAN", 
        `2`= "Non-Hispanic\nAPI", `3`= "Non-Hispanic\nBlack", 
        `4`= "Non-Hispanic\nWhite", `5`= "Hispanic")) %>%
  filter(racen != "Non-Hispanic\nAIAN")

## set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))


# Non-Hispanic API
nhac <- "#e41a1c"
amp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nAPI"), shape=21, colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Men", subtitle="Non-Hispanic API") +
  theme(plot.subtitle = element_text(colour=nhac))

# Non-Hispanic Blacks
nhbc <- "#377eb8"
bmp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nBlack"), shape=21, colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic Black") +
  theme(plot.subtitle = element_text(colour=nhbc),
        axis.line.y = element_blank(), axis.text.y = element_blank())


# Non-Hispanic Whites
nhwc <- "#4daf4a"
wmp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nWhite"), shape=21, colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic White") +
  theme(plot.subtitle = element_text(colour=nhwc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Hispanic
hc <- "#984ea3"
hmp <- ggplot(subset(sr, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Male" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(sr, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Hispanic"), shape=21, colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,10,20,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Hispanic") +
  theme(plot.subtitle = element_text(colour=hc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())


mp <- (amp | bmp | wmp | hmp) + plot_annotation(subtitle = 'Men')
mp


## Now for women
# Non-Hispanic API
nhac <- "#e41a1c"
awp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nAPI"), shape=21, colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Women", subtitle="Non-Hispanic API") +
  theme(plot.subtitle = element_text(colour=nhac))

# Non-Hispanic Blacks
nhbc <- "#377eb8"
bwp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nBlack"), shape=21, colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic Black") +
  theme(plot.subtitle = element_text(colour=nhbc),
        axis.line.y = element_blank(), axis.text.y = element_blank())


# Non-Hispanic Whites
nhwc <- "#4daf4a"
wwp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nWhite"), shape=21, colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic White") +
  theme(plot.subtitle = element_text(colour=nhwc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Hispanic
hc <- "#984ea3"
hwp <- ggplot(subset(sr, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(sr, gender=="Female" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(sr, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Hispanic"), shape=21, colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Hispanic") +
  theme(plot.subtitle = element_text(colour=hc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())


wp <- (awp | bwp | wwp | hwp)
wp

# put men and women's plots togetehr
p <- wp / mp + plot_annotation(
  title = 'Age-adjusted death rates per 100,000 population for suicide', caption = 'Note: ICD-10 codes X60-X84', theme = theme(plot.title = element_text(size = 18)))
p

# export to file
ggsave(here("figures", "suic-race-trends.png"), plot=p, width=11, height=7)
