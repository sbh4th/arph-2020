#  program:  etoh-trends.R
#  task:     trends on alcohol-related mortality
#  input:    etoh-k70-1999-2018.txt, etoh-f10-1999-2018.txt
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
##### Deaths for cirrhosis and chronic liver dx ICD10 K70
etoh <- read_tsv(here("data", "etoh-k70-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=40,
  col_types = "cccdcdddd")

etoh1 <- mutate(etoh, cause="Cirrhosis")

##### Deaths for alcohol disorders, acute intox ICD10 F10
etoh <- read_tsv(here("data", "etoh-f10-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=40,
  col_types = "cccdcdddd")

etoh2 <- mutate(etoh, cause="Dependence")

## Bind together
etohd <- bind_rows(etoh1, etoh2)


##### 2  #####
##### Trends for

## set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

e <- ggplot(etohd, aes(x=year, y=aadr, colour=cause)) + 
  geom_line(show.legend=F) + facet_wrap(~gender) + stheme


##### 3  #####
##### Age-specific death rates from SEER for cirrhosis and CVD
etoh <- read_tsv(here("data", "etoh-k70-age-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "year", "ycode", 
              "age", "acode", "deaths", "pop", "crate"), n_max=320, 
  col_types = "cccdcccddc")

etoh1 <- etoh %>% select(gender, acode, year, deaths, pop) %>% 
  mutate(age=recode_factor(acode, `15-19`="15-24yrs",
  `20-24`="15-24yrs", `25-34`="25-34yrs", `35-44`="35-44yrs",
  `45-54`="45-54yrs", `55-64`="55-64yrs", `65-74`="65-74yrs",
  `75-84`="75-84yrs",`85+`="85+yrs"), 
  rate = deaths / pop * 100000, cause="Cirrhosis")

ggplot(etoh1, aes(x=year, y=rate, colour=gender)) + geom_point(alpha=0.2) + 
  geom_smooth() + stheme + facet_wrap(~acode, scales="free") + 
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) + 
  xlab("") + ylab("") + ggtitle("Age-adjusted cirrhosis deaths rates per 100k")

    
## Same for   
raw <- read_tsv(here("data", "aaasdr-etoh-1990-2017.txt"),
                 col_names=c("sex", "raceeth", "age4", "year", "aadr", "count", "pop"), col_types = "ddddddd")
# rescale year
raw$year <- raw$year + 1990

# race-ethnicity
raw$raceethf <- recode_factor(raw$raceeth, `0`= "Non-Hispanic White", `1`= "Non-Hispanic Black", `2`= "Non-Hispanic AI/AN", `3`= "Non-Hispanic API", `4`= "Hispanic")

# age-group
raw$age4f <- recode_factor(raw$age4, `0`= "15-34yrs", `1`= "35-54yrs", `2`= "55-64yrs", `3`= "65+yrs")

m <- ggplot(subset(raw, sex == 0 & raceeth!=2), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_smooth(aes(colour=as.factor(raceethf))) + facet_wrap(~ age4f, scales="free") +
  scale_color_discrete(name="Race-Ethnicity") + labs(y = "", x = "") +
  ggtitle("Men") + stheme 

w <- ggplot(subset(raw, sex == 1 & raceeth!=2), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_smooth() + facet_wrap(~ age4f , scales="free") +
  scale_color_discrete(name="Race-Ethnicity") + labs(y = "", x = "") +
  ggtitle("Women") + stheme 

p <- m / w
p



##### 1  #####
##### Deaths for cirrhosis and chronic liver dx ICD10 K70
e <- read_tsv(here("data", "liver-race-aadr-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=160,
  col_types = "cccfcdcdddd")

# keep demographics, deaths, population, create rates
e2 <- select(e, gender, race, year, deaths, pop, aadr) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

eh <- read_tsv(here("data", "liver-hisp-aadr-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "year", "ycode", 
              "deaths", "pop", "crate", "aadr"), n_max=40,
  col_types = "cccdcdddd")

eh2 <- select(eh, gender, year, deaths, pop, aadr) %>%
  mutate(racen = 5, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
er <- bind_rows(e2, eh2) %>%
  select(-race) %>%
  mutate(racen = recode_factor(racen, `1`= "Non-Hispanic\nAIAN", 
        `2`= "Non-Hispanic\nAPI", `3`= "Non-Hispanic\nBlack", 
        `4`= "Non-Hispanic\nWhite", `5`= "Hispanic")) %>%
  filter(racen != "Non-Hispanic\nAIAN")

## set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))


# Non-Hispanic API
nhac <- "#e41a1c"
amp <- ggplot(subset(er, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Male" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(er, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nAPI"), shape=21, colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,15,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Men", subtitle="Non-Hispanic API") +
  theme(plot.subtitle = element_text(colour=nhac))

# Non-Hispanic Blacks
nhbc <- "#377eb8"
bmp <- ggplot(subset(er, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Male" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(er, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nBlack"), shape=21, colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,15,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic Black") +
  theme(plot.subtitle = element_text(colour=nhbc),
        axis.line.y = element_blank(), axis.text.y = element_blank())


# Non-Hispanic Whites
nhwc <- "#4daf4a"
wmp <- ggplot(subset(er, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Male" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(er, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nWhite"), shape=21, colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,15,30)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic White") +
  theme(plot.subtitle = element_text(colour=nhwc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Hispanic
hc <- "#984ea3"
hmp <- ggplot(subset(er, gender=="Male"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Male" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(er, gender=="Male" & (year==1999 | year==2018) & 
    racen=="Hispanic"), shape=21, colour="white", fill=hc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,30), breaks=c(0,15,30)) +
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
awp <- ggplot(subset(er, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Female" & racen=="Non-Hispanic\nAPI"),
            colour=nhac, size=1.5) +
  geom_point(data=subset(er, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nAPI"), shape=21, colour="white", fill=nhac, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Women", subtitle="Non-Hispanic API") +
  theme(plot.subtitle = element_text(colour=nhac))

# Non-Hispanic Blacks
nhbc <- "#377eb8"
bwp <- ggplot(subset(er, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Female" & racen=="Non-Hispanic\nBlack"),
            colour=nhbc, size=1.5) +
  geom_point(data=subset(er, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nBlack"), shape=21, colour="white", fill=nhbc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic Black") +
  theme(plot.subtitle = element_text(colour=nhbc),
        axis.line.y = element_blank(), axis.text.y = element_blank())


# Non-Hispanic Whites
nhwc <- "#4daf4a"
wwp <- ggplot(subset(er, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Female" & racen=="Non-Hispanic\nWhite"),
            colour=nhwc, size=1.5) +
  geom_point(data=subset(er, gender=="Female" & (year==1999 | year==2018) & 
    racen=="Non-Hispanic\nWhite"), shape=21, colour="white", fill=nhwc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,5,10)) +
  scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("", subtitle="Non-Hispanic White") +
  theme(plot.subtitle = element_text(colour=nhwc), 
        axis.line.y = element_blank(), axis.text.y = element_blank())

# Hispanic
hc <- "#984ea3"
hwp <- ggplot(subset(er, gender=="Female"), 
       aes(x=year, y=aadr, group=racen))  + 
  geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(er, gender=="Female" & racen=="Hispanic"),
            colour=hc, size=1.5) +
  geom_point(data=subset(er, gender=="Female" & (year==1999 | year==2018) & 
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
  title = 'Age-adjusted death rates per 100,000 population for chronic liver disease, cirrhosis, and alcohol use', caption = 'Note: ICD-10 codes K70, K73, K47 (Chronic liver disease and cirrhosis), F10 (Alcohol use disorders)', theme = theme(plot.title = element_text(size = 18)))
p

# export to file
ggsave(here("figures", "liver-race-trends.png"), plot=p, width=11, height=7)
