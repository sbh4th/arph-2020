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
##### 
b <- read_tsv(here("data", "binge-old.tsv"), col_names=TRUE)

b$var <- ((b$Confidence_limit_High - b$Confidence_limit_Low) / (2 * 1.96))^2
b$wt <- 1 / b$var 


b1 <- b %>% filter(Locationabbr!="PR" & Locationabbr!="VI" &
                   Locationabbr!= "GU") %>%
  group_by(Year, Break_Out) %>%
  drop_na(Data_value) %>% 
  summarise(prev = mean(Data_value),
            wprev = weighted.mean(Data_value, wt))

b2 <- read_tsv(here("data", "binge-new.tsv"), col_names=TRUE)
b2$var <- ((b2$Confidence_limit_High - b2$Confidence_limit_Low) / (2 * 1.96))^2
b2$wt <- 1 / b2$var 

b3 <- b2 %>% filter(Locationabbr!="PR" & Locationabbr!="VI" &
                   Locationabbr!= "GU") %>%
  group_by(Year, Break_Out) %>%
  drop_na(Data_value) %>% 
  summarise(prev = mean(Data_value),
            wprev = weighted.mean(Data_value, wt))

binge <- bind_rows(b1,b3)

ggplot(b1, aes(x=Year, y=prev, colour=Break_Out)) + 
  geom_smooth() + geom_smooth(data=b3, aes(x=Year, y=prev, colour=Break_Out))


h <- read_tsv(here("data", "heavy-old.tsv"), col_names=TRUE)

h$var <- ((h$Confidence_limit_High - h$Confidence_limit_Low) / (2 * 1.96))^2
h$wt <- 1 / h$var 


h1 <- h %>% group_by(Year, Break_Out) %>%
  drop_na(Data_value) %>% 
  summarise(prev = mean(Data_value),
            wprev = weighted.mean(Data_value, wt))

ggplot(h1, aes(x=Year, y=prev, colour=Break_Out)) + 
  geom_line() 

h2 <- read_tsv(here("data", "heavy-new.tsv"), col_names=TRUE)

h2$var <- ((h2$Confidence_limit_High - h2$Confidence_limit_Low) / (2 * 1.96))^2
h2$wt <- 1 / h2$var 


h3 <- h2 %>% group_by(Year, Break_Out) %>%
  drop_na(Data_value) %>% 
  summarise(prev = mean(Data_value),
            wprev = weighted.mean(Data_value, wt))

ggplot(h3, aes(x=Year, y=prev, colour=Break_Out)) + 
  geom_line() 






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

    
## Same for CVD  
raw <- read_tsv(here("data", "aaasdr-cvd-1990-2017.txt"),
                 col_names=c("sex", "raceeth", "age4", "year", "aadr", "count", "pop"), col_types = "ddddddd")
# rescale year
raw$year <- raw$year + 1990

# race-ethnicity
raw$raceethf <- recode_factor(raw$raceeth, `0`= "Non-Hispanic White", `1`= "Non-Hispanic Black", `2`= "Non-Hispanic AI/AN", `3`= "Non-Hispanic API", `4`= "Hispanic")

# age-group
raw$age4f <- recode_factor(raw$age4, `0`= "15-34yrs", `1`= "35-54yrs", `2`= "55-64yrs", `3`= "65+yrs")

m <- ggplot(subset(raw, sex == 0 & age4>0), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_line(show.legend=T) + facet_wrap(~ age4f, nrow=1, scales="free") +
  scale_color_discrete(name="Race-Ethnicity") + labs(y = "", x = "") +
  ggtitle("Men") + stheme 

w <- ggplot(subset(raw, sex == 1 & age4>0), 
       aes(x = year, y = aadr, colour = as.factor(raceethf))) + 
  geom_line(show.legend=F) + facet_wrap(~ age4f, nrow=1 , scales="free") +
  scale_color_discrete(name="Race-Ethnicity") + labs(y = "", x = "") +
  ggtitle("Women") + stheme 

p <- m / w
p
