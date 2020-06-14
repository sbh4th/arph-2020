#  program:  single-age-trends-usa.R
#  task:     analyses of LE in OECD countries
#  input:    none
#  output:   
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-05-28

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### Bring in mortality by single ages and build dataset

# Non-Hispanics
sa <- read_tsv(here("data", "single-age-25-64-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "race", "rcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=4800,
  col_types = "cccfccddcddc")

# keep demographics, deaths, population, create rates
sa2 <- select(sa, gender, race, acode, year, deaths, pop) %>%
  mutate(racen = as.numeric(race), rate = deaths / pop * 100000)

# Hispanics
sah <- read_tsv(here("data", "single-age-hisp-25-64-1999-2018.txt"), skip=1, 
  col_names=c("notes", "gender", "gcode", "hisp", "hcode", "age", "acode", "year", 
  "ycode", "deaths", "pop", "crate"), n_max=3200,
  col_types = "cccfccddcddc")

# keep demographics, deaths, population, create rates
sah2 <- select(sah, gender, hisp, acode, year, deaths, pop) %>%
  filter(hisp == "Hispanic or Latino") %>%
  mutate(racen = 4, rate = deaths / pop * 100000)

# put Hispanics and non-Hispanics together
sar <- bind_rows(sa2, sah2)

# create new race group
sar$raceabb <- recode_factor(sar$racen, 
  `1` = "NH API", `2` = "NH Black", `3` = "NH White", `4`= "Hispanic")

sar$age <- paste(sar$acode, "yrs", sep=" ")

w <- ggplot(subset(sar, gender=="Female"), 
            aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=T) + facet_wrap(~age, scales="free", ncol=5) + 
  scale_color_manual(name="", values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + xlab("") + ylab("") + 
  ggtitle("Death rates per 100,000 by single years of age for women, 1999-2018") + 
  theme_classic() + 
  theme(axis.text.x = element_text(colour = "grey60"), 
        axis.text.y = element_text( colour="grey60"), 
        panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), 
        panel.grid.major.x = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white")) + 
  theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.background = element_rect(colour="white")) + theme(legend.position="top")

ggsave(here("figures", "single-ages-women.png"), plot=w, height=11, width=8.5)

m <- ggplot(subset(sar, gender=="Male"), 
            aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=T) + facet_wrap(~age, scales="free", ncol=5) + 
  scale_color_manual(name="", values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  xlab("") + ylab("") + 
  ggtitle("Death rates per 100,000 by single years of age for men, 1999-2018") + 
  theme_classic() + 
    theme(axis.text.x = element_text(colour = "grey60"), 
          axis.text.y = element_text( colour="grey60"), 
          panel.grid.major.y = element_line(linetype="dotted", colour="grey60"),
          panel.grid.major.x = element_line(colour="white"), 
          panel.grid.minor = element_line(colour="white")) + 
    theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.background = element_rect(colour="white")) + theme(legend.position="top")

ggsave(here("figures", "single-ages-men.png"), plot=m, height=11, width=8.5)




p25 <- ggplot(subset(sar, gender=="Male" & acode==25), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  geom_text_repel(data=subset(sar, gender=="Male" & acode==25 & year==2018), 
    nudge_x = .5, hjust= 0, point.padding = NA, segment.color="white") + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 300), breaks=c(100,200,300)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle="Age 25") +
  stheme

# create for loop to produce ggplot2 graphs 
age_list <- unique(26:29)
for (i in seq_along(age_list)) { 
  assign(paste0("p",age_list[i]), ggplot(subset(sar, gender=="Male" & acode==age_list[i]), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 300), breaks=c(100,200,300)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle=paste("Age", age_list[i], sep=" ")) + 
  stheme + theme(axis.text.y=element_blank()) )
}       

age2529 <- p25 + p26 + p27 + p28 + p29 + plot_layout(ncol=5)  


### Now for 30-34
p30 <- ggplot(subset(sar, gender=="Male" & acode==30), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  geom_text_repel(data=subset(sar, gender=="Male" & acode==30 & year==2018), 
    nudge_x = .5, hjust= 0, point.padding = NA, segment.color="white") + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 350), breaks=c(100,200,300)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle="Age 30") +
  stheme

# create for loop to produce ggplot2 graphs 
age_list <- unique(31:34)
for (i in seq_along(age_list)) { 
  assign(paste0("p",age_list[i]), ggplot(subset(sar, gender=="Male" & acode==age_list[i]), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 350), breaks=c(100,200,300)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle=paste("Age", age_list[i], sep=" ")) + 
  stheme + theme(axis.text.y=element_blank()) )
}       

age3034 <- p30 + p31 + p32 + p33 + p34 + plot_layout(ncol=5)  


### Now for 35-39
p35 <- ggplot(subset(sar, gender=="Male" & acode==35), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  geom_text_repel(data=subset(sar, gender=="Male" & acode==35 & year==2018), 
    nudge_x = .5, hjust= 0, point.padding = NA, segment.color="white") + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 450), breaks=c(150,300,450)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle="Age 35") +
  stheme

# create for loop to produce ggplot2 graphs 
age_list <- unique(36:39)
for (i in seq_along(age_list)) { 
  assign(paste0("p",age_list[i]), ggplot(subset(sar, gender=="Male" & acode==age_list[i]), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 450), breaks=c(150,300,450)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle=paste("Age", age_list[i], sep=" ")) + 
  stheme + theme(axis.text.y=element_blank()) )
}       

age3539 <- p35 + p36 + p37 + p38 + p39 + plot_layout(ncol=5)

### Now for 40-44
p40 <- ggplot(subset(sar, gender=="Male" & acode==40), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  geom_text_repel(data=subset(sar, gender=="Male" & acode==40 & year==2018), 
    nudge_x = .5, hjust= 0, point.padding = NA, segment.color="white") + 
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 700), breaks=c(200,400,600)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle="Age 35") +
  stheme

# create for loop to produce ggplot2 graphs 
age_list <- unique(41:44)
for (i in seq_along(age_list)) { 
  assign(paste0("p",age_list[i]), ggplot(subset(sar, gender=="Male" & acode==age_list[i]), 
              aes(x=year, y=rate, colour=raceabb, label=raceabb)) + 
  geom_line(show.legend=F) +
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_y_continuous(limits=c(0, 700), breaks=c(200,400,600)) +
  scale_x_continuous(limits=c(1999, 2021), breaks=c(2005, 2015)) +
  xlab("") + ylab("") + ggtitle("", subtitle=paste("Age", age_list[i], sep=" ")) + 
  stheme + theme(axis.text.y=element_blank()) )
}       

age4044 <- p40 + p41 + p42 + p43 + p44 + plot_layout(ncol=5)


age2544 <- age2529 / age3034 / age3539 / age4044

ggsave(here("figures", "single-ages-men.png"), plot=age2544, height=11, width=8.5)
