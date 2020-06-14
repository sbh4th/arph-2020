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
lb <- read_csv(here("data", "lbpain_m.csv"), col_names=TRUE) %>%
  mutate(source = 1)

np <- read_csv(here("data", "npain_m.csv"), col_names=TRUE) %>%
  mutate(source = 2)

fp <- read_csv(here("data", "fpain_m.csv"), col_names=TRUE) %>%
  mutate(source = 3)

ap <- read_csv(here("data", "anypain_m.csv"), col_names=TRUE) %>%
  mutate(source = 4)

pain <- bind_rows(lb, np, fp, ap) %>%
  mutate(gender = recode_factor(male, `0` = "Women", `1` = "Men"),
         educ = recode_factor(univ, `0` = "University or more", 
                              `1` = "<University"),
         time = recode_factor(year, `1` = "1997-99", `2` = "2000-02",
                              `3` = "2003-05", `4` = "2006-08", 
                              `5` = "2009-11", `6` = "2012-2014",
                              `7` = "2015-2018"),
         source = recode_factor(source, `1` = "Lower back", `2` = "Neck",
                                `3` = "Facial", `4` = "Any"))

## set some plot characteristics
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

m <- ggplot(subset(pain, gender=="Men"), 
       aes(x = year, y = estimate, colour=educ)) +
  geom_line(show.legend=F, size=1.5) + facet_wrap(~ source, nrow=1) + 
  scale_y_continuous(limits=c(0,.5), breaks=c(0,.25,.50)) +
  scale_x_continuous(breaks=c(2,4,6), labels=c("2" = "2000-\n02", "4" = "2006-\n08",
                              "6" = "2012-\n2014")) +
  scale_color_manual(name="Education", values=c("#4daf4a","#377eb8")) + 
  labs(y = "", x = "") + ggtitle("Men") + stheme 

w <- ggplot(subset(pain, gender=="Women"), 
       aes(x = year, y = estimate, colour=educ)) +
  geom_line(show.legend=F, size=1.5) + facet_wrap(~ source, nrow=1) + 
  scale_y_continuous(limits=c(0,.5), breaks=c(0,.25,.50)) +
  scale_x_continuous(breaks=c(2,4,6), labels=c("2" = "2000-\n02", "4" = "2006-\n08",
                              "6" = "2012-\n14")) +
  scale_color_manual(name="Education", values=c("#4daf4a","#377eb8")) + 
  geom_text(data=subset(pain, gender=="Women" & source=="Lower back"), aes(y = estimate), label = "<University\ndegree", x = 1, y = 0.4, size = 4, hjust=0, colour = "#377eb8") +
  geom_text(data=subset(pain, gender=="Women" & source=="Lower back"), aes(y = estimate), label = "University\nor more", x = 1, y = 0.2, size = 4, hjust=0, colour = "#4daf4a") +
  labs(y = "", x = "") + ggtitle("Women") + stheme 

p <- w / m + plot_layout(guides = "collect") & theme(legend.position = 'none', legend.text = element_text(size=12)) 

p2 <- p + plot_annotation(
  title = 'Probability of reporting pain lasting a day or more, 1997-99 to 2015-18', theme = theme(plot.title = element_text(size = 18)))
p2

ggsave(here("figures", "pain-trends-educ.png"), plot=p2, width=11, height=8.5)


    