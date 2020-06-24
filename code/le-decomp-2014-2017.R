#  program:  le-decomp-2014-2017.R
#  task:     le decomposition graphs
#  input:    le-age-cause-decomp.csv
#  output:   le-age-decomp-2014-2017.png, le-cause-decomp-2014-2017.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-24

# 0
# load libraries
library(tidyverse)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
## Read in raw data and label it

# Raw data
rawd <- read_csv(here("data/cdc-wonder", "le-age-cause-decomp-2014-2017.csv"))

rawt <- rawd %>%
  group_by(sex, race, age) %>%
  summarise(total = sum(cont) * -1 )

tot <- aggregate(rawt$total, list(sex = rawt$sex, race = rawt$race), FUN = sum )
tot$age <- 12
colnames(tot)[3] <- "total"

raw <- rawt %>%
  bind_rows(tot)

# gender as factor
raw$gender <- recode_factor(raw$sex, `1`= "Women", 
  `2`= "Men")

# race-ethnicity as factor
raw$raceeth <- recode_factor(raw$race, `1`= "Non-Hispanic\nAPI", 
  `2`= "Non-Hispanic\nBlack", `3`= "Non-Hispanic\nWhite", 
  `4`= "Hispanic")

raw$age4f <- recode_factor(raw$age, `1` = "<1 yrs", `2` = " 1-15 yrs", 
  `3` = " 1-15 yrs", `4` = "15-44 yrs", `5` = "15-44 yrs",
  `6` = "15-44 yrs", `7` = "45-64 yrs", `8` = "45-64 yrs", 
  `9` = "65+ yrs", `10` = "65+ yrs", `11` = "65+ yrs",
  `12` = "Total change")


raw4 <- raw %>%
  group_by(gender, raceeth, age4f) %>%
  summarise(total = sum(total))


##### 2  #####
## Plot options and style

stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.x=element_text(size=16, colour="grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="white"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="grey60"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16, colour="grey60"), strip.background = element_rect(colour="white"))



##### 3  #####
## Plot of decomposition by age

# women
wr <- ggplot(subset(raw4, gender=="Women")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(data=subset(raw4, gender=="Women" & total<0), aes(y=age4f, weight=total), width=0.5, colour = "#e41a1c", fill="#e41a1c") + 
  geom_bar(data=subset(raw4, gender=="Women" & total>=0), aes(y=age4f, weight=total), width=0.5, colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 1), breaks=c(-1, 0, 1)) +
  labs(y = "", x = "") + 
  ggtitle("Age group contribution to change in life expectancy at birth, 2014-2017", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14))

# men
mr <- ggplot(subset(raw4, gender=="Men")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(data=subset(raw4, gender=="Men" & total<0), aes(y=age4f, weight=total), width=0.5, colour = "#e41a1c", fill="#e41a1c") + 
  geom_bar(data=subset(raw4, gender=="Men" & total>=0), aes(y=age4f, weight=total), width=0.5, colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 1), breaks=c(-1, 0, 1)) +
  labs(y = "", x = "Years") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text.y = element_text(size = 14))


# put both plots together
pr <- wr / mr 
pr

# export to file
ggsave(here("figures", "le-age-decomp-2014-2017.png"), 
       plot=pr, width=11, height=8.5)




##### 4  #####
##### Group contribution by cause of death

# total over causes
rawt <- rawd %>%
  group_by(sex, race, cod) %>%
  summarise(total = sum(cont) * -1 )

# create grouping for all causes ("total")
tot <- aggregate(rawt$total, list(sex = rawt$sex, race = rawt$race), FUN = sum )
tot$cod <- 15
colnames(tot)[3] <- "total"

# bind together
raw <- rawt %>%
  bind_rows(tot)

# gender as factor
raw$gender <- recode_factor(raw$sex, `1`= "Women", 
  `2`= "Men")

# race-ethnicity as factor
raw$raceeth <- recode_factor(raw$race, `1`= "Non-Hispanic\nAPI", 
  `2`= "Non-Hispanic\nBlack", `3`= "Non-Hispanic\nWhite", 
  `4`= "Hispanic")

raw$codf <- recode_factor(raw$cod, `1` = "Cardiovascular", `2` = " Cancers", 
  `3` = "Diabetes", `4` = "Alzheimer's", `5` = "Flu/Pneumonia",
  `6` = "HIV", `7` = "Respiratory disease", `8` = "Liver disease", 
  `9` = "Kidney disease", `10` = "Motor vehicle crashes", 
  `11` = "Unintentional poisoning", `12` = "Suicide", 
  `13` = "Homicide", `14` = "All other causes", 
  `15` = "Total change")

# summarize by gender, race, cause
raw15 <- raw %>%
  group_by(gender, raceeth, codf) %>%
  summarise(total = sum(total))




##### 5  #####
##### plot for cause of death

# women
wr <- ggplot(subset(raw15, gender=="Women")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(data=subset(raw15, gender=="Women" & total<0), aes(y=codf, weight=total), width=0.5, colour = "#e41a1c", fill="#e41a1c") + 
  geom_bar(data=subset(raw15, gender=="Women" & total>=0), aes(y=codf, weight=total), width=0.5, colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=codf, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw15$codf))) +
  scale_x_continuous(limits=c(-1.1, 1), breaks=c(-1, 0, 1)) +
  labs(y = "", x = "") + 
  ggtitle("Cause contribution to change in life expectancy at birth, 2014-2017", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14))
  
# men
mr <- ggplot(subset(raw15, gender=="Men")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(data=subset(raw15, gender=="Men" & total<0), aes(y=codf, weight=total), width=0.5, colour = "#e41a1c", fill="#e41a1c") + 
  geom_bar(data=subset(raw15, gender=="Men" & total>=0), aes(y=codf, weight=total), width=0.5, colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=codf, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw15$codf))) +
  scale_x_continuous(limits=c(-1.1, 1), breaks=c(-1, 0, 1)) +
  labs(y = "", x = "Years") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14))
  

# put both plots together
pr <- wr / mr 
pr

# export to file
ggsave(here("figures", "le-cause-decomp-2014-2017.png"), 
       plot=pr, width=11, height=10)

