#  program:  le-age-cause-decomp-2010-2018.R
#  task:     le decomposition graphs
#  input:    le-age-cause-decomp-2010-2018.csv
#  output:   
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-05-27

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
rawd <- read_csv(here("data", "le-age-cause-decomp-2010-2018.csv"))

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

# different color for total change 
# (reversed so need to assign it to <1y)
a <- levels(raw$age4f)
b <- ifelse(a == "<1 yrs", "#e41a1c", "grey60")

# women
w <- ggplot(subset(raw4, gender=="Women")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=age4f, weight=total), width=0.5, 
           colour = "#377eb8", fill = "#377eb8") +
  geom_bar(data=subset(raw4, gender=="Women" & age4f=="Total change"),
           aes(y=age4f, weight=total), width=0.5,
           colour = "#e41a1c", fill="#e41a1c") +
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.2, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "") + 
  ggtitle("Age group contribution to change in life expectancy at birth, 2010-2018", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14, colour = b))
  
# men
m <- ggplot(subset(raw4, gender=="Men")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=age4f, weight=total), width=0.5, 
           colour = "#377eb8", fill = "#377eb8") + 
  geom_bar(data=subset(raw4, gender=="Men" & age4f=="Total change"),
           aes(y=age4f, weight=total), width=0.5,
           colour = "#e41a1c", fill="#e41a1c") +
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.2, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "Years") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14, colour = b))

# put both plots together
p <- w / m 
p

# export to file
ggsave(here("figures", "le-age-decomp-2010-2018.png"), plot=p, width=11, height=8.5)

# alternative color scheme
wr <- ggplot(subset(raw4, gender=="Women")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(data=subset(raw4, gender=="Women" & total<0), aes(y=age4f, weight=total), width=0.5, colour = "#e41a1c", fill="#e41a1c") + 
  geom_bar(data=subset(raw4, gender=="Women" & total>=0), aes(y=age4f, weight=total), width=0.5, colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.2, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "Years") + 
  ggtitle("Age group contribution to change in life expectancy at birth, 2014-2017", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14))

mr <- ggplot(subset(raw4, gender=="Men")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(data=subset(raw4, gender=="Men" & total<0), aes(y=age4f, weight=total), width=0.5, colour = "#e41a1c", fill="#e41a1c") + 
  geom_bar(data=subset(raw4, gender=="Men" & total>=0), aes(y=age4f, weight=total), width=0.5, colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=age4f, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.2, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw4$age4f))) +
  scale_x_continuous(limits=c(-1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "Years") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text.y = element_text(size = 14))


# put both plots together
pr <- wr / mr 
pr

# export to file
ggsave(here("figures", "le-age-decomp-2010-2018.png"), 
       plot=pr, width=11, height=8.5)



##### 4  #####
## Plot of decomposition by cause of death

rawt <- rawd %>%
  group_by(sex, race, cod) %>%
  summarise(total = sum(cont) * -1 )

tot <- aggregate(rawt$total, list(sex = rawt$sex, race = rawt$race), FUN = sum )
tot$cod <- 15
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

raw$codf <- recode_factor(raw$cod, `1` = "Cardiovascular", `2` = " Cancers", 
  `3` = "Diabetes", `4` = "Alzheimer's", `5` = "Flu/Pneumonia",
  `6` = "HIV", `7` = "Respiratory disease", `8` = "Liver disease", 
  `9` = "Kidney disease", `10` = "Motor vehicle crashes", 
  `11` = "Unintentional poisoning", `12` = "Suicide", 
  `13` = "Homicide", `14` = "All other causes", 
  `15` = "Total change")

raw15 <- raw %>%
  group_by(gender, raceeth, codf) %>%
  summarise(total = sum(total))

##### 3  #####
# different color for total change 
# (reversed so need to assign it to CVD)
a <- levels(raw$codf)
b <- ifelse(a == "Cardiovascular", "#e41a1c", "grey60")

# women
w <- ggplot(subset(raw15, gender=="Women")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=codf, weight=total), width=0.5, 
           colour = "#377eb8", fill = "#377eb8") + 
  geom_bar(data=subset(raw15, gender=="Women" & codf=="Total change"),
           aes(y=codf, weight=total), width=0.5,
           colour = "#e41a1c", fill="#e41a1c") +
  geom_text(aes(y=codf, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.2, 1.2))) +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw15$codf))) +
  scale_x_continuous(limits=c(-1.1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "") + 
  ggtitle("Cause contribution to change in life expectancy at birth, 2010-2018", subtitle="Women") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14, colour = b))
  
# men
m <- ggplot(subset(raw15, gender=="Men")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=codf, weight=total), width=0.5, 
           colour = "#377eb8", fill = "#377eb8") +  
  geom_text(aes(y=codf, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.2, 1.2))) +
    geom_bar(data=subset(raw15, gender=="Men" & codf=="Total change"),
           aes(y=codf, weight=total), width=0.5,
           colour = "#e41a1c", fill="#e41a1c") +
  facet_wrap(~raceeth, nrow=1) +
  scale_y_discrete(limits = rev(levels(raw15$codf))) +
  scale_x_continuous(limits=c(-1.1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "Years") + 
  ggtitle("", subtitle="Men") +
  stheme + theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.y = element_text(size = 14, colour = b))
  

# put both plots together
p <- w / m 
p

# export to file
ggsave(here("figures", "le-cause-decomp-2010-2018.png"), plot=p, width=11, height=10)


## 5
## Age and cause in the same plot (Appendix)

# Raw data
rawd <- read_csv(here("data", "le-age-cause-decomp-2010-2018.csv"))

rawt <- rawd %>%
  group_by(sex, race, cod, age) %>%
  summarise(total = sum(cont) * -1 )

tot <- aggregate(rawt$total, list(sex = rawt$sex, 
  race = rawt$race, age = rawt$age), FUN = sum )
tot$cod <- 15
colnames(tot)[4] <- "total"

rawc <- rawt %>%
  bind_rows(tot)

# gender as factor
rawc$gender <- recode_factor(rawc$sex, `1`= "Women", 
  `2`= "Men")

# race-ethnicity as factor
rawc$raceeth <- recode_factor(rawc$race, `1`= "Non-Hispanic\nAPI", 
  `2`= "Non-Hispanic\nBlack", `3`= "Non-Hispanic\nWhite", 
  `4`= "Hispanic")

rawc$age4f <- recode_factor(rawc$age, `1` = "<1 yrs", `2` = " 1-15 yrs", 
  `3` = " 1-15 yrs", `4` = "15-44 yrs", `5` = "15-44 yrs",
  `6` = "15-44 yrs", `7` = "45-64 yrs", `8` = "45-64 yrs", 
  `9` = "65+ yrs", `10` = "65+ yrs", `11` = "65+ yrs")

rawc$codf <- recode_factor(rawc$cod, `1` = "Cardiovascular", `2` = " Cancers", 
  `3` = "Diabetes", `4` = "Alzheimer's", `5` = "Flu/Pneumonia",
  `6` = "HIV", `7` = "Respiratory disease", `8` = "Liver disease", 
  `9` = "Kidney", `10` = "Motor vehicle crashes", 
  `11` = "Unintentional poisoning", `12` = "Suicide", 
  `13` = "Homicide", `14` = "All other causes", 
  `15` = "Total change")

raw4 <- rawc %>%
  group_by(gender, raceeth, age4f, codf) %>%
  summarise(total = sum(total))

raw15 <- rawc %>%
  group_by(gender, raceeth, codf) %>%
  summarise(total = sum(total))

acw <- ggplot(subset(raw4, gender=="Women")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=codf, weight=total, fill=age4f), width=0.75) +
  facet_wrap(~raceeth, nrow=1) + 
  scale_fill_brewer("Age group", type="qual", palette="Set1") +
  geom_text(data=subset(raw15, gender=="Women"), 
            aes(y=codf, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  scale_y_discrete(limits = rev(levels(raw4$codf))) +
  scale_x_continuous(limits=c(-1, 2), breaks=c(-1, 0, 1, 2)) +
 labs(y = "", x = "Years") + 
 stheme + theme(panel.spacing = unit(2, "lines"), 
                 legend.position="top", legend.text = element_text(size=14),
                legend.title=element_text(size=14))

# export to file
ggsave(here("figures", "le-age-cause-decomp-2010-2018-women.png"), 
       plot=acw, width=11, height=7.5)

# Figure for Men
acm <- ggplot(subset(raw4, gender=="Men")) + 
  geom_vline(xintercept = 0, linetype="dotted", colour="grey60") +
  geom_bar(aes(y=codf, weight=total, fill=age4f), width=0.75) +
  facet_wrap(~raceeth, nrow=1) + 
  scale_fill_brewer("Age group", type="qual", palette="Set1") +
  geom_text(data=subset(raw15, gender=="Men"), 
            aes(y=codf, x=total, label = round(total, 2), 
            hjust=ifelse(total > 0, -0.3, 1.2))) +
  scale_y_discrete(limits = rev(levels(raw15$codf))) +
  scale_x_continuous(limits=c(-1.1, 2), breaks=c(-1, 0, 1, 2)) +
  labs(y = "", x = "Years") +
  stheme + theme(panel.spacing = unit(2, "lines"), 
                 legend.position="top", legend.text = element_text(size=14),
                legend.title=element_text(size=14)) 

# export to file
ggsave(here("figures", "le-age-cause-decomp-2010-2018-men.png"), 
       plot=acm, width=11, height=8)

