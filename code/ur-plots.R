# 0
# Load reqired packages, set working directory
# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

library(ggplot2)
library(foreign)
library(gridExtra)
devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)

dc <- read_csv(here("data", "ur-decomp-cod.csv"))
dc$val <- sprintf("%.2f", round(dc$cont, 1))

dc$codn <- factor(dc$cod6, levels = c(7,6,5,4,3,2,1), labels=c("Total", "Other", "Infant", "Injuries", "Communicable", "CVDs", "Cancers"))
dc6 <- subset(dc, dc$cod6!=7 & dc$sex==1)

rw <- ggplot(subset(dc, sex==2 & cod6!=7), 
       aes(x=year, y=cont, group=codn, colour=codn, label=codn)) +
  geom_line() + 
  geom_point(shape = 21, colour = "white", fill = "white", 
             size = 5, stroke = 5) + 
  geom_text(aes(label=val)) + 
  geom_hline(yintercept=0, color="grey60") + xlab("") + ylab("") +
  scale_y_continuous(limits = c(-0.5, 1.5)) +
  scale_x_continuous(limits = c(-1, 2)) +
  geom_text_repel(data=subset(dc, sex==2 & year==0 & cod6!=7), colour="black", 
                  fontface="bold", nudge_x = -0.2, direction = "both", hjust = 1, 
                  segment.size = 0.2, point.padding = 1.5, size=3.5) +
  geom_text_repel(data=subset(dc, sex==2 & year==1 & cod6!=7), colour="black", 
                  fontface="bold", nudge_x = 0.2, direction = "both", hjust = 0, 
                  segment.size = 0.2, point.padding = 1.5, size=3.5) +
  annotate("text", label = "Years contributed to\nthe rural-urban\nlife expectancy gap", x = -1, y = 1, size = 5, hjust=0, colour = "grey60") +
  scale_color_manual(values=c("#4daf4a","#377eb8", "#e41a1c", "#984ea3", "#ff7f00", "#f781bf")) +
  annotate("text", label="1969-71\n(-0.3 yrs)", x = 0, y = 1.5, size = 5, 
           hjust="center", colour = "grey60") +
  annotate("text", label="2012-2016\n(2.0 yrs)", x = 1, y = 1.5, size = 5, 
           hjust="center", colour = "grey60") +
  theme_classic() + ggtitle("Women") +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none", 
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x=element_blank(), 
    axis.line.y=element_line(colour="grey60"),
    axis.text.y = element_text(size = 16, colour="grey60"),
  )

rm <- ggplot(subset(dc, sex==1 & cod6!=7), 
       aes(x=year, y=cont, group=codn, colour=codn, label=codn)) +
  geom_line() + 
  geom_point(shape = 21, colour = "white", fill = "white", 
             size = 5, stroke = 5) + 
  geom_text(aes(label=val)) + 
  geom_hline(yintercept=0, color="grey60") + xlab("") + ylab("") +
  scale_y_continuous(limits = c(-0.5, 1.5)) +
  scale_x_continuous(limits = c(-1, 2)) +
  geom_text_repel(data=subset(dc, sex==1 & year==0 & cod6!=7), colour="black", 
                  fontface="bold", nudge_x = -0.2, direction = "both", hjust = 1, 
                  segment.size = 0.2, point.padding = 1.5, size=3.5) +
  geom_text_repel(data=subset(dc, sex==1 & year==1 & cod6!=7), colour="black", 
                  fontface="bold", nudge_x = 0.2, direction = "both", hjust = 0, segment.size = 0.2, point.padding = 1.5, size=3.5) +
  scale_color_manual(values=c("#4daf4a","#377eb8", "#e41a1c", "#984ea3", "#ff7f00", "#f781bf")) +
  annotate("text", label="1969-71\n(0.7 yrs)", x = 0, y = 1.5, size = 5, 
           hjust="center", colour = "grey60") +
  annotate("text", label="2012-16\n(2.3 yrs)", x = 1, y = 1.5, size = 5, 
           hjust="center", colour = "grey60") +
  theme_classic() + ggtitle("Men") +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none", 
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x=element_blank(), 
    axis.line.y=element_blank(),
  )

p <- rw + rm
p

# export to file
ggsave(here("figures", "ur-decomp.png"), plot=p, width=11, height=8)

ggplot(subset(dc, sex==1 & cod6!=7), 
       aes(x=year, y=cont, group=codn, colour=codn, label=codn))


dc$codcont <- ifelse(dc$year==1, paste("(",dc$val,")", dc$codn),
                     paste(dc$codn, "(",dc$val,")"))
dc$codcontv <- paste(dc$codn, dc$val, sep="\n")

rm <- ggplot(subset(dc, sex==1 & cod6!=7),
       aes(x = year, stratum = codn, alluvium = codn,
           y = cont, fill = codn, label = codcont)) +
  geom_lode(decreasing = FALSE) + geom_flow(decreasing = FALSE) + 
  geom_label(stat = "stratum", decreasing = FALSE) +
  geom_hline(yintercept=0, color="grey60") + xlab("") + ylab("") +
  scale_y_continuous(limits = c(-1, 2.5)) +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  # geom_text_repel(data=subset(dc, sex==1 & year==0 & cod6!=7), 
  #                aes(label=codn, colour=codn), nudge_x = -0.2, direction = "both", hjust = 1, segment.size = 0.2, point.padding = 1.5, size=3.5) +
 #  geom_text_repel(data=subset(dc, sex==1 & year==1 & cod6!=7), colour="black", 
 #                  nudge_x = 0.2, direction = "both", hjust = 0, 
 #                 segment.size = 0.2, point.padding = 1.5, size=3.5) +
  scale_fill_brewer(type="qual", palette="Set1") +
  scale_colour_brewer(type="qual", palette="Set1") +
  theme_classic() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none", 
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x=element_blank(), 
    axis.line.y=element_blank(),
  )





dc$sexn <- factor(dc$sex, levels=c(1,2), labels=c("Men","Women"))
dc$yearn <- factor(dc$year, levels = c(0,1), labels=c("1970","2014"))

dc$codn <- factor(dc$cod6, levels = c(7,6,5,4,3,2,1), labels=c("Total", "Other causes", "Infant", "Injuries", "Communicable", "Cardiovascular", "Cancer"))
dc6 <- subset(dc, dc$cod6!=7 & dc$sex==1)
dc6$contr <- round(dc6$cont,2)

newggslopegraph(dataframe = dc6,
                Times = yearn,
                Measurement = contr,
                Grouping = codn,
                Title = "Men (1970: 0.7yrs, 2014: 2.3yrs)",
                SubTitle = "Years of life expectancy, by cause",
                Caption = NULL, DataTextSize=5, XTextSize = 14, YTextSize = 5.5, SubTitleTextSize = 16, TitleTextSize = 20
)

lem <- data.frame("Year" = c(1, 2, 1, 2), "LE" = c(67.1, 76.7, 66.4, 74.4), "Area" = c("Urban","Urban", "Rural", "Rural"), stringsAsFactors = T)
lem$yearn <- factor(lem$Year, levels = c(1,2), labels=c("1970","2014"))

newggslopegraph(dataframe = lem,
                Times = yearn,
                Measurement = LE,
                Grouping = Area,
                Title = "Men (Gap in 1970: 0.7yrs, 2014: 2.3yrs)",
                SubTitle = "Life expectancy at birth",
                Caption = NULL, DataTextSize=7, XTextSize = 16, YTextSize = 8, SubTitleTextSize = 18, TitleTextSize = 20
)

lew <- data.frame("Year" = c(1, 2, 1, 2), "LE" = c(74.8, 81.6, 75.1, 79.6), "Area" = c("Urban","Urban", "Rural", "Rural"), stringsAsFactors = T)
lew$yearn <- factor(lew$Year, levels = c(1,2), labels=c("1970","2014"))

newggslopegraph(dataframe = lew,
                Times = yearn,
                Measurement = LE,
                Grouping = Area,
                Title = "Women (Gap in 1970: -0.3yrs, 2014: 2.0yrs)",
                SubTitle = "Life expectancy at birth",
                Caption = NULL, DataTextSize=7, XTextSize = 16, YTextSize = 8, SubTitleTextSize = 18, TitleTextSize = 20
)

le <- data.frame("Year" = c(1, 2, 1, 2, 1, 2, 1, 2), "LE" = c(74.8, 81.6, 75.1, 79.6, 67.1, 76.7, 66.4, 74.4), "Area" = c("Urban Women","Urban Women", "Rural Women", "Rural Women","Urban Men","Urban Men", "Rural Men", "Rural Men"), stringsAsFactors = T)
le$yearn <- factor(le$Year, levels = c(1,2), labels=c("1970","2014"))

newggslopegraph(dataframe = le,
                Times = yearn,
                Measurement = LE,
                Grouping = Area,
                Title = "Life expectancy at birth",
                SubTitle = "Life expectancy at birth",
                Caption = NULL, DataTextSize=7, XTextSize = 16, YTextSize = 8, SubTitleTextSize = 18, TitleTextSize = 20
)
