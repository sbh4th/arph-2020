#  program:  wdi-le-plot-use.R
#  task:     analyses of LE in OECD countries
#  input:    wdi-le.csv
#  output:   us-le-lagging.png, us-le-change.png, us-le-trends.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 28feb2020

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(patchwork)

here::here()

##### 1  #####
##### import OECD data, transform for analysis

# Read in life expectancy data from World Bank
d <- read_csv(here("data/world-development-indicators", "wdi-le.csv"))

# convert to numeric
type_convert(d)

# make dataset long rather than wide
dl <- d %>%
  gather(Year, LE, YR1969:YR2017)

# remove string from year
dl$Year <- str_remove(dl$Year, "YR")

# export a .csv file for joinpoint analysis
dljp <- filter(dl, Country.Code == "OED" | 
                 Country.Code == "USA") %>%
  arrange(Country.Code, Year) %>%
  group_by(Country.Code) %>%
  mutate(year0 = row_number() - 1) %>%
  write_delim(here("data", "us-oed-le.csv"), delim = ",")


# rank of USA in each year
ranks <- dl %>% arrange(Year, -LE) %>%
    group_by(Year) %>% 
    mutate(rank = rank(-LE, ties.method = "first"))

print(as_tibble(ranks %>% filter(Year == 1970)), n = 30)
print(as_tibble(ranks %>% filter(Year == 2017)), n = 30)

# subsets of data for USA and OECD
# for labeling in plots
dlusa <- subset(dl, Country.Code=="USA")
dloecd <- subset(dl, Country.Code=="OED")


##### 2  #####
##### plot trends in life expectancy
# set common theme for charts
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank())

# plot life expectancy over time
# highlighting relative lagging in USA
p <- ggplot(dl, aes(x=Year, y=LE, group=Country.Name)) + geom_line(colour="grey") + scale_y_continuous(limits=c(65,85)) + geom_line(data=dlusa, colour="#d95f02", size=1.5) + geom_line(data=subset(dl, Country.Code=="OED"), colour="#1b9e77", size=1.5) + theme_classic() + scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) + annotate("text", label = "USA", x = 48, y = 78, size = 5, colour = "#d95f02") + annotate("text", label = "OECD\naverage", x = 1, y = 68, size = 5, colour = "#1b9e77", hjust=0) + ylab("") + xlab("") + ggtitle("Life expectancy at birth (years)") + stheme

# export to figures folder
ggsave(here("figures", "us-le-lagging.png"), plot=p)


# generate lagged values of LE, calculate annual change
dlc <- dl %>%
  group_by(Country.Code) %>%
  mutate(lechange = LE - lag(LE, order_by = Year))

# subsets of change data for USA
dlcusa <- subset(dlc, Country.Code=="USA")

# plot absolute change life expectancy over time
# highlighting relative lagging in USA
pc <- ggplot(dlc, aes(x=Year, y=lechange, group=Country.Name)) + geom_hline(yintercept=0, linetype="dashed", color = "black") + geom_line(colour="grey") + scale_y_continuous(limits=c(-2,2.5)) + geom_point(data=dlcusa, colour="#d95f02", size=1) + geom_smooth(data=dlcusa, colour="#d95f02", se=F, span=0.25) + geom_point(data=subset(dlc, Country.Code=="OED"), colour="#1b9e77",, size=1)  + geom_smooth(data=subset(dlc, Country.Code=="OED"), colour="#1b9e77",, se=F, span=0.25) + theme_classic() + scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) + annotate("text", label = "USA", x = 48, y = -0.4, size = 5, colour = "#d95f02") + annotate("text", label = "OECD\naverage", x = 2, y = 2, size = 5, colour = "#1b9e77", hjust=0) + geom_curve(aes(x = 1, y = 2, xend = 1, yend = 0.4), curvature=0.2, arrow = arrow(length = unit(0.03, "npc")), colour="#1b9377") + ylab("") + xlab("") + ggtitle("Annual change (years)") + stheme

# export to figures file
ggsave(here("figures", "us-le-change.png"), plot=pc)

# single figure with both plots
t <- p + pc
ggsave(here("figures", "us-le-trends.png"), plot=t, width=11, height=6.5)


# trends since 2010 only
dlc10 <- subset(dlc, Year>=2010)
dlcusa10 <- subset(dlcusa, Year>=2010)
pci <- ggplot(dlc10, aes(x=Year, y=lechange, group=Country.Name)) + geom_hline(yintercept=0, linetype="dashed", color = "black") + geom_line(colour="grey") + scale_y_continuous(limits=c(-1,1.5)) + geom_line(data=dlcusa10, colour="red", size=1.5) + theme_classic() + scale_x_discrete(breaks=c(2010, 2012, 2014, 2016, 2018)) + annotate("text", label = "USA", x = 8, y = -0.2, size = 4, colour = "red") + ylab("") + xlab("") + theme(axis.text.x = element_text(size = 16), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 16), legend.position="none",panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white"))



ggplot(dlc, aes(x=Year, y=lechange, group=Country.Name)) + geom_hline(yintercept=0, linetype="dashed", color = "black") + geom_line(colour="white") + scale_y_continuous(limits=c(-0.5,1)) + geom_point(data=dlcusa) + geom_smooth(data=dlcusa, colour="red", span=0.8, size=1.5, fill="red", alpha=0.1) + theme_classic() + scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) + ylab("Annual change in life expectancy at birth (years)") + xlab("") + theme(axis.text.x = element_text(size = 16), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 16), legend.position="none",panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white"))
