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

# subsets of data for USA and OECD
# for labeling in plots
dlusa <- subset(dl, Country.Code=="USA")
dloecd <- subset(dl, Country.Code=="OED")


##### 2  #####
##### plot trends in life expectancy

# plot life expectancy over time
# highlighting relative lagging in USA
p <- ggplot(dl, aes(x=Year, y=LE, group=Country.Name)) + geom_line(colour="grey") + scale_y_continuous(limits=c(65,85)) + geom_line(data=dlusa, colour="red", size=1.5)  + theme_classic() + scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) + annotate("text", label = "USA", x = 48, y = 78, size = 4, colour = "red") + ylab("Life expectancy at birth (years)") + xlab("") + theme(axis.text.x = element_text(size = 16), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 16), legend.position="none",panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white"))

ggplotly(p)

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
pc <- ggplot(dlc, aes(x=Year, y=lechange, group=Country.Name)) + geom_hline(yintercept=0, linetype="dashed", color = "black") + geom_line(colour="grey") + scale_y_continuous(limits=c(-2,3)) + geom_line(data=dlcusa, colour="red", size=1.5)  + theme_classic() + scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) + annotate("text", label = "USA", x = 48, y = -0.4, size = 4, colour = "red") + ylab("Annual change in life expectancy at birth (years)") + xlab("") + theme(axis.text.x = element_text(size = 16), axis.title.y=element_text(size=16, angle=90), axis.text.y = element_text(size = 16), legend.position="none",panel.grid.major = element_line(colour="white"), panel.grid.minor = element_line(colour="white"))

ggplotly(pc)

# export to figures file
ggsave(here("figures", "us-le-change.png"), plot=pc)

# single figure with both plots
t <- p + pc
ggsave(here("figures", "us-le-trends.png"), plot=t, width=11, height=6.5)

