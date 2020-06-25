#  program:  wdi-le-plot-oecd.R
#  task:     analyses of LE in OECD countries
#  input:    wdi-le-gender.csv
#  output:   us-le-trends-gender.png, us-le-change.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-24

# 0
# load libraries
library(tidyverse)
library(plotly)
library(here)
library(patchwork)

here::here()

##### 1  #####
##### import OECD data, transform for joinpoint analysis

# Read in life expectancy data from World Bank
d <- read_csv(here("data/wdi", "wdi-le-gender.csv"))

# convert to numeric
type_convert(d)

# make dataset long rather than wide
dl <- d %>% 
  gather(Year, LE, YR1960:YR2018, -`Series Name`) %>%
  mutate(group = recode(`Series Name`, 
    "Life expectancy at birth, female (years)" = 1,
    "Life expectancy at birth, male (years)" = 2,
    "Life expectancy at birth, total (years)" = 0)) %>%
  filter(Year != "YR2018")

# remove string from year
dl$Year <- str_remove(dl$Year, "YR")
dl$LE <- as.numeric(dl$LE)

# export a .csv file for joinpoint analysis
dljp <- filter(dl, Year>=1970 & (`Country Code` == "OED" | 
                 `Country Code` == "USA")) %>%
  arrange(group, `Country Code`, Year) %>%
  group_by(group, `Country Code`) %>%
  mutate(country = `Country Code`, year0 = row_number() - 1) %>%
  write_delim(here("data/wdi", "us-oed-le-gender.csv"), delim = ",")

# quick tables
# rank of USA in each year
ranks <- dl %>% arrange(group, Year, -LE) %>%
    group_by(group, Year) %>% 
    mutate(rank = rank(-LE, ties.method = "first"))

# rank in 1960
print(as_tibble(ranks %>% filter(group==0 & Year == 1970)), n = 30)

# rank in 2017
print(as_tibble(ranks %>% filter(group==0 & Year == 2017)), n = 30)

# subsets of data for USA and OECD
# for labeling in plots
dlusa <- subset(dl, `Country Code`=="USA")
dloecd <- subset(dl, `Country Code`=="OED")


##### 2  #####
##### set plot characteristics

# set common theme for charts
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank())


##### 3 #####
##### read in data from joinpoint analysis

# Raw data
jp <- read_tsv(here("data/wdi", "wdi-jp-le-data.txt"))

# format year
jp <-rename(jp, lem = Model) %>%
  mutate(Year = as.character(year0 + 1970))

# define US and OECD average  
jp$country <- ifelse(jp$country==0, "OED", "USA")

# join to full sample
dljpp <- full_join(dl, jp) %>%
  filter(Year>=1970)



##### 4 #####
##### plot life expectancy over time
##### highlighting relative lagging in USA

# gender-specific plots
# women
w <- ggplot(subset(dljpp, `Country Code`!="USA" & 
                     `Country Code`!="OED" & group==1), 
  aes(x=Year, y=LE, group=`Country Code`)) + geom_line(colour="grey") +
  geom_line(data=subset(dljpp, `Country Code`=="OED" & group==1), 
            colour="#1b9e77", size=1.5) +
  geom_point(data=subset(dljpp, `Country Code`=="USA" & group==1), 
             aes(x=Year, y=LE), colour="#d95f02", size=2, alpha=0.3) +
  geom_line(data=subset(dljpp, `Country Code`=="USA" & group==1), 
             aes(x=Year, y=lem), colour="#d95f02", size=1) +
  scale_y_continuous(limits=c(55,90)) +
  scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), 
                   expand = c(0,2)) + 
  annotate("text", label = "USA", x = 47, y = 79, size = 5, colour = "#d95f02") +
  annotate("text", label = "OECD\naverage", x = 1, y = 69, size = 5, colour = "#1b9e77", hjust=0) + ylab("") + xlab("") + 
  ggtitle("Life expectancy at birth (years)", subtitle="Women") + stheme

# men
m <- ggplot(subset(dljpp, `Country Code`!="USA" & 
                     `Country Code`!="OED" & group==2), 
  aes(x=Year, y=LE, group=`Country Code`)) + geom_line(colour="grey") +
  geom_line(data=subset(dljpp, `Country Code`=="OED" & group==2), 
            colour="#1b9e77", size=1.5) +
  geom_point(data=subset(dljpp, `Country Code`=="USA" & group==2), 
             aes(x=Year, y=LE), colour="#d95f02", size=2, alpha=0.3) +
  geom_line(data=subset(dljpp, `Country Code`=="USA" & group==2), 
             aes(x=Year, y=lem), colour="#d95f02", size=1) +
  scale_y_continuous(limits=c(55,90)) +
  scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), 
                   expand = c(0,2)) + 
  ylab("") + xlab("") + ggtitle("", subtitle="Men") + stheme

wm <- w + m
wm

# export to file
ggsave(here("figures", "us-le-trends-gender.png"), plot=wm, width=11, height=6.5)


##### 4 #####
##### plot of annual changes in life expectancy over time

# generate lagged values of LE, calculate annual change
dlc <- dl %>% filter(Year>=1970) %>%
  group_by(`Series Name`, `Country Code`) %>%
  mutate(lechange = LE - lag(LE, order_by = Year))

# subsets of change data for USA
dlcusa <- subset(dlc, `Country Code`=="USA")

# plot absolute change life expectancy over time
pc <- ggplot(subset(dlc, group==0), aes(x=Year, y=lechange, group=`Country Name`)) + geom_hline(yintercept=0, linetype="dashed", color = "black") + geom_line(colour="grey") + scale_y_continuous(limits=c(-2,2.5)) + geom_point(data=subset(dlcusa, group==0), colour="#d95f02", size=1) + geom_smooth(data=subset(dlcusa, group==0), colour="#d95f02", se=F, span=0.25) + geom_point(data=subset(dlc, `Country Code`=="OED" & group==0), colour="#1b9e77", size=1)  + geom_smooth(data=subset(dlc, `Country Code`=="OED" & group==0), colour="#1b9e77", se=F, span=0.25) + scale_x_discrete(breaks=c(1970, 1980, 1990, 2000, 2010, 2020), expand = c(0,2)) + annotate("text", label = "USA", x = 47, y = -0.4, size = 5, colour = "#d95f02") + annotate("text", label = "OECD\naverage", x = 2, y = 2, size = 5, colour = "#1b9e77", hjust=0) + geom_curve(aes(x = 1, y = 2, xend = 1, yend = 0.4), curvature=0.2, arrow = arrow(length = unit(0.03, "npc")), colour="#1b9377") + ylab("") + xlab("") + ggtitle("Annual change (years)") + stheme

# export to figures file
ggsave(here("figures", "us-le-change.png"), plot=pc, width=11, height=8.5)



