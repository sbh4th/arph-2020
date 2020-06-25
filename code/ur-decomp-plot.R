#  program:  ur-decomp-plot.R
#  task:     urban-rural decomposition
#  input:    ur-decomp-cod.csv
#  output:   ur-decomp-plot.png
#  project:  ARPH Life Expectancy
#  author:   sam harper \ 2020-06-25

##### 0
##### load libraries
library(tidyverse)
library(plotly)
library(here)
library(ggrepel)
library(patchwork)

here::here()

##### 1  #####
##### Read in data

dc <- read_csv(here("data/seer-stat", "ur-decomp-cod.csv"))
dc$val <- sprintf("%.2f", round(dc$cont, 1))

dc$codn <- factor(dc$cod6, levels = c(7,6,5,4,3,2,1), labels=c("Total", "Other", "Infant", "Injuries", "Communicable", "CVDs", "Cancers"))
dc6 <- subset(dc, dc$cod6!=7 & dc$sex==1)


##### 2  #####
##### Make the plot

# women
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

# men
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

# both plots together
p <- rw + rm
p2 <- p + plot_annotation(title = 'Major causes of death contributing to urban-rural differences in life expectancy at birth', theme = theme(plot.title = element_text(size = 16)))
p2

# export to file
ggsave(here("figures", "ur-decomp-plot.png"), plot=p2, width=11, height=8)
