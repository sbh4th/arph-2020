

devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)

vmt <- data.frame(country=rep(c("United States", "Norway", "Sweden", "United Kingdom", "Denmark", "Ireland"), each=2), year=rep(c("1990","2015")), rate=c(12.9,7.0, 12.0,2.6, 12.0,3.2, 12.8,3.4, 17.3,3.4, 19.2,3.5))

newggslopegraph(dataframe = vmt,
                Times = year,
                Measurement = rate,
                Grouping = country,
                Title = "Deaths per billion vehicle kilometers travelled",
                SubTitle = "",
                LineColor = c(rep("gray60",5),"red"),
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
