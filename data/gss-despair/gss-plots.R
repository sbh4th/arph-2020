library(haven)
library(tidyverse)
library(rethinking)
library(brms)

d <- read_dta("gss-data.dta")
t <- read_csv("gss-data.csv", na=c(".a", ".b") )

# create smaller dataframe for analysis
dr <- select(t, "nthappy", "educ4", "race", "agegp", "year") %>%
  filter(race==1 | race==2) %>%
  mutate(black = ifelse(race==2, 2, 1), 
         lhs = ifelse(educ4==0, 2, 1),
         hs = ifelse(educ4==1, 2, 1),
         coll = ifelse(educ4==2, 2, 1),
         univ = ifelse(educ4==3, 1, 0),
         year = as.integer(year) ) %>%
  drop_na %>%
  sample_frac(size=0.1, replace=FALSE)

yrs <- unique(as.ordered(dr$year))
dr$yeari <- factor(dr$year, ordered=TRUE)

dat <- list(
    y = dr$nthappy,
    black = dr$black,
    lhs = dr$lhs,
    hs = dr$hs,
    coll = dr$coll,
    year = as.integer(dr$yeari) ) 


# approximate posterior
m1 <- ulam(
    alist(
        y ~ dbinom( 1 , p ) ,
        logit(p) <- a[year] ,
        a[year] ~ dnorm( 0 , 1.5 )
    ), data=dat , chains=4 , log_lik=TRUE )

prior <- extract.prior(m1)

precis(m1, depth=2)

# compute mean intercept for each year
drc <- dr %>%
  group_by(yeari) %>%
  summarise(prnthappy = mean(nthappy))

post <- extract.samples( m1 )

drc$prnthappy.est.np <- logistic( apply( post$a , 2 , mean ) )

# add estimated

m2 <- ulam(
    alist(
        y ~ dbinom( 1 , p ) ,
        logit(p) <- a[year] ,
        a[year] ~ dnorm( a_bar , sigma ) ,
        a_bar ~ dnorm( 0, 1.5) ,
        sigma ~ dexp( 1 )
    ), data=dat , chains=4 , log_lik=TRUE )

compare(m1, m2)

# extract posterior estimates
post <- extract.samples( m2 )

# also transform to probability with logistic
drc$prnthappy.est.pp <- logistic( apply( post$a , 2 , mean ) )

# display raw proportion "not too happy" in each year
plot( drc$prnthappy , ylim=c(0,0.4) , pch=16 , xaxt="n" ,
    xlab="year" , ylab="proportion not too happy" , col=rangi2 )

# overlay posterior means
points( drc$prnthappy.est.pp )


# expand the model to include race and education
m3 <- ulam(
    alist(
        y ~ dbinom( 1 , p ) ,
        logit(p) <- a[year] + b[black],
        a[year] ~ dnorm( a_bar , sigma_a ) ,
        b[black] ~ dnorm( 0, 1 ) ,
        a_bar ~ dnorm( 0, 1.5) ,
        sigma_a ~ dexp( 1 ) 
    ), data=dat , chains=4 , log_lik=TRUE )

drp <- dr %>%
  group_by(yeari, black, lhs) %>%
  summarise(prnthappy = mean(nthappy))
ggplot(drp, aes(y=prnthappy, x=yeari, group=as.factor(black))) + 
  geom_smooth(aes(colour=as.factor(black))) + facet_wrap(~lhs)

drw <- dr %>%
  group_by(yeari, black, educ4) %>%
  summarise(prnthappy = mean(nthappy))
ggplot(subset(drw, black==1), aes(y=prnthappy, x=yeari, group=as.factor(educ4))) + 
  geom_smooth(aes(colour=as.factor(educ4))) 
