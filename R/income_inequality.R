# this script uses ipums data to examine income inequality, specifically by
# race, using an empirical bayesian analisys (see Robinson, 2016)

library(tidyverse)
library(survey)

# read data - 2015 acs pums 5yr
acs <- read_csv('data/ipums/usa_00011.csv')

names(acs) <- tolower(names(acs))

# setting up the survey design

acs1 <- acs[rep(row.names(acs), acs$perwt), ]


# filter only those currently in the labor force

lf <- filter(acs, empstat %in% c(1, 2))


# exploring
