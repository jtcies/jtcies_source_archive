library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)


# ----- read data -----

staff <- read_excel(
  "data/pde/2015-16 Professional Staff Summary Report.xlsx",
  sheet = "LEA_Averages", skip = 5
  )

dict <- read_excel(
  "data/pde/2016-17 Professional Staff Summary Report_revised.xlsx",
  sheet = 1
)

names(staff) <- make.names(tolower(names(staff)))

fi <- read_excel("data/pde/finances afr expdetail 0102-1516.xlsx")

names(fi) <- tolower(make.names(names(fi)))

li <- read_excel("data/pde/2015-16 Public Schools Percent Low Income.xls",
                 sheet = 2, skip = 4)

names(li) <- tolower(make.names(names(li)))

staff$aun <- as.numeric(staff$aun)

pde <- left_join(fi, staff, by = "aun")

pde <- left_join(pde, li, by = "aun")

fed <- read_excel("data/pde/Finances AFR FederalRev 0304-1516.xlsx",
                  sheet = "2015-16")

names(fed) <- tolower(make.names(names(fed)))

st <- read_excel("data/pde/Finances AFR StateRev 0304-1516.xlsx",
                  sheet = "2015-16")

names(st) <- tolower(make.names(names(st)))

loc <- read_excel("data/pde/Finances AFR LocalRev 0304-1516.xlsx",
                  sheet = "2015-16")

names(loc) <- tolower(make.names(names(loc)))

pde <- left_join(pde, fed, by = "aun") %>% 
  left_join(st, by = "aun") %>% 
  left_join(loc, by = "aun")

pde <- filter(pde, !is.na(aun))

# ----- transform -----

pde <- pde %>% 
  mutate(
    total.rev = 
      revenue.from.federal.sources.8000 +
      total.state.revenue.7000 +
      total.local.revenue.6000,
    pct.fed = revenue.from.federal.sources.8000 / total.rev,
    pct.st = total.state.revenue.7000 / total.rev,
    pct.loc = total.local.revenue.6000 / total.rev
    ) %>% 
  filter(pct.st > 0 & pct.fed < .5) # remove 2 charters with anomolies
  

# ----- visualize ----- 

qplot(pct.loc, data = pde, geom = "histogram")

cols <- character(nrow(pde))
cols[] <- "black"
cols[pde$lea.type.x == 'CS'] <- "red"


ggpairs(pde, columns = c(5, 149, 294:297), aes(colour = lea.type.x))
