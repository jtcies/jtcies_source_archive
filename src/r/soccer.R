# this started as a script to determine if corners mattered
# it became pretty apparent quickl that they didn't
# that they were mostly a function of shots taken
# and that they didn't lead to goals
# so I transitioned it to try out a monte carlo simulation
# to see if we could determine how 

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(GGally)
library(lubridate)
library(tidyr)
library(purrr)

# ----- functions -----

season_assign <- function(date) {
  # assings the season based on the date
  require(lubridate)
  require(dplyr)
  season <- ifelse(month(date) >= 8,
                   as.character(year(date) + 1),
                   as.character(year(date)))
  season
}
  
read_soccer <- function(path, contains) {
  # reads in the different tables
  require(readr)
  require(dplyr)
  dat <- list.files(path)
  dat <- paste0(path, "/", dat[grepl(contains, dat)])
  tables <- lapply(dat, read_csv)
  bind_rows(tables)
  
}

rmse <- function(error) {
  sqrt(mean(error^2))
}


# ----- read data --- 

s <- read_soccer(path = "data/soccer", contains = "E")
names(s) <- tolower(names(s))


# ----- clean ----- 
s$date <- dmy(s$date)

# create a data frame where each row is one team in one game
# and the columns are season, team name, goals scored, goals allowed,
# result, yellows, reds, cornders, shots, and shots on target

s <- s %>% 
  mutate(game = row_number(),
         season = season_assign(date))

home <- s %>% 
  select(
    date,
    season,
    game,
    team = hometeam,
    opp = awayteam,
    goals = fthg,
    allowed = ftag,
    ht.goal = hthg,
    ht.allowed = htag,
    shots = hs,
    shots.allow = as,
    st = hst,
    st.allow = ast,
    fouls.conceded = hf,
    fouls.earned = af,
    corners = hc,
    corners.allow = ac,
    yc = hy,
    yc.earned = ay,
    red = hr,
    red.eared = ar
  )

away <- s %>% 
  select(
    date,
    season,
    game,
    team = awayteam,
    opp = hometeam,
    goals = ftag,
    allowed = fthg,
    ht.goal = htag,
    ht.allowed = hthg,
    shots = as,
    shots.allow = hs,
    st = ast,
    st.allow = hst,
    fouls.conceded = af,
    fouls.earned = hf,
    corners = ac,
    corners.allow = hc,
    yc = ay,
    yc.earned = hy,
    red = ar,
    red.eared = hr
  )

games <- bind_rows(home, away) %>% 
  arrange(game) %>% 
  mutate(
    result = case_when(
      goals > allowed ~ "win",
      allowed > goals ~ "loss",
      goals == allowed ~ "draw",
      TRUE ~ NA_character_
    ),
    points = recode(result,
                           win = 3,
                           draw = 1,
                           loss = 0),
    season = factor(season)
    ) %>% 
  filter(!is.na(date)) 


# split out test and train

set.seed(7)

games$sample <- sample(c(0,1),
                       size = nrow(games),
                       prob = c(4, 1),
                       replace = TRUE)

test <- filter(games, sample == 1) %>% 
  select(-sample)

train <- filter(games, sample == 0) %>% 
  select(-sample) %>% 
  mutate(sot = st - shots)

# ----- visualize -----
# let's see if any teams only show up a few times

# see the distribution of goals by team
ggplot(train, aes(x = goals)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~team)

# goals allowed
ggplot(train, aes(x = allowed)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~team)

train <- train %>% 
  mutate(dif = goals - allowed)

ggplot(train, aes(x = dif)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~team)

# these comes from a poisson distribution
# so we need to incorporate that into our model

# ---- preproccess for modeling -----
# figure out the distribution of goals and allowed

# monte carlo simulation

run_team_simulation <- function(team.name,
                                opp.name,
                                n.draws = 1000) {
  
  team.of.int <- filter(train, team == team.name)
  
  team.dif <- sample(team.of.int$dif, n.draws, replace = TRUE)
  
  opp.of.int <- filter(train, team == opp.name)
  
  opp.dif <- sample(opp.of.int$dif, n.draws, replace = TRUE)
  
  dif <- data.frame(team.dif, opp.dif) %>% 
    mutate(
      difference = team.dif - opp.dif,
      result = case_when(
        difference >=  1 ~ "win",
        difference <= -1 ~ "loss",
        TRUE             ~ "draw"
      )
    )
  
  results <- dif %>% 
    group_by(result) %>% 
    summarise(n = n()) %>% 
    mutate(pct = n / sum(n)) %>% 
    select(-n) %>% 
    spread(result, pct)
  
  results
}

teams.sim <- train$team
opps.sim <- train$opp

set.seed(7)

# run the simulation
sim.results <- map2(teams.sim, 
                    opps.sim, 
                    run_team_simulation) %>% 
  bind_rows()

# now we can sample from the win / draw / loss columsn based on percentages
# to get a predicted result
# them comapre how frequntly this is off from the true result

pred <- function(draw.pct, loss.pct, win.pct) {
  
  possible.results <- c("draw", "loss", "win")
  prob.results <- c(draw.pct, loss.pct, win.pct)
  
  pred.result <- sample(possible.results, size = 1, prob = prob.results)
  
  pred.result
  
}

args <- list(draw.pct = sim.results$draw, 
             loss.pct = sim.results$loss,
             win.pct = sim.results$win)

sim.results$pred.result <- pmap(args, pred) %>% 
  unlist()

train <- bind_cols(train, sim.results)

prop.table(table(train$result, train$pred.result), 1)

# one thing that seems clear is that I'm really underestimating the amount of
# draws. I might have to bump up the probabilities for those in prediction
# in some way or use a model to predict the result based on those
# probabilities plus a few other variables
