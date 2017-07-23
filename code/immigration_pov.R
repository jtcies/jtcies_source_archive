library('tidyverse')
library('stringr')

set.seed(7)

tract <- read_csv('data/ACS_15_5YR_charc_tract/ACS_15_5YR_S0601_with_ann.csv')

tract <- slice(tract, -1)

# rename tracts that we are using

cols <- c('total.pop', 'imm', 'pov.denom', 'imm.pov.denom', 'total.pov')

tract <- rename(tract,
            total.pop = HC01_EST_VC01,
             imm = HC04_EST_VC01,         # born outside of us - immmigrant
             pov.denom = HC01_EST_VC66,   # pop for whom pov status is available
             imm.pov.denom = HC04_EST_VC66,  # pov status for immigrants
             total.pov = HC01_EST_VC67,   # total below pov line
             imm.pov = HC04_EST_VC67)     # born outside us blw pov line

# changing those columsn to numeric

tract[,cols] <- sapply(tract[,cols], as.numeric)

# percent immigrant

tract <- mutate(tract, pct.imm = imm / total.pop)

ggplot(tract, aes(x = pct.imm, y = total.pov)) +
  geom_point()
