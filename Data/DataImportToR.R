## ===========================
## Data import from EXCEL file
## ===========================
library(dplyr)
library(openxlsx)
nbaupd <- read.xlsx("NBA_raw_data2019.xlsx", detectDates = TRUE) %>%
  mutate_at(vars(state, etni, death, pos, place, lefthanded), factor)

## Subset of players who finished NBA career latest in 2019.
## ---------------------------------------------------------
# nbaformupd: Data frame of 3962 former NBA players.
nbaformupd <- nbaupd %>%
  filter(state == "Former") %>%
  # endest: Estimated date of last NBA match. If endest >= dateevent, we assume
  # that two days elapsed between last NBA match and death (==> survtime > 0).
  mutate(endest = as.Date(paste(fins, "6", "1", sep = "-")),
         endest = if_else(endest >= dateevent, dateevent - 2, endest),
         survtime = as.vector(round(difftime(dateevent, endest) / 365.25, 2)),
         ageend = as.vector(round(difftime(endest, birthdate) / 365.25, 2)),
         len = fins - from) %>%
  select(id:birthdate, pos:fins, endest, len, ageleft, ageend, dateevent,
         survtime, cens, ageevent, kilos:lefthanded)

## Creation of subsets with of white and African-american players
## --------------------------------------------------------------
nbaupdBW <- nbaupd %>%
  filter(etni %in% c("White", "Black")) %>%
  mutate(etni = factor(etni, levels = c("White", "Black")))

nbaformuBW <- nbaformupd %>%
  filter(etni %in% c("White", "Black")) %>%
  mutate(etni = factor(etni, levels = c("White", "Black")))

## Mortatlity data
## ---------------
library(openxlsx)
mortality <- read.xlsx("MortalityUSA2000to2018.xlsx", sheet = "ForR",
                       rows = 1:84)
names(mortality)[1] <- "age"

## All data frames are saved in a new workspace
## --------------------------------------------
comment(mortality) <- "US mortality rates from 2000 to 2018"
comment(nbaupd) <- "Active and former NBA players (by July 31, 2019)"
comment(nbaupdBW) <- "Active and former African-american and white NBA players"
comment(nbaformupd) <- "Former NBA players (by July 31, 2019)"
comment(nbaformuBW) <- "Former African-american and white NBA players"
save(mortality, nbaupd, nbaupdBW, nbaformupd, nbaformuBW,
     file = "NBAdata2019.RData")
