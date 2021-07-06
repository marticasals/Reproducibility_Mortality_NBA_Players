## July 5th, 2021
## IMPORTANT: The following instructions were used to create the 2021
##            data set when still not all information was available.
##            The main data frame was then exported to an EXCEL file, which
##            was updated in June 2021; see last lines of this script.
## ============================================================================

library(dplyr)
library(forcats)
library(openxlsx)
library(Hmisc)
library(stringr)

## Auxiliar data frame with updated variable "dateevent2020".
## Contains death or censored dates of all former players in the
## previous nbadat data frame (n = 3504).
## Notice: Variable "cens" is NOT updated in the case of the
##         players that died between April 15, 2014 and July 31, 2019.
## -------------------------------------------------------------------
dateveform <- read.xlsx("nbaFormer2020_Jose.xlsx", cols = c(1, 3, 8),
                        detectDates = TRUE)[c(2, 1, 3)]

## Main data file including all NBA players
## ----------------------------------------
nbaupd <- read.xlsx("nbadat2020_KL_MC_4375.xlsx", detectDates = TRUE) %>%
  mutate_at(vars(position, place2, etni, pos, state, lefthanded), factor) %>%
  ## Update of variables "dateevent2020", "ageevent", and "cens"
  merge(dateveform, all.x = TRUE) %>%
  mutate(fins = fins2020,
         dateevent = if_else(is.na(dateevent2020), dateevent, dateevent2020),
         ageevent = as.vector(trunc(difftime(dateevent, birthdate) / 365.25)),
         ageleft = as.vector(round(difftime(debut, birthdate) / 365.25, 2)),
         ageright = as.vector(round(difftime(dateevent, birthdate) / 365.25, 2)),
         cens = ifelse(dateevent == as.Date("2019-07-31"), 0, 1),
         death = factor(ifelse(dateevent == as.Date("2019-07-31"), "No", "Yes")),
         state = factor(fins < 2020, labels = c("Active", "Former"))) %>%
  select(id:birthdate, state, pos, place = place2, etni, debut, from,
         fins, dateevent, cens, death, ageevent, ageleft,
         ageright, kilos, cms, cms2, bmi, g, lefthanded) %>%
  mutate_at(vars(kilos, bmi), round, 2)

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
  select(id:birthdate, pos:fins, endest, len, ageend, dateevent, survtime, cens,
         ageevent, kilos:lefthanded)

## Creation of subsets with of white and African-american players
## --------------------------------------------------------------
nbaupdBW <- nbaupd %>%
  filter(etni %in% c("White", "Black")) %>%
  mutate(etni = factor(etni, levels = c("White", "Black")))

nbaformuBW <- nbaformupd %>%
  filter(etni %in% c("White", "Black")) %>%
  mutate(etni = factor(etni, levels = c("White", "Black")))

rm(dateveform)

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
     file = "NBAdata2021.RData")

## Definite data base in EXCEL
## ---------------------------
write.xlsx(nbaupd, "NBAdata_2019_7_31.xlsx")

## =====================
## Update (July 5, 2021)
## =====================
library(openxlsx)
nbaupd <- read.xlsx("NBAdata_2019_7_31_Jose.xlsx", detectDates = TRUE) %>%
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
  select(id:birthdate, pos:fins, endest, len, ageend, dateevent, survtime, cens,
         ageevent, kilos:lefthanded)

## Creation of subsets with of white and African-american players
## --------------------------------------------------------------
nbaupdBW <- nbaupd %>%
  filter(etni %in% c("White", "Black")) %>%
  mutate(etni = factor(etni, levels = c("White", "Black")))

nbaformuBW <- nbaformupd %>%
  filter(etni %in% c("White", "Black")) %>%
  mutate(etni = factor(etni, levels = c("White", "Black")))

## All data frames are saved in a new workspace
## --------------------------------------------
comment(mortality) <- "US mortality rates from 2000 to 2018"
comment(nbaupd) <- "Active and former NBA players (by July 31, 2019)"
comment(nbaupdBW) <- "Active and former African-american and white NBA players"
comment(nbaformupd) <- "Former NBA players (by July 31, 2019)"
comment(nbaformuBW) <- "Former African-american and white NBA players"
save(mortality, nbaupd, nbaupdBW, nbaformupd, nbaformuBW,
     file = "NBAdata2021.RData")
