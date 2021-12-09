## R code corresponding to the analysis of Martínez et al. (2019):
## "Data Set on Mortality of National Basketball Association (NBA)
## Players"
## Update analysis with data base closed at July 31, 2019
## =======================================================================
load("NBAdata2019.RData")

## Data frames
## -----------
## nbaformuBW: Former African-american and white NBA players (Dim: 3845 x 22)
## nbaformupd: Former NBA players (by July 31, 2019) (Dim: 3962 x 22)
## nbaupd: Active and former NBA players (by July 31, 2019) (Dim: 4374 x 22)
## nbaupdBW: Active and former African-american and white NBA players
##          (Dim: 4217 x 22)

### ===========================================================================
### Table 1 ===================================================================
### ===========================================================================
library(compareGroups)
### Active vs Former
res1 <- compareGroups(state ~ pos + etni + place + lefthanded + ageleft +
                      ageright + cms2 + kilos + g, data = nbaupd)
restab1 <- createTable(res1, show.all = TRUE, show.p.overall = FALSE)
restab1

## Alive vs dead (among former NBA players)
res2 <- compareGroups(cens ~ pos + etni + place + lefthanded  +
                      ageend + cms2 + kilos + g, data = nbaformupd)
restab2 <- createTable(res2, show.all = TRUE, show.p.overall = FALSE)
restab2

detach(package:compareGroups)

### ===========================================================================
### Figure 1 ==================================================================
### ===========================================================================
# windows(width = 9, height = 6)
library(survival)
library(rms)
pdf("Survival2_2021.pdf", width = 10)
# tiff("Survival2_2021.tiff", width = 720)
par(las = 1, font.lab = 2, font.axis = 2, font = 2, mar = c(5, 4, 2, 2))
survplot(npsurv(Surv(ageleft, ageright, cens) ~ 1, nbaupd), lwd = 3,
         xlab = "Age at death",
         time.inc = 5, col.fill = grey(0.75),
         ylab = "Estimated survival probability")
segments(0, 0.5, 100, 0.5, lwd = 3)
dev.off()


### ===========================================================================
### Figure 2 (Standard Mortality Ratios) ======================================
### ===========================================================================
library(data.table)
library(epitools)

SMRs <- vector("list", 19)
names(SMRs) <- 2000:2018
for(i in seq_along(2000:2018)) {
  SMRs[[i]] <- vector("list", 2)
  names(SMRs[[i]]) <- c("AfricanAmerican", "White")
  SMRs[[i]][["AfricanAmerican"]] <- SMRs[[i]][["White"]] <- numeric(3)
}

years <- 2000:2018
for(y in seq_along(2000:2018)) {
  cat("\n")
  cat("Year", years[y], fill = TRUE)
  # Age distribution by January 1
  jan1 <- paste0(years[y], "-1-1")
  nbaupdBW$age <- as.vector(trunc(difftime(jan1, nbaupdBW$birthdate) / 365.25))

  # Excluded those with later debut and death before. Age is computed, hence,
  #   for active players on January 1 and alive former players.
  nbaupdBW$age <- with(nbaupdBW, ifelse(debut > as.Date(jan1) |
                                        dateevent < as.Date(jan1), NA, age))
  # New variable: Death in current year
  nbaupdBW$deathyr <- as.numeric(year(nbaupdBW$dateevent) == years[y] &
                                 nbaupdBW$cens == 1)

  # Separate tables for African-American and white players
  tabWDeaths <- with(subset(nbaupdBW, etni == "White"), table(age, deathyr))
  tabBDeaths <- with(subset(nbaupdBW, etni == "Black"), table(age, deathyr))

  # Auxiliary data frames
  auxW <- data.frame(age = as.numeric(rownames(tabWDeaths)),
                     nkW = rowSums(tabWDeaths), mkW = tabWDeaths[, 2])
  auxB <- data.frame(age = as.numeric(rownames(tabBDeaths)),
                     nkB = rowSums(tabBDeaths), mkB = tabBDeaths[, 2])

  # Mortality rates
  mrates <- merge(auxW, auxB, all = TRUE)

  # Merging with death rates of current year
  auxvs <- c("age", paste0("prob", years[y], "M", c("white", "black")))
  mrates <- merge(mrates, mortality[, auxvs], all = TRUE)
  names(mrates)[6:7] <- c("probWhite", "probBlack")

  # Computation of the SMRs
  smrW <- with(na.omit(mrates[, c(1:3, 6)]),
               round(ageadjust.indirect(mkW, nkW, stdcount = NULL, stdpop = NULL,
                                        stdrate = probWhite)$sir, 2))
  smrB <- with(na.omit(mrates[, c(1, 4, 5, 7)]),
               round(ageadjust.indirect(mkB, nkB, stdcount = NULL, stdpop = NULL,
                                        stdrate = probBlack)$sir, 2))

  # Results
  cat("White (ex-) players: SMR = ", smrW[3], "; 95%-CI: [", smrW[4], ", ",
      smrW[5], "].", sep = "", fill = TRUE)
  cat("African-American (ex-) players: SMR = ", smrB[3], "; 95%-CI: [",
      smrB[4], ", ", smrB[5], "].", sep = "", fill = TRUE)

  # Saving results
  SMRs[[y]][["White"]] <- smrW[3:5]
  SMRs[[y]][["AfricanAmerican"]] <- smrB[3:5]

  names(SMRs[[y]][["AfricanAmerican"]]) <- names(SMRs[[y]][["White"]]) <-
                                   c("SMR", "95%Low", "95%Upp")

  nbaupdBW$age <- nbaupdBW$deathyr <- NULL
  rm(jan1, tabWDeaths, tabBDeaths, auxW, auxB, auxvs)
}
rm(y)

## Graphical representation
## ------------------------
smrs <- data.frame(matrix(unlist(SMRs), nc = 6, byrow = TRUE))
rownames(smrs) <- years
names(smrs) <- paste0(rep(c("bl", "wh"), each = 3), c("smr", "low95", "upp95"))

# windows(width = 9)
pdf("SMRsOverTime_2021.pdf", width = 12)
# tiff("SMRsOverTime_2021.tiff", width = 860, height = 500)
par(las = 1, font = 2, font.lab = 2, font.axis = 4, xlog = TRUE,
    mar = c(5, 4, 2, 2))
with(smrs, plot(years - 0.1, blsmr, pch = 16, xlab = "Year", ylab = "SMR",
                ylim = c(0.2, 1.4), log = "y", cex = 1.5, type = "b", lwd = 2))
points(years + 0.1, smrs$whsmr, pch = 15, cex = 1.5, col = 4, type = "b",
       lwd = 2)
legend("bottomleft", c("African-American", "White"), lwd = 3, pch = c(16, 15),
       col = c(1, 4), bty = "n", cex = 1.3)
axis(1, at = 2000:2018)
abline(h = 1, lty = 2, lwd = 2)
dev.off()