## The simulation model sim2.mod as R functions, and the true covariate effects derived from it.
##
## In sim2.mod:
##   F1 = TVF1 * exp(ETA1), CL = TOTCL * exp(ETA2), V = TVV * exp(ETA3),
##   TOTCL = CLH + CLR, where the renal fraction FR = logistic(logit(0.4) + ETA7) varies between
##   subjects.
## So log CL/F = log TOTCL(ETA7) - log TVF1 + ETA2 - ETA1, and log V/F = log TVV - log TVF1 + ETA3 - ETA1.
## ETA1, ETA2 and ETA3 have mean zero and do not depend on covariates; they drop out of
## differences in expected values. ETA7 enters non-linearly and is integrated out here by
## Gauss-Hermite quadrature.

THETA_CL <- 6    # THETA(2) of sim2.mod; cancels in every ratio
THETA_V  <- 90   # THETA(3); cancels
OM_FR    <- 0.7  # variance of ETA7 (renal fraction, logit scale)

## The covariate part of $PK, vectorised over subjects. nci: grade 0 to 3; food: 1 fed.
simTVF1 <- function(food, smok, nci) {
  f <- ifelse(food == 1, 0.9, 1)
  f <- f * ifelse(smok == 1, 0.8, 1)
  f * c(1, 1.2, 1.3, 1.4)[nci + 1]
}
simTOTCL <- function(wt, crcl, smok, nci, eta7) {
  fr  <- 1 / (1 + exp(-(log(0.4 / 0.6) + eta7)))
  clh <- THETA_CL * (1 - fr) * (wt / 70)^0.75
  clh <- clh * c(1, 0.7, 0.5, 0.3)[nci + 1]
  clh <- clh * ifelse(smok == 1, 1.2, 1)
  clr <- THETA_CL * fr * (crcl / 120)^0.055
  clh + clr
}
simTVV <- function(wt, sex) THETA_V * (wt / 70) * ifelse(sex == 2, 1.3, 1)

## Gauss-Hermite nodes and weights for N(0, var), by the Golub-Welsch method.
ghNormal <- function(n = 40, var = OM_FR) {
  i <- seq_len(n - 1)
  J <- matrix(0, n, n)
  J[cbind(i, i + 1)] <- sqrt(i / 2)
  J[cbind(i + 1, i)] <- sqrt(i / 2)
  e <- eigen(J, symmetric = TRUE)
  list(x = e$values * sqrt(2 * var), w = e$vectors[1, ]^2)
}

## Expected log CL/F and log V/F for each row of a data frame with WT, CRCL, SMOK, NCIORG, SEX, FOOD.
## draws = NULL: quadrature; a number: Monte Carlo with that many draws of ETA7 (for checks).
expectedLog <- function(d, draws = NULL, seed = 1) {
  if (is.null(draws)) {
    gh <- ghNormal()
    lcl <- sapply(seq_len(nrow(d)), function(k)
      sum(gh$w * log(simTOTCL(d$WT[k], d$CRCL[k], d$SMOK[k], d$NCIORG[k], gh$x))))
  } else {
    set.seed(seed)
    eta7 <- rnorm(draws, 0, sqrt(OM_FR))
    lcl <- sapply(seq_len(nrow(d)), function(k)
      mean(log(simTOTCL(d$WT[k], d$CRCL[k], d$SMOK[k], d$NCIORG[k], eta7))))
  }
  lf <- log(simTVF1(d$FOOD, d$SMOK, d$NCIORG))
  data.frame(CLF = lcl - lf, VF = log(simTVV(d$WT, d$SEX)) - lf)
}

## One row per subject with the values the simulation used: NCI grade from NCIORG, height from
## sourceFile, a file with the columns ID and HT for every subject (133 heights are -99 in the
## analysis data), and fed.
truthSubjects <- function(dataFile = "dataset3-r2.csv",
                          sourceFile = "true-height.csv") {
  d <- read.csv(dataFile)
  s <- d[!duplicated(d$ID), ]
  src <- read.csv(sourceFile)
  src <- src[!duplicated(src$ID), ]
  s$HT <- src$HT[match(s$ID, src$ID)]
  stopifnot(!anyNA(s$HT), all(s$HT > 0))
  s$FOOD <- 1
  s$RACE_2 <- as.numeric(s$RACE == 2)
  s$RACE_3 <- as.numeric(s$RACE == 3)
  cbind(s, expectedLog(s))
}

## The FREM covariate columns, as the FREM model is parameterised
fremCols <- c("LNWT", "HT", "AGE", "BMI", "CRCL", "RACE_2", "RACE_3", "NCI", "SEX", "SMOK")
catGroups <- c("RACE", "NCI", "SEX", "SMOK")

## The columns of one covariate group in a row of setupDfCovs()
groupCols <- function(g) if (g == "RACE") c("RACE_2", "RACE_3") else g
## The value of a categorical group in a row: the level
groupLevel <- function(row, g) {
  if (g == "RACE") return(1 + row[["RACE_2"]] + 2 * row[["RACE_3"]])
  row[[g]]
}

## True effects at the rows of a Forest plot, as ratios to the reference.
##   dfCovs: rows from setupDfCovs() (columns LNWT, HT, ..., RACE_2, RACE_3, NCI, SEX, SMOK,
##           COVARIATEGROUPS); dfRef: one reference row in the same coding (for type
##           "unconditional", one row per row of dfCovs, as setupDfRefRow(singleRef = FALSE) gives).
##   type "unconditional": simple linear regression on the covariate of the row (categorical:
##           mean by level), what a FREM model gives when the other covariates vary as in the data;
##   type "conditionalSim": the reference subject with the covariate of the row changed, through
##           the simulation model;
##   type "conditionalFREM": the coefficients of a multiple linear regression on all FREM columns,
##           what a fully conditioned FREM model estimates when the truth is not linear on its scale.
##   on: for type "conditionalFREM", the covariate groups of the regression (default all FREM
##           covariates). With the covariates of a model for one parameter, the coefficients are
##           what a FREM model conditioned on those covariates alone estimates, the others
##           integrated out.
trueEffects <- function(subj, dfCovs, dfRef, type, params = c("CLF", "VF"),
                        on = c("LNWT", "HT", "AGE", "BMI", "CRCL", "RACE", "NCI", "SEX", "SMOK")) {
  dfCovs <- as.data.frame(dfCovs)
  dfRef <- as.data.frame(dfRef)
  if (nrow(dfRef) == 1) dfRef <- dfRef[rep(1, nrow(dfCovs)), ]
  out <- expand.grid(row = seq_len(nrow(dfCovs)), param = params, stringsAsFactors = FALSE)
  out$ratio <- NA_real_
  onCols <- unlist(lapply(on, groupCols))
  fitAll <- lapply(setNames(params, params), function(p)
    lm(reformulate(onCols, response = p), data = subj))
  for (k in seq_len(nrow(out))) {
    i <- out$row[k]; p <- out$param[k]
    g <- dfCovs$COVARIATEGROUPS[i]
    row <- dfCovs[i, ]; ref <- dfRef[i, ]
    if (type == "unconditional") {
      if (g %in% catGroups) {
        lev <- groupLevel(row, g); lref <- groupLevel(ref, g)
        sl <- if (g == "RACE") subj$RACE else subj[[g]]
        d <- mean(subj[[p]][sl == lev]) - mean(subj[[p]][sl == lref])
      } else {
        b <- coef(lm(subj[[p]] ~ subj[[g]]))[2]
        d <- b * (row[[g]] - ref[[g]])
      }
    } else if (type == "conditionalFREM") {
      cols <- groupCols(g)
      d <- sum(coef(fitAll[[p]])[cols] * (unlist(row[cols]) - unlist(ref[cols])))
    } else if (type == "conditionalSim") {
      ## the reference subject in the units of the simulation
      sim0 <- data.frame(WT = exp(ref$LNWT), CRCL = ref$CRCL, SMOK = ref$SMOK,
                         NCIORG = 0, SEX = ref$SEX, FOOD = 1)
      stopifnot(ref$NCI == 0)
      sim1 <- sim0
      if (g == "LNWT") sim1$WT <- exp(row$LNWT)
      if (g == "CRCL") sim1$CRCL <- row$CRCL
      if (g == "SMOK") sim1$SMOK <- row$SMOK
      if (g == "SEX")  sim1$SEX <- row$SEX
      if (g == "NCI" && row$NCI == 1) {
        ## NCI grade 1 or 2: the mix of grades in the data
        w <- table(subj$NCIORG[subj$NCI == 1])
        sim1 <- sim1[rep(1, length(w)), ]
        sim1$NCIORG <- as.numeric(names(w))
        e1 <- weighted.mean(expectedLog(sim1)[[p]], as.numeric(w))
      } else {
        e1 <- expectedLog(sim1)[[p]]
      }
      d <- e1 - expectedLog(sim0)[[p]]
    } else stop("unknown type ", type)
    out$ratio[k] <- exp(d)
  }
  out
}

## True explained variability: the share of the between-subject variability in log CL/F and
## log V/F that the covariates explain. The random effects of sim2.mod add var(ETA2 - ETA1) =
## 0.162 to log CL/F and var(ETA3 - ETA1) = 0.14 to log V/F; ETA7 adds its own part to log CL/F.
trueExplained <- function(subj) {
  gh <- ghNormal()
  varEta7 <- sapply(seq_len(nrow(subj)), function(k) {
    l <- log(simTOTCL(subj$WT[k], subj$CRCL[k], subj$SMOK[k], subj$NCIORG[k], gh$x))
    sum(gh$w * l^2) - sum(gh$w * l)^2
  })
  covVar <- c(CLF = var(subj$CLF), VF = var(subj$VF))
  etaVar <- c(CLF = 0.1 + 0.1 - 2 * 0.019 + mean(varEta7), VF = 0.1 + 0.1 - 2 * 0.030)
  covVar / (covVar + etaVar)
}
