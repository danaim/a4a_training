#' ==========================================================================
#' Module 1.0 — Welcome & Course Overview
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' Purpose: confirm that your machine can run everything we need this week.
#' Run this script top to bottom. It prints a report at the end.
#' If anything fails, tell us NOW — before Module 1.1.
#'

# ---- Section 1: R version ----

cat("R version:", R.version.string, "\n")

# a4a needs a reasonably recent R. Anything from 4.1 is fine.
r_ok <- getRversion() >= "4.1.0"


# ---- Section 2: The packages we need ----

# FLCore    the building blocks (FLQuant, FLStock, FLIndex)     Day 1
# FLa4a     the assessment model itself                         Days 2-4
# ggplotFL  plotting methods for FLR objects                    Day 1 onwards
# ggplot2   the grammar of graphics, used throughout            Day 1 onwards
# a4adiags  extra diagnostics: retrospective, hindcasting        Day 4
# FLBRP     equilibrium reference points (Fmsy, Bmsy)           Day 5
# FLasher   short-term forecasting and projections              Day 5

needed <- c("FLCore", "FLa4a", "ggplotFL", "ggplot2",
            "a4adiags", "FLBRP", "FLasher")

installed <- vapply(needed, requireNamespace, logical(1), quietly = TRUE)

for (p in needed) {
  if (installed[[p]]) {
    cat(sprintf("  OK      %-10s %s\n", p, packageVersion(p)))
  } else {
    cat(sprintf("  MISSING %-10s\n", p))
  }
}

#' If something is missing, install it from the FLR repository:
#'
#'   install.packages(c("FLCore", "FLa4a", "ggplotFL",
#'                      "a4adiags", "FLBRP", "FLasher"),
#'                    repos = "https://flr.r-universe.dev")
#'


# ---- Section 3: Load them ----

library(FLCore)
library(FLa4a)
library(ggplotFL)
library(ggplot2)
library(a4adiags)
library(FLBRP)
library(FLasher)

theme_set(theme_bw())


# ---- Section 4: Does an FLStock behave? ----

# ple4 is North Sea plaice, the example stock shipped with FLCore.
# We use it all week for demonstrations.
data(ple4)

ple4
plot(ple4)

# A quick sanity check on the object
stock_ok <- is(ple4, "FLStock") && all(dim(ple4) > 0)


# ---- Section 5: Does the assessment model run? ----

# This is the real test: FLa4a is compiled code, so it can install
# but still fail to run. We fit a tiny model to prove it works.
data(ple4.index)

# NOTE you will see a warning about "too many parameters" being removed.
# That is expected: the default submodel is richer than this data supports,
# and a4a drops the redundant terms. It is not an error, and it is exactly
# the kind of message we learn to read on Day 2.

fit_ok <- tryCatch({
  fit <- sca(ple4, FLIndices(ple4.index))
  is(fit, "a4aFit")
}, error = function(e) {
  cat("  sca() failed:", conditionMessage(e), "\n")
  FALSE
})

# If that worked, this is the assessment result as an FLStock
if (isTRUE(fit_ok)) {
  ple4_fitted <- ple4 + fit
  plot(FLStocks(input = ple4, assessment = ple4_fitted))
}


# ---- Section 6: Do the later-in-the-week tools run? ----

# We only need these from Thursday, but FLBRP and FLasher are compiled too,
# so we check them now rather than discovering a problem on Day 5.

# FLBRP — equilibrium reference points (Day 5, Module 5.1)
brp_ok <- tryCatch({
  rp <- brp(FLBRP(ple4))
  is(refpts(rp), "FLPar") && any(is.finite(c(refpts(rp))))
}, error = function(e) {
  cat("  FLBRP failed:", conditionMessage(e), "\n")
  FALSE
})

# FLasher — short-term forecast (Day 5, Module 5.2)
# Project 3 years at the most recent fbar, with geometric mean recruitment.
fwd_ok <- tryCatch({
  maxyr <- dims(ple4)$maxyear
  yrs   <- (maxyr + 1):(maxyr + 3)
  prj   <- fwd(stf(ple4, nyears = 3),
               sr      = predictModel(model = rec ~ a,
                                      params = FLPar(a = exp(mean(log(rec(ple4)))))),
               control = fwdControl(year  = yrs,
                                    quant = "fbar",
                                    value = c(fbar(ple4)[, ac(maxyr)])))
  all(!is.na(catch(prj)[, ac(yrs)]))
}, error = function(e) {
  cat("  FLasher failed:", conditionMessage(e), "\n")
  FALSE
})

# a4adiags — extra diagnostics (Day 4). Hindcast cross-validation refits the
# model many times, which is far too slow for a setup check, so we just
# confirm the package exposes the functions we will need on Thursday.
diags_ok <- all(c("a4ahcxval", "mase2", "plotXval2") %in%
                  ls(asNamespace("a4adiags")))


# ---- Section 7: Report ----

checks <- c(
  "R version >= 4.1"      = r_ok,
  "All packages present"  = all(installed),
  "FLStock usable"        = stock_ok,
  "sca() runs"            = isTRUE(fit_ok),
  "FLBRP runs"            = isTRUE(brp_ok),
  "FLasher runs"          = isTRUE(fwd_ok),
  "a4adiags available"    = isTRUE(diags_ok)
)

cat("\n---------------------------------------------\n")
for (nm in names(checks)) {
  cat(sprintf("  %-22s %s\n", nm, ifelse(checks[[nm]], "PASS", "FAIL")))
}
cat("---------------------------------------------\n")

if (all(checks)) {
  cat("  ALL CHECKS PASSED — you are ready.\n")
} else {
  cat("  SOMETHING FAILED — flag this before Module 1.1.\n")
}
cat("---------------------------------------------\n")


# ---- Key takeaway ----
#'
#' You now have a working a4a toolchain, and you have already seen the
#' two objects the whole week revolves around: an FLStock (your stock)
#' and an FLIndex (your survey). Module 1.1 explains where they come from.
#'
