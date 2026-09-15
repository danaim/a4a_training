#' ==========================================================================
#' Module 2.1 — The "mean" model
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' We start the assessment workflow from the simplest model that will fit,
#' and use its residuals to find out what the data are asking for.
#'
#' Stock: mut09, red mullet in GSA 9. It ships with FLa4a.
#'

# ---- Setup ----
library(FLCore)
library(FLa4a)
library(ggplotFL)
library(lattice)
library(latticeExtra)


data(mut09)
data(mut09.idx)

stk <- mut09
idx <- mut09.idx


# ---- Section 1: Know your stock first ----

# Everything from Day 1 applies
stk
summary(stk)

dim(stk)                                  # 5 ages, 19 years
dimnames(catch.n(stk))$age                # 0 to 4
range(stk)                                # plusgroup 4, fbar 1-3

plot(stk)

# And the index
idx
names(idx)
dimnames(index(idx[[1]]))$age             # 1 to 4 — no age 0

#' NOTE the survey does not cover age 0. That is normal: the index ages
#' only have to sit inside the stock's ages, not match them.

plot(idx[[1]])


# ---- Section 2: The mean model ----

#' Every submodel set to ~1: a single parameter, no age effect,
#' no year effect, nothing.

fit01 <- sca(stk, idx,
             fmodel  = ~1,     # one F for all ages and years
             qmodel  = list(~1),   # one catchability for all ages
             srmodel = ~1,     # constant recruitment
             vmodel  = list(~1, ~1),  # one variance for catch, one for index
             n1model = ~1)     # flat age structure in the first year

fit01

fitSumm(fit01)

# 6 parameters for 171 observations. It is meant to fit badly.


# ---- Section 3: Residuals ----

res01 <- residuals(fit01, stk, idx)

# One component per data source
names(res01)

# Each is an FLQuant of standardized residuals, on the log scale.
# Positive = the model predicted LESS than was observed.
res01$catch.n


# ---- Section 4: The two standard views ----

# By year: one panel per age, residuals along time
plot(res01)

# By age: one panel per year, residuals across ages.
# auxline="l" adds a line so the shape is easier to see.
plot(res01, auxline = "l", by = "age")


# This is a structural issue of the model.


# ---- Section 6: Look at the fit itself ----

# What did a single F and a single q actually produce?
stk01 <- stk + fit01

plot(stk01)

# Flat F at every age and year — by construction
harvest(stk01)

# Assumption of a flat selection pattern: every age fished equally

