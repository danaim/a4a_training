#' ==========================================================================
#' Module 2.4 — Adding year effects
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================

# ---- Setup ----
library(FLCore)
library(FLa4a)
library(ggplotFL)
library(lattice)
library(latticeExtra)

lattice.options(default.args = list(as.table = TRUE))

data(mut09)
data(mut09.idx)

stk <- mut09
idx <- mut09.idx


# ---- Section 1: Where we left off ----

# Both age effects, no year effect — the degenerate fit from Module 2.3
fit04 <- sca(stk, idx, fmodel = ~factor(age), qmodel = list(~factor(age)),
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)

res04 <- residuals(fit04, stk, idx)

range(c(fbar(stk + fit04)))    # essentially zero

# The year pattern we are answering: positive early, negative recently
plot(res04)


# ---- Section 2: A year effect on F ----

fit05 <- sca(stk, idx,
             fmodel  = ~factor(age) + factor(year),
             qmodel  = list(~1),
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)

res05 <- residuals(fit05, stk, idx)

plot(res05)
plot(res05, auxline = "l", by = "age")

# fbar is a real number again
range(c(fbar(stk + fit05)))

# Still not finished: the survey catchability looks like it has a year trend,
# and srmodel, n1model and vmodel are all still constants.


# ---- Section 3: The initial population ----

# n1model sets numbers at age in the first year, and propagates along cohorts
fit06 <- sca(stk, idx,
             fmodel  = ~factor(age) + factor(year),
             qmodel  = list(~factor(age)),
             srmodel = ~1, vmodel = list(~1, ~1),
             n1model = ~factor(age))

res06 <- residuals(fit06, stk, idx)

# Zoom into the early years, where N1 acts. Compare the two.
plot(window(res05, end = 2010), auxline = "l", by = "age")
plot(window(res06, end = 2010), auxline = "l", by = "age")


# ---- Section 4: The stock-recruitment submodel ----

fit07 <- sca(stk, idx,
             fmodel  = ~factor(age) + factor(year),
             qmodel  = list(~factor(age)),
             srmodel = ~factor(year),
             vmodel  = list(~1, ~1),
             n1model = ~factor(age))

res07 <- residuals(fit07, stk, idx)

# The by-year plot is the one that shows the effect
plot(res07)

# Recruitment is no longer a flat line
plot(FLQuants(constant = rec(stk + fit06), varying = rec(stk + fit07)))


# ---- Section 5: The assessments side by side ----

plot(FLStocks(F.year = stk + fit05, plus.N1 = stk + fit06, plus.SR = stk + fit07))
