#' ==========================================================================
#' Module 2.3 — Adding age effects
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' The mean model left a clear age pattern. We explore the age effect on F
#' and on q in isolation, then bring them together.
#'
#' Change ONE thing, refit, look at the residuals by year AND by age.
#'

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


# ---- Section 1: The reference ----

# The mean model from Module 2.1
fit01 <- sca(stk, idx, fmodel = ~1, qmodel = list(~1),
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)

res01 <- residuals(fit01, stk, idx)

plot(res01)
plot(res01, auxline = "l", by = "age")


# ---- Section 2: An age effect on F ----

#' factor(age) gives as many parameters as ages minus one, each independent
#' of the others.

fit02 <- sca(stk, idx,
             fmodel  = ~factor(age),    # <- the only change
             qmodel  = list(~1),
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)

res02 <- residuals(fit02, stk, idx)

plot(res02)
plot(res02, auxline = "l", by = "age")

# Less staggered, and the mean residual by age is nearer 0
round(c(yearMeans(res02$catch.n)), 2)


# ---- Section 3: An age effect on q instead ----

#' Add the age effect to catchability and remove it from the catch.

fit03 <- sca(stk, idx,
             fmodel  = ~1,
             qmodel  = list(~factor(age)),   # <- the only change
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)

res03 <- residuals(fit03, stk, idx)

plot(res03)
plot(res03, auxline = "l", by = "age")


# ---- Section 4: Both together ----

fit04 <- sca(stk, idx,
             fmodel  = ~factor(age),
             qmodel  = list(~factor(age)),
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)

res04 <- residuals(fit04, stk, idx)

plot(res04)
plot(res04, auxline = "l", by = "age")


# ---- Section 5: Never stop at the residuals ----

#' The residuals of fit04 look good. Now look at what it ESTIMATED.

range(c(fbar(stk + fit04)))    # essentially zero
range(c(ssb(stk + fit04)))     # impossibly large

plot(FLStocks(mean = stk + fit01, F.age = stk + fit02,
              q.age = stk + fit03, both = stk + fit04))

# Clean residuals do not make a possible stock. Check fbar, SSB and
# recruitment every time. The year effect in Module 2.4 fixes this one.
