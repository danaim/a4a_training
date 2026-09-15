#' ==========================================================================
#' Module 2.2 — Reading residuals
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

# The mean model from Module 2.1
fit01 <- sca(stk, idx, fmodel = ~1, qmodel = list(~1),
             srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)


# ---- Section 1: Three kinds of residual ----

d_s <- residuals(fit01, stk, idx)                      # standardized (default)
d_p <- residuals(fit01, stk, idx, type = "pearson")
d_r <- residuals(fit01, stk, idx, type = "deviances")  # raw

# Same fit, three scalings
round(c(quantile(c(d_r$catch.n), c(0.05, 0.5, 0.95))), 2)
round(c(quantile(c(d_s$catch.n), c(0.05, 0.5, 0.95))), 2)
round(c(quantile(c(d_p$catch.n), c(0.05, 0.5, 0.95))), 2)


# ---- Section 2: The two standard views ----

plot(d_s)                 # by year: one panel per age
plot(d_s, by = "age")     # by age: one panel per year

# Look at both, every time.


# ---- Section 3: Guide lines ----

# "l" loess   "r" regression   "g" grid   "h" line at zero
plot(d_s, auxline = "l")
plot(d_s[1], auxline = c("r", "g"))          # d_s[1] = catch, d_s[2] = survey
plot(d_s, by = "age", auxline = c("h", "g"))

names(d_s)


# ---- Section 4: Zoom in ----

plot(window(d_s, start = 2015), by = "age", auxline = c("h", "g"))


# ---- Section 5: Bubbles ----

bubbles(d_s)

# rows = age effect, columns = year effect, diagonals = cohort/recruitment


# ---- Section 6: Are they normal? ----

qqmath(d_s)


# ---- Section 7: Predicted against observed ----

plot(fit01, stk)     # catch at age
plot(fit01, idx)     # the survey


# ---- Section 8: Catch diagnostics ----

cd <- computeCatchDiagnostics(fit01, stk)

plot(cd)
plot(cd, type = "prediction", probs = c(0.025, 0.975))


# ---- Section 9: The numbers ----

fitSumm(fit01)      # convergence MUST be 0

AIC(fit01)
BIC(fit01)

# AIC and BIC only compare models fitted to the SAME data.


# ---- Section 10: Try it ----

# Refit with an age effect on F and repeat two diagnostics of your choice.
# Did what you expected to improve actually improve? This is Module 2.3.

# fit02 <- sca(stk, idx, fmodel = ~factor(age), qmodel = list(~1),
#              srmodel = ~1, vmodel = list(~1, ~1), n1model = ~1)
