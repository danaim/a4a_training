#' ==========================================================================
#' Module 1.6 — Inspecting, manipulating and visualising
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' We work on the MUT GSA 32 stock you built this morning, fitted.
#' Everything here applies just as well to your own stock.
#'

# ---- Setup ----
library(FLCore)
library(FLa4a)
library(ggplotFL)
library(patchwork)
library(lattice)

theme_set(theme_bw())

data_dir <- "../../_data/MUT_GSA32"
yrs <- 1998:2017; ages <- 1:10

rd  <- function(f) read.csv(file.path(data_dir, f))
fq  <- function(x, u = "") FLQuant(x, dimnames = list(age = ages, year = yrs), units = u)
tot <- function(x) FLQuant(x, dimnames = list(age = "all", year = yrs), units = "t")

ln <- fq(rd("landings_n.csv")$data, "1000"); lw <- fq(rd("landings_wt.csv")$data, "kg")
dn <- fq(rd("discards_n.csv")$data, "1000"); dw <- fq(rd("discards_wt.csv")$data, "kg")
cn <- ln + dn

stk <- FLStock(cn)
catch.n(stk) <- cn
catch(stk) <- tot(rd("landings.csv")$data) + tot(rd("discards.csv")$data)
catch.wt(stk) <- (dn / cn) * dw + (ln / cn) * lw
landings(stk) <- tot(rd("landings.csv")$data)
landings.n(stk) <- ln; landings.wt(stk) <- lw
discards(stk) <- tot(rd("discards.csv")$data)
discards.n(stk) <- dn; discards.wt(stk) <- dw
stock.wt(stk) <- fq(rd("stock_wt.csv")$data, "kg")
m(stk) <- fq(0.1, "m"); mat(stk) <- fq(c(0, 0.5, 0.5, rep(1, 7)))
m.spwn(stk) <- fq(0); harvest.spwn(stk) <- fq(0)
range(stk)[c("minfbar", "maxfbar")] <- c(2, 6)
name(stk) <- "MUT"; units(harvest(stk)) <- "f"

idx <- FLIndex(fq(rd("index.csv")$data))
index(idx) <- fq(rd("index.csv")$data)
range(idx)[c("startf", "endf")] <- c(6/12, 8/12)
name(idx) <- "MEDITS"

fit     <- sca(stk, FLIndices(MEDITS = idx))
stk.fit <- stk + fit


# ---- Section 1: The quick check ----

summary(stk.fit)

dim(stk.fit)
dimnames(catch.n(stk.fit))$year
range(stk.fit)

# Units, slot by slot. Check these before you plot anything —
# an unlabelled axis usually means a missing unit.
units(stk.fit)

# Missing ones can be set in bulk
units(stock(stk.fit)) <- "t"
units(stock.n(stk.fit)) <- "1000"


# ---- Section 2: metrics ----

# The standard quantities, all at once
names(metrics(stk.fit))
metrics(stk.fit)$SSB

# Each is also a method you can call directly
ssb(stk.fit)
rec(stk.fit)
fbar(stk.fit)

# Less common, but there when you need them
tsb(stk.fit)      # total stock biomass
vb(stk.fit)       # vulnerable biomass
fapex(stk.fit)    # F at the most-selected age
sp(stk.fit)       # surplus production


# ---- Section 3: Subsetting and trimming ----

# By position or by label — remember dimnames are character
catch.n(stk.fit)[, "2005"]
catch.n(stk.fit)[1:3, 1:5]

# window() cuts or extends years
stk.short <- window(stk.fit, start = 2005, end = 2017)
dim(stk.short)

# trim() subsets any dimension by name
trim(catch.n(stk.fit), age = 1:4)

# Subsetting a whole FLStock works too
dim(stk.fit[1:5, ])


# ---- Section 4: plot() on each class ----

plot(ssb(stk.fit))        # an FLQuant
plot(stk.fit)             # an FLStock
plot(idx)                 # an FLIndex

# Several stocks, overlaid — the comparison plot of the week
plot(FLStocks(input = stk, fitted = stk.fit))


# ---- Section 5: Choose your own panels ----

# The default four panels come from plot()'s metrics argument.
# Look at it again if you like:
#   getMethod("plot", signature = list("FLStock", "missing"))

plot(stk.fit, metrics = list(SSB = ssb, Fbar = fbar))

# Any named list of functions works
plot(stk.fit, metrics = list(Biomass = stock, Landings = landings,
                             Discards = discards, F = fapex))

#' CAREFUL panel names are parsed as plotmath expressions, so they cannot
#' contain spaces. `Mean F` throws "unexpected symbol". Use Fbar, or Mean.F,
#' or proper plotmath:

plot(stk.fit, metrics = list(SSB = ssb, `bar(F)` = fbar))

# EXERCISE 1 -----------------------------------------------------------
# Plot only SSB and Recruitment, with your own panel titles.
# Remember the no-spaces rule.
# ----------------------------------------------------------------------


# ---- Section 6: Write your own metric ----

# A metric is just a function that takes the stock and returns an FLQuant.
# Here: the proportion of total biomass sitting in the plusgroup.

pgroup <- function(x) {
  (stock.n(x) * stock.wt(x))[dim(x)[1], ] / stock(x)
}

plot(stk.fit, metrics = list(Recruits = rec, Plusgroup = pgroup))

#' A high and rising plusgroup proportion is worth noticing — it can mean
#' the plusgroup is absorbing too much of the stock.


# ---- Section 7: FLQuants — your own collection ----

# Bundle any FLQuants together and plot them in one go
fqs <- FLQuants(F      = fbar(stk.fit),
                SSB    = ssb(stk.fit),
                Rec    = rec(stk.fit),
                Catch  = catch(stk.fit),
                Biomass = stock(stk.fit))

plot(fqs)

# metrics() already returns an FLQuants, so this works directly
plot(metrics(stk.fit))

# And it is a ggplot, so keep adding
plot(metrics(stk.fit)) +
  geom_line(aes(colour = qname)) +
  geom_point(size = 0.6) +
  theme(legend.position = "none")


# ---- Section 8: FLQuant plots — 1D, 2D, 3D ----

# 1D: a single series
plot(catch(stk.fit))

# 2D: age x year gets faceted
plot(catch.n(stk.fit))

# Logs often make age structure readable
plot(log(catch.n(stk.fit))) + facet_grid(age ~ .)

# 3D: a surface, via lattice
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk.fit)),
          drape = TRUE, main = "Population",
          col.regions = colorRampPalette(c("green", "red"))(100),
          screen = list(x = -90, y = -45))

wireframe(data ~ age + year, data = as.data.frame(harvest(stk.fit)),
          drape = TRUE, main = "Fishing mortality",
          col.regions = colorRampPalette(c("green", "red"))(100),
          screen = list(x = -85, y = -50))

#' RULE OF THUMB for FLR plots: data ~ year, with age, season and area
#' as facets, and unit as the grouping variable.


# ---- Section 9: Adding elements with + ----

# plot() returns a ggplot, so you can keep building on it
plot(catch(stk.fit)) +
  ylab("Catch (t)") +
  ylim(c(0, NA)) +
  ggtitle("MUT GSA32")

# plotmath for proper axis labels — see ?plotmath for the syntax
plot(catch(stk.fit)) +
  ylab(expression(Catch (t %.% 10^3))) +
  xlab("")


# ---- Section 10: as.data.frame — the escape hatch ----

df <- as.data.frame(catch.n(stk.fit))
head(df)

# Extra columns you can ask for: a real date, the cohort, the timestep.
# cohort is the useful one — it is year minus age.
head(as.data.frame(catch.n(stk.fit), date = TRUE, cohort = TRUE, timestep = TRUE))

# With a date column, ggplot handles the time axis properly
ggplot(catch(stk.fit), aes(x = date, y = data)) + geom_line()


# ---- Section 11: Your own ggplots ----

# Bubble plot. scale_size controls how dramatic the bubbles are.
ggplot(catch.n(stk.fit), aes(year, as.factor(age), size = data)) +
  geom_point(shape = 21) +
  scale_size(range = c(1, 12)) +
  ylab("age") +
  theme(legend.position = "none") +
  ggtitle("Catch at age")

# Biomass at age, as a distribution over years
ggplot(stock.n(stk.fit) * stock.wt(stk.fit), aes(x = factor(age), y = data)) +
  geom_boxplot() +
  xlab("Age") + ylab("Biomass (t)")

# Cohorts: follow a year class along the diagonal
ggplot(stock.n(stk.fit), aes(year, data, colour = factor(age))) +
  geom_line() +
  scale_y_log10() +
  labs(colour = "age", y = "stock.n (log scale)")

# Weight at age, by decade
ggplot(stock.wt(stk.fit), aes(x = age, y = data, group = year)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ (floor(year / 10) * 10)) +
  labs(y = "Stock weight (kg)")

# EXERCISE 2 -----------------------------------------------------------
# Make a bubble plot of the SURVEY index instead of the catch.
# Does the survey see the same strong year classes as the catch?
# ----------------------------------------------------------------------


# ---- Section 12: geom_flpar — reference points on a plot ----

# FLPar holds named parameters; geom_flpar draws them as labelled lines.
# We compute real ones on Day 5 — these are invented.
refpts <- FLPar(FMSY = 0.30, `F0.1` = 0.20)
class(refpts)

plot(fbar(stk.fit)) +
  ylim(c(0, NA)) +
  geom_flpar(data = refpts, x = 2000)   # x positions the labels

# One FLPar per panel, using FLPars
plot(stk.fit, metrics = list(SSB = ssb, F = fbar)) +
  geom_flpar(data = FLPars(SSB = FLPar(Blim = 3e5, Bpa = 5e5),
                           F   = FLPar(FMSY = 0.30)),
             x = c(2000, 2000, 2000))


# ---- Section 13: cohcorrplot — cohort correlations ----

# Correlation between numbers at age, following cohorts.
# A strong diagonal signal means the year classes track through the ages.
cohcorrplot(catch.n(stk.fit))

#' NOTE this may print a warning from ggplotFL. It still draws.


# ---- Section 14: patchwork — assembling figures ----

# https://patchwork.data-imaginist.com/

p1 <- ggplot(mat(stk.fit), aes(x = age, y = data)) + geom_line() +
  ylab("Maturity") + xlab("Age")

p2 <- ggplot(stock.wt(stk.fit), aes(x = age, y = data, group = year)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ (floor(year / 10) * 10)) + ylab("Weight (kg)") + xlab("Age")

p3 <- plot(stk.fit, metrics = list(SSB = ssb, F = fbar))

# Side by side with |, stacked with /
p1 | p2
p1 / p2

# Combine in almost any way
p3 / (p1 | p2)

# Or let wrap_plots do the arranging
wrap_plots(p1, p2, p3, ncol = 2)

wrap_plots(p1 | p2, p3, ncol = 1, heights = c(1, 1))

# Highlight a period, and centre the title
plot(metrics(stk.fit)) +
  ggtitle("MUT in GSA 32") +
  annotate("rect", xmin = 2010, xmax = 2017, ymin = -Inf, ymax = Inf,
           fill = "#E69F00", alpha = 0.1) +
  theme(plot.title = element_text(hjust = 0.5))


# ---- Section 15: Saving ----

# ggsave takes the last plot, or a named object
ggsave("mut_overview.png", plot = p3 / (p1 | p2),
       width = 10, height = 6, dpi = 150)

# Clean up so the folder stays tidy
file.remove("mut_overview.png")


# ---- Key takeaway ----
#'
#' summary() to check, plot() for the overview, as.data.frame() plus
#' ggplot2 for everything else. plot() takes a metrics argument, so the
#' panels are your choice, not a fixed default — and a metric is just a
#' function returning an FLQuant, so you can write your own.
#'
#' That is Day 1: your data is in FLR, checked, and fitted.
#' Tomorrow we ask whether the fit is any good.
#'
