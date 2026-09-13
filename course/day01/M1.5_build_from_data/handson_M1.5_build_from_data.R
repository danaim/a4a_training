#' ==========================================================================
#' Module 1.5 — Building a complete FLStock + FLIndices
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' Part A: finish the demo. We rebuild the MUT GSA 32 stock and MEDITS index
#' from Modules 1.3 and 1.4, put them together, and run the first assessment.
#'
#' Part B is your own data — see exercise_M1.5_own_data.R
#'

# ---- Setup ----
library(FLCore)
library(FLa4a)
library(ggplotFL)

data_dir <- "../../_data/MUT_GSA32"

yrs  <- 1998:2017
ages <- 1:10


# ---- Section 1: Rebuild the stock (Module 1.3, condensed) ----

rd <- function(f) read.csv(file.path(data_dir, f))
fq <- function(x, units = "") FLQuant(x, dimnames = list(age = ages, year = yrs),
                                      units = units)
tot <- function(x, units = "t") FLQuant(x, dimnames = list(age = "all", year = yrs),
                                        units = units)

landings.flq    <- tot(rd("landings.csv")$data)
landings.n.flq  <- fq(rd("landings_n.csv")$data,  "1000")
landings.wt.flq <- fq(rd("landings_wt.csv")$data, "kg")

discards.flq    <- tot(rd("discards.csv")$data)
discards.n.flq  <- fq(rd("discards_n.csv")$data,  "1000")
discards.wt.flq <- fq(rd("discards_wt.csv")$data, "kg")

stock.wt.flq    <- fq(rd("stock_wt.csv")$data, "kg")

catch.flq    <- landings.flq + discards.flq
catch.n.flq  <- landings.n.flq + discards.n.flq
catch.wt.flq <- (discards.n.flq / catch.n.flq) * discards.wt.flq +
                (landings.n.flq / catch.n.flq) * landings.wt.flq

m.flq      <- fq(0.1, "m")
mat.flq    <- fq(c(0, 0.5, 0.5, rep(1, 7)))
m.spwn.flq <- fq(0)
h.spwn.flq <- fq(0)

stk <- FLStock(catch.n.flq)
catch(stk) <- catch.flq;             catch.n(stk) <- catch.n.flq
catch.wt(stk) <- catch.wt.flq
landings(stk) <- landings.flq;       landings.n(stk) <- landings.n.flq
landings.wt(stk) <- landings.wt.flq
discards(stk) <- discards.flq;       discards.n(stk) <- discards.n.flq
discards.wt(stk) <- discards.wt.flq
stock.wt(stk) <- stock.wt.flq
m(stk) <- m.flq;                     mat(stk) <- mat.flq
m.spwn(stk) <- m.spwn.flq;           harvest.spwn(stk) <- h.spwn.flq

range(stk)["minfbar"] <- 2
range(stk)["maxfbar"] <- 6
name(stk) <- "MUT"
desc(stk) <- "MUT in GSA32. WGSAD 2018"
units(harvest(stk)) <- "f"

stk


# ---- Section 2: Rebuild the index (Module 1.4, condensed) ----

idx.flq <- FLQuant(rd("index.csv")$data,
                   dimnames = list(age = ages, year = yrs))

idx <- FLIndex(idx.flq)
index(idx) <- idx.flq
range(idx)["startf"] <- 6 / 12
range(idx)["endf"]   <- 8 / 12
name(idx) <- "MEDITS"
desc(idx) <- "Medits in GSA 32"


# ---- Section 3: Check before fitting ----

validObject(stk)

# verify() reports, slot by slot, whether values are plausible
verify(stk)

# ---- Section 4: Wrap the index ----

# Naming the element pays off later: diagnostics label plots with it.
idxs <- FLIndices(MEDITS = idx)

idxs
names(idxs)


# ---- Section 5: The first fit ----

# The default submodels. We do not configure anything yet — that is Day 2.
fit <- sca(stock = stk, indices = idxs)

fit

#' You may see a warning about parameters being removed. That is a4a telling
#' you the default model is richer than this data supports. It is not an
#' error, and we learn to read it properly tomorrow.


# ---- Section 6: Put the results back into the stock ----

# stk + fit fills the three slots we deliberately left empty:
# stock.n, stock and harvest.
stk.fit <- stk + fit

# Before: rec, ssb and fbar were empty
summary(stk)

# After: they are estimated
summary(stk.fit)


# ---- Section 7: Look at it ----

plot(stk.fit)

# Input against assessment, side by side
plot(FLStocks(input = stk, assessment = stk.fit))


# ---- Key takeaway ----
#'
#' The data work is done. You built an FLStock and an FLIndices from plain
#' CSV files, checked them, and the model accepted them.
#'
#' Nothing here says the fit is GOOD. Reading that is Day 2.
#'
