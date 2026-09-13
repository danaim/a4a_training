#' ==========================================================================
#' Module 1.3 — FLStock: structure, slots and meaning
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' We build a complete FLStock for red mullet (MUT) in GSA 32,
#' 1998-2017, ages 1-10, from plain CSV files.
#'
#' This is the same route your own data will take.
#' The survey index comes in Module 1.4, the assessment in Module 1.5.
#'

# ---- Setup ----
library(FLCore)
library(ggplotFL)

# All eight CSV files live here
data_dir <- "../../_data/MUT_GSA32"

# The dimensions of this stock. Everything we build must agree with these.
yrs  <- 1998:2017
ages <- 1:10


# ---- Section 1: What are we aiming at? ----

# An empty FLStock has 20 slots: 17 FLQuants, plus name, desc and range.
slotNames("FLStock")
length(slotNames("FLStock"))

# Many we fill ourselves. stock, stock.n and harvest are estimated
# by the assessment model, so we leave them alone.


# ---- Section 2: Landings ----

# Total landings: one value per year, so the quant dimension is "all"
land <- read.csv(file.path(data_dir, "landings.csv"))
head(land)

landings.flq <- FLQuant(land$data,
                        dimnames = list(age = "all", year = yrs),
                        units = "t")
landings.flq

# Landings numbers at age: a full ages x years matrix
land.n <- read.csv(file.path(data_dir, "landings_n.csv"))

landings.n.flq <- FLQuant(land.n$data,
                          dimnames = list(age = ages, year = yrs),
                          units = "1000")

# Landings mean weight at age
land.wt <- read.csv(file.path(data_dir, "landings_wt.csv"))

landings.wt.flq <- FLQuant(land.wt$data,
                           dimnames = list(age = ages, year = yrs),
                           units = "kg")

#' ALWAYS spot-check one value against the raw file. FLQuant fills in array
#' order, so a file sorted the other way round transposes silently.
land.n$data[land.n$age == 3 & land.n$year == 2005]
c(landings.n.flq["3", "2005"])


# ---- Section 3: Discards ----

disc <- read.csv(file.path(data_dir, "discards.csv"))

discards.flq <- FLQuant(disc$data,
                        dimnames = list(age = "all", year = yrs),
                        units = "t")

disc.n <- read.csv(file.path(data_dir, "discards_n.csv"))

# EXERCISE 1 -----------------------------------------------------------
# Build the discards numbers-at-age FLQuant, following the landings example.
# discards.n.flq <- FLQuant( ... )
# ----------------------------------------------------------------------

disc.wt <- read.csv(file.path(data_dir, "discards_wt.csv"))

discards.wt.flq <- FLQuant(disc.wt$data,
                           dimnames = list(age = ages, year = yrs),
                           units = "kg")


# ---- Section 4: Catch is derived, not read ----

# Catch is simply landings plus discards
catch.flq   <- landings.flq   + discards.flq
catch.n.flq <- landings.n.flq + discards.n.flq

# Catch mean weight at age is NOT the average of the two weights.
# It is the average weighted by how many fish came from each source.
disc.prop <- discards.n.flq / catch.n.flq
land.prop <- landings.n.flq / catch.n.flq

catch.wt.flq <- disc.prop * discards.wt.flq + land.prop * landings.wt.flq

#' Discards are small fish and usually far more numerous, so a plain
#' mean of the two weight matrices would be badly wrong.

# Sanity check: the proportions must sum to 1 everywhere
summary(c(disc.prop + land.prop))


# ---- Section 5: Stock weights ----

stk.wt <- read.csv(file.path(data_dir, "stock_wt.csv"))

stock.wt.flq <- FLQuant(stk.wt$data,
                        dimnames = list(age = ages, year = yrs),
                        units = "kg")

# Note: stock.wt is yours to supply. stock.n and stock are NOT.


# ---- Section 6: Biology ----

# Natural mortality: here a single assumed value for all ages and years
m.flq <- FLQuant(0.1, dimnames = list(age = ages, year = yrs), units = "m")

# Maturity: one ogive, repeated for every year
maturity.vector <- c(0, 0.5, 0.5, rep(1, 7))
mat.flq <- FLQuant(maturity.vector,
                   dimnames = list(age = ages, year = yrs), units = "")

# Fraction of natural mortality before spawning. 0 = spawning on 1 January.
m.spwn.flq <- FLQuant(0, dimnames = list(age = ages, year = yrs), units = "")

# EXERCISE 2 -----------------------------------------------------------
# The fraction of FISHING mortality before spawning is also 0.
# Build that FLQuant.
# h.spwn.flq <- FLQuant( ... )
# ----------------------------------------------------------------------


# ---- Section 7: Assemble the FLStock ----

# Initialise with one FLQuant, so the object knows its dimensions
stk <- FLStock(catch.n.flq)
stk          # everything else is still empty

# Now fill it, slot by slot, through the accessors
catch(stk)        <- catch.flq
catch.n(stk)      <- catch.n.flq
catch.wt(stk)     <- catch.wt.flq

landings(stk)     <- landings.flq
landings.n(stk)   <- landings.n.flq
landings.wt(stk)  <- landings.wt.flq

discards(stk)     <- discards.flq
discards.n(stk)   <- discards.n.flq
discards.wt(stk)  <- discards.wt.flq

stock.wt(stk)     <- stock.wt.flq

m(stk)            <- m.flq
mat(stk)          <- mat.flq
m.spwn(stk)       <- m.spwn.flq
harvest.spwn(stk) <- h.spwn.flq

# That is 14 of the 17 FLQuant slots.
# stock, stock.n and harvest stay empty — sca() will estimate them.


# ---- Section 8: range, name, desc, units ----

range(stk)

# minfbar and maxfbar decide which ages are averaged to report mean F.
# This is a CHOICE, not data.
range(stk)["minfbar"] <- 2

# EXERCISE 3 -----------------------------------------------------------
# Set maxfbar to age 6.
# ----------------------------------------------------------------------

# Say what this object is, so you recognise it in six months
name(stk) <- "MUT"
desc(stk) <- "MUT in GSA32. WGSAD 2018"

# Units: harvest is estimated, but we still declare what it will hold
units(stk)
units(harvest(stk)) <- "f"


# ---- Section 9: Check it ----

stk
summary(stk)

validObject(stk)   # does it satisfy the class definition?

plot(stk)

# Individual slots are just FLQuants — plot them however you like
plot(catch.n(stk))


# ---- Key takeaway ----
#'
#' An FLStock is 17 FLQuants that must agree with each other in shape,
#' plus name, desc and range. You supply 14 of them; the assessment fills
#' the other three. Nothing checks that landings + discards = catch,
#' or that your units are right — that responsibility is yours.
#'
