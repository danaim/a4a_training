#' ==========================================================================
#' Module 1.2 — FLQuant, the building block
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' Run each section step by step during the guided session.
#' Everything here is standalone: we build FLQuants from scratch,
#' no stock objects yet.
#'

# ---- Setup ----
library(FLCore)


# ---- Section 1: The constructor ----

# The empty object. Note: six dimensions, all of length 1.
FLQuant()

dim(FLQuant())
names(dimnames(FLQuant()))

# A single value, recycled over the dimensions you name
FLQuant(0.2, dimnames = list(age = 1:2, year = 2000:2001))

# A vector. Values fill down the quant first, then across years.
FLQuant(1:6, dimnames = list(age = 1:3, year = 2000:2001), units = "kg")


# ---- Section 2: The quant dimension is flexible ----

# "quant" is a placeholder — it takes the name you give it
names(dimnames(FLQuant(dimnames = list(age = 1:3))))
names(dimnames(FLQuant(dimnames = list(length = c(10, 20, 30)))))

# This matters for length-based data. For us it will always be age.


# ---- Section 3: From a matrix ----

# This is the route your own data will take in Module 1.5:
#   read.csv() -> as.matrix() -> FLQuant()

ma <- matrix(rlnorm(50), nrow = 5, ncol = 10)

flm <- FLQuant(ma, quant = "age")
flm

# Without dimnames, FLR numbers the ages and years 1..n for you.
dimnames(flm)$age
dimnames(flm)$year

# Usually you want to say what they really are:
flm2 <- FLQuant(ma, dimnames = list(age = 1:5, year = 2010:2019), units = "1000")
flm2


# ---- Section 4: Data AND metadata ----

flq <- FLQuant(1:6, dimnames = list(age = 1:3, year = 2000:2001), units = "kg")

dim(flq)        # the shape
dimnames(flq)   # the labels
units(flq)      # the units of measurement

# Units can be set after
units(flq) <- "t"
units(flq)


# ---- Section 5: Units are not decoration ----

# FLR knows how units combine. Numbers in thousands times weight in kg
# gives tonnes — and FLR works that out on its own.

n <- FLQuant(1000, units = "1000")   # numbers, thousands
w <- FLQuant(0.5,  units = "kg")     # mean weight, kg

units(n * w)   # "t"

# Division works too
units(n / n)


# ---- Section 6: Arithmetic ----

# FLQuants behave like arrays: operations are element by element.

fla <- FLQuant(1:9, dimnames = list(age = 1:3, year = 2000:2002), units = "1000")

fla + fla
fla * 2
fla / (fla * 3)

# Units are carried through the arithmetic
units(fla + fla)   # "1000" — unchanged
units(fla / fla)   # ""     — they cancel

# Dimensions have to match.
x <- FLQuant(runif(6), dim = c(3, 2))
y <- FLQuant(runif(9), dim = c(3, 3))

# x * y            # ERROR: non-conformable arrays
#                  # uncomment to see it fail

# The case you WILL hit: one value per year, to be applied at every age.
# Think of a yearly discard ratio multiplying a weight-at-age matrix.

flb <- FLQuant(c(2, 4, 6), dimnames = list(age = "all", year = 2000:2002))

dim(fla)   # 3 ages x 3 years
dim(flb)   # 1 age  x 3 years

# A plain * refuses, because 3 ages and 1 age do not conform
# fla * flb        # ERROR: non-conformable arrays
#                  # uncomment to see it fail

# %*% expands the length-1 dimension for you
fla %*% flb

#' NOTE the iter dimension is the other exception: 1 vs N is allowed and
#' gets expanded automatically. That matters on Day 4, not today.


# ---- Section 7: Summarising along dimensions ----

# Collapse one dimension, keep the rest.

quantSums(fla)    # sum over ages   -> one value per year
quantMeans(fla)   # mean over ages
yearSums(fla)     # sum over years  -> one value per age
yearMeans(fla)    # mean over years

# Geometric mean over years, the usual recruitment summary
exp(yearMeans(log(fla)))

# The general tool underneath: apply over the dimensions you keep
apply(fla, 2:6, sum, na.rm = TRUE)

# See help("quantSums") for the whole family


# ---- Section 8: Two things that catch everybody ----

# 1. Dimnames are CHARACTER, not numbers
dimnames(flq)$year          # "2000" "2001"

flq[, "2000"]               # by label  — works
flq[, 1]                    # by position — works

# flq[, 2000]               # by "year number" — ERROR, out of bounds
#                           # uncomment to see it fail

# 2. Dimensions are NOT dropped by default
dim(flq[1, ])               # still 6 dimensions
dim(flq[1, , drop = TRUE])  # now a plain vector

class(flq[1, ])
class(flq[1, , drop = TRUE])


# ---- Section 9: Exercise ----

#' Reproduce EXACTLY this object. Check it with all.equal().
#'
#' An object of class "FLQuant"
#' , , unit = unique, season = all, area = unique
#'
#'      year
#' age   2000 2001 2002 2003 2004 2005
#'   all    9   10    2    3    2    9
#'
#' units:  NA
#'

# Hint: what is the quant dimension called, and how long is it?

# target <- FLQuant( ... )          # <- fill this in
# all.equal(target, solution)


# ---- Section 10: Exercise ----

#' You are handed a catch-at-age matrix: 6 ages (1-6), 20 years (1998-2017),
#' in thousands of fish.
#'
#' 1. Build it as an FLQuant with the correct dimnames and units
#' 2. Confirm dim() is what you expect
#' 3. Mean weight at age is 0.35 kg for every age and year. Build that too.
#' 4. Multiply them. What units do you get, and is that what you wanted?

cam <- matrix(rlnorm(120, log(500), 0.4), nrow = 6, ncol = 20)

# your code here


# ---- Key takeaway ----
#'
#' An FLQuant is a six-dimensional array that carries its own labels and
#' units. You will rarely use more than the first two dimensions, but the
#' metadata is what makes every other FLR package able to read your data.
#'
