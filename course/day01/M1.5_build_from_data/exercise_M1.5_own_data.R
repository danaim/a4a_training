#' ==========================================================================
#' Module 1.5 — Build an FLStock and FLIndices from YOUR data
#' a4a Training Course — Own-data Exercise Template
#' ==========================================================================
#'
#' HOW TO USE THIS FILE
#'
#' It runs as-is on the demo data, so you can see it work first.
#' Then change the settings in Section 0 and the file names in Section 1
#' to your own, and work down. Every place you must decide something is
#' marked TODO.
#'
#' Section 8 is a PASS/FAIL checklist. Get it green before you fit.
#'

# ---- Setup ----
library(FLCore)
library(FLa4a)
library(ggplotFL)


# ---- Section 0: Settings — TODO, all of these ----

data_dir <- "../../_data/MUT_GSA32"   # TODO your folder

yrs  <- 1998:2017                     # TODO your year range
ages <- 1:10                          # TODO your age range

fbar_range <- c(2, 6)                 # TODO ages averaged for mean F
plusgroup  <- NA                      # TODO age to collapse into, or NA

stock_name <- "MUT"                             # TODO
stock_desc <- "MUT in GSA32. WGSAD 2018"        # TODO

# Units. Get these right — FLR does unit algebra with them.
u_numbers <- "1000"   # TODO "1000" if thousands, "1" if individuals
u_weight  <- "kg"     # TODO "kg" or "g"
u_total   <- "t"      # TODO "t" or "kg"

# Does your data have discards reported separately?
have_discards <- TRUE                 # TODO


# ---- Section 1: Read your files ----

#' Two common layouts. Use whichever matches your files.
#'
#' LONG  — one row per age and year:   age, year, data
#' WIDE  — ages down, years across:    age, 1998, 1999, ...

read_long <- function(f) {
  d <- read.csv(file.path(data_dir, f))
  FLQuant(d$data, dimnames = list(age = ages, year = yrs))
}

read_wide <- function(f) {
  d <- read.csv(file.path(data_dir, f), row.names = 1)
  FLQuant(as.matrix(d), dimnames = list(age = ages, year = yrs))
}

# TODO pick one
read_aa <- read_long
# read_aa <- read_wide

# Totals are one value per year, so they are built differently
read_total <- function(f) {
  d <- read.csv(file.path(data_dir, f))
  FLQuant(d$data, dimnames = list(age = "all", year = yrs), units = u_total)
}

# TODO your file names
landings.n.flq  <- read_aa("landings_n.csv")
landings.wt.flq <- read_aa("landings_wt.csv")
landings.flq    <- read_total("landings.csv")

if (have_discards) {
  discards.n.flq  <- read_aa("discards_n.csv")
  discards.wt.flq <- read_aa("discards_wt.csv")
  discards.flq    <- read_total("discards.csv")
}

stock.wt.flq <- read_aa("stock_wt.csv")

# Set the units
units(landings.n.flq)  <- u_numbers
units(landings.wt.flq) <- u_weight
units(stock.wt.flq)    <- u_weight
if (have_discards) {
  units(discards.n.flq)  <- u_numbers
  units(discards.wt.flq) <- u_weight
}


# ---- Section 2: Spot-check ONE value ----

#' Do this every time. A transposed matrix gives a plausible,
#' completely wrong assessment, with no error anywhere.

# TODO pick an age and year that exist in your data, and compare
# the number below against what you see in the raw file.
landings.n.flq[ac(ages[2]), ac(yrs[3])]


# ---- Section 3: Catch ----

if (have_discards) {

  catch.flq   <- landings.flq   + discards.flq
  catch.n.flq <- landings.n.flq + discards.n.flq

  # Catch weight is the CATCH-WEIGHTED mean, not a plain average
  catch.wt.flq <- (discards.n.flq / catch.n.flq) * discards.wt.flq +
                  (landings.n.flq / catch.n.flq) * landings.wt.flq

} else {

  # No discards reported: catch is landings
  catch.flq    <- landings.flq
  catch.n.flq  <- landings.n.flq
  catch.wt.flq <- landings.wt.flq

}


# ---- Section 4: Biology — TODO ----

# Natural mortality. A single value, or a vector by age.
m.flq <- FLQuant(0.1, dimnames = list(age = ages, year = yrs), units = "m")

# Maturity ogive, one value per age, recycled across years
mat_ogive <- c(0, 0.5, 0.5, rep(1, length(ages) - 3))   # TODO yours
mat.flq <- FLQuant(mat_ogive, dimnames = list(age = ages, year = yrs))

# Fraction of M and F before spawning. 0 = spawning on 1 January.
m.spwn.flq <- FLQuant(0, dimnames = list(age = ages, year = yrs))
h.spwn.flq <- FLQuant(0, dimnames = list(age = ages, year = yrs))


# ---- Section 5: Assemble ----

stk <- FLStock(catch.n.flq)

catch(stk)        <- catch.flq
catch.n(stk)      <- catch.n.flq
catch.wt(stk)     <- catch.wt.flq

landings(stk)     <- landings.flq
landings.n(stk)   <- landings.n.flq
landings.wt(stk)  <- landings.wt.flq

if (have_discards) {
  discards(stk)    <- discards.flq
  discards.n(stk)  <- discards.n.flq
  discards.wt(stk) <- discards.wt.flq
}

stock.wt(stk)     <- stock.wt.flq
m(stk)            <- m.flq
mat(stk)          <- mat.flq
m.spwn(stk)       <- m.spwn.flq
harvest.spwn(stk) <- h.spwn.flq

range(stk)["minfbar"] <- fbar_range[1]
range(stk)["maxfbar"] <- fbar_range[2]
name(stk) <- stock_name
desc(stk) <- stock_desc
units(harvest(stk)) <- "f"

# Collapse a plusgroup, if you set one
if (!is.na(plusgroup)) stk <- setPlusGroup(stk, plusgroup)


# ---- Section 6: Your index ----

# TODO your index file, and its survey timing
idx.flq <- read_aa("index.csv")
units(idx.flq) <- ""

idx <- FLIndex(idx.flq)
index(idx) <- idx.flq

range(idx)["startf"] <- 6 / 12    # TODO survey start, fraction of year
range(idx)["endf"]   <- 8 / 12    # TODO survey end

name(idx) <- "MEDITS"             # TODO
desc(idx) <- "Medits in GSA 32"   # TODO

idxs <- FLIndices(idx)
names(idxs) <- name(idx)


# ---- Section 7: Look at it ----

summary(stk)
plot(stk)
plot(idx) + ggtitle(name(idx))


# ---- Section 8: PASS / FAIL checklist ----

#' Run this after every change. Everything must pass before you fit.

stk_ages <- dimnames(catch.n(stk))$age

catch_ok <- if (have_discards) {
  all.equal(c(catch.n(stk)), c(landings.n(stk) + discards.n(stk)))
} else TRUE

checks <- c(
  "FLStock is valid"        = isTRUE(validObject(stk)),
  "no NA in catch.n"        = !any(is.na(catch.n(stk))),
  "no NA in catch.wt"       = !any(is.na(catch.wt(stk))),
  "catch.n = land + disc"   = isTRUE(catch_ok),
  "maturity within 0-1"     = all(mat(stk) >= 0 & mat(stk) <= 1),
  "weights positive"        = all(stock.wt(stk) > 0, na.rm = TRUE),
  "fbar range within ages"  = all(fbar_range %in% as.numeric(stk_ages)),
  "units set on catch.n"    = !is.na(units(catch.n(stk))) && units(catch.n(stk)) != "",
  "survey timing set"       = !any(is.na(range(idx)[c("startf", "endf")]))
)

cat("\n---------------------------------------------\n")
for (nm in names(checks)) {
  cat(sprintf("  %-26s %s\n", nm, ifelse(isTRUE(checks[[nm]]), "PASS", "FAIL")))
}
cat("---------------------------------------------\n")
cat(if (all(vapply(checks, isTRUE, logical(1))))
      "  READY TO FIT\n" else "  FIX THE FAILURES ABOVE FIRST\n")
cat("---------------------------------------------\n")


# ---- Section 8b: verify() — things to look at ----

#' verify() checks every slot against a set of rules.
#'
#' THREE of its rules always fail before you fit, because stock, stock.n
#' and harvest are deliberately empty: "anyna", "harvest" and "cohorts".
#' That is expected. Ignore them.
#'
#' Anything ELSE it flags is worth understanding before you go on.

v <- as.data.frame(verify(stk))

empty_until_fitted <- c("anyna", "harvest", "cohorts")
to_inspect <- v[!v$name %in% empty_until_fitted & !v$valid,
                c("name", "items", "fails", "rule")]

if (nrow(to_inspect) == 0) {
  cat("\n  verify(): nothing else to inspect\n\n")
} else {
  cat("\n  verify() flagged these. Check they make sense for your stock:\n\n")
  print(to_inspect, row.names = FALSE)
  cat("\n  Zero weights are common where there were no fish of that age.\n")
  cat("  Negative or missing values are not — investigate those.\n\n")
}


# ---- Section 9: The first fit ----

fit <- sca(stock = stk, indices = idxs)

stk.fit <- stk + fit

plot(FLStocks(input = stk, assessment = stk.fit))


# ---- Key takeaway ----
#'
#' Your own stock is now in the shape a4a expects, and it fits.
#' Whether the fit is any GOOD is Day 2.
#'
