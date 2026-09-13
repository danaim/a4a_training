#' ==========================================================================
#' Module 1.4 — FLIndex: survey and CPUE data
#' a4a Training Course — Guided Hands-on Script
#' ==========================================================================
#'
#' We build the MEDITS survey index for red mullet (MUT) in GSA 32.
#' Same data, same years and ages as the FLStock from Module 1.3.
#'
#' Putting the two together and fitting comes in Module 1.5.
#'

# ---- Setup ----
library(FLCore)
library(ggplotFL)

data_dir <- "../../_data/MUT_GSA32"

yrs  <- 1998:2017
ages <- 1:10


# ---- Section 1: What are we aiming at? ----

# An FLIndex has 12 slots, but a standard a4a fit uses only two of them:
# the index itself, and the range (which carries the survey timing).
slotNames("FLIndex")

# Compare with the biomass version, for surveys with no age data
slotNames("FLIndexBiomass")


# ---- Section 2: Read the survey data ----

idx.df <- read.csv(file.path(data_dir, "index.csv"))
head(idx.df)

# Same shape as the catch-at-age data: ages down, years across
idx.flq <- FLQuant(idx.df$data,
                   dimnames = list(age = ages, year = yrs))

idx.flq

#' NOTE no units. The index is RELATIVE — the model estimates catchability
#' to link it to absolute numbers. Do not rescale it to match the catch.

# Spot-check one value against the raw file, as always
idx.df$data[idx.df$age == 3 & idx.df$year == 2005]
c(idx.flq["3", "2005"])


# ---- Section 3: Create the FLIndex ----

# Initialise with an FLQuant so the object knows its dimensions
idx <- FLIndex(idx.flq)
idx

# Note the index slot is still empty — initialising only set the shape
index(idx)

# Now put the data in
index(idx) <- idx.flq
idx


# ---- Section 4: Survey timing ----

range(idx)

# startf and endf are the start and end of the survey, as a fraction
# of the year. The MEDITS survey runs June to August.
range(idx)["startf"] <- 6 / 12
range(idx)["endf"]   <- 8 / 12

range(idx)

#' These two numbers tell the model how much mortality has already
#' occurred when the survey observes the stock. Getting them wrong
#' compares the index against the population at the wrong moment.


# ---- Section 5: Say what it is ----

name(idx) <- "MEDITS"
desc(idx) <- "Medits in GSA 32"


# ---- Section 6: Look at it ----

plot(idx) + ggtitle(name(idx))

# The index slot is just an FLQuant, so everything from Module 1.2 works
dim(index(idx))
dimnames(index(idx))$year

# Trends by age
ggplot(index(idx), aes(x = year, y = data)) +
  geom_line() +
  facet_wrap(~age, scales = "free_y") +
  labs(y = "Survey index", title = name(idx))


# ---- Section 7: Exercise ----

#' Your own survey probably does not start in the same year as your catches.
#'
#' 1. Make a version of this index covering 2005-2017 only.
#'    Hint: window() from Module 1.2 works on FLQuant.
#' 2. Check its dimensions. Would it still work with a 1998-2017 stock?
#' 3. Set its name to something that says it is the short series.

# your code here


# ---- Section 8: Exercise ----

#' A second survey exists: an autumn acoustic survey, biomass only,
#' running October to November, 2010-2017.
#'
#' 1. Invent a plausible biomass series (one value per year)
#' 2. Build it as an FLIndexBiomass
#' 3. Set its survey timing correctly
#'
#' Hint: the index FLQuant needs a single row, age = "all".

# your code here


# ---- Key takeaway ----
#'
#' An FLIndex is an FLQuant of relative abundance plus the survey timing
#' in range. You fill index, startf, endf, name and desc; everything else
#' is either estimated by the model or used by other FLR packages.
#'
#' sca() accepts either a single FLIndex or an FLIndices. Wrapping even one
#' survey in FLIndices() is the safer habit, because some other methods
#' require the plural. That is Module 1.5.
#'
