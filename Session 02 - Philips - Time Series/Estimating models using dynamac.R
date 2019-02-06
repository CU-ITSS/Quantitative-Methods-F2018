# 10/4/18
# Estimating models using dynamac
# Andrew Q. Philips
# ----------------------------
setwd("~/Dropbox/My_Folder/TS workshop") # helpful to set your working directory

# we'll need a few packages. Uncomment to intall them for the first time:
#install.packages("dynamac")
library(dynamac)
#install.packages("fUnitRoots") 
library(fUnitRoots)
#install.packages("foreign")
library(foreign) # to load Stata .dta


# ----------------
# LOAD IN DATA
# ----------------
ssr.dat <- read.dta("SSR data.dta")


# ----------------
# PLOTTING
# ----------------
# our goal is to predict tone of the media (tone1) as a function of leading economic indicators (hp_cb_lead) and contemporaneous economic indicators (hp_cbcoinus)
plot.ts(ssr.dat$tone1)
plot.ts(ssr.dat$hp_cb_lead)
plot.ts(ssr.dat$hp_cbcoinus)


# ----------------
# UNIT ROOT TESTING
# ----------------
# the classic unit root test is Dickey-Fuller:
adfTest(ssr.dat$tone1, lags = 1) # reject null, so I(0). Also a good idea to include lags...helps purge autocorrelation
adfTest(ssr.dat$hp_cb_lead, lags = 1) # reject null, so I(0).
adfTest(ssr.dat$hp_cbcoinus, lags = 1) # reject null, so I(0).

# ----------------
# MODELING AND SIMULATIONS
# ----------------

# since everything was stationary, we'll estimate an ARDL(1,1):
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), # we want a lag of each variable
  levels = c("hp_cb_lead", "hp_cbcoinus"), # we want these variables to appear at time t
  simulate = FALSE, ec = FALSE)
summary(ardl.model$model)


# Turns out that interpreting TS results involves lots of "hidden" effects. Instead of calculating these, we can simulate substantively meaningful changes. How about a 1 SD increase in leading economic indicators?
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), 
  levels = c("hp_cb_lead", "hp_cbcoinus"),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE)

# we can specify the shock amount ourselves:
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), 
  levels = c("hp_cb_lead", "hp_cbcoinus"),
  simulate = TRUE, shockvar = "hp_cb_lead", shockval = 1, graph = TRUE)

# We can create other types of plots. Let's create a spike plot, going back to a +1SD shock:
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), 
  levels = c("hp_cb_lead", "hp_cbcoinus"),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE, rarea = FALSE)

# Things are a bit bumpy, so we can up the number of sims to smooth the CIs:
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), 
  levels = c("hp_cb_lead", "hp_cbcoinus"),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE, rarea = FALSE, sims = 5000)

# Let's say we want to set the values of the independent varibles to something other than their means (default):
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), 
  levels = c("hp_cb_lead", "hp_cbcoinus"),
  forceset = list("hp_cb_lead" = 5, "hp_cbcoinus" = -1),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE, rarea = FALSE)

# Sometimes we may want multiple lags of a variable. Let's add an additional lag to the DV and hp_cb_lead:
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1:2, "hp_cb_lead" = 1:2, "hp_cbcoinus" = 1), 
  levels = c("hp_cb_lead", "hp_cbcoinus"),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE, rarea = FALSE)
summary(ardl.model$model)

# Let's say we had needed to first-difference hp_cb_lead (either for theoretical reasons or b/c we found it was I(1)). We can add first-differenced varibles as follows:
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1:2, "hp_cbcoinus" = 1), 
  levels = c("hp_cbcoinus"),
  diffs = c("hp_cb_lead"),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE, rarea = FALSE)
summary(ardl.model$model)

# we could have both a first difference and lag if we thought that the change in leading econ matters, and the (lagged) level:
ardl.model <- dynardl(tone1 ~ hp_cb_lead + hp_cbcoinus, data = ssr.dat, 
  lags = list("tone1" = 1:2, "hp_cb_lead" = 1, "hp_cbcoinus" = 1), 
  levels = c("hp_cbcoinus"),
  diffs = c("hp_cb_lead"),
  simulate = TRUE, shockvar = "hp_cb_lead", graph = TRUE, rarea = FALSE)
summary(ardl.model$model)

