# 10/3/18
# TS Correlations
# Andrew Q. Philips
# ---------------------------- 

set.seed(20395) # always set seed w/ random number generating

# create 8 uncorrelated series of length 60 from a normal:
for(i in 1:8) {
  name <- paste("y", i, sep = "")
  assign(name, rnorm(60))
}
# plot a few of these over time:
plot.ts(y1, ylim = c(-5,5))
lines(y2)
lines(y3)
lines(y4)
lines(y5)

# how many pairwise correlations do you expect to be statistically sig?
library(Hmisc)
data <- as.matrix(cbind(y1, y2, y3, y4, y5, y6, y7, y8)) # need to turn into a matrix first
correlations <- rcorr(data) # create pw correlations
correlations
2/28 # number of sig correlations

# now we'll create a slightly different type of series that is common in time series. These series are also unrelated:
for(i in 1:8) {
  name <- paste("ts.y", i, sep = "")
  assign(name, cumsum(rnorm(60)))
}
# plot a few of these over time:
plot.ts(ts.y1, ylim = c(-8,8))
lines(ts.y2)
lines(ts.y3)
lines(ts.y4)
lines(ts.y5)

# how many pairwise correlations do you expect to be statistically sig?
data.ts <- as.matrix(cbind(ts.y1, ts.y2, ts.y3, ts.y4, ts.y5, ts.y6, ts.y7, ts.y8))
correlations.ts <- rcorr(data.ts) # create pw correlations
correlations.ts
17/28 # number of sig correlations

# Why would this be a problem?
dat <- data.frame(data.ts)
res <- lm(ts.y1 ~ ts.y2, data = dat)
summary(res)

