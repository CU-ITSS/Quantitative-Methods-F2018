# 10/4/18
# Simulate AR series
# Andrew Q. Philips
# ----------------------------

# This program helps you visualize and simulate a variety of AR processes. We will start by setting the seed:
set.seed(23923)


# let's create data of AR(1), where phi = 0.9:
y <- rnorm(100)
for(i in 2:100){
  y[i] <- 0.9*y[i-1] + rnorm(1)
}
plot.ts(y, main = "phi = 0.9")

# AR(1), phi = 0:
y2 <- rnorm(100)
for(i in 2:100){
  y2[i] <- 0*y2[i-1] + rnorm(1)
}
plot.ts(y2, main = "phi = 0")

# AR(1), phi = 0.2
y3 <- rnorm(100)
for(i in 2:100){
  y3[i] <- 0.2*y3[i-1] + rnorm(1)
}
plot.ts(y3, main = "phi = 0.2")

# AR(1), phi = -0.9
y4 <- rnorm(100)
for(i in 2:100){
  y4[i] <- -0.9*y4[i-1] + rnorm(1)
}
plot.ts(y4, main = "phi = -0.9")

par(mfrow = c(2,2))
plot.ts(y, main = "phi = 0.9")
plot.ts(y2, main = "phi = 0")
plot.ts(y3, main = "phi = 0.2")
plot.ts(y4, main = "phi = -0.9")
dev.off()


# To speed things up, we can use the arima.sim() command, which automates
# simulating AR processes:

y <- arima.sim(n = 100, list(ar = c(0.9)))
plot.ts(y, main = "phi = 0.9")

# we can even include further AR lags...here is an AR(3):
y.ar3 <- arima.sim(n = 100, list(ar = c(0.9,-0.5,0.1)))
plot.ts(y.ar3, main = "phi = 0.9, -0.5, 0.1")


# Now you try:
# 1. what happens as phi becomes positive and large?
# 2. negative and large?
# 3. Near 0? how does a histogram look? Does it look normal?


