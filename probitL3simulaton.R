rm(list = ls())

# simulate log10 dose
x <- log10(c(0.1, 1, 10, 100))

# simulate dose response as probits
y <- -0.6 + 0.5 * x + rnorm(length(x), sd = 0.1)
# the normal noise added to the straight line relationship represents 
# random deviation from perfect linearity. this seems realistic.

# plot probit failure by log10 dose
plot(x, y)

# plot failure probability by log10 dose
plot(x, pnorm(y), ylim = 0:1)

# pnorm(y) is the true & unknowable failure probability at each dose
pnorm(y)

# to simulate binomial data we need to expose virtual eggs, i.e. 
# take a random draw from a binomial distribution at each dose

l3 <- rbinom(length(x), 100, pnorm(y))
l3

# plot the observed proportion failing to reach L3
plot(x, l3/100, ylim = 0:1)

# fit a probit regression model
fit <- glm(cbind(l3, 100-l3) ~ x, family = binomial(link = "probit"))

summary(fit)

# extract coefficients, intercept (a) and slope (b)
a <- coef(fit)["(Intercept)"]
b <- coef(fit)["x"]
a
b

# the predicted probits are y = a + b*x
x.smooth <- seq(min(x), max(x), 0.01)
y.pred <- a + b*x.smooth

# add the fitted model to the plot
lines(x.smooth, pnorm(y.pred), lty = 2)

# we have y = a + b*x so 
# x = (y - a)/b
# we want the ED50. the probit of 50% is 0:
qnorm(0.5)
# so y drops out of the equation.
# so to get the ED50 we calculate
# x = -a/b
-a/b
# this gives the log10 dose, so the ED50 is
10^(-a/b)

# compare this to the true ED50, which from the initial model is
10^(0.6/0.5)
