# http://stats.stackexchange.com/questions/66946/how-are-the-standard-errors-computed-for-the-fitted-values-from-a-logistic-regre

# Making fake data and fitting the model and getting a prediction
set.seed(500)
dat <- data.frame(x = runif(20), y = factor(rbinom(20, 2, .5)))
dat <- data.frame(x = runif(20), y = rbinom(20, 1, .5))
o <- clm(y ~ x, data = dat, Hess=TRUE)
summary(o)
prediction <- predict(o, newdata = data.frame(x=seq(0, 5, 0.5)), se.fit = TRUE)

# To obtain a prediction for x=1.5 I'm really
# asking for yhat = b0 + 1.5*b1 so my
# C = c(1, 1.5)
# and vcov applied to the glm object gives me
# the covariance matrix for the estimates

vcov(o)

o$Theta

C <- data.frame(o$Theta, x=seq(0, 5, 0.5))
C <- data.frame(intercept=1, x=seq(0, 5, 0.5))

calc.se <- function(coefs, model) {
  sqrt(t(coefs) %*% vcov(model) %*% coefs)
}

std.er <- apply(C, 1, calc.se, model=o)

prediction$se.fit
std.er
