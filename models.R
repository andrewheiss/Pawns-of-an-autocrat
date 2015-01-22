library(MASS)
library(ordinal)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

load("data/pawns_clean.RData")

# General model: ASSN ~ regime type + stability + competitiveness

# THIS IS IT - JUST FIGURE OUT ROBUST CLUSTERED SEs, but this is random effects for year and country
model2 <- clmm(assn ~ icrg_stability + uds_mean + all.comp + 
                 (1|year) + (1|country), 
               data=pawns.data, link="logit", Hess=TRUE)
summary(model2)

# Predicted probabilities
pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
  Theta <- c(-1000, theta, 1000)
  sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
}

newdata <- expand.grid(icrg_stability = seq(1, 12, 1),
                       uds_mean=mean(pawns.data$uds_mean, na.rm=TRUE),
                       all.comp = c(0, 1),
                       country=0, year=0)

coefs <- c(model2$beta, unlist(model2$ST))
xbetas <- sweep(newdata, MARGIN=2, coefs, `*`)

# Make predictions
pred.mat <- data.frame(pred(eta=rowSums(xbetas), theta=model2$Theta)) %>%
  set_colnames(levels(pawns.data$assn))

# Create plot data
plot.data.single <- cbind(newdata, pred.mat) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata))) %>%
  mutate(all.comp = factor(all.comp, labels=c("Not competitive", "Competitive")))

# Plot
assn.colours <- c("#8C2318", "#F2C45A", "#88A65E")
ggplot(plot.data.single, aes(x=icrg_stability, y=assn.prob, colour=assn)) + 
  geom_line(size=2) + 
  labs(x="Government stability (ICRG)", y="Probability of outcome") + 
  scale_colour_manual(values=assn.colours, name="Freedom of association") + 
  theme_bw() + facet_wrap(~ all.comp)


# Draw random coefficients instead of mucking around with SEs and confints
ologit.predict <- function(i, coefs, newdata, mod) {
  # Pass the row number instead of the actual row so it can be 
  # included in final data frame
  x <- coefs[i,]
  n.thetas <- length(mod$Theta)
  theta.draw <- x[1:n.thetas]
  coefs.draw <- x[(n.thetas+1):length(x)]

  xbetas1 <- sweep(newdata, MARGIN=2, coefs.draw, `*`)
  pred.mat1 <- data.frame(pred(eta=rowSums(xbetas1), theta=theta.draw)) %>%
    set_colnames(levels(pawns.data$assn)) %>%
    mutate(sim.round = i)
}

num.simulations <- 500
draw <- MASS::mvrnorm(num.simulations, 
                      c(model2$Theta, model2$beta, unlist(model2$ST)), 
                      vcov(model2))

sim.predict <- lapply(1:nrow(draw), FUN=ologit.predict,
                      coefs=draw, newdata=newdata, mod=model2)

plot.data <- newdata %>% cbind(bind_rows(sim.predict)) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata), sim.round)) %>%
  mutate(all.comp = factor(all.comp, labels=c("Not competitive", "Competitive")))

# http://www.colourlovers.com/palette/110225/Vintage_Modern
assn.colours <- c("#8C2318", "#F2C45A", "#88A65E")
p <- ggplot(plot.data, aes(x=icrg_stability, y=assn.prob, colour=assn)) + 
  geom_line(aes(group=interaction(sim.round, assn)), alpha=0.03, size=1) + 
  geom_line(data=plot.data.single, size=2) +
  labs(x="Government stability (ICRG)", y="Probability of outcome") + 
  scale_colour_manual(values=assn.colours, name="Freedom of association") + 
  theme_bw() + theme(legend.position = "bottom") + facet_wrap(~ all.comp)
p
ggsave(p, filename="example.png", width=8, height=8, units="in")



# Findings, kind of
# Convergence between sever and unrestricted with ~ icrg + uds and no year
# Prob of severe restrictions decrease in low polity, like polity=-6 with ~ icrg + polity2 + year=1996
# Same when using uds_mean + year
