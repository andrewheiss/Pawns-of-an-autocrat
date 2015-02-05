# Load libraries
library(ordinal)  # Must come before dplyr because of slice()
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(grid)
library(scales)
library(Cairo)
library(stargazer)

# Load data and functions
load("data/pawns_clean.RData")
source("model_functions.R")


#---------
# Models
#---------
# TODO: Sim UDS < X

uds.threshold <- 0
polity.threshold <- 0

# Simple model using UDS
model.simple.uds <- clm(assn ~ icrg_stability + yrsoffc + 
                          years.since.comp + opp1vote, 
                        data=pawns.data, link="logit", Hess=TRUE, 
                        subset=(uds_mean < uds.threshold))
summary(model.simple.uds)

newdata <- model.simple.uds$model %>% select(-1)

fitted.values <- data.frame(predict(model.simple.uds, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.simple.uds$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Simple model using polity
model.simple.polity <- clm(assn ~ icrg_stability + yrsoffc + 
                             years.since.comp + opp1vote, 
                           data=pawns.data, link="logit", Hess=TRUE, 
                           subset=(polity2 < polity.threshold))
summary(model.simple.polity)

newdata <- model.simple.polity$model %>% select(-1)

fitted.values <- data.frame(predict(model.simple.polity, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.simple.polity$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls with UDS
model.big.uds <- clm(assn ~ icrg_stability + yrsoffc + 
                       years.since.comp + opp1vote + 
                       physint + gdpcap.log + population.log + 
                       oda.log + globalization, 
                     data=pawns.data, link="logit", Hess=TRUE, 
                     subset=(uds_mean < uds.threshold))
summary(model.big.uds)

newdata <- model.big.uds$model %>% select(-1)

fitted.values <- data.frame(predict(model.big.uds, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.big.uds$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls with polity
model.big.polity <- clm(assn ~ icrg_stability + yrsoffc + 
                          years.since.comp + opp1vote + 
                          physint + gdpcap.log + population.log + 
                          oda.log + globalization, 
                        data=pawns.data, link="logit", Hess=TRUE, 
                        subset=(polity2 < polity.threshold))
summary(model.big.polity)

newdata <- model.big.polity$model %>% select(-1)

fitted.values <- data.frame(predict(model.big.polity, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.big.polity$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls + random effects with UDS
model.full.uds <- clmm(assn ~ icrg_stability + yrsoffc + 
                        years.since.comp + opp1vote + 
                        physint + gdpcap.log + population.log + 
                        oda.log + globalization + (1|year) + (1|country), 
                      data=pawns.data, link="logit", Hess=TRUE, 
                      subset=(uds_mean < uds.threshold))
summary(model.full.uds)

newdata <- model.full.uds$model %>% select(-1) %>%
  mutate(year = 0, country = 0)

fitted.values <- fake.predict.clmm(model.full.uds, newdata)

sep.plot(fitted.values, actual.char=as.character(model.full.uds$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls + random effects with polity
model.full.polity <- clmm(assn ~ icrg_stability + yrsoffc + 
                            years.since.comp + opp1vote + 
                            physint + gdpcap.log + population.log + 
                            oda.log + globalization + (1|year) + (1|country), 
                          data=pawns.data, link="logit", Hess=TRUE, 
                          subset=(polity2 < polity.threshold))
summary(model.full.polity)

newdata <- model.full.polity$model %>% select(-1) %>%
  mutate(year = 0, country = 0)

fitted.values <- fake.predict.clmm(model.full.polity, newdata)

sep.plot(fitted.values, actual.char=as.character(model.full.polity$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))

thetas <- rbind(model.simple.uds$Theta, model.simple.polity$Theta, 
                model.big.uds$Theta, model.big.polity$Theta, 
                model.full.uds$Theta, model.full.polity$Theta)

extra.lines <- list(c("Severely restricted|Limited", round(thetas[,1], 3)),
                    c("Limited|Unrestricted", round(thetas[,2], 3)),
                    c("Random country and year effects", c(rep("No", 4), rep("Yes", 2))))

stargazer(model.simple.uds, model.simple.polity, model.big.uds, model.big.polity, 
          fake.clm(model.full.uds), fake.clm(model.full.polity), type="text",
          add.lines = extra.lines)


newdata <- expand.grid(icrg_stability = mean(pawns.data$icrg_stability, na.rm=TRUE),
                       yrsoffc = mean(pawns.data$yrsoffc, na.rm=TRUE),
                       years.since.comp = c(4, 8, 16),
                       opp1vote = seq(0, 50, by=5),
                       physint = mean(pawns.data$physint, na.rm=TRUE),
                       gdpcap.log = mean(pawns.data$gdpcap.log, na.rm=TRUE),
                       population.log = mean(pawns.data$population.log, na.rm=TRUE),
                       oda.log = mean(pawns.data$oda.log, na.rm=TRUE),
                       globalization = mean(pawns.data$globalization, na.rm=TRUE),
                       year = 0, country = 0)

pred.mat.uds <- fake.predict.clmm(model.full.uds, newdata)
pred.mat.polity <- fake.predict.clmm(model.full.polity, newdata)

# Create plot data
plot.data.single <- cbind(newdata, pred.mat.polity) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata))) %>%
  mutate(years.since.comp = factor(years.since.comp), 
         years.since.comp = factor(years.since.comp, 
                                   labels=paste(levels(
                                     years.since.comp), "years since election")))

# Plot
assn.colours <- c("#8C2318", "#F2C45A", "#88A65E")
ggplot(plot.data.single, aes(x=opp1vote, y=assn.prob, colour=assn)) + 
  geom_line(size=2) + 
  labs(x="Opposition vote share (%)", y="Probability of outcome") + 
  scale_colour_manual(values=assn.colours, name="Freedom of association") + 
  theme_bw() + facet_wrap(~ years.since.comp)



# Draw random coefficients instead of mucking around with SEs and confints
ologit.predict <- function(i, coefs, newdata, mod) {
  pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
    Theta <- c(-1000, theta, 1000)
    sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
  }
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
mu <- head(c(model.full.uds$Theta, model.full.uds$beta, unlist(model.full.uds$ST)), -1)
draw <- cbind(MASS::mvrnorm(num.simulations, mu, vcov(model.full.uds)), 0)

sim.predict <- lapply(1:nrow(draw), FUN=ologit.predict,
                      coefs=draw, newdata=newdata, mod=model.full.uds)

plot.data <- newdata %>% cbind(bind_rows(sim.predict)) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata), sim.round)) %>%
  mutate(years.since.comp = factor(years.since.comp), 
         years.since.comp = factor(years.since.comp, 
                                   labels=paste(levels(
                                     years.since.comp), "years since election")))

# http://www.colourlovers.com/palette/110225/Vintage_Modern
assn.colours <- c("#8C2318", "#F2C45A", "#88A65E")
p <- ggplot(plot.data, aes(x=opp1vote, y=assn.prob, colour=assn)) + 
  geom_line(aes(group=interaction(sim.round, assn)), alpha=0.03, size=1) + 
  geom_line(data=plot.data.single, size=2) +
  labs(x="Opposition vote share (%)", y="Probability of outcome") + 
  ggtitle(expression(atop("Predicted probabilities of freedom of association restrictions", 
                          atop("500 simulated draws. All variables held at their means."), ""))) +
  scale_colour_manual(values=assn.colours, name="Freedom of association") + 
  scale_y_continuous(labels=percent) + 
  theme_clean(legend.bottom = TRUE) + 
  theme(legend.key = element_blank()) + 
  facet_wrap(~ years.since.comp)
p

ggsave(p, filename="fancy_plot.pdf", width=9, height=6, units="in", device=cairo_pdf)

