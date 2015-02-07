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


# http://www.colourlovers.com/palette/110225/Vintage_Modern
assn.colours <- c("#8C2318", "#F2C45A", "#88A65E")
model.colours <- c("#1f78b4", "#ff7f00", "#6a3d9a")


#---------
# Models
#---------
# TODO: Sim UDS < X

uds.threshold <- 0
polity.threshold <- 0

# Simple model using UDS
model.simple.uds <- clm(assn ~ icrg_stability + yrsoffc + 
                          years.since.comp + opp1vote + uds_mean, 
                        data=pawns.data, link="logit", Hess=TRUE)
summary(model.simple.uds)

newdata <- model.simple.uds$model %>% select(-1)

fitted.values <- data.frame(predict(model.simple.uds, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.simple.uds$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Simple model using polity
model.simple.polity <- clm(assn ~ icrg_stability + yrsoffc + 
                             years.since.comp + opp1vote + polity2, 
                           data=pawns.data, link="logit", Hess=TRUE)
summary(model.simple.polity)

newdata <- model.simple.polity$model %>% select(-1)

fitted.values <- data.frame(predict(model.simple.polity, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.simple.polity$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls with UDS
model.big.uds <- clm(assn ~ icrg_stability + yrsoffc + 
                       years.since.comp + opp1vote + uds_mean + 
                       physint + gdpcap.log + population.log + 
                       oda.log + countngo + globalization, 
                     data=pawns.data, link="logit", Hess=TRUE)
summary(model.big.uds)

newdata <- model.big.uds$model %>% select(-1)

fitted.values <- data.frame(predict(model.big.uds, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.big.uds$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls with polity
model.big.polity <- clm(assn ~ icrg_stability + yrsoffc + 
                          years.since.comp + opp1vote + polity2 + 
                          physint + gdpcap.log + population.log + 
                          oda.log + countngo + globalization, 
                        data=pawns.data, link="logit", Hess=TRUE)
summary(model.big.polity)

newdata <- model.big.polity$model %>% select(-1)

fitted.values <- data.frame(predict(model.big.polity, newdata)$fit) %>%
  set_colnames(gsub("\\.", " ", colnames(.)))  # Remove the .

sep.plot(fitted.values, actual.char=as.character(model.big.polity$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls + random effects with UDS
model.full.uds <- clmm(assn ~ icrg_stability + yrsoffc +
                        years.since.comp + opp1vote + uds_mean + 
                        physint + gdpcap.log + population.log + 
                        oda.log + countngo + globalization + 
                         (1|year) + (1|country), 
                      data=pawns.data, link="logit", Hess=TRUE)
summary(model.full.uds)

newdata <- model.full.uds$model %>% select(-1) %>%
  mutate(year = 0, country = 0)

fitted.values <- fake.predict.clmm(model.full.uds, newdata)

sep.plot(fitted.values, actual.char=as.character(model.full.uds$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Model + controls + random effects with polity
model.full.polity <- clmm(assn ~ icrg_stability + yrsoffc + 
                            years.since.comp + opp1vote + polity2 +
                            physint + gdpcap.log + population.log + 
                            oda.log + countngo + globalization + 
                            (1|year) + (1|country), 
                          data=pawns.data, link="logit", Hess=TRUE)
summary(model.full.polity)

newdata <- model.full.polity$model %>% select(-1) %>%
  mutate(year = 0, country = 0)

fitted.values <- fake.predict.clmm(model.full.polity, newdata)

sep.plot(fitted.values, actual.char=as.character(model.full.polity$model[,1]), 
         actual.levels=rev(levels(pawns.data$assn)))


# Stargazer table
model.list <- list(model.simple.uds, model.simple.polity, model.big.uds, 
                   model.big.polity, model.full.uds, model.full.polity)

model.names <- c("Simple (UDS)", "Simple (Polity)", "Full (UDS)", "Full (Polity)",
                 "Full + random effects (UDS)", "Full + random effects (Polity)")

coef.names <- c("Government stability (ICRG)", "Years executive in office", 
                "Years since competitive election", "Opposition vote share",
                "Unified democracy score (mean)", "Polity IV",
                "Physical integrity rights (CIRI)", "GDP per capita (log)", 
                "Population (log)", "Development aid (log)", 
                "INGO members/volunteers (log)", "Globalization")

# Add cutpoints to the startgazer output
thetas <- bind_rows(lapply(model.list, FUN=function(x) data.frame(x$Theta)))

extra.lines <- list(c("Severely restricted|Limited", round(thetas[[1]], 3)),
                    c("Limited|Unrestricted", round(thetas[[2]], 3)),
                    c("Random country and year effects", c(rep("No", 4), rep("Yes", 2))))

stargazer(model.simple.uds, model.simple.polity, model.big.uds, model.big.polity, 
          fake.clm(model.full.uds), fake.clm(model.full.polity), type="text",
          add.lines=extra.lines, covariate.labels=coef.names,
          dep.var.caption="Freedom of association")


# Coefficient plot
model.list <- list(model.simple.polity, model.big.polity, model.full.polity)

model.names <- c("Simple", "Full", "Full + random effects")

coef.names.clean <- c("Government stability (ICRG)", "Years executive in office", 
                      "Years since competitive election", "Opposition vote share",
                      "Polity IV",
                      "Physical integrity rights (CIRI)", "GDP per capita (log)", 
                      "Population (log)", "Development aid (log)", 
                      "INGO members/volunteers (log)", "Globalization")

coef.plot.data <- bind_rows(lapply(1:length(model.list), FUN=extract.coef.plot, 
                                   models=model.list, names=model.names)) %>%
  mutate(IV = factor(as.numeric(IV), labels=coef.names.clean, ordered=TRUE),
         IV = factor(IV, levels=rev(levels(IV))),
         model.name = factor(model.name, levels=rev(model.names), ordered=TRUE))

ggplot(coef.plot.data, aes(x=IV, y=estimate, colour=model.name)) + 
  geom_hline(yintercept=0, colour="#8C2318", alpha=0.6, size=1) + 
  geom_pointrange(aes(ymin=ymin, ymax=ymax), size=.75, 
                  position=position_dodge(width=.75)) + 
  scale_colour_manual(values=model.colours, name="", 
                      guide=guide_legend(reverse=TRUE)) + 
  labs(x=NULL, y="Log odds") + 
  coord_flip() + 
  theme_clean(legend.bottom = TRUE) + 
  theme(legend.key = element_blank())

# Predicted probabilities
newdata <- expand.grid(icrg_stability = mean(pawns.data$icrg_stability, na.rm=TRUE),
                       yrsoffc = mean(pawns.data$yrsoffc, na.rm=TRUE),
                       years.since.comp = c(5, 15),
                       opp1vote = seq(0, 50, by=5),
                       polity2 = c(-6, 6),
                       physint = mean(pawns.data$physint, na.rm=TRUE),
                       gdpcap.log = mean(pawns.data$gdpcap.log, na.rm=TRUE),
                       population.log = mean(pawns.data$population.log, na.rm=TRUE),
                       oda.log = mean(pawns.data$oda.log, na.rm=TRUE),
                       countngo = mean(pawns.data$countngo, na.rm=TRUE),
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
                                     years.since.comp), "years since election"))) %>%
  mutate(polity2 = factor(polity2, labels=c("Autocracy", "Democracy")),
         opp1vote = opp1vote/100)

# Simulations
num.simulations <- 500
mu <- c(model.full.polity$Theta, model.full.polity$beta, unlist(model.full.polity$ST))
draw <- MASS::mvrnorm(num.simulations, mu, vcov(model.full.polity))

sim.predict <- lapply(1:nrow(draw), FUN=ologit.predict,
                      coefs=draw, newdata=newdata, mod=model.full.polity)

plot.data <- newdata %>% cbind(bind_rows(sim.predict)) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata), sim.round)) %>%
  mutate(years.since.comp = factor(years.since.comp), 
         years.since.comp = factor(years.since.comp, 
                                   labels=paste(levels(
                                     years.since.comp), "years since election"))) %>%
  mutate(polity2 = factor(polity2, labels=c("Autocracy", "Democracy")),
         opp1vote = opp1vote/100)

p <- ggplot(plot.data, aes(x=opp1vote, y=assn.prob, colour=assn)) + 
  geom_line(aes(group=interaction(sim.round, assn)), alpha=0.01, size=1) + 
  geom_line(data=plot.data.single, size=2) +
  labs(x="Opposition vote share", y="Probability of outcome") + 
  ggtitle(expression(atop("Predicted probabilities of freedom of association restrictions", 
                          atop("500 simulated draws. All variables held at their means."), ""))) +
  scale_colour_manual(values=assn.colours, name="Freedom of association") + 
  scale_y_continuous(labels=percent, breaks=seq(0, 0.8, 0.2)) + 
  scale_x_continuous(labels=percent) + 
  theme_clean(legend.bottom = TRUE) + 
  theme(legend.key = element_blank()) + 
  facet_wrap(~ polity2 + years.since.comp)
p

ggsave(p, filename="fancy_plot.pdf", width=10, height=7, units="in", device=cairo_pdf)


# Stability + yrsoffc
newdata <- expand.grid(icrg_stability = seq(0, 12, by=0.5),
                       yrsoffc = c(5, 15),
                       years.since.comp = mean(pawns.data$years.since.comp, na.rm=TRUE),
                       opp1vote = mean(pawns.data$opp1vote, na.rm=TRUE),
                       polity2 = c(-6, 6), 
                       physint = mean(pawns.data$physint, na.rm=TRUE),
                       gdpcap.log = mean(pawns.data$gdpcap.log, na.rm=TRUE),
                       population.log = mean(pawns.data$population.log, na.rm=TRUE),
                       oda.log = mean(pawns.data$oda.log, na.rm=TRUE),
                       countngo = mean(pawns.data$countngo, na.rm=TRUE),
                       globalization = mean(pawns.data$globalization, na.rm=TRUE),
                       year = 0, country = 0)

pred.mat.uds <- fake.predict.clmm(model.full.uds, newdata)
pred.mat.polity <- fake.predict.clmm(model.full.polity, newdata)

# Create plot data
plot.data.single <- cbind(newdata, pred.mat.polity) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata))) %>%
  mutate(yrsoffc = factor(yrsoffc), 
         yrsoffc = factor(yrsoffc, 
                          labels=paste(levels(
                            yrsoffc), "years in office"))) %>%
  mutate(polity2 = factor(polity2, labels=c("Autocracy", "Democracy")))

# Simulations
num.simulations <- 500
mu <- c(model.full.polity$Theta, model.full.polity$beta, unlist(model.full.polity$ST))
draw <- MASS::mvrnorm(num.simulations, mu, vcov(model.full.polity))

sim.predict <- lapply(1:nrow(draw), FUN=ologit.predict,
                      coefs=draw, newdata=newdata, mod=model.full.polity)

plot.data <- newdata %>% cbind(bind_rows(sim.predict)) %>%
  gather(assn, assn.prob, -c(1:ncol(newdata), sim.round)) %>%
  mutate(yrsoffc = factor(yrsoffc), 
         yrsoffc = factor(yrsoffc, 
                          labels=paste(levels(
                            yrsoffc), "years in office"))) %>%
  mutate(polity2 = factor(polity2, labels=c("Autocracy", "Democracy")))

p1 <- ggplot(plot.data, aes(x=icrg_stability, y=assn.prob, colour=assn)) + 
  geom_line(aes(group=interaction(sim.round, assn)), alpha=0.01, size=1) + 
  geom_line(data=plot.data.single, size=2) +
  labs(x="Government stability (ICRG)", y="Probability of outcome") + 
  ggtitle(expression(atop("Predicted probabilities of freedom of association restrictions", 
                          atop("500 simulated draws. All variables held at their means."), ""))) +
  scale_colour_manual(values=assn.colours, name="Freedom of association") + 
  scale_y_continuous(labels=percent, breaks=seq(0, 0.8, 0.2)) + 
  theme_clean(legend.bottom = TRUE) + 
  theme(legend.key = element_blank()) + 
  facet_wrap(~ polity2 + yrsoffc)
p1

ggsave(p1, filename="fancy_plot1.pdf", width=10, height=7, units="in", device=cairo_pdf)
