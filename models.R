# Load libraries
library(ordinal)  # Must come before dplyr because of slice()
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(grid)
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
