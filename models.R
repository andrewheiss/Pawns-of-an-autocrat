library(MASS)
library(ordinal)
library(dplyr)

load("data/pawns_clean.RData")

# General model: ASSN ~ regime type + stability + competitiveness

asdf <- clm(assn ~ uds_mean + icrg_stability + all.comp, data=pawns.data, link="logit", subset=(polity2 <= 0))
asdf <- clm(assn ~ uds_mean + icrg_stability + all.comp, data=pawns.data, link="logit")
summary(asdf)

# Using ordinal::clm since predict.clm can return standard errors and 
# predict.polr can't. #sadface because ordinal loads so slowly.
model <- clm(assn ~ icrg_stability + uds_mean, data=pawns.data, link="logit")
summary(model)

model <- clm(assn ~ icrg_stability + uds_mean + YEAR, data=pawns.data, link="logit")
summary(model)

model <- clm(assn ~ icrg_stability + polity2 + YEAR, data=pawns.data, link="logit")
summary(model)

newdata <- data.frame(icrg_stability = seq(1, 12, .05), 
                      uds_mean=mean(assn.data$uds_mean, na.rm=TRUE))
newdata1 <- data.frame(icrg_stability = seq(1, 12, .05),
                       polity2=-6, YEAR=2000)
newdata2 <- data.frame(icrg_stability = seq(1, 12, .05), 
                       uds_mean=mean(assn.data$uds_mean, na.rm=TRUE),
                       YEAR=2000)

phat <- predict(model, newdata=newdata, type="prob", se.fit=TRUE)

upper <- data.frame(phat$fit + (1.96 * phat$se.fit)) %>% gather(assn.level, upper)
lower <- data.frame(phat$fit - (1.96 * phat$se.fit)) %>% gather(assn.level, lower)
se.fit <- data.frame(phat$se.fit) %>% gather(assn.level, se.fit)

phat.plot <- data.frame(phat$fit) %>% cbind(newdata) %>%
  # select(-uds_mean) %>%  # Remove constants
  select(Severely.restricted, Limited, Unrestricted, icrg_stability) %>%
  gather(assn.level, prob, -icrg_stability) %>%
  mutate(se.fit = se.fit[['se.fit']],
         upper = upper[['upper']],
         lower = lower[['lower']]) %>%
  mutate(assn.level = factor(assn.level, labels=gsub("\\.", " ", levels(assn.level))))


plot.colours <- c("#8C2318", "#F2C45A", "#88A65E")
ggplot(phat.plot, aes(x=icrg_stability, y=prob, colour=assn.level)) + 
  geom_ribbon(aes(ymax=upper, ymin=lower, fill=assn.level), alpha=0.3, colour=NA) + 
  geom_line(size=2) +
  labs(x="ICRG Government Stability", y="Probability of outcome") + 
  scale_colour_manual(values=plot.colours, name="Freedom of association") + 
  scale_fill_manual(values=plot.colours, name="Freedom of association") + 
  theme_bw() #+ theme(legend.position="top")
# http://www.colourlovers.com/palette/110225/Vintage_Modern


# Findings, kind of
# Convergence between sever and unrestricted with ~ icrg + uds and no year
# Prob of severe restrictions decrease in low polity, like polity=-6 with ~ icrg + polity2 + year=1996
# Same when using uds_mean + year
# TODO: Figure out why there's no convergence, why rescaling needs to happen - try in Stata with their actual fixed effects thing?