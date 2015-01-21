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

model <- polr(assn ~ icrg_stability + uds_mean + all.comp + year, data=pawns.data, method="logistic")
model1 <- clm(assn ~ icrg_stability + uds_mean + all.comp, data=pawns.data, link="logit")
summary(model)
summary(model1)

# THIS IS IT - JUST FIGURE OUT ROBUST CLUSTERED SEs, but this is random effects for year and country
model2 <- clmm(assn ~ icrg_stability + uds_mean + all.comp + (1|year) + (1|country), data=pawns.data, link="logit", Hess=TRUE)
summary(model2)
model2$terms


model3 <- clmm(assn ~ icrg_stability + uds_mean + all.comp + factor(year) + (1|country), data=pawns.data, link="logit", Hess=TRUE)
summary(model3)

library(lmtest)
coeftest(model2, vcov=vcovHC(model2, cluster="country"))

robust.clusterify(model2, pawns.data, pawns.data$country)

robust.clusterify <- function(model, dat, cluster) {
  attach(dat, warn.conflicts = F)
  require(sandwich)
  require(lmtest)
  not <- attr(model$model,"na.action")
    
  if( ! is.null(not)) {  # only drop the NA values if there are any left
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  
  with(dat, {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
    coefs <- coeftest(model, vcovCL, type="HC1")  # HC1 or HC0 are close to Stata
    return(list(clcov=vcovCL, coefs=coefs))
  })
}


library(pglm)
op <- pglm(assn ~ icrg_stability + uds_mean + all.comp, data=pawns.data,
           family = ordinal('logit'), R = 5, print.level = 3,
           method = 'bfgs', index = c('country', "year"), model = "random")

pooled.ologit <- pglm(as.numeric(assn) ~ icrg_stability + uds_mean, model=("pooling"), effect=("individual"), index=c("country", "year"), family=ordinal(link="logit"), data=pawns.data)
summary(pooled.ologit)


qwer <- orm(assn ~ icrg_stability + uds_mean + all.comp, data=pawns.data)
summary(qwer)

summary(op)

library(glmmML)
library(rms)

data('Fairness', package = 'pglm')
Parking <- subset(Fairness, good == 'parking')
op <- pglm(as.numeric(answer) ~ education + rule,
           Parking[1:105, ],
           family = ordinal('probit'), R = 5, print.level = 3,
           method = 'bfgs', index = 'id',  model = "random")
summary(op)


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