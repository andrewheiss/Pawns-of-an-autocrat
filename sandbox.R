library(ordinal)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

data(wine)

fm2 <- clmm2(rating ~ temp + contact, random=judge, data=wine, Hess=TRUE, nAGQ=10)
summary(fm2)

fm2.mm <- clmm(rating ~ temp + contact + (1|judge), data=wine, Hess=TRUE, link="logit")
summary(fm2.mm)

# Random effects for each judge
ci <- fm2$ranef + qnorm(0.975) * sqrt(fm2$condVar) %o% c(-1, 1)
ord.re <- order(fm2$ranef)
ci <- ci[order(fm2$ranef),]
plot(1:9, fm2$ranef[ord.re], axes=FALSE, ylim=range(ci),
       xlab="Judge", ylab="Judge effect")
axis(1, at=1:9, labels = ord.re)
axis(2)
for(i in 1:9) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)


# Predictions
pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
  Theta <- c(-1000, theta, 1000)
  sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
}

# Create hypothetical data
# Judge = 5th %ile, average, and 95th %ile judge
hypo <- expand.grid(judge = qnorm(0.95) * c(-1, 0, 1),
                    contact = c(0, 1), 
                    temp = c(0, 1))

# Get the coefficients in the correct order
coefs <- c(fm2$stDev, fm2$beta[2], fm2$beta[1])
coefs1 <- c(unlist(fm2.mm$ST), fm2.mm$beta[2], fm2.mm$beta[1])

# Multiply each column in the hypothetical data with each coefficent 
xbetas <- sweep(hypo, MARGIN=2, coefs1, `*`)

# Make predictions
pred.mat <- data.frame(pred(eta=rowSums(xbetas), theta=fm2$Theta)) %>%
  set_colnames(paste("rating", levels(wine$rating), sep=""))

# Create plot data
plot.data <- cbind(hypo, pred.mat) %>%
  gather(rating, prob.rating, -c(1:ncol(hypo)))

# Plot
ggplot(plot.data, aes(x=rating, y=prob.rating, colour=judge)) + 
  geom_line(aes(group = judge)) + facet_wrap(~ contact + temp)




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


newdata <- data.frame(icrg_stability = seq(1, 12, .05), 
                      uds_mean=mean(pawns.data$uds_mean, na.rm=TRUE),
                      all.comp = TRUE)
newdata1 <- data.frame(icrg_stability = seq(1, 12, .05),
                       polity2=-6, YEAR=2000)
newdata2 <- data.frame(icrg_stability = seq(1, 12, .05), 
                       uds_mean=mean(assn.data$uds_mean, na.rm=TRUE),
                       YEAR=2000)

phat <- pred(model2, newdata=newdata, type="prob", se.fit=TRUE)


model3 <- clmm(assn ~ icrg_stability + uds_mean + all.comp + factor(year) + (1|country), data=pawns.data, link="logit", Hess=TRUE)
model3 <- clmm(assn ~ icrg_stability + uds_mean + all.comp + factor(year), random=country, data=pawns.data, link="logit", Hess=TRUE)
summary(model3)



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



model <- clm(assn ~ icrg_stability + uds_mean, data=pawns.data, link="logit")
summary(model)

newdata <- data.frame(icrg_stability = seq(1, 12, .05), 
                      uds_mean=mean(pawns.data$uds_mean, na.rm=TRUE))
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
