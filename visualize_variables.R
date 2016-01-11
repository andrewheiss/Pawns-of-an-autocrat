#--------------------------
# Load libraries and data
#--------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(Cairo)

load("data/cs_auth_clean.RData")
source("general_functions.R")


#---------------------
# Dependent variable
#---------------------
# Freedom of Assembly and Association (assn)
# Source: CIRI Human Rights Dataset (www.humanrightsdata.com)
assn.data <- cs.auth.data %>%
  select(country, year, assn) %>% na.omit() %>% 
  count(assn, year) %>% group_by(assn) %>% 
  summarize(assn.n = length(n), assn.mean = mean(n),
            assn.sd = sd(n), assn.se = assn.sd / sqrt(assn.n)) %>%
  mutate(assn.ci = assn.se * qt(0.95/2 + 0.5, assn.n))

assn.plot <- ggplot(assn.data, aes(x=assn, y=assn.mean, fill=assn)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=assn.mean - assn.ci, ymax=assn.mean + assn.ci),
                width=0.3) + 
  labs(x="Freedom of assembly and association (CIRI)", 
       y="Average number of countries per year") + 
  scale_fill_manual(values=assn.colours, guide=FALSE) + 
  theme_clean(16) + theme(panel.grid.major.x=element_blank())

ggsave(assn.plot, filename="output/assn_summary.pdf", 
       width=2.5, height=1.5, units="in", device=cairo_pdf, scale=2)

# theme_clean(16)
ggsave(assn.plot, filename="output/assn_summary_presentation.pdf", 
       width=7, height=3.5, units="in", device=cairo_pdf)


#------------------------
# Independent variables
#------------------------

# Regime type
#------------
plot.data <- cs.auth.data %>%
  select(assn, polity2) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

polity.plot <- ggplot(plot.data, aes(x=assn, y=polity2, fill=assn)) + 
  geom_hline(yintercept=c(0), colour="grey70", size=1) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x="Freedom of assembly and association (CIRI)",
       y="Polity IV score") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean()


plot.data <- cs.auth.data %>%
  select(assn, uds_mean) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

uds.plot <- ggplot(plot.data, aes(x=assn, y=uds_mean, fill=assn)) + 
  geom_hline(yintercept=c(0), colour="grey70", size=1) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x=NULL, y="Mean UDS score") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean() + theme(axis.text.y=element_blank())

regime_type_summary <- arrangeGrob(polity.plot, uds.plot, nrow=1)
ggsave(regime_type_summary, filename="output/regime_type_summary.pdf", 
       width=8, height=5, units="in", device=cairo_pdf)


# Regime stability
#-----------------
# ICRG government stability (icrg_stability)
plot.data <- cs.auth.data %>%
  select(assn, icrg_stability) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

icrg.plot <- ggplot(plot.data, aes(x=assn, y=icrg_stability, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x="Freedom of assembly and association (CIRI)",
       y="Government stability (ICRG)") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean(16)


# Years in office
plot.data <- cs.auth.data %>%
  select(assn, yrsoffc) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

years.office.plot <- ggplot(plot.data, aes(x=assn, y=yrsoffc, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x=NULL,
       y="Years executive has been in office") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean(16) + theme(axis.text.y=element_blank())

stability <- arrangeGrob(icrg.plot, years.office.plot, nrow=1)
ggsave(stability, filename="output/stability_summary.pdf", 
       width=8, height=5, units="in", device=cairo_pdf)
ggsave(stability, filename="output/stability_summary_presentation.pdf", 
       width=8, height=5, units="in", device=cairo_pdf)


# Regime competitiveness
#-----------------------
plot.data <- cs.auth.data %>%
  select(assn, years.since.comp) %>% 
  filter(years.since.comp < 20) %>%
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

years.comp.plot <- ggplot(plot.data, aes(x=assn, y=years.since.comp, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x="Freedom of assembly and association (CIRI)",
       y="Years since a competitive election") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean(16)


plot.data <- cs.auth.data %>%
  select(assn, opp1vote) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn))),
         opp1vote = opp1vote / 100) %>% 
  na.omit()

opp.vote.plot <- ggplot(plot.data, aes(x=assn, y=opp1vote, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x=NULL,
       y="Vote share for largest opposition party") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  scale_y_continuous(labels=percent) + 
  theme_clean(16) + theme(axis.text.y=element_blank())

competitiveness <- arrangeGrob(years.comp.plot, opp.vote.plot, nrow=1)
ggsave(competitiveness, filename="output/competitiveness_summary_presentation.pdf", 
       width=8, height=5, units="in", device=cairo_pdf)
