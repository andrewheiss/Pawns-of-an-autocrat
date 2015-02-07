# Exploratory data analysis

#--------------------------
# Load libraries and data
#--------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)

load("data/pawns_clean.RData")


#---------------------
# Dependent variable
#---------------------
# Freedom of Assembly and Association (assn)
# Source: CIRI Human Rights Dataset (www.humanrightsdata.com)
# Description: "This variable indicates the extent to which the freedoms of assembly and association are subject to actual governmental limitations or restrictions (as opposed to strictly legal protections). A score of 0 indicates that citizens’ rights to freedom of assembly or association were severely restricted or denied completely to all citizens; a score of 1 indicates that these rights were limited for all citizens or severely restricted or denied for select groups; and a score of 2 indicates that these rights were virtually unrestricted and freely enjoyed by practically all citizens in a given year."

assn.data <- pawns.data %>%
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
  theme_clean() + theme(panel.grid.major.x=element_blank())
assn.plot


#------------------------
# Independent variables
#------------------------

# Regime type
#------------
plot.data <- pawns.data %>%
  select(assn, polity2) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

regime.plot <- ggplot(plot.data, aes(x=assn, y=polity2, fill=assn)) + 
  geom_hline(yintercept=c(-5, 5), colour="grey70", size=1) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x="Freedom of assembly and association (CIRI)",
       y="Polity IV score") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean()
regime.plot


# Regime stability
#-----------------
# ICRG government stability (icrg_stability)
plot.data <- pawns.data %>%
  select(assn, icrg_stability) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

icrg.plot <- ggplot(plot.data, aes(x=assn, y=icrg_stability, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x="Freedom of assembly and association (CIRI)",
       y="Government stability (ICRG)") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean()


# Years in office
plot.data <- pawns.data %>%
  select(assn, yrsoffc) %>% 
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

years.office.plot <- ggplot(plot.data, aes(x=assn, y=yrsoffc, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x=NULL,
       y="Years executive has been in office") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean()

stability <- arrangeGrob(icrg.plot, years.office.plot, nrow=1)
stability


# Regime competitiveness
#-----------------------
plot.data <- pawns.data %>%
  select(assn, years.since.comp) %>% 
  filter(years.since.comp < 20) %>%
  mutate(assn = factor(assn, levels=rev(levels(assn)))) %>% 
  na.omit()

years.comp.plot <- ggplot(plot.data, aes(x=assn, y=years.since.comp, fill=assn)) + 
  geom_violin(colour=NA) + coord_flip() + 
  labs(x="Freedom of assembly and association (CIRI)",
       y="Years since a competitive election") +
  scale_fill_manual(values=rev(assn.colours), guide=FALSE) + 
  theme_clean()


plot.data <- pawns.data %>%
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
  theme_clean()

competitiveness <- arrangeGrob(years.comp.plot, opp.vote.plot, nrow=1)
competitiveness
