# Exploratory data analysis
# Load libraries and data
library(dplyr)
library(ggplot2)

load("data/pawns_clean.RData")


#---------------------
# Dependent variable
#---------------------
# Freedom of Assembly and Association (assn)
# Source: CIRI Human Rights Dataset (www.humanrightsdata.com)
# Description: "This variable indicates the extent to which the freedoms of assembly and association are subject to actual governmental limitations or restrictions (as opposed to strictly legal protections). A score of 0 indicates that citizens’ rights to freedom of assembly or association were severely restricted or denied completely to all citizens; a score of 1 indicates that these rights were limited for all citizens or severely restricted or denied for select groups; and a score of 2 indicates that these rights were virtually unrestricted and freely enjoyed by practically all citizens in a given year."

assn.data <- pawns.data %>%
  select(CTRY, YEAR, assn) %>% na.omit() %>% 
  count(assn, YEAR) %>% group_by(assn) %>% 
  summarize(assn.n = length(n), assn.mean = mean(n),
            assn.sd = sd(n), assn.se = assn.sd / sqrt(assn.n)) %>%
  mutate(ci.mult = qt(0.95/2 + 0.5, assn.n),
         assn.ci = assn.se + ci.mult)

ggplot(assn.data, aes(x=assn, y=assn.mean)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=assn.mean - assn.ci, ymax=assn.mean + assn.ci))


#------------------------
# Independent variables
#------------------------

# Regime type
#------------
# UDS (uds_*)


# Regime stability
#-----------------
# ICRG government stability (icrg_stability)
icrg.data <- pawns.data %>%
  select(CTRY, YEAR, icrg_stability) %>% na.omit() %>%
  group_by(YEAR) %>% summarize(asdf = mean(icrg_stability))

ggplot(icrg.data, aes(x=YEAR, y=icrg_stability)) + geom_point(alpha=0.5)


# Regime competitiveness
#-----------------------



# Other controls
#---------------



#--------------------------
# Bivariate relationships
#--------------------------
# UDS/Polity + assn
plot.data <- pawns.data %>%
  select(CTRY, YEAR, assn, uds_mean) %>% na.omit()
  
ggplot(plot.data, aes(x=assn, y=uds_mean)) + geom_violin()


# icrg + assn
plot.data <- pawns.data %>%
  select(CTRY, YEAR, assn, icrg_stability) %>% na.omit()

ggplot(plot.data, aes(x=assn, y=icrg_stability)) + geom_violin()


# competitiveness + assn


# competitiveness + uds


# icrg + uds
plot.data <- pawns.data %>%
  select(YEAR, CTRY, icrg_stability, uds_mean) %>% na.omit()

ggplot(plot.data, aes(x=icrg_stability, y=uds_mean)) + 
  geom_point(alpha=0.5) + geom_smooth(method="lm")


# icrg + competitiveness



