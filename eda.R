# Exploratory data analysis

#--------------------------
# Load libraries and data
#--------------------------
library(dplyr)
library(ggplot2)
library(grid)
library(scales)

load("data/pawns_clean.RData")


#-------------------
# Useful functions
#-------------------
theme_ath <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          #panel.border = element_blank(), 
          #panel.grid=element_blank(), 
          #axis.line=element_blank(),
          axis.ticks=element_blank(),
          #legend.position="none", 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"))
  ret
}


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
  mutate(ci.mult = qt(0.95/2 + 0.5, assn.n),
         assn.ci = assn.se * ci.mult)

ggplot(assn.data, aes(x=assn, y=assn.mean)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=assn.mean - assn.ci, ymax=assn.mean + assn.ci),
                width=0.3) + 
  labs(x="Freedom of assembly and association (CIRI)", 
       y="Average number of countries per year") + 
  theme_ath() + theme(panel.grid.major.x=element_blank())


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
  select(country, year, icrg_stability) %>% na.omit() %>%
  group_by(year) %>% 
  summarize(n = n(),
            icrg.mean = mean(icrg_stability),
            icrg.sd = sd(icrg_stability),
            icrg.se = icrg.sd / sqrt(n)) %>%
  mutate(ci.mult = qt(0.95/2 + 0.5, n),
         ci = icrg.se * ci.mult)

icrg.data1 <- pawns.data %>%
  select(country, year, icrg_stability) %>% na.omit()

ggplot(icrg.data, aes(x=year, y=icrg.mean)) + 
  geom_pointrange(aes(ymin=icrg.mean - ci, ymax=icrg.mean + ci)) + 
  labs(x="Year", y="Government stability (ICRG)") +
  theme_ath()

ggplot(icrg.data1, aes(x=year, y=icrg_stability, group=year)) + geom_violin() + theme_ath()


# Regime competitiveness
#-----------------------
ggplot(pawns.data, aes(x=years.since.comp)) + geom_histogram()


# Other controls
#---------------



#--------------------------
# Bivariate relationships
#--------------------------
# UDS/Polity + assn
plot.data <- pawns.data %>%
  select(country, year, assn, uds_mean) %>% na.omit()
  
ggplot(plot.data, aes(x=assn, y=uds_mean)) + 
  geom_violin() + 
  labs(x="Freedom of assembly and association (CIRI)", y="Mean UDS score") + 
  theme_ath() + theme(panel.grid.major.x=element_blank())


# icrg + assn
plot.data <- pawns.data %>%
  select(country, year, assn, icrg_stability) %>% na.omit()

ggplot(plot.data, aes(x=assn, y=icrg_stability)) + 
  geom_violin() + 
  labs(x="Freedom of assembly and association (CIRI)", 
       y="Government stability (ICRG)") + 
  theme_ath() + theme(panel.grid.major.x=element_blank())


# competitiveness + assn
plot.data <- pawns.data %>%
  select(year, country, assn, all.comp) %>% na.omit()

ggplot(plot.data, aes(x=assn))

# competitiveness + uds


# icrg + uds
plot.data <- pawns.data %>%
  select(year, country, icrg_stability, uds_mean) %>% na.omit()

ggplot(plot.data, aes(x=icrg_stability, y=uds_mean)) + 
  geom_point(alpha=0.5) + geom_smooth(method="lm") + 
  labs(x="Government stability (ICRG)", y="Mean UDS score") + 
  theme_ath()


# icrg + competitiveness



