#-----------------
# Load libraries
#-----------------
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)


#-------------------
# Useful functions
#-------------------
fix.999 <- function(x) {
  ifelse(x == -999, NA, x)
}


#--------------------------------
# Clean individual data sources
#--------------------------------
# CIRI Human Rights Data
# http://www.humanrightsdata.com/
ciri <- read.csv("raw_data/CIRI Data 1981_2011 2014.04.14.csv") %>%
  select(year = YEAR, cow = COW, assn = ASSN) %>%
  # Handle duplicate COWs like USSR and Yugoslavia where names change
  group_by(year, cow) %>%
  summarize(assn = max(assn, na.rm=TRUE)) %>%
  mutate(assn = ifelse(assn < 0, NA, assn),
         assn = factor(assn, labels=c("Severely restricted",
                                      "Limited", "Unrestricted"),
                       ordered=TRUE))


# National Elections Across Democracy and Autocracy (NELDA)
# http://hyde.research.yale.edu/nelda/
# 
# TODO: type of most recent election? + time since previous election?
# True if all elections that year were competitive
nelda <- read.dta("raw_data/nelda.dta") %>%
  mutate(competitive = ifelse(nelda3 == "yes" & nelda4 == "yes" & 
                                nelda5 == "yes", TRUE, FALSE)) %>%
  group_by(ccode, year) %>%
  summarise(all.comp = all(competitive), num.elections = n()) %>%
  rename(cow = ccode)


# Database of Political Institutions 2012
# http://go.worldbank.org/2EAGGLRZ40
pol.inst <- read.dta("raw_data/DPI2012.dta") %>%
  filter(!countryname %in% c("Turk Cyprus", "PRK")) %>%
  mutate(yrsoffc = fix.999(yrsoffc), oppfrac = fix.999(oppfrac),
         opp1seat = fix.999(opp1seat), totalseats = fix.999(totalseats),
         finittrm = factor(ifelse(finittrm == -999, NA, finittrm), 
                           labels=c("No", "Yes"))) %>%
  mutate(countryname = ifelse(countryname == "UAE", 
                              "United Arab Emirates", countryname),
         countryname = ifelse(countryname == "GDR", 
                              "German Democratic Republic", countryname),
         countryname = ifelse(countryname == "S. Africa",
                              "South Africa", countryname),
         countryname = ifelse(countryname == "Dom. Rep.",
                              "Dominican Republic", countryname)) %>%
  mutate(cow = countrycode(countryname, "country.name", "cown")) %>%
  select(year, cow, yrsoffc, finittrm, 
         opp1vote, oppfrac, opp1seat, totalseats)


# Unified Democracy Scores (UDS)
# http://www.unified-democracy-scores.org/
uds <- read.csv("raw_data/uds_summary.csv") %>%
  rename(cow = cowcode, uds_mean = mean, uds_sd = sd, uds_median = median,
         uds025 = pct025, uds975 = pct975) %>%
  select(-country)


# International Country Risk Guide (ICRG)
# Government stability + three components
# https://epub.prsgroup.com/list-of-all-variable-definitions
# http://www.prsgroup.com/wp-content/uploads/2012/11/icrgmethodology.pdf
# http://library.duke.edu/data/collections/icrg
icrg <- read.csv("raw_data/icrg_raw.csv") %>%
  mutate(cow = countrycode(Country, "country.name", "cown")) %>%
  select(-Country) %>%
  gather(year, icrg_stability, -cow) %>%
  mutate(year = as.integer(gsub("X", "", as.character(year)))) %>%
  # Handle duplicate COWs like Russia and Germany where names change
  group_by(year, cow) %>%
  summarize(icrg_stability = max(icrg_stability, na.rm=TRUE))


# Polity IV
# http://www.systemicpeace.org/polity/polity4.htm
p4 <- read.csv("raw_data/p4v2012.csv") %>%
  select(year, cow = ccode, polity2)


# Global Media Freedom Dataset, 1948-2012
# http://faculty.uml.edu/Jenifer_whittenwoodring/MediaFreedomData_000.aspx
media.freedom <- read.csv("raw_data/Global_Media_Freedom_Data.csv") %>%
  select(year, cow=ccode, mediascore) %>%
  mutate(mediascore = ifelse(mediascore == 8 | mediascore == 0, NA, mediascore),
         mediascore = ifelse(mediascore == 4, 3, mediascore),
         mediascore = factor(mediascore, 
                              labels=c("Free", "Imperfectly Free", "Not Free"),
                              ordered=TRUE))


#-----------------------
# Merge all that data!
#-----------------------  
pawns.data <- ciri %>%
  left_join(nelda, by=c("year", "cow")) %>%
  left_join(pol.inst, by=c("year", "cow")) %>%
  left_join(uds, by=c("year", "cow")) %>%
  left_join(icrg, by=c("year", "cow")) %>%
  left_join(p4, by=c("year", "cow")) %>%
  left_join(media.freedom, by=c("year", "cow")) %>%
  mutate(country = countrycode(cow, "cown", "country.name"),
         iso3 = countrycode(cow, "cown", "iso3c"))

save(pawns.data, file="data/pawns_clean.RData")
