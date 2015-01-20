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
ciri <- read.csv("raw_data/CIRI Data 1981_2011 2014.04.14.csv")


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
  rename(YEAR = year, COW = ccode)


# Database of Political Institutions 2012
# http://go.worldbank.org/2EAGGLRZ40
pol.inst <- read.dta("raw_data/DPI2012.dta") %>%
  mutate(yrsoffc = fix.999(yrsoffc), oppfrac = fix.999(oppfrac),
         opp1seat = fix.999(opp1seat), totalseats = fix.999(totalseats),
         finittrm = factor(ifelse(finittrm == -999, NA, finittrm), 
                           labels=c("No", "Yes"))) %>%
  mutate(countryname = ifelse(countryname == "UAE", 
                              "United Arab Emirates", countryname),
         countryname = ifelse(countryname == "GDR", 
                              "German Democratic Republic", countryname)) %>%
  mutate(COW = countrycode(countryname, "country.name", "cown")) %>%
  select(YEAR = year, COW, yrsoffc, finittrm, 
         opp1vote, oppfrac, opp1seat, totalseats)


# Unified Democracy Scores (UDS)
# http://www.unified-democracy-scores.org/
uds <- read.csv("raw_data/uds_summary.csv") %>%
  rename(YEAR = year, COW = cowcode,
         uds_mean = mean, uds_sd = sd, uds_median = median,
         uds025 = pct025, uds975 = pct975)


# International Country Risk Guide (ICRG)
# Government stability + three components
# https://epub.prsgroup.com/list-of-all-variable-definitions
# http://www.prsgroup.com/wp-content/uploads/2012/11/icrgmethodology.pdf
# http://library.duke.edu/data/collections/icrg
icrg <- read.csv("raw_data/icrg_raw.csv") %>%
  # Handle Germanys?
  mutate(iso3 = countrycode(Country, "country.name", "iso3c"),
         country.name = countrycode(iso3, "iso3c", "country.name"),
         COW = countrycode(iso3, "iso3c", "cown")) %>%
  gather(year, icrg_stability, -c(Country, iso3, country.name, COW)) %>%
  mutate(YEAR = as.integer(gsub("X", "", as.character(year)))) %>%
  select(-year)


# Polity IV
# http://www.systemicpeace.org/polity/polity4.htm
p4 <- read.csv("raw_data/p4v2012.csv") %>%
  rename(YEAR = year, POLITY = ccode) %>%
  select(YEAR, POLITY, polity2)


# Global Media Freedom Dataset, 1948-2012
# http://faculty.uml.edu/Jenifer_whittenwoodring/MediaFreedomData_000.aspx
media.freedom <- read.csv("raw_data/Global_Media_Freedom_Data.csv") %>%
  rename(YEAR = year, COW=ccode) %>%
  mutate(mediascore = ifelse(mediascore == 8 | mediascore == 0, NA, mediascore),
         mediascore = ifelse(mediascore == 4, 3, mediascore),
         mediascore = factor(mediascore, 
                              labels=c("Free", "Imperfectly Free", "Not Free"),
                              ordered=TRUE))


#-----------------------
# Merge all that data!
#-----------------------
pawns.data <- ciri %>%
  merge(pol.inst, all.x=TRUE) %>%
  merge(nelda, all.x=TRUE) %>%
  merge(p4, all.x=TRUE) %>%
  merge(uds, all.x=TRUE) %>%
  merge(icrg, all.x=TRUE) %>%
  merge(media.freedom, all.x=TRUE) %>%
  mutate(year.group = factor(YEAR),
         assn.clean = ifelse(ASSN < 0, NA, ASSN),
         assn = factor(assn.clean, labels=c("Severely restricted", 
                                            "Limited", "Unrestricted"), 
                       ordered=TRUE))

save(pawns.data, file="data/pawns_clean.RData")
