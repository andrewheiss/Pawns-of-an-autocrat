#-----------------
# Load libraries
#-----------------
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(WDI)


#-------------------
# Useful functions
#-------------------
# What it says on the proverbial tin: convert -999 to NA
fix.999 <- function(x) {
  ifelse(x == -999, NA, x)
}

# Calculate the number of years since an election based on a boolean vector 
# indicating if the running total should be increased
calc.years.since.comp <- function(x) {
  rle.x <- rle(x)$lengths
  as.numeric(unlist(sapply(split(x, rep(seq(along = rle.x), rle.x)), cumsum)))
}

# Holy crap this is complicated.
# If a competitive election has happened already, then figure out if the 
# running total of years since election needs to be increased. This marks years 
# *before* any competitive election as missing. For example, if country A 
# doesn't hold an election until 1960, the years from 1945-59 will be NA.
get.increase <- function(x) {
  has.been.competitive <- FALSE
  increase <- logical(length(x))
  for(i in 1:length(x)) {
    if(!is.na(x[i]) & x[i] == TRUE) {
      has.been.competitive <- TRUE
    } else {
      increase[i] <- NA
    }
    
    if(has.been.competitive) {
      if(x[i] == FALSE | is.na(x[i])) {
        increase[i] <- TRUE
      } else {
        increase[i] <- FALSE
      }
    }
  }
  return(increase)
}


#--------------------------------
# Clean individual data sources
#--------------------------------
# CIRI Human Rights Data
# http://www.humanrightsdata.com/
ciri <- read.csv("raw_data/CIRI Data 1981_2011 2014.04.14.csv") %>%
  select(year = YEAR, cow = COW, assn = ASSN, physint = PHYSINT) %>%
  # Handle duplicate COWs like USSR and Yugoslavia where names change
  group_by(year, cow) %>%
  summarize(assn = max(assn, na.rm=TRUE),
            physint = max(physint, na.rm=TRUE)) %>%
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

# Years since last competitive election
# Get every year
nelda.full <- expand.grid(cow = unique(nelda$cow),
                          year = min(nelda$year):max(nelda$year)) %>%
  arrange(cow, year) %>%
  left_join(nelda, by=c("cow", "year")) %>%
  group_by(cow) %>%
  mutate(years.since.comp = calc.years.since.comp(get.increase(all.comp)),
         num.elections = ifelse(is.na(num.elections), 0, num.elections))


# Database of Political Institutions 2012
# http://go.worldbank.org/2EAGGLRZ40
pol.inst <- read.dta("raw_data/DPI2012.dta") %>%
  filter(!countryname %in% c("Turk Cyprus", "PRK")) %>%
  mutate(yrsoffc = fix.999(yrsoffc), oppfrac = fix.999(oppfrac),
         opp1seat = fix.999(opp1seat), totalseats = fix.999(totalseats),
         opp1vote = fix.999(opp1vote),
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


# Data from Murdie:2013a
murdie <- read.dta("raw_data/11558_2013_9180_MOESM1_ESM.dta") %>%
  select(year, cow=cowcode, countngo)
  

# World Bank World Development Indicators (WDI)
wdi.indicators <- c("NY.GDP.PCAP.KD",  # GDP per capita (constant 2005 US$)
                    "NY.GDP.MKTP.KD",  # GDP (constant 2005 US$)
                    "SP.POP.TOTL",  # Population, total
                    "DT.ODA.ALLD.CD")  # Net ODA and official aid received (current US$)
wdi.countries <- countrycode(na.exclude(unique(ciri$cow)), "cown", "iso2c")
wdi.raw <- suppressWarnings(WDI(wdi.countries, wdi.indicators, 
                                extra=TRUE, start=1981, end=2014))

wdi.clean <- wdi.raw %>%
  rename(gdpcap = NY.GDP.PCAP.KD, gdp = NY.GDP.MKTP.KD, 
         population = SP.POP.TOTL, oda = DT.ODA.ALLD.CD) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  # Ignore negative values of oda
  mutate(oda.log = sapply(oda, FUN=function(x) ifelse(x < 0, NA, log1p(x)))) %>%
  mutate(cow = countrycode(iso2c, "iso2c", "cown"),
         region = factor(region),  # Get rid of unused levels first
         region = factor(region, labels = 
                           gsub(" \\(all income levels\\)", "", levels(region)))) %>%
  select(-c(iso2c, iso3c, country, capital, longitude, latitude, income, lending))


# KOF Index of Globalization, 2014
# http://globalization.kof.ethz.ch/
kof <- read.csv("raw_data/globalization_2014_long.csv", na.strings=".") %>%
  mutate(cow = countrycode(code, "iso3c", "cown")) %>%
  select(cow, year, globalization = index) %>%
  filter(!is.na(cow))


#-----------------------
# Merge all that data!
#-----------------------  
pawns.data <- ciri %>%
  left_join(nelda.full, by=c("year", "cow")) %>%
  left_join(pol.inst, by=c("year", "cow")) %>%
  left_join(uds, by=c("year", "cow")) %>%
  left_join(icrg, by=c("year", "cow")) %>%
  left_join(p4, by=c("year", "cow")) %>%
  left_join(media.freedom, by=c("year", "cow")) %>%
  left_join(wdi.clean, by=c("year", "cow")) %>%
  left_join(kof, by=c("year", "cow")) %>%
  left_join(murdie, by=c("year", "cow")) %>%
  mutate(country = countrycode(cow, "cown", "country.name"),
         iso3 = countrycode(cow, "cown", "iso3c")) %>%
  group_by(cow) %>%
  arrange(year) %>%
  mutate(assn.lead = factor(lead(assn)))

save(pawns.data, file="data/pawns_clean.RData")
