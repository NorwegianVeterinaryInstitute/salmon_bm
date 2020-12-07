### Descriptive results baseline mortality in Norwegian salmon farming ###
# Author(s): Victor H. S. Oliveira, Katharine R. Dean, Lars Qviller, Carsten Kirkeby and Britt Bang Jensen

Sys.setlocale("LC_TIME", "C")

# Loading required packages
library(here)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)

# A df with the study population without the exclusion criteria
pop_noExc <- readRDS(here("pop_noExc.rds")) 

# A df for analysis after the exclusion criteria
dfanalysis <- readRDS(here("dfanalysis.rds")) 

# Number of farms in our study poulation
length(unique (pop_noExc$location))

# Summaries of active farms per month
pop_noExc %>%
  group_by(date) %>%
  summarise(count = n_distinct(location)) %>%
  summarise(min = min(count), mean = mean(count), median = median(count), max = max(count))

# Number of farms in analysed df
length(unique (dfanalysis$location))

# Number of cohorts in analysed df
length (unique (dfanalysis$cohort.group))

# Summaries fish at risk per month 
summary (dfanalysis$ar.count)

# Summaries mortality rate
summary(dfanalysis$mort.rate)*1000

# Outcome versus explanatory variables

# Variables in a continuous scale:

# Sea surface temperature
qplot(temp, mort.rate*1000, data = dfanalysis, geom = c("point", "smooth"))

# Sea surface salinity
qplot(salinity, mort.rate*1000, data = dfanalysis, geom = c("point", "smooth"))

# Weight upon stocking at sea
qplot(w.start.fish, mort.rate*1000, data = dfanalysis, geom = c("point", "smooth"))

# Fish weight
qplot(w.fish, mort.rate, data = dfanalysis, geom = c("point", "smooth"))

# Local biomass density
qplot(lbd, mort.rate*1000, data = dfanalysis, geom = c("point", "smooth"))

# Sea lice count
qplot(lice.count, mort.rate*1000, data = dfanalysis, geom = c("point", "smooth"))

# Variables in a categorical scale

# Production zone
dfanalysis %>%
  group_by(zone) %>%
  summarize(count = n(),
            q1 = quantile(mort.rate, 0.25)*1000,
            median = median(mort.rate)*1000,
            q3 = quantile(mort.rate, 0.75)*1000,
            mean = mean (mort.rate)*1000,
            SD=sqrt(var(mort.rate))*1000,
            max = max(mort.rate)*1000,
            min = min(mort.rate)*1000)

# Month of first stocking at sea
dfanalysis %>%
  group_by(start.mth) %>%
  summarize(count = n(),
            q1 = quantile(mort.rate, 0.25)*1000,
            median = median(mort.rate)*1000,
            q3 = quantile(mort.rate, 0.75)*1000,
            mean = mean (mort.rate)*1000,
            SD=sqrt(var(mort.rate))*1000,
            max = max(mort.rate)*1000,
            min = min(mort.rate)*1000)

# Sea lice medicinal + H2O2 treatment
dfanalysis %>% 
  group_by(med.treat) %>%
  summarize(count = n(),
            q1 = quantile(mort.rate, 0.25)*1000,
            median = median(mort.rate)*1000,
            q3 = quantile(mort.rate, 0.75)*1000,
            mean = mean (mort.rate)*1000,
            SD=sqrt(var(mort.rate))*1000,
            max = max(mort.rate)*1000,
            min = min(mort.rate)*1000)

# Sea lice non-medicinal treatment
dfanalysis %>%
  group_by(nonmed.treat) %>%
  summarize(count = n(),
            q1 = quantile(mort.rate, 0.25)*1000,
            median = median(mort.rate)*1000,
            q3 = quantile(mort.rate, 0.75)*1000,
            mean = mean (mort.rate)*1000,
            SD=sqrt(var(mort.rate))*1000,
            max = max(mort.rate)*1000,
            min = min(mort.rate)*1000)

