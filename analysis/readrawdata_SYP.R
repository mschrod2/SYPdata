library(readr)
library(readxl)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(dbplyr)
#read in raw data from OR Secretary of State. The orgs data is Statement of 
#Organization data where a committee self-identified as an exclusive measure committee. 
#The list includes effective and expiration dates of each Statement of Organization, 
#along with information about the measures supported or opposed.
# The 'transactions' data is all contribution transactions in ORESTAR, which went 
#live on Jan 1, 2006, for committees that at some point in their existence 
#self-identified as an exclusive measure committee, supporting or opposing one or more measures
orgs <- read_excel("input/orgs.xlsx", na='na')
public_opinion <- read_excel("input/public_opinion.xlsx")
PAC_contributions <- read_excel("input/PAC_contributions.xlsx", na="na")
contributions <- read_excel("input/contributions.xlsx", na="na")
contributions <- subset(contributions, !is.na(state)) 
max(contributions$aggregate_amount)
