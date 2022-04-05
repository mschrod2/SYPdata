library(ggplot2)
library(ggalt)
library(knitr)
library(textreg)
library(scales)
library(ggthemes)
library(kableExtra)
library(hrbrthemes)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(writexl)
source("readrawdata_SYP.R")
#remove all values that were not assoicated with a ballot measure
contributions <- contributions[!(contributions$bill_number == ""), ]
PAC_contributions <- PAC_contributions[!(PAC_contributions$bill_range == ""), ]
contributions$year <- strftime(contributions$tran_date, "%Y")
contributions <- rename(contributions, ballot_measure=bill_number)
contributions <- subset(contributions, !grepl("-", ballot_measure))
contributions <- subset(contributions, !grepl("metro", ballot_measure))
#oregon contributions
dfstateOR <- subset(contributions, state=="OR", na.rm= TRUE)
#data frame of all states except Oregon
dfstateother <- subset(contributions, !state=="OR", na.rm= TRUE)
#support
#sum of aggregate contributions
totals <-table((sum(dfstateOR$aggregate_amount, na.rm= TRUE)),
               (sum(dfstateother$aggregate_amount, na.rm = TRUE)))
#adding sums together to check against raw data
sum((sum(dfstateOR$aggregate_amount, na.rm= TRUE)),
    (sum(dfstateother$aggregate_amount, na.rm = TRUE)))

#aggregate by bill number 
aggregate_OR_year <- aggregate(cbind(aggregate_amount)~ballot_measure+`support/oppose`,
                          data=dfstateOR,FUN=sum)
#aggregate by bill number 
aggregate_OoS_year <- aggregate(cbind(aggregate_amount)~ballot_measure+`support/oppose`,
                           data=dfstateother,FUN=sum)
aggregate_amount_total <- aggregate(cbind(aggregate_amount)~ballot_measure+`support/oppose`,
                                    data=contributions,FUN=sum)

#merge by bill number and year
aggall_year <- merge(aggregate_OR_year, aggregate_OoS_year, by=c("support/oppose","ballot_measure"), all=T)
aggall_year <- merge(aggall_year, aggregate_amount_total, by=c("support/oppose","ballot_measure"), all=T)
aggall_year <- rename(aggall_year, aggregate_amount_OR = aggregate_amount.x,
                     aggregate_amount_OoS = aggregate_amount.y)
aggall_year[is.na(aggall_year)] = 0
#separate data by support and oppose so they don't end up in the same column
aggall_year_support <- subset(aggall_year, grepl("support", `support/oppose`))
aggall_year_oppose <- subset(aggall_year, grepl("oppose", `support/oppose`))
test <- merge(aggall_year_support, aggall_year_oppose, by=c("ballot_measure"), all=T, na.rm=TRUE)
test <- rename(test, 
      aggregate_amount_OR_support = aggregate_amount_OR.x,
       aggregate_amount_OoS_support = aggregate_amount_OoS.x,
       aggregate_amount_OR_oppose = aggregate_amount_OR.y,
       aggregate_amount_OoS_oppose = aggregate_amount_OoS.y,
       aggregate_amount_support =aggregate_amount.x,
       aggregate_amount_oppose =aggregate_amount.y)
test[is.na(test)] = 0
test <- subset(test, select=c("ballot_measure",
                              "aggregate_amount_OR_support",
                              "aggregate_amount_OoS_support",
                              "aggregate_amount_OR_oppose",
                              "aggregate_amount_OoS_oppose",
                              "aggregate_amount_support",
                              "aggregate_amount_oppose" 
                              ))
alldata_2 <- merge(test, public_opinion, by="ballot_measure")
alldata_2 <- subset(alldata_2, select=-salience)

alldata_2$propsupport_OR <- (alldata_2$aggregate_amount_OR_support/alldata_2$aggregate_amount_support)
alldata_2$propsupport_OoS <- (alldata_2$aggregate_amount_OoS_support/alldata_2$aggregate_amount_support)
alldata_2$propoppose_OR <- (alldata_2$aggregate_amount_OR_oppose/alldata_2$aggregate_amount_oppose)
alldata_2$propoppose_OoS <- (alldata_2$aggregate_amount_OoS_oppose/alldata_2$aggregate_amount_oppose)
alldata_2$propsupport_OR[is.na(alldata_2$propsupport_OR)] = 0
alldata_2$propsupport_OoS[is.na(alldata_2$propsupport_OoS)] = 0
alldata_2$propoppose_OR[is.na(alldata_2$propoppose_OR)] = 0
alldata_2$propoppose_OoS[is.na(alldata_2$propoppose_OoS)] = 0
#ALLDATA SAVE
save(alldata_2, file="output/alldata.RData")

write_xlsx(alldata_2, "output/alldata.xlsx")

alldata_year_pac <- merge(alldata_2, PAC_ag, by=c("ballot_measure"), na.rm=TRUE)

#Models
model1_prop <- lm((`pass/fail`=="TRUE")~propsupport_OR,
                  data = alldata_2)
model2_prop <- lm((`pass/fail`=="TRUE")~propsupport_OoS,
                  data = alldata_2)
model3_prop <- lm((`pass/fail`=="TRUE")~propoppose_OR,
                  data = alldata_2)
model4_prop <- lm((`pass/fail`=="TRUE")~propoppose_OoS,
                  data = alldata_2)
model5_prop <- lm((`pass/fail`=="TRUE")~propsupport_OR+propsupport_OoS+propoppose_OR+propoppose_OoS,
                  data = alldata_2)
#models with time fixed effects
model1_prop_t <- lm((`pass/fail`=="TRUE")~propsupport_OR-1,
             data = alldata_2)
model2_prop_t <- lm((`pass/fail`=="TRUE")~propsupport_OoS-1,
             data = alldata_2)
model3_prop_t <- lm((`pass/fail`=="TRUE")~propoppose_OR-1,
             data = alldata_2)
model4_prop_t <- lm((`pass/fail`=="TRUE")~propoppose_OoS-1,
             data = alldata_2)
model5_prop_t <- lm((`pass/fail`=="TRUE")~propsupport_OR+propsupport_OoS+propoppose_OR+propoppose_OoS-1,
             data = alldata_2)

texreg::knitreg(list(model1_prop, model2_prop, model3_prop, model4_prop, model5_prop),
        digits=3,
        caption="Table 1: OLS Regression Models Contributions",
        
        caption.above = TRUE)

PAC_ag <- aggregate(cbind(aggregate_amount,amount)~bill_range,
                    data=PAC_contributions,FUN=sum)

knitr::kable(aggall_year, caption = "Aggregate Contributions by Year", "simple")
aggall_year %>%
  kbl(caption= "Aggregate Contributions by Year") %>%
  kable_paper("hover", full_width = F)
knitr::kable(alldata_year, caption = "Aggregate Contributions by Year", "simple")
alldata_year %>%
  kbl(caption= "Aggregate Contributions by Year") %>%
  kable_paper("hover", full_width = F)

#PAC DATA
dfstateOR_PAC <- subset(PAC_contributions, state=="OR", na.rm= TRUE)
#data frame of all states except Oregon
dfstateother_PAC <- subset(PAC_contributions, !state=="OR", na.rm= TRUE)
#sum of aggregate contributions
totals_PAC <-table((sum(dfstateOR_PAC$aggregate_amount, na.rm= TRUE)),
                   (sum(dfstateother_PAC$aggregate_amount, na.rm = TRUE)))
#adding sums together to check against raw data
sum((sum(dfstateOR_PAC$aggregate_amount, na.rm= TRUE)),
    (sum(dfstateother_PAC$aggregate_amount, na.rm = TRUE)))
#add year column into data frames before plotting
dfstateOR_PAC$year <- strftime(dfstateOR_PAC$tran_date, "%Y")

dfstateother_PAC$year <- strftime(dfstateother_PAC$tran_date, "%Y")

#aggregate by bill number 
aggregate_OR_year_PAC <- aggregate(cbind(aggregate_amount,amount)~`support/oppose`,
                                   data=dfstateOR_PAC,FUN=sum)
aggregate_OoS_year_PAC <- aggregate(cbind(aggregate_amount,amount)~`support/oppose`,
                                    data=dfstateother_PAC,FUN=sum)
PAC_ag_year <- merge(aggregate_OR_year_PAC, aggregate_OoS_year_PAC, by="support/oppose")
PAC_ag_year <- rename(PAC_ag_year,  aggregate_amount_OR = aggregate_amount.x,
                      aggregate_amount_OoS = aggregate_amount.y,
                      amount_OR = amount.x,
                      amount_OoS = amount.y)

aggall_year$ballot_measure <- as.character(aggall_year$ballot_measure)
public_opinion$ballot_measure <- as.character(public_opinion$ballot_measure)
PAC_ag_year$ballot_measure <- as.character(PAC_ag$ballot_measure)
