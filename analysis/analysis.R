library(ggplot2)
library(ggalt)
library(knitr)
library(textreg)
library(ggthemes)
library(kableExtra)
library(hrbrthemes)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
source("readrawdata_SYP.R")
#remove all values that were not assoicated with a ballot measure
contributions <- contributions[!(contributions$bill_number == ""), ]
contributions <- strftime(contributions$tran_date, "%Y")
PAC_contributions <- PAC_contributions[!(PAC_contributions$bill_range == ""), ]
dfstateOR <- subset(contributions, state=="OR", na.rm= TRUE)
#data frame that is only OR contribution
dfstateOR <- subset(contributions, state=="OR", na.rm= TRUE)
#data frame of all states except Oregon
dfstateother <- subset(contributions, !state=="OR", na.rm= TRUE)
#sum of aggregate contributions
totals <-table((sum(dfstateOR$aggregate_amount, na.rm= TRUE)),
(sum(dfstateother$aggregate_amount, na.rm = TRUE)))
#adding sums together to check against raw data
sum((sum(dfstateOR$aggregate_amount, na.rm= TRUE)),
    (sum(dfstateother$aggregate_amount, na.rm = TRUE)))
#add year column into data frames before plotting
dfstateOR$year <- strftime(dfstateOR$tran_date, "%Y")

dfstateother$year <- strftime(dfstateother$tran_date, "%Y")

#create proportion support and proportion oppose
dfstateOR$support <- 
  dfstateOR%>% 
  count(aggregate , statefip, year) %>%
  group_by(statefip, year) %>%
  mutate(prop = prop.table(n)*100)
#aggregate by bill number and year
aggregate_OR <- aggregate(cbind(aggregate_amount,amount)~bill_number,
                   data=dfstateOR,FUN=sum)
#aggregate by bill number and year
aggregate_OoS <- aggregate(cbind(aggregate_amount,amount)~bill_number,
                   data=dfstateother,FUN=sum)

#merge by bill number and year
aggall <- merge(aggregate_OR, aggregate_OoS, by="bill_number")
#rename columns
aggall <- rename(aggall, aggregate_amount_OR = aggregate_amount.x,
       aggregate_amount_OoS = aggregate_amount.y,
       amount_OR = amount.x,
       amount_OoS = amount.y,
       ballot_measure = bill_number)
#subset to only include statewide ballot measures
aggall <- subset(aggall, !grepl("-", ballot_measure))
aggall <- subset(aggall, !grepl("metro", ballot_measure))

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
aggregate_OR_PAC <- aggregate(cbind(aggregate_amount,amount)~bill_range,
                                   data=dfstateOR_PAC,FUN=sum)
aggregate_OoS_PAC <- aggregate(cbind(aggregate_amount,amount)~bill_range,
                                    data=dfstateother_PAC,FUN=sum)
PAC_ag <- merge(aggregate_OR_PAC, aggregate_OoS_PAC, by="bill_range")
PAC_ag <- rename(PAC_ag,  aggregate_amount_OR = aggregate_amount.x,
                 aggregate_amount_OoS = aggregate_amount.y,
                 amount_OR = amount.x,
                 amount_OoS = amount.y,
                 ballot_measure = bill_range)
#change to character
aggall$ballot_measure <- as.character(aggall$ballot_measure)
public_opinion$ballot_measure <- as.character(public_opinion$ballot_measure)
PAC_ag$ballot_measure <- as.character(PAC_ag$ballot_measure)

alldata <- merge(aggall, public_opinion, by="ballot_measure", na.rm=TRUE)

alldata <- alldata[order(alldata$ballot_measure, decreasing = TRUE), ]

#cannot merge into full set
alldata_PAC <- merge(alldata, PAC_ag, by="ballot_measure", na.rm=TRUE)

MergedDF <- merge(aggall, public_opinion) %>%
  merge(PAC_ag)
#set 

model1 <- lm(`pass/fail`~I(`aggregate_amount_OR`/1000),
              data = alldata)
model2 <- lm(`pass/fail`~I(`aggregate_amount_OoS`/1000),
              data = alldata)
model3 <- lm(`pass/fail`~support,
              data = alldata)
model4 <- lm(`pass/fail`~oppose,
              data = alldata)
model5 <- lm(`pass/fail`~I(`aggregate_amount_OoS`/1000)+I(`aggregate_amount_OR`/1000),
             data = alldata)
model6 <- model3 <- lm(`pass/fail`~support+oppose,
                         data = alldata)

model6 <- lm(`aggregate_amount_OoS`~support,
             data = alldata)
model7 <- lm(`aggregate_amount_OoS`~oppose,
             data = alldata)
model8 <- lm(`aggregate_amount_OoS`~support+oppose,
             data = alldata)

texreg::knitreg(list(model1, model2, model3, model4, model6, model5),
        digits=3,
        caption="Table 1: OLS Regression Models Contributions",
        caption.above = TRUE,
        table = FALSE)

texreg::knitreg(list(model6, model7, model8),
        digits=3,
        caption="Table 2: OLS Regression Models Contributions",
        caption.above = TRUE)

data_PAC <- merge(alldata, PAC_contributions, by="aggregate amo")


test <-  subset(contributions, !grepl("-", bill_number))

knitr::kable(aggall, caption = "Aggregate Contributions by Year", "simple")
aggall %>%
  kbl(caption= "Aggregate Contributions by Year") %>%
  kable_paper("hover", full_width = F)
knitr::kable(alldata, caption = "Aggregate Contributions by Year", "simple")
alldata %>%
  kbl(caption= "Aggregate Contributions by Year") %>%
  kable_paper("hover", full_width = F)


#not sure here
combdat <- dplyr::bind_rows(list(oregon_contributions=dfstateOR,OoS_contributions=dfstateother),
                                        .id="dataset")

alldata_contributions <- subset(combdat, select=c("dataset","tran_id", "tran_date", "filer",
                  "contributor_payee", "subtype", "amount", "aggregate_amount", 
                  "state", "year", "bill_number", "support/oppose"))
#end of not sure


#GRAPHS
ggplot(alldata, aes(x=ballot_measure, y=aggregate_amount_OoS))+
  geom_histogram(stat='identity', binwidth = 30)+
  scale_y_continuous(labels=scales::dollar_format())+
  xlab("Ballot Measure")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,size = rel(1), margin = margin(1, unit = "cm"),vjust =1))+
  ylab("Aggregate Contribution Amount")+
  ggtitle("Contributions by Year")

ggplot(alldata, aes(x=year, y=aggregate_amount, fill=dataset))+
  geom_point(stat='identity', binwidth = 30)+
  scale_y_continuous(labels=scales::dollar_format())+
  xlab("Year")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,size = rel(1), margin = margin(1, unit = "cm"),vjust =1))+
  ylab("Aggregate Contribution Amount")+
  ggtitle("Contributions by Year")


#Graph 1: bar chart for Oregon contributions by year
ggplot(dfstateOR, aes(x=year, y=aggregate_amount))+
  geom_histogram(stat='identity', fill="#FF6666")+
  scale_y_continuous(labels=scales::dollar_format())+
  xlab("Year")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,size = rel(1), margin = margin(1, unit = "cm"),vjust =1))+
  ylab("Aggregate Contribution Amount")+
  ggtitle("Contributions by Year")

# Graph 2: bar chart for OoS contributions by year  
ggplot(dfstateother, aes(x=year, y=aggregate_amount))+
  geom_histogram(stat='identity', fill="#FF6666")+
  scale_y_continuous(labels=scales::dollar_format())+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,size = rel(1), margin = margin(1, unit = "cm"),vjust =1))+
  xlab("Year")+
  ylab("Aggregate Contribution Amount")+
  ggtitle("Contributions by Year")

ggplot(dfstateother, aes(x=year, y=aggregate_amount))+
  geom_histogram(stat='identity', fill="#FF6666")+
  scale_y_continuous(labels=scales::dollar_format())+
  xlab("Year")+
  ylab("Aggregate Contribution Amount")+
  ggtitle("Contributions by Year")

# Graph 3: point for Oregon data
ggplot(dfstateOR, aes(x=year, y=aggregate_amount))+
  geom_point(stat='identity')+
  scale_y_continuous(labels=scales::dollar_format())

#Graph 4: point for OoS data
ggplot(dfstateother, aes(x=year, y=aggregate_amount))+
  geom_point(stat='identity')+
  scale_y_continuous(labels=scales::dollar_format())
