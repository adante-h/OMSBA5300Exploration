install.packages("purrr")
install.packages("tidyverse")
install.packages("fixest")

library(purrr)
library(tidyverse)
library(lubridate)
library(sys)
library(fixest)

filesnames <- list.files("./data/googletrends", full.names = TRUE)

google_data <- map_df(filesnames, read_csv) %>%
drop_na(index) %>%
mutate(dateranked = ymd(str_sub(monthorweek, end = 10))) %>%
mutate(datemonth = floor_date(dateranked, unit = "month")) %>%
group_by(keyword) %>% mutate(standardindexkeyword = ((index - mean(index))/sd(index))) %>%
group_by(schname) %>% mutate(standardindexschool = ((index - mean(index))/sd(index)))

scorecarddata <- read_csv("./data/Most+Recent+Cohorts+(Scorecard+Elements).csv") %>% rename(unitid = UNITID)
id_name_link <- read_csv("./data/id_name_link.csv")

id_name_link <- id_name_link %>% group_by(schname) %>% mutate(n= n()) %>% filter( n == 1)

joineddata <- inner_join(id_name_link, google_data, by = "schname")
joineddata <- inner_join(scorecarddata, joineddata, by = "unitid")

joineddata <- joineddata %>% select(schid, schname, keyword, index,standardindexkeyword, standardindexschool, dateranked, datemonth, PREDDEG,HBCU,RELAFFIL,'md_earn_wne_p10-REPORTED-EARNINGS', 'NPT4_PUB-AVERAGE-ANNUAL-COST',NPT4_PRIV)%>%
  rename(avgstudearnings = 'md_earn_wne_p10-REPORTED-EARNINGS', avgpubcost = 'NPT4_PUB-AVERAGE-ANNUAL-COST')

joineddata <- joineddata %>% 
  mutate(costofattend = case_when(avgpubcost == 'NULL' ~ NPT4_PRIV,TRUE ~ avgpubcost)) %>%
  filter(costofattend != 'NULL') %>%
  filter(PREDDEG == '3') %>%
  filter(avgstudearnings != 'NULL' & avgstudearnings != 'PrivacySuppressed')%>%
  mutate(costofattend = as.numeric(costofattend), avgstudearnings = as.numeric(avgstudearnings))%>%
  mutate(highearning = avgstudearnings >= (mean(avgstudearnings) + sd(avgstudearnings)), highcost = costofattend >= (mean(costofattend) + sd(costofattend)))%>%
  mutate(lowearning = avgstudearnings <= (mean(avgstudearnings) - sd(avgstudearnings)), lowcost = costofattend <= (mean(costofattend) - sd(costofattend)))%>%
  mutate(avgearning = avgstudearnings <= (mean(avgstudearnings) + sd(avgstudearnings)) & avgstudearnings >= (mean(avgstudearnings) - sd(avgstudearnings)))%>%
  mutate(postScorecards = dateranked >= ymd('2015-09-01')) %>%
  mutate(monthinyear = month(dateranked)) %>%
  mutate(daysfrominitial = dateranked - ymd('2013-03-24')) 


schoolmo <- joineddata %>% group_by(schname,datemonth) %>%summarize(monthchange = sum(standardindexschool))
schoolmocost <- joineddata %>% group_by(schname,datemonth, highcost) %>%summarize(monthchange = sum(standardindexschool))
schoolmoearnings <- joineddata %>% group_by(schname,datemonth, highearning) %>%summarize(monthchange = sum(standardindexschool))


#Plotting data to observe overall trends
ggplot(schoolmo, aes(datemonth,monthchange)) + geom_point() + stat_smooth(method = "lm", col = "red")

#Trends separated into high cost schools and low cost
ggplot(schoolmocost, aes(datemonth, monthchange, color = highcost)) + geom_point() 
ggplot(schoolmocost, aes(datemonth, monthchange, color = highcost)) + geom_point() + facet_grid(rows = vars(highcost)) + stat_smooth(method = "lm", col = "black")

#Trends separated into high earning schools and low earning schools
ggplot(schoolmoearnings,aes(datemonth, monthchange, color = highearning)) + geom_point()
ggplot(schoolmoearnings, aes(datemonth, monthchange, color = highearning)) + geom_point() + facet_grid(rows = vars(highearning)) + stat_smooth(method = "lm", col = "black")


#Regressions Exploring the Dataset
reg1 <- feols(standardindexschool ~ postScorecards + highearning + postScorecards*highearning,data = joineddata)
etable(reg1)

reg2 <- feols(standardindexschool ~ postScorecards + highearning + postScorecards*highearning + highcost, data = joineddata)
etable(reg2)

reg3 <- feols(standardindexschool ~ daysfrominitial + postScorecards + highearning + postScorecards *highearning, data = joineddata)
etable(reg3)

reg4 <- feols(standardindexschool ~ daysfrominitial + postScorecards + highearning + postScorecards*highearning + i(monthinyear), data = joineddata)
etable(reg4)

reg5 <- feols(standardindexschool ~ daysfrominitial + postScorecards + lowearning + lowcost + postScorecards*lowearning + i(monthinyear), data = joineddata)
etable(reg5)

reg6 <- feols(standardindexschool ~ daysfrominitial + postScorecards + avgearning + postScorecards*avgearning + i(monthinyear), data = joineddata)
etable(reg6)



