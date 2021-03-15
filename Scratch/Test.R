library(tidyverse)
library(dplyr)

test <- read.csv("Data/test.csv")

test_summary <- test %>% group_by(site_id, characteristic) %>% 
  mutate(minDate = min(date), maxdate = max(date), count = sum(Count)) %>% 
  ungroup() %>% group_by(site_id,characteristic,date) %>% add_tally() 



test2 <- read.csv("Data/test2.csv")

test2_summary <- test2 %>% group_by(site_id, characteristic) %>% 
  mutate(minDate = min(date), maxdate = max(date), count = sum(Count)) %>% 
  ungroup() %>% group_by(site_id,characteristic,date) %>% add_tally() 

library(lubridate)
testWQP$ActivityStartDate <- mdy(testWQP$ActivityStartDate)

testWQP <- read.csv("Data/test_wqp.csv")

testWQP$Year <- as.Date(testWQP$ActivityStartDate, format="%Y")

testWQP_summary <- testWQP %>% group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  mutate(minDate = min(date), maxdate = max(date)) %>% 
  ungroup() %>% group_by(site_id,characteristic,date) %>% add_tally() 

