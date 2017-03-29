library(dplyr)
library(tidyverse)
#read in crosssystem data
library(readxl)
crosssystem<- read_excel("~/Desktop/Capstone/DHS_CrossSystem.xlsx", sheet = "SystemInvolvement_EC2016")
crosssystem<-rename(crosssystem, CrossID=CLIENT_ID, CYFK1=CYF_KPL_MIN_ACTIVE)

datdumcross <- crosssystem %>%
  select(CrossID, CYFK1) %>%
  mutate(isplaced=!is.na(CYFK1)) %>%
  group_by(CrossID) %>%
  summarise(placedafter09=sum(isplaced) %>% as.logical)