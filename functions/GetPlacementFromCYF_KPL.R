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

## Shuning
# from original data to mergedData
# the same with that in GetPlacementFromACCEPT_REASON
placeData2 <- mergedData %>%
  select(CLIENT_ID, CASE_ID, CYF_KPL_MIN_ACTIVE, CYF_KPL_MAX_ACTIVE) %>%
  mutate(isplaced=!is.na(CYF_KPL_MIN_ACTIVE) | !is.na(CYF_KPL_MAX_ACTIVE)) %>%
  group_by(CASE_ID) %>%
  summarise(placedafter09=sum(isplaced) %>% as.logical)

all(placeData$CASE_ID == placeData2$CASE_ID)
# TRUE
table(placeData$chplaced) # from ACCEPT_REASON
# FALSE  TRUE 
# 1269   280 
table(placeData2$placedafter09) # from CYF
# FALSE  TRUE 
# 1170   379 

# make placement TRUE if either of them TRUE
placeDataNew <- placeData %>%
  mutate(placedafter09 = placeData2$placedafter09) %>%
  mutate(or=chplaced | placedafter09)




# find inconsistency with GetPlacementFromACCEPT_REASON
inconsistencyCaseId <- placeData %>%
  filter(placeData$chplaced != placeData2$placedafter09)%>%
  select(CASE_ID)
  
inconsistencyCaseId2 <- inconsistencyCaseId$CASE_ID
inconsistencyData <- arrange(filter(mergedData, CASE_ID %in% inconsistencyCaseId2), CASE_ID)

