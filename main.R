rm(list = ls())
setwd("~/Desktop/Capstone") # change to where you put your data

library(readxl)
dat1 <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("DHSTeam1/functions/shuning/GetPlacementFromACCEPT_REASON.R")
mergedData <- mergeData(dat1, dat2)
placeData <- getPlaceData(mergedData)

source("DHSTeam1/functions/shuning/GetPlacementFromCrossSystem.R")
placeData2 <- getPlaceData2(mergedData)
all(placeData$CASE_ID == placeData2$CASE_ID) # TRUE
table(placeData$isPlacedFromAcceptReason) # from ACCEPT_REASON
table(placeData2$isPlacedFromCrossSystem) # from CrossSystem

# make placement TRUE if either of them TRUE
placeDataNew <- placeData %>%
  mutate(isPlacedFromCrossSystem = placeData2$isPlacedFromCrossSystem) %>%
  mutate(isPlacedFromAny= isPlacedFromAcceptReason | isPlacedFromCrossSystem)

source("DHSTeam1/functions/shuning/GetNumberOfServices.R")
serviceDataNew <- calAverNumServiceFamily(mergedData)

source("DHSTeam1/functions/shuning/MergeServiceAndPlacement.R")
finalData <- getFinalData(placeDataNew, serviceDataNew)

source("DHSTeam1/functions/shuning/Boxplot.R")
library(ggplot2)
generateBoxPlot(finalData)


