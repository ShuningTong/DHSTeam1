# since 0.5 service per person does not make sense,
# this is a second way of calculating number of services

rm(list = ls())
setwd("~/Desktop/Capstone/DHSTeam1") # change to where you put DHSTeam1 folder

library(readxl)
dat1 <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("DHS_CrossSystem.xlsx",
                   sheet = "SystemInvolvement_EC2016") # 8206 obs

source("functions/shuning/GetPlacementFromACCEPT_REASON.R")
mergedData <- mergeData(dat1, dat2)
placeData <- getPlaceData(mergedData)

source("functions/shuning/GetPlacementFromCrossSystem.R")
placeData2 <- getPlaceData2(mergedData)
all(placeData$CASE_ID == placeData2$CASE_ID) # TRUE
table(placeData$isPlacedFromAcceptReason) # from ACCEPT_REASON
table(placeData2$isPlacedFromCrossSystem) # from CrossSystem

# make placement TRUE if either of them TRUE
placeDataNew <- placeData %>%
  mutate(isPlacedFromCrossSystem = placeData2$isPlacedFromCrossSystem) %>%
  mutate(isPlacedFromAny= isPlacedFromAcceptReason | isPlacedFromCrossSystem)
table(placeDataNew$isPlacedFromAny)

source("functions/shuning/GetNumberOfServices2.R")
serviceDataNew2 <- calAverNumServiceFamily(mergedData)

source("functions/shuning/MergeServiceAndPlacement.R")
finalData <- getFinalData(placeDataNew, serviceDataNew2)

source("functions/shuning/Boxplot.R")
library(ggplot2)
generateBoxPlot(finalData, "KindServiceFamily", "Number of Service Kinds and Child Placement")
g1 <- generateBoxPlot(finalData, "NumHousingFamily", "Number of Housing Service and Child Placement")
g2 <- generateBoxPlot(finalData, "NumBehaviorFamily", "Number of Behavior Service and Child Placement")
g3 <- generateBoxPlot(finalData, "NumNutritionFamily", "Number of Nutrition Service and Child Placement")
g4 <- generateBoxPlot(finalData, "NumMentalFamily", "Number of Mental Service and Child Placement")

source("functions/shuning/Multiplot.R")
multiplot(g1, g2, g3, g4, cols=2)

