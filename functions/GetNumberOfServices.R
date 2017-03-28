## Shuning
# from original data to mergedData
# the same with that in GetPlacementFromACCEPT_REASON


serviceData <- mergedData %>%
  select(CLIENT_ID, CASE_ID, ACHA_MIN_ACTIVE, ACHA_MAX_ACTIVE, HACP_MIN_ACTIVE, HACP_MAX_ACTIVE, HH_MIN_ACTIVE, HH_MAX_ACTIVE, DA_MIN_ACTIVE, DA_MAX_ACTIVE, DPW_FS_MIN_ACTIVE, DPW_FS_MAX_ACTIVE, MH_MIN_ACTIVE, MH_MAX_ACTIVE)

# calculate each person's num of services

# calculate each family's num of services per person
numServices <- function(x){
  
}
tapply(serviceData$CLIENT_ID, serviceData$CASE_ID, numServices)  
