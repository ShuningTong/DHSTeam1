## Shuning
# from original data to mergedData
# the same with that in GetPlacementFromACCEPT_REASON

serviceData <- mergedData %>%
  select(CLIENT_ID, CASE_ID, ACHA_MIN_ACTIVE, ACHA_MAX_ACTIVE, HACP_MIN_ACTIVE, HACP_MAX_ACTIVE, HH_MIN_ACTIVE, HH_MAX_ACTIVE, DA_MIN_ACTIVE, DA_MAX_ACTIVE, DPW_FS_MIN_ACTIVE, DPW_FS_MAX_ACTIVE, MH_MIN_ACTIVE, MH_MAX_ACTIVE, ID_MIN_ACTIVE, ID_MAX_ACTIVE)       

# calculate each person's num of services
calNumServicesPerson <- function(x){
  l <- length(x)
  num <- 0
  for (i in seq(3, l - 1, by = 2)){
    if (!is.na(x[i]) | !is.na(x[i + 1])){
      num <- num + 1
    }
  }
  return(num)
}
numServicesPerson <- apply(serviceData, 1, calNumServicesPerson)

serviceData <- mutate(serviceData, numServicesPerson = numServicesPerson)

# calculate each family's num of services per person
serviceDataNew <- serviceData %>%
  group_by(CASE_ID) %>%
  summarise(numServicesFamily=sum(numServicesPerson)/n())

all(placeDataNew$CASE_ID == serviceDataNew$CASE_ID)
# TRUE
finalData <- placeDataNew %>%
  select(CASE_ID, placement = or) %>%
  mutate(numServiceAverage = serviceDataNew$numServicesFamily)

library(ggplot2)
ggplot(data=finalData, aes(x = as.factor(placement), y = numServiceAverage)) + 
  geom_boxplot()
