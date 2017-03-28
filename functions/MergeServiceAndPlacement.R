### Shuning's Code

# get final data
getFinalData <- function(placeData, serviceData){
  print(paste("Does the case ids in place data and service data match?", 
              all(placeData$CASE_ID == serviceData$CASE_ID)))
  finalData <- placeData %>%
    select(CASE_ID, placement = or) %>%
    mutate(averNumService = serviceData$averNumServiceFamily)
  return(finalData)
}

### how to call these functions
# finalData <- getFinalData(placeDataNew, serviceDataNew)