### Shuning's Code

# housing: ACHA, HACP, HH
# behavior: DA
# nutrition: DPW_FS
# mental health: MH, ID

# calculate num of services for each person
calNumServicePerson <- function(x){
  l <- length(x)
  num <- 0
  for (i in seq(3, l - 1, by = 2)){
    if (!is.na(x[i]) | !is.na(x[i + 1])){
      num <- num + 1
    }
  }
  return(num)
}

# calculate average num of services for each family
calAverNumServiceFamily <- function(mergedData){
  serviceData <- mergedData %>%
    select(CLIENT_ID, CASE_ID, ACHA_MIN_ACTIVE, ACHA_MAX_ACTIVE, 
           HACP_MIN_ACTIVE, HACP_MAX_ACTIVE, HH_MIN_ACTIVE, HH_MAX_ACTIVE, 
           DA_MIN_ACTIVE, DA_MAX_ACTIVE, DPW_FS_MIN_ACTIVE, DPW_FS_MAX_ACTIVE, 
           MH_MIN_ACTIVE, MH_MAX_ACTIVE, ID_MIN_ACTIVE, ID_MAX_ACTIVE) 
  numServicePerson <- apply(serviceData, 1, calNumServicePerson)
  serviceData <- mutate(serviceData, numServicePerson = numServicePerson)
  serviceDataNew <- serviceData %>%
    group_by(CASE_ID) %>%
    summarise(averNumServiceFamily=sum(numServicePerson)/n())
  return(serviceDataNew)
}

### how to call these functions
# library(readxl)
# dat1 <- read_excel("~/Desktop/Capstone/DHS_Case_Clients_2016EntryCohort.xlsx", 
#                    sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
# dat2 <- read_excel("~/Desktop/Capstone/DHS_CrossSystem.xlsx", 
#                    sheet = "SystemInvolvement_EC2016") # 8206 obs
# mergedData <- mergeData(dat1, dat2) # 8084 obs
# serviceDataNew <- calAverNumServiceFamily(mergedData)
