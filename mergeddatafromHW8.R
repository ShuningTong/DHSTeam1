library(readxl)
DHS_Case_Clients_2016EntryCohort <- read_excel("~/Desktop/17 SPRING/Capstone /DHS_Case_Clients_2016EntryCohort.xlsx", sheet = "DHS_Case_Clients_2016EntryCohor")
dat<-DHS_Case_Clients_2016EntryCohort

#create a function that pastes vectors into a string 
pastev<-function(x) {
  n<-length(x)
  temp<-""
  for (i in 1:n) {
    temp<-paste(temp,x[i],sep = ",")
  }
  temp<-sub(",", "", temp)
  return(temp)
}

#paste closedates to a string 
closedatestring<-tapply(dat$CLOSE_DT,dat$CLIENT_ID, pastev)

#paste reasons to a string 
closereasonstr<-tapply(dat$CASE_CLOSE_REASON, dat$CLIENT_ID, pastev)

#create a new data frame that grasp one row for each client ID
x<-1:dim(dat)[1]
idx<-tapply(x, dat$CLIENT_ID, min)
dattemp<-dat[idx,]

# replace the closedate and closereason column
dattemp$CLOSE_DT<-closedatestring
dattemp$CASE_CLOSE_REASON<-closereasonstr

#read in crosssystem data
library(readxl)
DHS_CrossSystem <- read_excel("~/Desktop/17 SPRING/Capstone /DHS_CrossSystem.xlsx", sheet = "SystemInvolvement_EC2016")
datsystem<-DHS_CrossSystem

#merge two datasets 
datmerged<-merge(datsystem, dattemp, by='CLIENT_ID', all = FALSE)
write.table(datmerged, file = "ClientsMerged.txt", sep = "\t")
dim(datmerged)

#2.
datmergednew = read.table("ClientsMerged.txt")
new<-c("CrossID","ACHA1", "ACHA2", "AIUC1", "AIUC2", "AIUP1", "AIUP2")
new<-c(new, "CYFK1", "CYFK2", "CYFP1", "CYFP2", "DA1", "DA2")
new<-c(new, "DPWF1", "DPWF2", "DPWG1", "DPWG2", "DPWS1", "DPWS2")
new<-c(new, "DPWT1", "DPWT2", "EI1", "EI2", "FSC1", "FSC2")
new<-c(new, "HA1", "HA2", "HH1", "HH2", "IL1", "IL2", "JPO1", "JPO2")
new<-c(new, "JPOC1", "JPOC2", "JPOK1", "JPOK2","JPOP1", "JPOP2")
new<-c(new, "MH1", "MH2", "ID1", "ID2","CaseID", "Role", "DOB", "AgeCYF")
new<-c(new, "Gender", "Race",  "CaseStart", "ClientMin", "ClientType","AcceptDate", "CaseType", "AcceptReason", "CloseDate", "CloseReason")
colnames(datmergednew)<-new
