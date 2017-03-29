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

source("functions/shuning/GetNumberOfServices.R")
serviceDataNew <- calAverNumServiceFamily(mergedData)

source("functions/shuning/MergeServiceAndPlacement.R")
finalData <- getFinalData(placeDataNew, serviceDataNew)

source("functions/shuning/Boxplot.R")
library(ggplot2)
generateBoxPlot(finalData)
g1 <- generateHousingBoxPlot(finalData)
g2 <- generateBehaviorBoxPlot(finalData)
g3 <- generateNutritionBoxPlot(finalData)
g4 <- generateMentalBoxPlot(finalData)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(g1, g2, g3, g4, cols=2)







