# Shuning's Code
generateBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumService)) + 
    geom_boxplot()
}

### how to call these functions
library(ggplot2)
generateBoxPlot(finalData)
