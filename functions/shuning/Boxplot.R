generateBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumServiceFamily)) + 
    geom_boxplot()
}
