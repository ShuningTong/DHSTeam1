generateBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumServiceFamily)) + 
    geom_boxplot()
}

generateHousingBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumHousingFamily)) + 
    geom_boxplot()
}

generateBehaviorBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumBehaviorFamily)) + 
    geom_boxplot()
}

generateNutritionBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumNutritionFamily)) + 
    geom_boxplot()
}

generateMentalBoxPlot <- function(finalData){
  ggplot(data=finalData, aes(x = as.factor(placement), y = averNumMentalFamily)) + 
    geom_boxplot()
}
