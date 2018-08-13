#Install the required packages of plot tools if this is your first time run s-plot
source(paste(getwd(),'/rFunction.R',sep=""))
installp(c("ggplot2","reshape2","dplyr","grid","extrafont","ggfortify","ggbeeswarm",'cowplot'))

## svg
installp(c('svglite','Cairo'))

## histogram-fitted curve
installp("MASS")

## pie
installp("plotly")

## point3d
installp("scatterplot3d")
