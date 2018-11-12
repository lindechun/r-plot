#Install the required packages of plot tools if this is your first time run s-plot
source(paste(getwd(),'/rFunction.R',sep=""))
installp(c("ggplot2","reshape2","dplyr","scales","ggbeeswarm","grid","plyr","amap","gplots","cluster","extrafont","fBasics","ggrepel","data.table","ggfortify","pheatmap","psych","fpc","car","ggbeeswarm","hexbin","gclus","UpSetR","VennDiagram",'ggpubr','cowplot','webshot'))

## packages of other tools
installp(c("fBasics",'naturalsort'))

## svg
installp(c('svglite','Cairo'))

## density_line_stat-identity
installp("splines")

## histogram-fitted curve
installp("MASS")

## pie
installp("plotly")

## point3d
installp("scatterplot3d")
