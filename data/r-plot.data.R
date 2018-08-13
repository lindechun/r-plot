library(ggplot2)
library(ggplot2movies)
library(gcookbook)
library(reshape2)
library(tidyr)
library(MASS)
library(plyr)

######################         function        ######################
writed <- function(data,file){
  write.table(data,file=file,sep="\t",quote=F,row.names=F)
}
######################         function        ######################

######################         save data       ######################

# point-3d
mtcars$name <- rownames(mtcars)
mtcars$mpg_z <- (mtcars$mpg -mean(mtcars$mpg))/sd(mtcars$mpg)
mtcars$mpg_grp <- factor(ifelse(mtcars$mpg_z < 0, "low", "high"), 
                      levels = c("low", "high"))
writed(mtcars,"mtcars.txt")

# histogram
writed(birthwt,"birthwt.txt")

set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
write.table(wdata,file="wdata.txt",sep="\t",quote = F)

# areaplot
writed(uspopage,"uspopage.txt")
######################         save data       ######################
