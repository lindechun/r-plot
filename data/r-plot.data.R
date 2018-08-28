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
# boxplot
## or PlantGrowth data
writed(ToothGrowth,"ToothGrowth.txt")
# 
# ToothGrowth$Sample <- rownames(ToothGrowth)
# spread(ToothGrowth,key=dose,value=len)
# write.table(ToothGrowth,file="ToothGrowth.txt",sep="\t",quote = F,row.names = F)

# barplot
uspopchange <- subset(uspopchange,rank(Change) > 40)
writed(uspopchange,"uspopchange.txt")
writed(cabbage_exp,"cabbage_exp.txt")
mtcars$name <- rownames(mtcars)
mtcars$mpg_z <- (mtcars$mpg -mean(mtcars$mpg))/sd(mtcars$mpg)
mtcars$mpg_grp <- factor(ifelse(mtcars$mpg_z < 0, "low", "high"), 
                      levels = c("low", "high"))
writed(mtcars,"mtcars.txt")

# line
## cabbage_exp, ToothGrowth

# point
# mtcars
writed(heightweight,"heightweight.txt")

# pheatmap

# 3d-point
## mtcars

# corrplot
# library(corrplot)
# mcor<- cor(mtcars)
# writed(mcor,"mcor.txt")

# PCA

# density
writed(birthwt,"birthwt.txt")

# histogram
## birthwt

# density
## birthwt
set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
write.table(wdata,file="wdata.txt",sep="\t",quote = F)

dt <- structure(list(Age = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("o80", "u80"), class = "factor"), NoP = c(47L, 5L, 33L, 98L, 287L, 543L, 516L, 222L, 67L, 14L, 13L, 30L, 1L, 6L, 17L, 30L, 116L, 390L, 612L, 451L, 146L, 52L), pctOAC = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)), .Names = c("Age", "NoP", "pctOAC"), row.names = c(NA, -22L), class = "data.frame")
dt<- dt
write.table(as.data.frame(bt),file="density_x_numtype.txt",sep="\t",quote = F)

# area
writed(uspopage,"uspopage.txt")

# density2d
faithful$group=2
faithful[faithful$eruptions<3,]$group <- 1
writed(faithful,"faithful.txt")

# tile
pres_rating <- data.frame(
  rating = as.numeric(presidents),
  year = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents))
)

writed(pres_rating,"pres_rating.txt")

# venn
set.seed(1234)
oneName <- function(){
  paste0(sample(LETTERS,5,replace=TRUE),collapse="")
}

geneNames <- replicate(1000, oneName())

GroupA <- sample(geneNames, 400, replace=FALSE)
GroupB <- sample(geneNames, 750, replace=FALSE)
GroupC <- sample(geneNames, 250, replace=FALSE)
GroupD <- sample(geneNames, 300, replace=FALSE)

index <- unique(c(GroupA,GroupB,GroupC,GroupD))

ma <- function(x){
  match(index,x)
}

vennData <- data.frame(row.names=index,plyr::llply(list(A=GroupA,B=GroupB,C=GroupC,D=GroupD),ma))

vennData[!is.na(vennData)]<-1
vennData[is.na(vennData)]<-0

write.table(vennData,file="vennData_wide.txt",sep="\t",quote=F,row.names=T)

## long format data
vennData$ID<- rownames(vennData)
vennDataLong <- melt(vennData)
vennDataLong <- subset(vennDataLong,value!=0)
colnames(vennDataLong) <- c("feature",'sample','value')
write.table(vennDataLong,file="vennData_long.txt",sep="\t",quote=F,row.names=T)
######################         save data       ######################
