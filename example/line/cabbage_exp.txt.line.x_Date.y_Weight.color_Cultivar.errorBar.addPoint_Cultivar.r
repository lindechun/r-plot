source('/opt/bin/r-plot/bin/rFunction.R')

library(ggplot2)
library(reshape2)
library(grid)
library(dplyr)
library(naturalsort)
# library(ggpubr)

###########                  read file                ###########

if ("FALSE" != "FALSE") {
        sampleGroup <- read.table("FALSE",sep="\t",header=1,check.names=F,row.names=1)
        sampleGroup$variable<- factor(rownames(sampleGroup))
}

if(! TRUE){
    if ("FALSE" != "FALSE") {

        data <- read.table(file="../../data/cabbage_exp.txt", sep="\t", header=TRUE, quote="", check.names=F)

        gene_temp=strsplit("FALSE",":")[[1]]

        data_m <- as.data.frame(t(data[data[gene_temp[1]]==gene_temp[2],]),stringsAsFactors=F)
        colnames(data_m)<-"value"
        data_m$Sample = rownames(data_m)
        data_m<- data_m[-1:-1,]
        data_m$value <- as.numeric(data_m$value)

        if ("FALSE" != "FALSE") {
            data_m <- merge(data_m, sampleGroup, by="row.names")
        }
        if ("Date" == "Sample") {
            print("Wainning: Because per x-axis tag contains only one data, so recommend you to use the scatterplot or lines script")
        }
    }else {
        data <- read.table(file="../../data/cabbage_exp.txt", sep="\t", header=TRUE, quote="", check.names=F)
        data_m <- melt(data)

        if ("FALSE" != "FALSE") {
            data_m <-left_join(data_m,sampleGroup,by="variable")
        }
    }
    data_m <- data_m[data_m$Weight !=0,]

} else {
    data_m <- read.table(file="../../data/cabbage_exp.txt", sep="\t",
    header=TRUE, quote="")
}


###########                  read file                 ###########

###########                   scales                   ###########

if (0 != 0){
    data_m$Weight <- data_m$Weight + 0
}

if("FALSE" !="FALSE"){
    data_m$Weight <- FALSE(data_m$Weight)
}

###########                   scales                   ###########

###########  level of x-axis, color, linetype and facet variable ###########

### x_val levels
if(TRUE){
    x_level <- c()
    if (length(x_level)>1){
        data_m$Date <- factor(data_m$Date,levels=x_level)
    }else{
        # data_m$Date <- ordered(data_m$Date, levels=naturalsort(unique(data_m$Date)))
        data_m$Date <- factor(data_m$Date)
    }
}else{
    data_m$Date <- as.numeric(data_m$Date)
}

### color levels
level_i <- c()
if (length(level_i) >1){
    data_m$Cultivar <- factor(data_m$Cultivar, levels=level_i)
}else{
    data_m$Cultivar <- factor(data_m$Cultivar)
}

### linetype levels
level_linetype <- c()
if("aAbBcC" != "aAbBcC"){
    if (length(level_linetype) >1){
        data_m$aAbBcC <- factor(data_m$aAbBcC, levels=level_linetype)
    }else{
        data_m$aAbBcC <- factor(data_m$aAbBcC)
    }
}

facet_level <- c(NA)
if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s"){
    if (length(facet_level) >1){
        data_m$NoMeAnInGTh_I_n_G_s <- factor(data_m$NoMeAnInGTh_I_n_G_s, levels=facet_level, ordered=T)
    }else{
        data_m$NoMeAnInGTh_I_n_G_s <- factor(data_m$NoMeAnInGTh_I_n_G_s)
    }
}

### value columns
data_m$Weight <- as.numeric(as.character(data_m$Weight))
data_m <- data_m[!is.na(data_m$Weight),]

###########  level of x-axis, color, linetype and facet variable ###########


############   calculate mean and sd about data_m    ############

s_mean_sd <- function(dat,group_va){
    mean_sd_dat <- dat %>% group_by_(.dots=group_va) %>% dplyr::summarise(sd=sd(Weight),Weight=mean(Weight))
    return(mean_sd_dat)
}

if ("se" == "ctCTct"){
    if ("aAbBcC" != "aAbBcC"){
        if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s") {
            data_m <- s_mean_sd(data_m,c("Cultivar","Date","aAbBcC","NoMeAnInGTh_I_n_G_s"))
        }else{
            data_m <- s_mean_sd(data_m,c("Cultivar","Date","aAbBcC"))
        }
    }else{
        if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s") {
            data_m <- s_mean_sd(data_m,c("Cultivar","Date","NoMeAnInGTh_I_n_G_s"))
        }else{
            data_m <- s_mean_sd(data_m,c("Cultivar","Date"))
        }
    }

}else if("se" != "ctCTct" && "TRUE" != "FALSE"){
    colnames(data_m)[which(colnames(data_m) =="se")] ='sd'
}

############   calculate mean and sd about data_m    ############

if(!TRUE){
    data_m$Date <- as.numeric(as.character(data_m$Date))
}

###########                geom layer                  ###########
## basic
if("Date" == "Cultivar"){
    p <- ggplot(data_m, aes(Date, Weight))
}else{
    p <- ggplot(data_m, aes(Date, Weight, color=Cultivar))
}
p <- p + xlab("Date") + ylab("Weight")+labs(title="")


## layer

pd <- position_dodge(0.3)

## error_bar
if("TRUE" != "FALSE"){
    if("Date" == "Cultivar"){
        p <- p + geom_errorbar(aes(ymin=Weight-sd,ymax=Weight+sd, group=1), colour="black", width=0.2, position=pd)
    }else{
        p <- p + geom_errorbar(aes(ymin=Weight-sd,ymax=Weight+sd, group=Cultivar), colour="black", width=0.2, position=pd)
    }
}

## line
if (FALSE){
    if("aAbBcC" == 'aAbBcC'){
        if("Date" == "Cultivar"){
            p <- p + geom_smooth(aes(group=1),method="auto",se=FALSE,size=0.5,position=pd)
        }else{
            p <- p + geom_smooth(aes(group=Cultivar),method="auto",se=FALSE,size=0.5,position=pd)
        }
    }else{
        if("Date" == "Cultivar"){
            p <- p + geom_smooth(aes(linetype=aAbBcC,group=aAbBcC),method="auto",se=FALSE,size=0.5,position=pd)
        }else{
            p <- p + geom_smooth(aes(linetype=aAbBcC,group=interaction(Cultivar,aAbBcC)),method="auto",se=FALSE,size=0.5,position=pd)
        }
    }
}else{
    if("aAbBcC" == 'aAbBcC'){
        if("Date" == "Cultivar" ){
            p <- p + geom_line(aes(group=1),size=0.5,position=pd)
        }else{
            p <- p + geom_line(aes(group=Cultivar),size=0.5,position=pd)
        }
    }else{
        if("Date" == "Cultivar" ){
            p <- p + geom_line(aes(linetype=aAbBcC,group=aAbBcC),size=0.5,position=pd)
        }else{
            p <- p + geom_line(aes(linetype=aAbBcC,group=interaction(Cultivar,aAbBcC)),size=0.5,position=pd)
        }
    }
}

## Whether add point
if("TRUE" != "FALSE"){
    if("aAbBcC" == 'aAbBcC'){
        if("Date" == "Cultivar"){
            p <- p+geom_point(shape=17,position=pd)
        }else{
            p <- p+geom_point(aes(shape=Cultivar),position=pd)
        }
    }else{
        if("Date" == "Cultivar"){
            p <- p+geom_point(aes(shape=aAbBcC),position=pd)
        }else{
            p <- p+geom_point(aes(shape=Cultivar),position=pd)
        }
    }
}

### xtics limit and label
xtics_limit <- c(FALSE)
xtics_label <- c(FALSE)

if ("discrete" == "continuous"){
    if("FALSE" != "FALSE" & "FALSE" != "FALSE"){
        p <- p + scale_x_continuous(limits=xtics_limit, breaks=xtics_label, labels=xtics_label)
    }else if("FALSE" != "FALSE" & "FALSE" == "FALSE"){
        p <- p + scale_x_continuous(limits=xtics_limit)
    }else if("FALSE" == "FALSE" & "FALSE" != "FALSE"){

        p <- p + scale_x_continuous(breaks=xtics_label, labels=xtics_label)
    }
}

# else{
#   if("FALSE" != "FALSE" && "FALSE" != "FALSE"){
#       p <- p + scale_x_discrete(limits=xtics_limit, labels=xtics_label)
#   }else if("FALSE" != "FALSE" && "FALSE" == "FALSE"){
#       p <- p + scale_x_discrete(limits=xtics_limit)
#   }else if("FALSE" == "FALSE" && "FALSE" != "FALSE"){
#       p <- p + scale_x_discrete(labels=xtics_label)
#   }
# }


### vline
vline_vector <- c(FALSE)
if("FALSE" != 'FALSE'){
    p <- p + geom_vline(xintercept=vline_vector,
    linetype="dotted", color="black", size=0.5)
}

### hline
hline_vector <- c(FALSE)
if("FALSE" != 'FALSE'){
    p <- p + geom_hline(yintercept=hline_vector,
    linetype="dotted", color="black", size=0.5)
}

### manunal change color
if("FALSE" != "FALSE"){
    p <- p + scale_color_manual(values=c(FALSE))
}

### coord_flip
if (FALSE) {
    p <- p + coord_flip()
}

if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s") {
    if("v" == 'h') {
        p <- p + facet_grid( .~ NoMeAnInGTh_I_n_G_s,scales="free",space="free")
    }else if("v" == 'v') {
        p <- p + facet_grid( NoMeAnInGTh_I_n_G_s~. ,scales="free",space="free")
    }else{
        print("-M should choose one of (h,v)")
    }
}

#add additional ggplot2 supported commands

p <- p

#select theme
p <- p + theme_classic2()

###########                geom layer                ###########

###########                  output                  ###########

#Correcting location of x-aixs label
p <- Xlable_angle_correct(p, 0)

#Set the position of legend
p <- LegendPos(p, "right")

#control margin of plot
p <- PLot_margin(p, '', 'Date', 'Weight')

#plot width and height auto calculate
w_h_auto <-Plot_size_final(length(unique(data_m$Date)),"FALSE","right","Cultivar", "Date", "NoMeAnInGTh_I_n_G_s", "v", nlevels(data_m$NoMeAnInGTh_I_n_G_s),plottype="line")
w_auto <- w_h_auto[1]
h_auto <- w_h_auto[2]

## print width and height of picture
PrintWD(FALSE,FALSE,w_auto,h_auto,FALSE)

#output
Plot_savefig('.//cabbage_exp.txt.line.x_Date.y_Weight.color_Cultivar.errorBar.addPoint_Cultivar.pdf', p, FALSE, FALSE, w_auto, h_auto, 300, FALSE,ftype='pdf')

###########                  output                  ###########
