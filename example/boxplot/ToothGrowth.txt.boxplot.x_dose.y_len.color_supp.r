source('/opt/bin/s-plot/bin/rFunction.R')

# installp(c("ggplot2", "reshape2", "scales","ggbeeswarm","dplyr"))

if(FALSE || FALSE || FALSE){
    library(ggbeeswarm)
}

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(naturalsort)

###########                 read file                ###########

if ("FALSE" != "FALSE") {
        sampleGroup <- read.table("FALSE",sep="\t",header=1,check.names=F,row.names=1)
        sampleGroup$variable <- factor(rownames(sampleGroup))
}

if(! TRUE){
    if ("FALSE" != "FALSE") {

        # data <- read.table(file="../../data/ToothGrowth.txt", sep="\t", header=TRUE, row.names=1, quote="", check.names=F)
        data <- read.table(file="../../data/ToothGrowth.txt", sep="\t", header=TRUE, quote="", check.names=F)

        gene_temp=strsplit("FALSE",":")[[1]]

        data_m <- as.data.frame(t(data[data[gene_temp[1]]==gene_temp[2],]),stringsAsFactors=F)
        colnames(data_m)<-"value"
        data_m$Sample = rownames(data_m)
        data_m<- data_m[-1:-1,]
        data_m$value <- as.numeric(data_m$value)

        if ("FALSE" != "FALSE") {
            data_m <- merge(data_m, sampleGroup, by="row.names")
        }
        if ("dose" == "Sample") {
            print("Wainning: Because per x-axis tag contains only one data, so recommend you to use the scatterplot or lines script")
        }
    }else {
        data <- read.table(file="../../data/ToothGrowth.txt", sep="\t", header=TRUE, quote="", check.names=F)

        if ("dose" %in% colnames(data)){
            data$dose <- ordered(data$dose,unique(as.character(data$dose)))
        }

        if ("supp" %in% colnames(data)){
            data$supp <- ordered(data$supp,unique(as.character(data$supp)))
        }

        data_m <- melt(data)

        if ("FALSE" != "FALSE") {
            # print(data_m$variable)
            data_m <-left_join(data_m,sampleGroup,by="variable")
        }
    }
} else {
    data_m <- read.table(file="../../data/ToothGrowth.txt", sep="\t",
    header=TRUE, quote="")
}

###########                 read file                ###########


if (0 != 0){
    data_m$len <- data_m$len + 0
}
if("FALSE" != 'FALSE'){
    data_m$len <- FALSE(data_m$len)
}

########### level of x-axis, legend and tag of facet ###########


if ("" != "") {
    data_m$supp <- cut(data_m$supp, )
}

if ("" != "") {
    data_m$dose <- cut(data_m$dose,)
}

x_level <- c()
if (length(x_level)>1){
    data_m$dose <- factor(data_m$dose,levels=x_level)
}else{
    data_m$dose <- ordered(data_m$dose, levels=naturalsort(unique(data_m$dose)))
}

level_i <- c()
if (length(level_i)>1){
    data_m$supp <- factor(data_m$supp, levels=level_i)
}else{
    data_m$supp <- factor(data_m$supp)
}


alpha_level <- c()
if ("c_t_c_t0304" != "c_t_c_t0304"){
    if (length(alpha_level) >1){
        data_m$c_t_c_t0304 <- factor(data_m$c_t_c_t0304, levels=alpha_level, ordered=T)
    }else{
        data_m$c_t_c_t0304 <- factor(data_m$c_t_c_t0304)
    }
}

### value columns
data_m$len <- as.numeric(as.character(data_m$len))
data_m <- data_m[!is.na(data_m$len),]

####sort value
if ("FALSE" !='FALSE'){

    if (FALSE & "dose" != "supp"){
        if (sum(xtabs(~dose+supp, data_m))/length(unique(data_m$dose)) == 1){

            temp_sort <- data_m %>% dplyr::group_by(dose) %>% dplyr::summarise(mean=mean(len))
            temp_sort <- dplyr::arrange(temp_sort, FALSEmean)
            data_m$dose <- ordered(data_m$dose, as.character(temp_sort$dose))
        }else{
            print("Warning: because each dose do not corresponds to only one supp, so igeore sort by group")
        }

    }else{
        if ("NoMeAnInGTh_I_n_G_s" == "dose"){
            data_m$supp <- reorder(data_m$supp,FALSEdata_m$len, FUN=mean)
        }else{
            data_m$dose <- reorder(data_m$dose,FALSEdata_m$len, FUN=mean)
        }
    }
}

####sort value

#### facet levels
facet_level <- c(NA)
if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s"){
    if (length(facet_level) >1){
        data_m$NoMeAnInGTh_I_n_G_s <- factor(data_m$NoMeAnInGTh_I_n_G_s, levels=facet_level, ordered=T)
    }else{
        data_m$NoMeAnInGTh_I_n_G_s <- factor(data_m$NoMeAnInGTh_I_n_G_s)
    }
}

########### level of x-axis, legend and tag of facet ###########

###########   calculate median line about boxplot    ###########

s_boxplot <- function(dat,group_va){

    median_dat <- dat %>% group_by_(.dots=group_va) %>% summarise(median=median(len))
    return(median_dat)

}

if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s") {
    data_bp_median <- s_boxplot(data_m,c("supp","dose","NoMeAnInGTh_I_n_G_s"))
} else {
    data_bp_median <- s_boxplot(data_m,c("supp","dose"))
}
###########   calculate median line about boxplot    ###########

## calculate point size of jitter
data_nrow=nrow(data_m)
if (data_nrow < 50) {
    jitter_size=1
}else{
    jitter_size=0.5
}

###########                geom layer                ###########

p <- ggplot(data_m, aes(dose, len)) + xlab("dose") +
    ylab("len") + labs(title="")

if (FALSE) {
    p <- p + geom_violin(aes(color=supp,fill=supp), 
    stat = "ydensity", position = "dodge", trim = TRUE, 
    scale = "width",width=0.75)

    if ("supp" != "dose"){
        p <- p+geom_boxplot(aes(fill=supp),color="black",size=0.4,
                outlier.colour='NA', width=0.75/3,position=position_dodge(width=0.75))
    }else{
        p <- p+geom_boxplot(fill='white',color="black",size=0.4,
                outlier.colour='NA', width=0.75/3,position=position_dodge(width=0.75))
    }

} else if (FALSE) {
    p <- p + geom_violin(aes(color=supp),size=0.5, 
    stat = "ydensity", position = "dodge", trim = TRUE, 
    scale = "width",width=0.75)+geom_quasirandom(size=jitter_size,alpha=0.7)

    p <- p+geom_point(data=data_bp_median,aes(x=dose,y=median,group=supp,color=supp),position=position_dodge(width=0.75),size=1.5,shape=17)

} else if (FALSE) {

    p <- p + geom_quasirandom(aes(color=supp),size=jitter_size)
    # p <- p + geom_dotplot(aes(color=supp,fill=supp), stackdir = 'center',size=jitter_size, binaxis ='y')

    p <- p + stat_summary(fun.y = "mean", geom = "text", label="----", size= 5, color= "black")

} else {
    if (FALSE){
        if (FALSE){
            p <- p + geom_boxplot(aes(fill=supp,color=supp), notch=TRUE,width=0.75, 
            notchwidth=0.3, outlier.colour='NA')
        }else{
            p <- p + geom_boxplot(aes(fill=supp,color=supp), notch=TRUE,outlier.size=0.5, width=0.75, 
            notchwidth=0.3)
        }
    } else {
        if (FALSE){
            p <- p + geom_boxplot(aes(fill=supp,color=supp),
            outlier.colour='NA', width=0.75)
        }else{
            p <- p + geom_boxplot(aes(fill=supp,color=supp),outlier.size=0.5, width=0.75)
        }
    }

    p <- p+geom_crossbar(data=data_bp_median,aes(x=dose,y=median,ymin=median,ymax=median,group=supp),position=position_dodge(width=0.75),width=0.75,fatten=0,size=0.5,color="white")
}

if (FALSE) {
    p <- p + geom_quasirandom(aes(group=supp),color='black',size=jitter_size,alpha=0.7)
}

if (FALSE) {
    #ylim_zoomin <- boxplot.stats(ddata_m$len)$stats[c(1,5)]
    stats <- boxplot.stats(data_m$len)$stats
    ylim_zoomin <- c(stats[1]/1.05, stats[5]*1.05)
    p <- p + coord_cartesian(ylim = ylim_zoomin)
}


if ("FALSE" != "FALSE") {
    if(FALSE || FALSE){
        p <- p +scale_colour_manual(values=c(FALSE))
    }else{
        p <- p + scale_fill_manual(values=c(FALSE))+scale_colour_manual(values=c(FALSE))
    }
}

if (TRUE) {
    p <- p + coord_flip()
}

if ("NoMeAnInGTh_I_n_G_s" != "NoMeAnInGTh_I_n_G_s") {
    if("h" == 'h') {
        p <- p + facet_grid( .~ NoMeAnInGTh_I_n_G_s,scales="free",space="free")
    }else if("h" == 'v') {
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
p<- PLot_margin(p, '', 'dose', 'len')

#plot width and height auto calculate
w_h_auto <-Plot_size_final(length(unique(data_m$dose)),"TRUE","right","supp", "dose", "NoMeAnInGTh_I_n_G_s", "h", nlevels(data_m$NoMeAnInGTh_I_n_G_s))
w_auto <- w_h_auto[1]
h_auto <- w_h_auto[2]

## print width and height of picture
PrintWD(FALSE,FALSE,w_auto,h_auto,TRUE)

#output
Plot_savefig('.//ToothGrowth.txt.boxplot.x_dose.y_len.color_supp.pdf', p, FALSE, FALSE, w_auto, h_auto, 300, "TRUE",ftype='pdf')

###########                  output                  ###########
