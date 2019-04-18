#!/bin/bash

# set -x

usage()
{
cat <<EOF
${txtcyn}

***CREATED BY Lin Dechun (lindechun@genomics.com)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This script is used to draw a line or multiple lines using plotly or ggplot2.

${txtbld}OPTIONS${txtrst}:

<input>
    -f  Data file (with header line, the first row is the
        colname, tab seperated. Multiple formats are allowed and described in r-plot_tutorial.md)
        ${bldred}[NECESSARY]${txtrst}
    -m  When true, it will skip preprocess. But the format must be
        the same as listed before (Matrix_melted).
        ${bldred}[Default FALSE, accept TRUE]${txtrst}
    -Q  Giving a sampleGroup file with format specified above to
        tell the group information for each sample. only can be used when <-m FALSE>. When <-Q> is given, <-c/n/j/i> and <-a> should be one of the column
        names of sampleGrp file.${bldred}[Default FALSE, accept a file name]${txtrst}
    -z  Is there a header. Only be used when -m FALSE.[${bldred}Default TRUE${txtrst}]
    -q  Giving one gene ID to do lineplot specifically for this gene.(only one row in data file)
        ${bldred}[Default FALSE, accept a string,like: "Name:A"]${txtrst}
    -H  When you set -q, but you normal data file have some non-numerical colmuns，So you must skip these. [Default 1]

<aes>
    -a  Name for x-axis variable
        [${txtred}Default variable, which is an inner name（when -m FALSE）, suitable 
        for data without 'Set' column. For the given example, 
        'Group' which represents groups of each gene should be 
        supplied to this parameter.
        ${bldred}[NECESSARY, such X_val, both text and number works]${txtrst}
    -d  The column represents the digital values, such as 'H3K27ac'.
        ${bldred}[Default "value" represents the column named "value".
        This parameter can only be set when -m is TRUE.]${txtrst}
    -c  Name for color and fill variable.
        ${bldred}[Optional, such as color]${txtrst}

<labs>
    -t  Title of picture[${txtred}Default NULL${txtrst}]
        [Scatter plot of horizontal and vertical variable]
    -x  xlab of picture[${txtred}Default NULL${txtrst}]
    -y  ylab of picture[${txtred}Default NULL${txtrst}]

<scales>
    -C  Manually specified colors.
        ${bldred}[Default system default.
        Accept string in format like <"'green', 'red'"> (both types of quotes needed).]
        ${txtrst}

<panel>
    -w  The width of output picture.
        [${txtred}Default 8${txtrst}]
    -u  The height of output picture.
        [${txtred}Default 6${txtrst}]
    -P  [Uppercase P] Legend position
        [${txtred}Default right. Accept top,bottom,left,none, or c(0.08,0.8) (relative to left-bottom).${txtrst}]
    -r  The resolution of output picture.[${txtred}Default 300 ppi${txtrst}]
    -E  The format of output figures. [${txtred}Default pdf, accept eps/ps, tex (pictex), pdf, jpeg, tiff, bmp, svg and wmf)${txtrst}]
    -T  The self-definited theme for ggplot2, give the followding theme_classic2 [Default], theme_classic3, theme_cin.${txtrst}

<other ggplot code>
    -p  [Lowercase p] Other legal R codes for gggplot2 will be given here.
        [${txtres}Begin with '+' ${txtrst}]

<output>
    -e  Execute or not[${bldred}Default TRUE${txtrst}]
    -o  Path of output.[Default path of data file(-f), Optinal]

<self-pie>
<<draw pie method>>
    -M  plotly(Default) or ggplot

<<Top Record>>
    -D  Cut the data. Default no
        integer. the top -D (integer) be retain, and other be regard as other
        decimals. less than -D (decimal)'s data be regard as Other

<<label>>
    -L  Whether show label on each fan-shaped window.
        Default 'integer'. You can set 'percent' or 'FALSE'.
        If -M plotly, you should set 'label'(default) or 'label+percent' or 'none'
    -l  label location. inside or outside[Default]

<<xtics limit and label>>
    -U  Manually set the limits of labels.
        ${bldred}Only be used when -A FALSE(numeric). [Default FALSE, accept a series of numbers in following format "-5000,5000"(numeric) or other R code (-p) that can generate a vector to set the position of xtics]${txtrst}
    -K  Manually set the displaying labels. [Default FALSE, accept a series of numbers in following format "-5000,3000,0,3000,5000"(numeric)or other R code(-p) that can generate a vector to set the position of xtics]${txtrst}]

example:
    r-plot pie -f ../../data/twoSample.pie.Species.report.txt -m TRUE -a Sample -d Percent -c Species -o ./ -M ggplot -L integer -D 0.005
EOF
}

#input
file=
melted='FALSE'
sampleGroup='FALSE'
header='TRUE'
gene='FALSE'
skip=1

#aes
x_variable='aAbBcC'
y_value=''
color_variable='c_t_c_t0304'

#labs
title=''
xlab=''
ylab=''

#scales
color_val_manual='FALSE'

#panel
uwid=8
vhig=6
legend_pos='right'
ext='pdf'
res=300
self_theme='theme_classic2'

# other ggplot code
par=''

#output
execute='TRUE'
output=''

#self-pie
##draw pie method
method="plotly"

##top_record
top_record=-1

## label
show_text="integer"
label_location="outside"

##xtics limit and label
xtics_limits='FALSE'
xtics_labels='FALSE'

while getopts "hf:m:Q:z:q:H:a:d:c:t:x:y:C:w:u:P:r:E:T:p:e:o:M:D:L:l:U:K" OPTION
do
    case $OPTION in
        h)
            usage
            exit 1
            ;;
        #input
        f)
            file=$OPTARG
            ;;
        m)
            melted=$OPTARG
            ;;
        Q)
            sampleGroup=$OPTARG
            ;;
        z)
            header=$OPTARG
            ;;
        q)
            gene=$OPTARG
            ;;
        H)
            skip=$OPTARG
            ;;
        #aes
        a)
            x_variable=$OPTARG
            ;;
        d)
            y_value=$OPTARG
            ;;
        c)
            color_variable=$OPTARG
            ;;
        # labs
        t)
            title=$OPTARG
            ;;
        x)
            xlab=$OPTARG
            ;;
        y)
            ylab=$OPTARG
            ;;
        #scales
        C)
            color_val_manual=$OPTARG
            ;;
        #panel
        w)
            uwid=$OPTARG
            ;;
        u)
            vhig=$OPTARG
            ;;
        P)
            legend_pos=$OPTARG
            ;;
        r)
            ext=$OPTARG
            ;;
        E)
            res=$OPTARG
            ;;
        T)
            self_theme=$OPTARG
            ;;
        #other ggplot code
        p)
            par=$OPTARG
            ;;
        #output
        e)
            execute=$OPTARG
            ;;
        o)
            output=$OPTARG
            ;;

        ##self-pie
        ##draw pie method
        M)
            method=$OPTARG
            ;;
        ##top_record
        D)
            top_record=$OPTARG
            ;;
        ##label
        L)
            show_text=$OPTARG
            ;;
        l)
            label_location=$OPTARG
            ;;
        ##xtics limit and label
        U)
            xtics_limits=$OPTARG
            ;;
        K)
            xtics_labels=$OPTARG
            ;;
        ?)
            usage
            exit 1
            ;;
    esac
done

## Variable inspection

if [ -z $file ]; then
	usage
	exit 1
fi

if test "$melted" == "FALSE"; then
    if [ -z $x_variable ]; then
        x_variable="variable"
    fi
fi

if test "$melted" == "FALSE" && test "$gene" != "FALSE"; then
    if test "${x_variable}" == "variable";then
        x_variable="Sample"
    fi
fi

## width and height of picture according to legend position
if test "$method" == "ggplot";then
    if test "$legend_pos" == "none"; then
        uwid=5
        vhig=5
    elif test "$legend_pos" == "top" || test "$legend_pos" == "bottom"; then
        vhig=6
        uwid=7
    else
        uwid=6
        vhig=6
    fi
fi

if test "$method" == "plotly" && test "$show_text" == "integer";then
    show_text="percent"
fi

if test "$melted" == "TRUE"  && test "$y_value" == ""; then
    echo "If '-m TRUE', -d should be set"
    exit 1
fi

### levels
##### color_variable level
if test "$color_variable" == "c_t_c_t0304" && test "$color_level" != "";then
    echo "Warning, you don't set -c(color variable), so -l (color level) will be ignore"
    color_level=""
fi
### levels

## Variable inspection

## Prefix of picture file
mid=$mid".pie.method_"$method

if test "$x_variable" != 'aAbBcC'; then
    mid=$mid'.x_'$x_variable
fi

if test "$color_variable" != "c_t_c_t0304";then
    mid=$mid'.color_'${color_variable}
fi


if test "$melted" == "FALSE" && test "$gene" == "FALSE"; then
    mid=$mid'.allRows'
fi

## Prefix of picture file

if test -z "$output";then
    output=$(cd `dirname $file`;pwd)
fi

cat <<END >$output/$(basename $file)${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

library(ggplot2)
library(plotly)
library(cowplot)
library(dplyr)

###########                  read file                ###########
if ("${sampleGroup}" != "FALSE") {
        sampleGroup <- read.table("${sampleGroup}",sep="\t",header=1,check.names=F,row.names=1)
        sampleGroup\$variable<- factor(rownames(sampleGroup))
}

if(! $melted){
    if ("${gene}" != "FALSE") {

        data <- read.table(file="${file}", sep="\t", header=$header, quote="", check.names=F)

        gene_temp=strsplit("${gene}",":")[[1]]

        data_m <- as.data.frame(t(data[data[gene_temp[1]]==gene_temp[2],]),stringsAsFactors=F)
        colnames(data_m)<-"value"
        data_m\$Sample = rownames(data_m)
        data_m<- data_m[-1:-${skip},]
        data_m\$value <- as.numeric(data_m\$value)

        if ("${sampleGroup}" != "FALSE") {
            data_m <- merge(data_m, sampleGroup, by="row.names")
        }
        if ("${x_variable}" == "Sample") {
            print("Wainning: Because per x-axis tag contains only one data, so recommend you to use the scatterplot or lines script")
        }
    }else {
        data <- read.table(file="${file}", sep="\t", header=$header, quote="", check.names=F)
        data_m <- melt(data)

        if ("${sampleGroup}" != "FALSE") {
            data_m <-left_join(data_m,sampleGroup,by="variable")
        }
    }
} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="",stringsAsFactors=F)
}
###########                  read file                ###########


## order and subset
order_and_subset <- function(dat){

    dat\$$y_value <- dat\$$y_value / sum(dat\$$y_value)  ##替换为百分比
    dat <- dat[order(dat\$$y_value, decreasing = T),]

    ## tidy y_value
    if ($top_record > 1){
        start <- $top_record+1

        dat[start:nrow(dat),]\$$color_variable <- rep("Other",nrow(dat[start:nrow(dat),]))

        dat <- dat %>% group_by(.dots=list('$color_variable')) %>% summarise($y_value=sum($y_value))

    }else if($top_record < 1 & $top_record > 0){
        dat[dat\$$y_value < $top_record,]\$$color_variable <- rep("Other",nrow(dat[dat\$$y_value < $top_record,]))
        dat <- dat %>% group_by(.dots=list('$color_variable')) %>% summarise($y_value=sum($y_value))
    }
    if($top_record != -1){
    ## 重新排序，并把Other放在最后
        dat <- dat[order(dat\$$y_value, decreasing = T),]

        color_order <- dat\$$color_variable[- match('Other',dat\$$color_variable)]
        color_order <- c(color_order, "Other")

        dat <- dat[pmatch(color_order,dat\$$color_variable),]
    }

    dat\$$color_variable <- ordered(dat\$$color_variable,rev(dat\$$color_variable))

    return(dat)
}

if ("$method" == "ggplot"){

    oneGgplotPie <- function(dat,Title){
        Pct <- c(dat\$$y_value/sum(dat\$$y_value))
        Pos <- c(cumsum(360*Pct)-(360*Pct/2))
        dat\$Pos <- c(ifelse(Pos<=180,Pos,Pos-180))

        myLabel1 = paste(round(dat\$$y_value / sum(dat\$$y_value) * 100, 2), "%", sep = "")

        if ("$show_text" == "percent"){
            myLabel = paste(round(dat\$$y_value / sum(dat\$$y_value) * 100, 2), "%", sep = "")
        }else if ("$show_text" == "integer"){
            myLabel = round(dat\$$y_value,2)
        }

        ## basic
        p <- ggplot(dat, aes(x="", y=$y_value, fill=$color_variable))
        p <- p + xlab("${xlab}") + ylab("$ylab") + labs(title=Title)

        ## layer
        p <- p + geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y")
        # p <- p + scale_fill_discrete(breaks = dat\$$color_variable, labels = paste(rev(levels(dat\$$color_variable)),' (',myLabel1,')',sep=""))

        p <- p+scale_fill_brewer(palette="Paired",labels = paste(rev(levels(dat\$$color_variable)),' (',myLabel1,')',sep=""),breaks = dat\$$color_variable)

        # if ("$show_text" == "percent" || "$show_text" == "integer"){
            # p <- p + geom_text(aes(y=Pct/2+c(0,cumsum(Pct)[-length(Pct)]),label = myLabel), size = 5)
        # }

        p <- p + guides(fill=guide_legend(override.aes=list(colour=NA)))

        ### manunal change color

        if ("${color_variable}" != "c_t_c_t0304" & "$color_val_manual" != "FALSE") {
            p <- p + scale_fill_manual(values=c(${color_val_manual}))
        }

        ### manunal change color

        #add additional ggplot2 supported commands
        p <- p${par}

        #select theme
        p <- p + ${self_theme}()

        ###########                  output                  ###########

        #Correcting location of x-aixs label
        # p <- Xlable_angle_correct(p, ${xtics_angle})

        #Set the position of legend
        p <- LegendPos(p, "${legend_pos}")

        #control margin of plot
        p <- PLot_margin(p, Title, '$xlab', '$ylab')

        ## pie specific theme element
        p <- p + theme(panel.background=element_blank(),panel.border=element_blank(),axis.line = element_blank(),axis.ticks = element_blank())
                  theme(axis.line = element_blank(),
                axis.ticks=element_blank(),
                axis.title=element_blank(),
                axis.text.y=element_blank(),
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
          )

        if ("$show_text" == "percent" || "$show_text" == "integer"){
            p <- p + theme(axis.text.x=element_text(color='black', size=10, angle=90-dat\$Pos)) + scale_y_continuous(breaks = cumsum(dat\$$y_value) -dat\$$y_value/2,labels=myLabel)
        }else{
            p <- p +theme(axis.text.x=element_blank())
        }

        return(p)
    }

    if ("$x_variable" != 'aAbBcC' && unique(data_m\$$x_variable) > 1){
        plots <- list()
        for (i in unique(data_m\$$x_variable)){
            data_m_subset <- data_m[data_m\$$x_variable == i,]

            data_m_subset <- order_and_subset(data_m_subset)

            plots[[i]] <- oneGgplotPie(data_m_subset,i)
        }
    }else{
        data_m <- order_and_subset(data_m)

        p <- oneGgplotPie(data_m,"$title")
    }

    if ('plots' %in% ls()){
        pdf(file='$output/$(basename $file)${mid}.${ext}',width=$uwid,height=6*0.75*length(unique(data_m\$$x_variable)))

        print(plot_grid(plotlist=plots,ncol=1,align=c("hv"),labels=LETTERS[1:10],hjust=-1))

        dev.off()

    }else{
        ## print width and height of picture
        print(paste("width of picture:",$uwid,sep=" "))
        print(paste("height of picture:",$vhig,sep=" "))

        #output
        Plot_savefig('$output/$(basename $file)${mid}.${ext}', p, $uwid, $vhig, 8, 6, $res, FALSE,ftype='${ext}')
    }

    ###########                  output                  ###########

} else if ("$method" == "plotly") {

    onePlot_lyPie <- function(dat,sample=""){
        p <- plot_ly(dat, labels = ~$color_variable, values = ~$y_value, type = 'pie',textposition = '$label_location',textinfo = "$show_text",width=$uwid*100, height = $vhig*100) %>%
                layout(title = sample,showlegend=TRUE,margin = list(t=150,b = 150),font=list(size=16),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   legend=list(orientation="v"))

        if (sample == ""){
            export(p,file='$output/$(basename $file)${mid}.${ext}')
        }else{
            export(p,file=paste('$output/$(basename $file)${mid}.${ext}.',sample,'.pdf',sep=""))
        }
    }

    if ("$x_variable" != 'aAbBcC' && unique(data_m\$$x_variable) > 1){
        for (i in unique(data_m\$$x_variable)){
            data_m_subset <- data_m[data_m\$$x_variable == i,]

            data_m_subset <- order_and_subset(data_m_subset)

            onePlot_lyPie(data_m_subset,i)
        }
    }else{
        data_m <- order_and_subset(data_m)
        onePlot_lyPie(data_m,"$title")
    }
}

END

if [ "$execute" == "TRUE" ]; then
    Rscript $output/$(basename $file)${mid}.r
    if [ "$?" == "0" ]; then
        /bin/rm -f $output/$(basename $file)${mid}.r
    fi
fi
