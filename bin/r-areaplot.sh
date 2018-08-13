#!/bin/bash

usage()
{
cat <<EOF
${txtcyn}

***CREATED BY Lin Dechun (lindechun@genomics.com)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This script is used to draw a area plot using ggplot2.

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
    -d  Name for y-axis variable the digital values, such as 'H3K27ac'.
        ${bldred}[Default "value" represents the column named "value".
        This parameter can only be set when -m is TRUE.]${txtrst}
    -c  Name for color and fill variable.
        ${bldred}[Optional, such as color]${txtrst}
    -l  The order for color and fill variable
        [${txtred}Default columns order, accept a string like
        "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.[Default legend_pos for <x_variable> column].${txtrst}]
    -i  The variable for alpha of point.
        ${bldred}[Optional, such as shape(column name in data) or 0.8]${txtrst}
    -I  The order for alpha variable.

<labs>
    -t  Title of picture[${txtred}Default empty title${txtrst}]
        [Scatter plot of horizontal and vertical variable]
    -x  xlab of picture[${txtred}Default name for -a${txtrst}]
    -y  ylab of picture[${txtred}Default nane for -d${txtrst}]

<scales>
    -b  Rotation angle for x-axis value(anti clockwise)
        [Default 0]
    -C  Manually specified colors.
        ${bldred}[Default system default.
        Accept string in format like <"'green', 'red'"> (both types of quotes needed).]
        ${txtrst}
    -B  Manually specified alpha.
        ${bldred}[Default system default.
        Accept string in format like <"'1','0.8','0.6'"> (both types of quotes needed).]
        ${txtrst}

<facet>
    -G  facet grid plots by given column. This is used to put multiple plot in one picture. Used when -m TRUE or -Q be seted, normally a string <set>
        should be suitable for this parameter.
    -g  The levels of wrapping to set the order of each group.
        ${txtred}Normally the unique value of the column given to G in
        a format like <"'a','b','c','d'">.${txtrst}
    -M  The direction of facet when -G is used. give the following h (horizontal), v (vertical, Default).

<coord>
    -R  Rotate the plot from vertical to horizontal. 
        Usefull for plots with many values or very long labels at X-axis.
        ${bldred}[Default FALSE]${txtrst}

<panel>
    -w  The width of output picture.
        [${txtred}Default auto calculate${txtrst}]
    -u  The height of output picture.
        [${txtred}Default auto calculate${txtrst}]
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

<self-density_plot>

<<lineWidth and fillAlpha>>
    -L whether add line ouside area. FALSE(Default) or TRUE
    -F whether fill. FALSE or TRUE(Default). when -L FALSE and -F FALSE, we will set -F TRUE
    -W line width [Default 0.3]. When -L TRUE, it will be used.
    -A fill alpha [Default 0.5]. When -F TRUE, it will be used.
    -O linetype. Only be used when -L TRUE.give the followding solid[Default], dashed, dotted, dotdash, longdash, twodash.${txtrst}

<<Position of each bar>>
    -Y The ways to place multiple histogram for one group if there are.
        Giving "dodge" to arrange multiple bars overlap. 
        Giving "stack" to arrange multiple bars stack.
        Giving "fill" to arrange multiple bars fill to 1.
        [${txtred}Default stack, accept dodge, fill.${txtrst}]

<<xtics limit and label>>
    -U  Manually set the limits of xtics.
        ${bldred}. [Default FALSE, accept a series of numbers in following format "-5000,5000"(numeric) or other R code (-p) that can generate a vector to set the position of xtics]${txtrst}
    -K  Manually set the displaying labels of xtics. [Default FALSE, accept a series of numbers in following format "-5000,3000,0,3000,5000"(numeric)or other R code(-p) that can generate a vector to set the position of xtics]${txtrst}]

example:
    r-plot areaplot -f ../../data/uspopage.txt -m TRUE -a Year -d Thousands -c AgeGroup -o ./ -L TRUE
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
x_variable=''
y_value=''
color_variable='c_t_c_t0304'
color_level=""
alpha_variable="c_t_c_t0304"
alpha_level=""

#labs
title=''
xlab=' '
ylab=' '

#scales
xtics_angle=0
color_val_manual='FALSE'
alpha_val_manual='FALSE'
color_t='factor'

#facet
facet='NoMeAnInGTh_I_n_G_s'
facet_direction='v'
facet_level='NA'

#coord
rotate_plot='FALSE'

#panel
uwid='FALSE'
vhig='FALSE'
legend_pos='right'
ext='pdf'
res=300
self_theme='theme_classic2'

# other ggplot code
par=''

#output
execute='TRUE'
output=''

#self-density
## linewidth and fillAlpha
whether_line="FALSE"
whether_fill="TRUE"
lineWidth=0.3
fillAlpha=0.5
linetype='solid'

## Position of each area
position='stack'

## xtics limit and label
xtics_limits='FALSE'
xtics_labels='FALSE'

while getopts "hf:m:Q:z:q:H:a:d:c:l:i:I:t:x:y:b:C:B:G:g:M:R:w:u:P:r:E:T:p:e:o:L:F:W:A:O:Y:U:K" OPTION
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
        l)
            color_level=$OPTARG
            ;;
        i)
            alpha_variable=$OPTARG
            ;;
        I)
            alpha_level=$OPTARG
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
        b)
            xtics_angle=$OPTARG
            ;;
        C)
            color_val_manual=$OPTARG
            ;;
        B)
            alpha_val_manual=$OPTARG
            ;;
        #facet
        G)
            facet=$OPTARG
            ;;
        g)
            facet_level=$OPTARG
            ;;
        M)

            facet_direction=$OPTARG
            ;;
        #coord
        R)
            rotate_plot=$OPTARG
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

        ##self-density
        ## lineWith and fillAlpha
        L)
            whether_line=$OPTARG
            ;;
        F)
            whether_fill=$OPTARG
            ;;
        W)
            lineWidth=$OPTARG
            ;;
        A)
            fillAlpha=$OPTARG
            ;;
        O)
            linetype=$OPTARG
            ;;
        ## Position of each bar
        Y)
            position=$OPTARG
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

if test "$melted" == "TRUE"  && test "$x_variable" == ""; then
    echo "If '-m TRUE', -a should be set"
    exit 1
fi

if test "$melted" == "TRUE"  && test "$y_value" == ""; then
    echo "If '-m TRUE', -d should be set"
    exit 1
fi

if test "$xlab" == " ";then
    xlab=$x_variable
fi

if test "$ylab" == " ";then
    if test "$position" == "fill";then
        ylab="Percent"
    else
        ylab=$y_value
    fi
fi

### levels
##### color_variable level
if test "$color_variable" == "c_t_c_t0304" && test "$color_level" != "";then
    echo "Warning, you don't set -c(color variable), so -l (color level) will be ignore"
    color_level=""
fi

##### alpha_variable level
if test "$alpha_variable" == "c_t_c_t0304" && test "$alpha_level" != "";then
    echo "Warning, you do not set -i(alpha variable), so -I(alpha level) will be ignore"
    alpha_level=""
fi

######### temp_level_relative_val
if test "$color_variable" == "c_t_c_t0304";then
    color_var='NULL'
else
    color_var="$color_variable"
fi

if test "$alpha_variable" == "c_t_c_t0304";then
    alpha_var='NULL'
else
    alpha_var="$alpha_variable"
fi

### levels

## position
if test "$position" == "dodge";then
    position="identity"
fi
## fill and color
if test "$whether_fill" == "TRUE" && test "$whether_line" == "FALSE" && test "$fillAlpha" == "0.5";then
    fillAlpha=1
fi

## Variable inspection

## Prefix of picture file
mid=$mid".areaplot"

if test "$melted" == 'TRUE' || (test "$melted" != 'TRUE' && test "$x_variable" != 'variable'); then
    mid=$mid'.x_'$x_variable'.y_'$y_value
fi

if test "$color_variable" != "c_t_c_t0304";then
    mid=$mid'.color_'${color_variable}
fi

if test "$alpha_variable" != "c_t_c_t0304"; then
    mid=$mid'.alpha_'${alpha_variable}
fi

mid=$mid'.'$position

if test "$facet" != "NoMeAnInGTh_I_n_G_s"; then
    mid=$mid'.facet_'$facet
fi

if test "$melted" == "FALSE" && test "$gene" == "FALSE"; then
    mid=$mid'.allRows'
fi

if test "$rotate_plot" != "FALSE";then
    mid=$mid'.coord_flip'
fi

## Prefix of picture file

if test -z "$output";then
    output=$(cd `dirname $file`;pwd)
fi

cat <<END >$output/$(basename $file)${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)
library(MASS)

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
    header=$header, quote="")
}
###########                  read file                ###########

###### level of x-axis, color, shape, alpha and facet variable #######

## color levels
color_level <- c(${color_level})
if ("$color_variable" != "c_t_c_t0304"){
    if (length(color_level) >1){
        data_m\$${color_variable} <- factor(data_m\$${color_variable}, levels=color_level, ordered=T)
    }else if ("${color_t}" == "factor"){
        data_m\$${color_variable} <- factor(data_m\$${color_variable})
    }
}

## alpha levels
alpha_level <- c(${alpha_level})
if ("$alpha_variable" != "c_t_c_t0304"){
    if (length(alpha_level) >1){
        data_m\$${alpha_variable} <- factor(data_m\$${alpha_variable}, levels=alpha_level, ordered=T)
    }else{
        data_m\$${alpha_variable} <- factor(data_m\$${alpha_variable})
    }
}

## facet levels
facet_level <- c(${facet_level})
if ("$facet" != "NoMeAnInGTh_I_n_G_s"){
    if (length(facet_level) >1){
        data_m\$${facet} <- factor(data_m\$${facet}, levels=facet_level, ordered=T)
    }else{
        data_m\$${facet} <- factor(data_m\$${facet})
    }
}

###### level of x-axis, color, shape, alpha and facet variable #######

###########                geom layer                  ###########

## basic
p <- ggplot(data_m, aes(x=$x_variable,y=$y_value))


# ggplot(uspopage,aes(x=Year,y=Thousands,fill=factor(AgeGroup)))+geom_area(position="dodge",alpha=0.5)


## layer
if ("$alpha_var" == "NULL"){

    if ("$whether_fill" == "TRUE"){
        p <- p + geom_area(aes(fill=$color_var),position="$position",color=NA,alpha=$fillAlpha)
    }

    if ("$whether_line" == "TRUE"){
        p <- p + geom_area(aes(color=$color_var),fill=NA,linetype="$linetype",alpha=$fillAlpha,size=$lineWidth,position="$position")
    }

    if ("$whether_fill" == "FALSE" & "$whether_line" == "FALSE"){
        p <- p + geom_area(aes(fill=$color_var),position="$position",color=NA,alpha=$fillAlpha)
    }

} else{
    if ("$whether_fill" == "TRUE"){
        p <- p + geom_area(aes(fill=$color_var,alpha=$alpha_var),position="$position",color=NA)
    }

    if ("$whether_line" == "TRUE"){
        p <- p + geom_area(aes(color=$color_var,alpha=$alpha_var),fill=NA,linetype="$linetype",size=$lineWidth,position="$position")
    }

    if ("$whether_fill" == "FALSE" & "$whether_line" == "FALSE"){
        p <- p + geom_area(aes(fill=$color_var,alpha=$alpha_var),position="$position",color=NA)
    }
}

p <- p + xlab("${xlab}") + ylab("$ylab") + labs(title="${title}")


### coord_cartesian
if ("$position" == "fill"){
    p <- p+ scale_y_continuous(labels=scales::percent,expand=c(0.015,0))+scale_x_continuous(expand=c(0.015,0))    
} else{
    p <- p+ scale_y_continuous(expand=c(0.015,0))+scale_x_continuous(expand=c(0.015,0))
}



if ("${xtics_scale}" == "continuous"){
    if("$xtics_limits" != "FALSE" & "$xtics_labels" != "FALSE"){
        p <- p + scale_x_continuous(limits=xtics_limit, breaks=xtics_label, labels=xtics_label)
    }else if("$xtics_limits" != "FALSE" & "$xtics_labels" == "FALSE"){
        p <- p + scale_x_continuous(limits=xtics_limit)
    }else if("$xtics_limits" == "FALSE" & "$xtics_labels" != "FALSE"){

        p <- p + scale_x_continuous(breaks=xtics_label, labels=xtics_label)
    }
}

### manunal change color and alpha

if ("${color_variable}" != "c_t_c_t0304" & "$color_val_manual" != "FALSE") {
    p <- p + scale_color_manual(values=c(${color_val_manual}))
    p <- p + scale_fill_manual(values=c(${color_val_manual}))
}

if ("${alpha_variable}" != "c_t_c_t0304") {
    if ("$alpha_val_manual" != "FALSE"){
        p <- p + scale_alpha_manual(values=c(${alpha_val_manual}))
    }else{
        alpha_n <- seq(0.8,0, by = -1/nlevels(data_m\$$alpha_variable))[1:nlevels(data_m\$$alpha_variable)]
        p <- p + scale_alpha_manual(values = alpha_n)
    }
}

### manunal change color and alpha

### coord_flip
if (${rotate_plot}) {
    p <- p + coord_flip()
}

if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
    if("${facet_direction}" == 'h') {
        p <- p + facet_grid( .~ ${facet},scales="free",space="free")
    }else if("${facet_direction}" == 'v') {
        p <- p + facet_grid( ${facet}~. ,scales="free",space="free")
    }else{
        print("-M should choose one of (h,v)")
    }
}

#add additional ggplot2 supported commands
p <- p${par}

#select theme
p <- p + ${self_theme}()

###########                  output                  ###########

#Correcting location of x-aixs label
p <- Xlable_angle_correct(p, ${xtics_angle})

#Set the position of legend
p <- LegendPos(p, "${legend_pos}")

#control margin of plot
p <- PLot_margin(p, '$title', '$xlab', '$ylab')

#plot width and height auto calculate
w_h_auto <-Plot_size_final(length(unique(data_m\$${x_variable})),"${rotate_plot}","$legend_pos","${color_variable}", "${x_variable}", "${facet}", "${facet_direction}", nlevels(data_m\$${facet}), plottype="areaplot", char=FALSE)

w_auto <- w_h_auto[1]
h_auto <- w_h_auto[2]

## print width and height of picture
PrintWD($uwid,$vhig,w_auto,h_auto,$rotate_plot)

#output
Plot_savefig('$output/$(basename $file)${mid}.${ext}', p, $uwid, $vhig, w_auto, h_auto, $res, ${rotate_plot},ftype='${ext}')
###########                  output                  ###########

END

if [ "$execute" == "TRUE" ]; then
    Rscript $output/$(basename $file)${mid}.r
    if [ "$?" == "0" ]; then
        /bin/rm -f $output/$(basename $file)${mid}.r
    fi
fi
