#!/bin/bash

#set -x

usage()
{
cat <<EOF
${txtcyn}

***CREATED BY Chen Tong (chentong_biology@163.com)***
***MODIFIED BY Lin Dechun (lindechun@genomics.com)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This script is used to do full functional scatter plot using ggplot2. 

The parameters for logical variable are either TRUE or FALSE.

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
    -d  Name for y-axis variable the digital values(-Y), such as 'H3K27ac'.
        ${bldred}[Default "value" represents the column named "value".
        This parameter can only be set when -m is TRUE.]${txtrst}
    -c  Name for point color variable.
        ${bldred}[Optional, such as color]${txtrst}
    -L  The order for x-axis variable
        [${txtred}Default data order,accept a string like
        "'g','a','j','x','s','c','o','u'". This will only be considered when -A is TRUE. ***for <Set> column***.${txtrst}]
    -l  The order for point color variable
        [${txtred}Default columns order, accept a string like
        "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.[Default legend_pos for <x_variable> column].${txtrst}]
    -n  The variable for point size.
        ${bldred}[Optional, such as a number or
        a variable like count, normally should be number column]${txtrst}
    -j  The variable for point shape.
        ${bldred}[Optional, such as shape]${txtrst}
    -J  The order for point shape.
        ${bldred}[Default alphabetical order, accept a string like
        "'K562','hESC','GM12878','HUVEC','NHEK','IMR90','HMEC'"]
        ${txtrst}
    -i  The variable for alpha of point.
        ${bldred}[Optional, such as shape(column name in data) or 0.8]${txtrst}
    -I  The order for alpha variable. Only be used when -S is not number.
        ${bldred}[Default alphabetical order, accept a string like
        "'K562','hESC','GM12878','HUVEC','NHEK','IMR90','HMEC'"]
        ${txtrst}
    -A  Whether the attribute of x-axis variable is character.
        [${txtred}Default FALSE, means X-axis label is numeric.
        TRUE means X-axis label is character.${txtrst}]

<labs>
    -t  Title of picture[${txtred}Default empty title${txtrst}]
        [Scatter plot of horizontal and vertical variable]
    -x  xlab of picture[${txtred}Default name for -a${txtrst}]
    -y  ylab of picture[${txtred}Default nane for -d${txtrst}]

<scales>
    -b  Rotation angle for x-axis value(anti clockwise)
        [Default 0]
    -s  Scale y axis
        [${txtred}Default FALSE. Accept the following
        log10[default], log2, or other formula.
        Also if the supplied number after -S is not 0, this
        parameter will be set to TRUE${txtrst}]
    -S  A number to add y axis（to -d) before log-transform to avoid log(0).
        [${txtred}Default 0. If a non-zero number is given, -s is
        TRUE.${txtrst}]
    -C  Manually specified colors.
        ${bldred}[Default system default.
        Accept string in format like <"'green', 'red'"> (both types of quotes needed).]
        ${txtrst}
    -B  Type of color variable.[num, factor(default)]
        <num> represents color variables are numbers. At this time, 
        two colors should be specified in <-C> represents low and high
        color.
        <factor> represents color variables are strings. At this time, 
        same number of colors as number of samples should be given.

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
    -T  The self-definited theme for ggplot2, give the followding theme_classic2 [Default], theme_classic3, theme_cin, theme_point1 (y-axis line), theme_point2 (x-axis line).${txtrst}

<output>
    -e  Execute or not[${bldred}Default TRUE${txtrst}]
    -o  Path of output.[Default path of data file(-f), Optinal]

<self-point_plot>

<<add label>>
    -p  The variable of Label of points.
        ${bldred}[Default no-label, accept a string like <Samp> here 
        to label Samp column text to points.]${txtrst}
    -Z  Label points using <geom_text_repel> which will generate
        non-overlap label texts by adding arrows when necessary.
        If there is sth wrong labeled especially when -J is TRUE, 
        please specify FALSE and use default
        <geom_text> to label text.
        ${bldred}[Default TRUE.]${txtrst}
    -D  Jitter points. Normally used when x and y axis variable is in text format
        or represents group information to avoid point overlaps.
        ${bldred}[Default FALSE]${txtrst}
    # -X  Label font size.
    #     ${bldred}[Default system default. Accept a number.]${txtrst}

<<sort value>>
    -X  Whether sort the value. FALSE [Default] or you can giving the following '-'(dscending) or '+'(ascending) order. '-' and '+' Only be used when -A TURE and -L no set.
    -Y  Whether sort inside each group. Default FALSE. Only be used when -c color_variable be setted, -A TRUE and -X TRUE.

<<add segments from point to 0(y_axis)>>
    -W  Whether add segments from y = 0 to dots. [Default(0, not add ), 1 (add solid line), 2 (add gray bar)]. Only be used when -A TURE and per x_variable only has one Property

<<smooth>>
    -F  Whether add smooth lines. [Default FALSE]
    # -F  Whether display confidence interval around smooth. [Default: TRUE]. Only be used when -F TRUE.
    -k  The smooth method you want to use. Only be used when -F TRUE.
        [${txtred} Default auto. eg. lm, glm, gam, loess, rlm.
        For datasets with n < 1000 default is 'loess'.
        For datasets with 1000 or more observations defaults to 'gam'.
        ${txtrst}]

<<xtics limit and label>>
    -U  Manually set the limits of xtics.
        ${bldred}. Only be used when -A FALSE(numeric). [Default FALSE, accept a series of numbers in following format "-5000,5000"(numeric) or other R code (-p) that can generate a vector to set the position of xtics]${txtrst}
    -K  Manually set the displaying labels of xtics. [Default FALSE, accept a series of numbers in following format "-5000,3000,0,3000,5000"(numeric)or other R code(-p) that can generate a vector to set the position of xtics]${txtrst}]

<<vhline>>
    -v  Add vertical lines. [Default FALSE, accept a series of
        numbers in following format '1,2,3,4,5' or 1].
    -V  Add  horizontal lines. [Default FALSE, accept a series of
        numbers in following format '1,2,3,4,5' or 1].
    -N  Color for vline or hline. [Default black]
    -O  linetype for vline or hline. Only be used when -K or -k is seted.give the followding dashed[Default], solid, dotted, dotdash, longdash, twodash.${txtrst}

example:
    r-plot point -f ../../data/mtcars.txt -m TRUE -A TRUE -a name -d mpg -c cyl -o ./ -b 90 -p mpg
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
x_level=""

y_value=''

color_variable='c_t_c_t0304'
color_level=""

size_variable="c_t_c_t0304"

shape_variable="c_t_c_t0304"
shape_level=""

alpha_variable="c_t_c_t0304"
alpha_level=""
x_type='FALSE'

#labs
title=''
xlab=' '
ylab=' '

#scales
xtics_angle=0
scaleY='FALSE'
y_add=0
color_val_manual='FALSE'
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

#output
execute='TRUE'
output=''

#self-point
## add label
label=''
geom_text_repel='TRUE'
jitter='FALSE'
label_font_size=0
check_overlap="FALSE"
point_hjust=0

#sort value
sort_var='FALSE'
sort_by_group='FALSE'
add_segment='FALSE'
add_label='FALSE'

##smooth
smooth='FALSE'
se_smooth='TRUE'
smooth_method='auto'

##xtics limit and label
xtics_limits='FALSE'
xtics_labels='FALSE'
xtics_scale='continuous'

##vhline
vhline_color='black'
linetype_vh='dotted'
vline='FALSE'
hline='FALSE'

while getopts "hf:m:Q:z:q:H:a:d:c:L:l:n:j:J:i:I:A:t:x:y:b:s:S:C:B:G:g:M:R:w:u:P:r:E:T:e:o:p:Z:D:X:Y:W:F:k:U:K:v:V:N:O" OPTION
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
        L)
            x_level=$OPTARG
            ;;
        l)
            color_level=$OPTARG
            ;;
        n)
            size_variable=$OPTARG
            ;;
        j)
            shape_variable=$OPTARG
            ;;
        J)
            shape_level=$OPTARG
            ;;
        i)
            alpha_variable=$OPTARG
            ;;
        I)
            alpha_level=$OPTARG
            ;;
        A)
            x_type=$OPTARG
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
        s)
            scaleY=$OPTARG
            ;;
        S)
            y_add=$OPTARG
            ;;
        C)
            color_val_manual=$OPTARG
            ;;
        B)
            color_t=$OPTARG
            ;;
        #facet
        G)
            facet=$OPTARG
            ;;
        g)
            facet_direction=$OPTARG
            ;;
        M)
            facet_level=$OPTARG
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
        #output
        e)
            execute=$OPTARG
            ;;
        o)
            output=$OPTARG
            ;;

        ##self-point
        # add label
        p)
            label=$OPTARG
            ;;
        Z)
            geom_text_repel=$OPTARG
            ;;
        D)
            Jitter=$OPTARG
            ;;
        # X)
        #     label_font_size=$OPTARG
        #     ;;
        # Y)
        #     check_overlap=$OPTARG
        #     ;;
        # W)
        #     point_hjust=$OPTARG
        #     ;;
        # sort value
        X)
            sort_var=$OPTARG
            ;;
        Y)
            sort_by_group=$OPTARG
            ;;
        W)
            add_segment=$OPTARG
            ;;
        # p)
            # add_label=$OPTARG
            # ;;
        ##smooth
        F)
            smooth=$OPTARG
            ;;
        # )
        #     se_smooth=$OPTARG
        #     ;;
        k)
            smooth_method=$OPTARG
            ;;
        ##xtics limit and label
        U)
            xtics_limits=$OPTARG
            ;;
        K)
            xtics_labels=$OPTARG
            ;;
        ##vhline
        v)
            vline=$OPTARG
            ;;
        V)
            hline=$OPTARG
            ;;
        N)
            vhline_color=$OPTARG
            ;;
        O)
            linetype_vh=$OPTARG
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
    # echo " the data file(-f) you give is unmelted, but there will have variable and value columns in dataFrame after melted, so you can set -W variable if need."
    if [ -z $x_variable ]; then
        x_variable="variable"
        # echo "Warning, when you give data(-f) file is unmelted, and you don't set -a, so there is set -a variable."
    fi
    if [ -z $y_value ]; then
        y_value="value"
        # echo "Warning, when you give data(-f) file is unmelted, and you don't set -d, so there is set -d value."
    fi
fi

if test "$melted" == "FALSE" && test "$gene" != "FALSE"; then
    if test "${x_variable}" == "variable";then
        x_variable="Sample"
    fi
fi

if test "$melted" == "TRUE"  && test "${x_variable}" == ""; then
    echo "If '-m TRUE', -a should be set"
    exit 1
fi

if test "$melted" == "TRUE" && test "${y_value}" == ""; then
    echo "If '-m TRUE', -d should be set"
    exit 1
fi

######################### sort value ################################
### sort value
if test "$x_level" != '' && test "$sort_var" != "FALSE";then
    echo "Warning: you have set -L(x-axis level), so we will ignore -X (sort value)"
fi
### sort by group
if test "$sort_by_group" != "FALSE" && test "$sort_var" != "FALSE" && test "$color_variable" == "c_t_c_t0304";then
    echo " You set -X"${sort_var}", but you don't set -c(color variable), so we ignore -Y TRUE"
    sort_by_group='FALSE'
fi
######################### sort value ################################

if test "$xlab" == " ";then
    xlab=$x_variable
fi

if test "$ylab" == " ";then
    ylab=$y_value
fi

if test "$x_type" == "FALSE"; then
    xtics_scale='continuous'
fi

### xtics limits
if test "$xtics_limits" != "FALSE" || test "$xtics_labels" != "FALSE";then
    if test "$x_type" == "TRUE";then
        echo "Warning, you set -A TRUE(Default), but you giving -U or -K, which only be used when -A FALSE(numeric X-axis), so we will ignore -U or -K parameter that you set"
    fi
fi

### levels
##### color_variable level
if test "$color_variable" == "c_t_c_t0304" && test "$color_level" != "";then
    echo "Warning, you do not set -c(color variable), so -l(color level) will be ignore"
    color_level=""
fi

##### shape_variable level
if test "$shape_variable" == "c_t_c_t0304" && test "$shape_level" != "";then
    echo "Warning, you do not set -j(shape variable), so -J(shape level) will be ignore"
    shape_level=""
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
if test "$shape_variable" == "c_t_c_t0304";then
    shape_var='NULL'
else
    shape_var="$shape_variable"
fi
if test "$alpha_variable" == "c_t_c_t0304";then
    alpha_var='NULL'
else
    alpha_var="$alpha_variable"
fi
if test "$size_variable" == "c_t_c_t0304";then
    size_var='NULL'
else
    size_var="$size_variable"
fi
### levels

### theme_point select
# if test "$self_theme" == "theme_point";then
#     if test "$rotate_plot" == "TRUE";then
#         self_theme="theme_point2"
#     else
#         self_theme="theme_point1"
#     fi
# fi

# if test "$self_theme" == "theme_point";then
#     self_theme="theme_point1"
# fi

## Variable inspection

## Prefix of picture file
mid=$mid".point"

if test "$melted" == 'TRUE' || (test "$melted" != 'TRUE' && test "$x_variable" != 'variable'); then
    mid=$mid'.x_'${x_variable}'.y_'${y_value}

fi

if test "$color_variable" != "c_t_c_t0304";then
    mid=$mid'.color_'${color_variable}
fi

if test "$size_variable" != "c_t_c_t0304"; then
    mid=$mid'.size_'${color_variable}
fi

if test "$shape_variable" != "c_t_c_t0304"; then
    mid=$mid'.shape_'${color_variable}
fi

if test "$alpha_variable" != "c_t_c_t0304"; then
    mid=$mid'.alpha_'${color_variable}
fi

if test "$smooth" == 'TRUE'; then
    mid=$mid'.smooth'
fi

if test "$vline" != "FALSE"; then
    mid=$mid'.vline'
fi

if test "$hline" != "FALSE"; then
    mid=$mid'.hline'
fi

if test "$facet" != "NoMeAnInGTh_I_n_G_s"; then
    mid=$mid'.facet_'$facet
fi

if test "$melted" == "FALSE" && test "$gene" == "FALSE"; then
    mid=$mid'.allRows'
fi

if test "${sort_var}" != 'FALSE';then
    mid=$mid'.sort_values'
fi

if test "$add_segment" != "FALSE";then
    mid=$mid'.add_segment'
fi

if test "$add_label" != "FALSE";then
    mid=$mid'.add_label'
fi

if test "$scaleY" != "FALSE"; then
    mid=$mid'.scaleY_'$scaleY
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

library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)

if (${jitter}) {
    library(ggbeeswarm)
    ### 扰动图可以用boxplot脚本实现
}

if (("${label}" != "") & (${geom_text_repel}) ) {
    library('ggrepel')
}

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
    data_m <- data_m[data_m\$$y_value !=0,]

} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="")
}

###########                  read file                 ###########

###########                   scales                   ###########
if (${y_add} != 0){
    data_m\$${y_value} <- data_m\$${y_value} + ${y_add}
}

if("$scaleY" !="FALSE"){
    data_m\$${y_value} <- $scaleY(data_m\$${y_value})
}

###########                   scales                   ###########

###### level of x-axis, color, size, shape, alpha and facet variable #######

## x_val levels
if(${x_type}){
    x_level <- c(${x_level})
    if (length(x_level)>1){
        data_m\$${x_variable} <- factor(data_m\$${x_variable},levels=x_level,ordered=T)
    }else{
        # data_m\$${x_variable} <- ordered(data_m\$${x_variable}, levels=naturalsort(unique(data_m\$${x_variable})))
        data_m\$${x_variable} <- factor(data_m\$${x_variable})
    }
}

## color levels
color_level <- c(${color_level})
if ("$color_variable" != "c_t_c_t0304"){
    if (length(color_level) >1){
        data_m\$${color_variable} <- factor(data_m\$${color_variable}, levels=color_level, ordered=T)
    }else if ("${color_t}" == "factor"){
        data_m\$${color_variable} <- factor(data_m\$${color_variable})
    }
}

## shape levels
shape_level <- c(${shape_level})
if ("$shape_variable" != "c_t_c_t0304"){
    if (length(shape_level) >1){
        data_m\$${shape_variable} <- factor(data_m\$${shape_variable}, levels=shape_level, ordered=T)
    }else{
        data_m\$${shape_variable} <- factor(data_m\$${shape_variable})
    }
    ### deal with too many shape level
    shape_level <- length(unique(data_m\$${shape_variable}))
    shapes = (1:shape_level)%%30
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

### value columns
data_m\$$y_value <- as.numeric(as.character(data_m\$$y_value))
data_m <- data_m[!is.na(data_m\$$y_value),]

###### level of x-axis, color, size, shape, alpha and facet variable #######

###########                 sort value                ###########

if($x_type & "$sort_var" != "FALSE"){
    if ($sort_by_group){
        data_m_sort_temp10 <- data_m %>% dplyr::group_by($color_variable) %>% dplyr::summarise(mean1=mean($y_value)) %>% dplyr::arrange(${sort_var}mean1)
        data_m\$$color_variable <- ordered(data_m\$$color_variable,unique(as.character(data_m_sort_temp10\$${color_variable})))

        data_m <- dplyr::arrange(data_m, ${color_variable}, ${sort_var}${y_value})
        data_m\$$x_variable <- ordered(data_m\$$x_variable,unique(as.character(data_m\$$x_variable)))
    }else{
        data_m\$${x_variable} <- reorder(data_m\$$x_variable,${sort_var}data_m\$${y_value}, FUN=mean)
    }

}
###########                 sort value                ###########

###########                geom layer                  ###########

## basic

p <- ggplot(data_m, aes(x=${x_variable},y=${y_value}))
p <- p + xlab("${xlab}") + ylab("${ylab}") + labs(title="${title}")

## layer
if($jitter){
    p <- p + geom_quasirandom(aes(size=${size_var}, color=${color_var},shape=${shape_var}, alpha=${alpha_var}))
}else{
    p <- p + geom_point(aes(size=${size_var}, color=${color_var},shape=${shape_var}, alpha=${alpha_var}))
}

## add label text
if ("${label}" != "" & "$add_segment" == "FALSE") {
    if (${geom_text_repel}) {
        if (${label_font_size} != 0) {
            p <- p + geom_text_repel(aes(label=${label}), size=${label_font_size})
        } else {
            p <- p + geom_text_repel(aes(label=${label}))
        }
    } else {
        if (${jitter}) {
            if (${label_font_size} != 0) {
                p <- p + geom_text(aes(label=${label}), 
                position=position_quasirandom(), 
                hjust=${point_hjust}, size=${label_font_size}, 
                check_overlap=${check_overlap})
            } else {
                p <- p + geom_text(aes(label=${label}), 
                position=position_quasirandom(), 
                hjust=${point_hjust}, check_overlap=${check_overlap})
            }
        } else {
            p <- p + geom_text(aes(label=${label}), 
            position="identity",
            hjust=${point_hjust}, size=${label_font_size}, 
            check_overlap=${check_overlap})
        }
    }
}

## add segment
if($add_segment){
    if("$label" != ""){
        p <- ggplot(data_m, aes(x=${x_variable},y=${y_value}))
        p <- p + xlab("${xlab}") + ylab("${ylab}") + labs(title="${title}")
        p <- p + geom_bar(stat="identity",width=0.3,fill="grey80")+geom_point(aes(color=$color_variable),size=5)+geom_text(aes(label=$label),size=2,color="white")
        p <- p + scale_y_continuous(limits=c(0,max(data_m\$$y_value)
            +max(data_m\$$y_value)/15),expand=c(0.05,0))
        # p <- p +coord_cartesian(ylim=c(min(data_m\$$y_value-(max(data_m\$$y_value)-min(data_m\$$y_value)/100)),max(data_m\$$y_value)+(max(data_m\$$y_value)-min(data_m\$$y_value)/20)),expand=F)
    }else{
        print(11)
        p <- p + geom_point(aes(color=$color_variable),shape=16,size=2.5)+geom_bar(aes(fill=$color_variable),stat="identity",width=0.1)
        # +scale_y_continuous(expand=c(0.025,0))
    }
}

## add smooth
temp_inter <- ''

if("$color_var" != "NULL"){
    if (is.factor(temp_inter)){temp_inter <- data_m\$$color_variable}
    temp_inter <- interaction(temp_inter,data_m\$$color_variable)
}
if("$shape_var" != "NULL"){
    if (is.factor(temp_inter)){temp_inter <- data_m\$$shape_variable}
    temp_inter <- interaction(temp_inter,data_m\$$shape_variable)
}
if("$alpha_var" != "NULL"){
    if (is.factor(temp_inter)){temp_inter <- data_m\$$alpha_variable}
    temp_inter <- interaction(temp_inter,data_m\$$alpha_variable)
}
if("$size_var" != "NULL"){
    if (is.factor(temp_inter)){temp_inter <- data_m\$$size_variable}
    temp_inter <- interaction(temp_inter,data_m\$$size_variable)
}

if ($smooth){
    p <- p + geom_smooth(aes(group=temp_inter),method="$smooth_method",se=$se_smooth,size=0.5,color="black")

    # if("$smooth_method" == "auto"){
    model <- lm($y_value ~ $x_variable,data_m)
    eqn <- as.character(as.expression(
        substitute(italic(y) == b*italic(x) * a *"," ~~ italic(r)^2 ~ "=" ~ r2,
            list(a=format(coef(model)[1],digits=3),
                 b=format(coef(model)[2],digits=3),
                 r2=format(summary(model)\$r.squared, digits=2)
    ))))

    p <- p + annotate("text", x=Inf, y=-Inf,vjust=-0.5,hjust=1.1, parse=T, size=4,label=eqn)
    # }
}

### xtics limit and label
xtics_limit <- c(${xtics_limits})
xtics_label <- c(${xtics_labels})

if ("${xtics_scale}" == "continuous"){
    if("$xtics_limits" != "FALSE" & "$xtics_labels" != "FALSE"){
        p <- p + scale_x_continuous(limits=xtics_limit, breaks=xtics_label, labels=xtics_label)
    }else if("$xtics_limits" != "FALSE" & "$xtics_labels" == "FALSE"){
        p <- p + scale_x_continuous(limits=xtics_limit)
    }else if("$xtics_limits" == "FALSE" & "$xtics_labels" != "FALSE"){

        p <- p + scale_x_continuous(breaks=xtics_label, labels=xtics_label)
    }
}

### vline
vline_vector <- c(${vline})
if("${vline}" != 'FALSE'){
    p <- p + geom_vline(xintercept=vline_vector,
    linetype="${linetype_vh}", color="${vhline_color}", size=0.5)
}

### hline
hline_vector <- c(${hline})
if("${hline}" != 'FALSE'){
    p <- p + geom_hline(yintercept=hline_vector,
    linetype="${linetype_vh}", color="${vhline_color}", size=0.5)
}

### manunal change color and shape

if ("${color_variable}" != "c_t_c_t0304" & "$color_val_manual" != "FALSE") {
    if ("${color_t}" == "factor"){
        p <- p + scale_color_manual(values=c(${color_val_manual}))
    }else if ("${color_t}" == "num" & length(strsplit("${color_val_manual}",",")[[1]]) > 1){
        p <- p + scale_colour_gradient(low=strsplit("$color_val_manual", ",")[[1]][1], high=strsplit("$color_val_manual", ",")[[1]][2], name="${color}")
    }
}
if ("${shape_variable}" != "c_t_c_t0304" & length(shape_level) > 10) {
    p <- p + scale_shape_manual(values=shapes)
}
### manunal change color and shape

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
# p <- p${par}

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
w_h_auto <-Plot_size_final(length(unique(data_m\$${x_variable})),"${rotate_plot}","$legend_pos","${color_variable}", "${x_variable}", "${facet}", "${facet_direction}", nlevels(data_m\$${facet}), plottype="point",char=$x_type)

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
