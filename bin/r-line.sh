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

This script is used to draw a line or multiple lines using ggplot2.
You can specify whether or not smooth your line or lines.

Two types of input files are supported, normal matrix or melted matrix format. Column separator for both types of input files is **tab**. 

Here is an example of normal matrix format. The first column will be treated as X-axis variables and other columns represents each type of lines. The number of columns is unlimited and names of columns is unlimited.

**Set** column is not needed. If given, <facet_plot> (multiple plots in one page) could be displayed.

------------------------------------------------------------
Pos H3K27ac CTCF    Enhancer    H3K4me3 polII
-5000   8.71298 10.69130    11.7359 10.02510    8.26866
-4000   8.43246 10.76680    11.8442 9.76927 7.78358
-3000   8.25497 10.54410    12.2470 9.40346 6.96859
-2000   7.16265 10.86350    12.6889 8.35070 4.84365
-1000   3.55341 8.45751 12.8372 4.84680 1.26110
0   3.55030 8.50316 13.4152 5.17401 1.50022
1000    7.07502 10.91430    12.3588 8.13909 4.88096
2000    8.24328 10.70220    12.3888 9.47255 7.67968
3000    8.43869 10.41010    11.9760 9.80665 7.94148
4000    8.48877 10.57570    11.6562 9.71986 8.17849
------------------------------------------------------

------------With SET------------------------------------------
Pos H3K27ac CTCF    Enhancer    H3K4me3 polII   Set
-5000   8.71298 10.69130    11.7359 10.02510    8.26866 1
-4000   8.43246 10.76680    11.8442 9.76927 7.78358 1
-3000   8.25497 10.54410    12.2470 9.40346 6.96859 1
-2000   7.16265 10.86350    12.6889 8.35070 4.84365 1
-1000   3.55341 8.45751 12.8372 4.84680 1.26110 1
0   3.55030 8.50316 13.4152 5.17401 1.50022 1
1000    7.07502 10.91430    12.3588 8.13909 4.88096 1
2000    8.24328 10.70220    12.3888 9.47255 7.67968 1
3000    8.43869 10.41010    11.9760 9.80665 7.94148 1
4000    8.48877 10.57570    11.6562 9.71986 8.17849 1
-5000   8.71298 10.69130    11.7359 10.02510    8.26866 2
-4000   8.43246 10.76680    11.8442 9.76927 7.78358 2
-3000   8.25497 10.54410    12.2470 9.40346 6.96859 2
-2000   7.16265 10.86350    12.6889 8.35070 4.84365 2
-1000   3.55341 8.45751 12.8372 4.84680 1.26110 2
0   3.55030 8.50316 13.4152 5.17401 1.50022 2
1000    7.07502 10.91430    12.3588 8.13909 4.88096 2
2000    8.24328 10.70220    12.3888 9.47255 7.67968 2
3000    8.43869 10.41010    11.9760 9.80665 7.94148 2
4000    8.48877 10.57570    11.6562 9.71986 8.17849 2
-------------------------------------------------------------

For matrix format, example command lines include:

* Attribute of X-axis value (first column of matrix) is <number>

    r-plot lines -f matrix.file -A FALSE

* Attribute of X-axis value (first column of matrix) is <text>
    r-plot lines -f matrix.file 

* Attribute of X-axis value (first column of matrix) is numbers, change legned order (default alphabet order)

    r-plot lines -f matrix.file -l "'polII', 'CTCF', 'Enhancer', 'H3K27ac', 'H3K4me3'"

* Attribute of X-axis value (first column of matrix) is numbers, change legned order (default alphabet order), smooth lines to look better (Pay attention to whether this will change the data trend)

    r-plot lines -f matrix.file -l "'polII', 'CTCF', 'Enhancer', 'H3K27ac', 'H3K4me3'" -o TRUE

* Attribute of X-axis value (first column of matrix) is numbers, with <Set> (Set is column name) column
    
    r-plot lines -f matrix.file -F "+facet_grid(Set ~ ., scale='free_y')"


FILEFORMAT when -m is true
#The name "value" shoud **not** be altered.
#variable can be altered using -H
#Actually this format is the melted result of last format.
--------------------------------------------------------------
Pos variable    value
-5000   H3K27ac 8.71298
-4000   H3K27ac 8.43246
-3000   H3K27ac 8.25497
-2000   H3K27ac 7.16265
-1000   H3K27ac 3.55341
0   H3K27ac 3.55030
1000    H3K27ac 7.07502
2000    H3K27ac 8.24328
3000    H3K27ac 8.43869
4000    H3K27ac 8.48877
-5000   CTCF    10.69130
-4000   CTCF    10.76680
-3000   CTCF    10.54410
-2000   CTCF    10.86350
-1000   CTCF    8.45751
0   CTCF    8.50316
1000    CTCF    10.91430
2000    CTCF    10.70220
3000    CTCF    10.41010
4000    CTCF    10.57570
-------------------------------------------------------------

* Attribute of X-axis value (melt format) is <number>

    r-plot lines -f matrix.file -m TRUE -a Pos -A FALSE

* Attribute of X-axis value (first column of matrix) is <text>

    r-plot lines -f matrix.file -m TRUE -a Pos

* If the name of the second column is <type> not <variable>, one should specify with <-F>. 
    
    r-plot lines -f matrix.file -m TRUE -a Pos -F type -A FALSE

* Attribute of X-axis value (first column of matrix) is numbers, change legned order (default alphabet order)

    r-plot lines -f matrix.file -m TRUE -a Pos -l "'polII', 'CTCF', 'Enhancer', 'H3K27ac', 'H3K4me3'"

* Attribute of X-axis value (first column of matrix) is numbers, change legned order (default alphabet order), smooth lines to look better (Pay attention to whether this will change the data trend)

    r-plot lines -f matrix.file -m TRUE -a Pos -l "'polII', 'CTCF', 'Enhancer', 'H3K27ac', 'H3K4me3'" -o TRUE

待修改# -o Smooth lines or not.

* Attribute of X-axis value (first column of matrix) is numbers, with <Set> (Set is column name) column
    
    r-plot lines -f matrix.file -F Set


${txtbld}OPTIONS${txtrst}:
<input>
    -f  Data file (with header line, the first row is the
        colname, tab seperated. Multiple formats are allowed and described in r-plot_tutorial.md)
        ${bldred}[NECESSARY]${txtrst}
    -m  When true, it will skip preprocess. But the format must be
        the same as listed before (Matrix_melted).
        ${bldred}[Default FALSE, accept TRUE]${txtrst}
    -Q  Giving a sampleGroup file with format specified above to
        tell the group information for each sample. only can be used when <-m FALSE>. When <-Q> is given, <-F> and <-a> should be one of the column
        names of sampleGrp file.${bldred}[Default FALSE, accept a file name]${txtrst}
    -z  Is there a header. Only be used when -m FALSE.[${bldred}Default TRUE${txtrst}]
    -q  Giving one gene ID to do lineplot specifically for this gene.(only one row in data file)
        ${bldred}[Default FALSE, accept a string,like: "Name:A"]${txtrst}
    -H  When you set -q, but you normal data file have some non-numerical colmuns，So you must skip these. [Default 1]

<aes>
    -a  Name for x-axis variable
        [${txtred}Default variable, which is an inner name, suitable 
        for data without 'Set' column. For the given example, 
        'Group' which represents groups of each gene should be 
        supplied to this parameter.
    -d  The column represents the digital values, such as 'H3K27ac'.
        ${bldred}[Default "value" represents the column named "value".
        This parameter can only be set when -m is TRUE.]${txtrst}
    -c  Name for line color variable meaning line_color_variable.
        If no-subclass of X-variavle(-a), this will be used X-axis variable.
        ${bldred}[Default "variable" represents the column named "variable".
        This parameter can only be set when -m is TRUE or -Q be seted.]${txtrst}
    -L  Levels for x-axis variable
        [${txtred}Default data order,accept a string like
        "'g','a','j','x','s','c','o','u'". This will only be considered when -A is TRUE. ***for <Set> column***.${txtrst}]
    -l  Levels for color variable
        [${txtred}Default columns order, accept a string like
        "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.[Default legend_pos for <xvariable> column].${txtrst}]
    -A  Whether the attribute of x-axis variable is character.
        [${txtred}Default TRUE, means X-axis label is text.
        FALSE means X-axis label is numeric.${txtrst}]

<labs>
    -t  Title of picture[${txtred}Default empty title${txtrst}]
    -x  xlab of picture[${txtred}Default name for -a${txtrst}]
    -y  ylab of picture[${txtred}Default name for -d${txtrst}]

<scales>
    -b  Rotation angle for x-axis value(anti clockwise)
        ${bldred}[Default 0]${txtrst}
    -s  Scale y axis
        [${txtred}Default FALSE. Accept the following
        log10[default], log2, or other formula.
        Also if the supplied number after -S is not 0, this
        parameter will be set to TRUE${txtrst}]
    -S  A number to add y axis（to -d)
        [${txtred}Default 0. If a non-zero number is given, -s is
        TRUE.${txtrst}]
    -C  Manually set colors for each line. Default FALSE, meaning using ggplot2 default Color for each bar.
        [${txtred}Or, you can giving str format, 
        ususlly the number of colors should
        be equal to the number of lines. example
        "'red','pink','blue','cyan','green','yellow'" or
        "rgb(255/255,0/255,0/255),rgb(255/255,0/255,255/255),rgb(0/255,0/255,255/255),
        rgb(0/255,255/255,255/255),rgb(0/255,255/255,0/255),rgb(255/255,255/255,0/255)"${txtrst}]

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
    -w  The width of output picture.[${txtred}Default auto calculate${txtrst}]
    -u  The height of output picture.[${txtred}Default auto calculate${txtrst}]
    -P  [Uppercase P] Legend position[${txtred}Default right. Accept
        top,bottom,left,none, or c(0.08,0.8) (relative to left-bottom).${txtrst}]
    -r  The resolution of output picture.[${txtred}Default 300 ppi${txtrst}]
    -E  The format of output figures. [${txtred}Default pdf, accept
        eps/ps, tex (pictex), pdf, jpeg, tiff, bmp, svg and wmf)${txtrst}]
    -T  The self-definited theme for ggplot2, give the followding theme_classic2 [Default], theme_classic3, theme_cin.${txtrst}

<other ggplot code>
    -p  [Lowercase p] Other legal R codes for gggplot2 will be given here.
        [${txtres}Begin with '+' ${txtrst}]

<output>
    -e  Execute or not[${bldred}Default TRUE${txtrst}]
    -o  Path of output.[Default path of data file(-f), Optinal]

<self-lineplot>
<<add point>>
    -W  Whether add point. [Default FALSE]
    -Y  Point shape. [Default 17]. you can giving a series of number to set -Y. Only be used when -W TRUE and (-F and -X) do not set.

<<line type, size and level>>
    -B  line size. [${txtred}Default 0.5. Accept a number.${txtrst}]
    -X  Name for linetype variable. [Default NULL, if you give, meaning linetype legend]
    -F  Levels for linetype variable
        ${txtred}Default data order,accept a string like
        "'a','b'". This will only be considered when -X is not NULL. ***for <Set> column***.${txtrst}]

<<smooth>>
    -J  Whether using smooth lines replace normal line. [Default FALSE]
    -k  The smooth method you want to use. Only be used when -J TRUE.
        [${txtred} Default auto. eg. lm, glm, gam, loess,rlm.
        For datasets with n < 1000 default is 'loess'.
        For datasets with 1000 or more observations defaults to 'gam'.${txtrst}]

<<error bar>>
    -Z  Whether show error-bar. Default FALSE. Be used when -I dodge and -D identity.
    -j  If show error-bar is TRUE, giving the Name for error-bar column in data file(sd value).[optinal]
        Specify the column containing sd value.
        should be set when -m TRUE, -I dodge and -D identity.

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

EOF
}
#input
file=
sampleGroup='FALSE'
melted='FALSE'
header='TRUE'
gene='FALSE'
skip=1

#aes
xvariable=''
value=''
variable=''
x_level=""
color_level=""
x_type='TRUE'

#labs
title=''
xlab=' '
ylab=' '

#scales
xtics_angle=0
scaleY='FALSE'
y_add=0
color_v='FALSE'

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


#other ggplot code
par=''

#output
execute='TRUE'
output=''


#self-line

## add point
point='FALSE'
point_shape=17

##line type, size and level
line_size=0.5
linetype='aAbBcC'
linetype_level=""

##smooth
smooth='FALSE'
smooth_method='auto'

##error bar
error_bar='FALSE'
error_bar_coloumn='ctCTct'

##xtics limit and label
xtics_limits='FALSE'
xtics_labels='FALSE'
xtics_scale='discrete'

##vhline
vhline_color='black'
linetype_vh='dotted'
vline='FALSE'
hline='FALSE'

## width of each sub-errorbar
width=0


while getopts "hf:m:Q:z:q:H:a:d:c:L:l:A:t:x:y:b:s:S:W:C:G:g:M:R:w:u:P:r:E:T:p:e:o:W:Y:B:X:F:J:k:Z:j:U:K:v:V:N:O:" OPTION
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
            xvariable=$OPTARG
            ;;
        d)
            value=$OPTARG
            ;;
        c)
            variable=$OPTARG
            ;;
        L)
            x_level=$OPTARG
            ;;
        l)
            color_level=$OPTARG
            ;;
        A)
            x_type=$OPTARG
            ;;
        #labs
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
            color_v=$OPTARG
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
            res=$OPTARG
            ;;
        E)
            ext=$OPTARG
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
        #self_line
        ##add point
        W)
            point=$OPTARG
            ;;
        Y)
            point_shape=$OPTARG
            ;;
        ##line type, size and level
        B)
            line_size=$OPTARG
            ;;
        X)
            linetype=$OPTARG
            ;;
        F)
            linetype_level=$OPTARG
            ;;
        ##smooth
        J)
            smooth=$OPTARG
            ;;
        k)
            smooth_method=$OPTARG
            ;;
        ##error bar
        Z)
            error_bar=$OPTARG
            ;;
        j)
            error_bar_coloumn=$OPTARG
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
    if [ -z $xvariable ]; then
        xvariable="variable"
        # echo "Warning, when you give data(-f) file is unmelted, and you don't set -a, so there is set -a variable."
    fi
    if [ -z $value ]; then
        value="value"
        # echo "Warning, when you give data(-f) file is unmelted, and you don't set -d, so there is set -d value."
    fi
fi

if test "$melted" == "FALSE" && test "$gene" != "FALSE"; then
    if test "$xvariable" == "variable";then
        xvariable="Sample"
    fi
fi

if test "$melted" == "TRUE"  && test "$xvariable" == ""; then
    echo "If '-m TRUE' be set, -a should be set"
    exit 1
fi

if test "$melted" == "TRUE" && test "$value" == ""; then
    echo "If '-m TRUE' be set, -d should be set"
    exit 1
fi

if test -z "$variable"; then
    variable=$xvariable
fi

if test "$xlab" == " ";then
    xlab=$xvariable
fi

if test "$ylab" == " ";then
    ylab=$value
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

### point shape
if test $point_shape -ne 17;then
    if test "$point" == "FALSE" ;then
        echo "Warning, -W (point) FALSE .We will ignore -Y(point shape)"
    fi

    if test "$linetype" != "aAbBcC" || test "$variable" != "$xvariable";then
        echo "Warning, -F or -X be set, so will ignore -Y(point shape)"
    fi
fi

##### linetype level
if test "$linetype" == "aAbBcC" && test "$linetype_level" != "";then
    echo "Warning, you do not set -X(linetype), so -c(linetype level) will be ignore"
    linetype_level=""
fi

### check width of each sub-errorbar
if test "$error_bar" != "FALSE";then
    width=0.3
fi

## Variable inspection

## Prefix of picture file
mid=$mid'.line'

if test "$melted" == 'TRUE' || (test "$melted" != 'TRUE' && test "$xvariable" != 'variable'); then
    if test "$xvariable" != "$variable";then
        mid=$mid'.x_'$xvariable'.y_'$value'.color_'$variable
    else
        mid=$mid'.x_'$xvariable'.y_'$value
    fi
fi

if test "$smooth" == 'TRUE'; then
    mid=$mid'.smooth'
fi

if test "$error_bar" != "FALSE"; then
    mid=$mid'.errorBar'
fi

if test "$point" != 'FALSE';then
    mid=$mid'.addPoint_'$variable
fi

if test "$linetype" != 'aAbBcC';then
    mid=$mid'.linetype_'$linetype
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

if test "$scaleY" != "FALSE"; then
    mid=$mid'.scaleY_'$scaleY
fi

## Prefix of picture file

if test -z "$output";then
    output=$(cd `dirname $file`;pwd)
fi

cat <<END >$output/$(basename $file)${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

library(ggplot2)
library(reshape2)
library(grid)
library(dplyr)
library(naturalsort)
# library(ggpubr)

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
        if ("${xvariable}" == "Sample") {
            print("Wainning: Because per x-axis tag contains only one data, so recommend you to use the scatterplot or lines script")
        }
    }else {
        data <- read.table(file="${file}", sep="\t", header=$header, quote="", check.names=F)
        data_m <- melt(data)

        if ("${sampleGroup}" != "FALSE") {
            data_m <-left_join(data_m,sampleGroup,by="variable")
        }
    }
    data_m <- data_m[data_m\$$value !=0,]

} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="")
}


###########                  read file                 ###########

###########                   scales                   ###########

if (${y_add} != 0){
    data_m\$${value} <- data_m\$${value} + ${y_add}
}

if("$scaleY" !="FALSE"){
    data_m\$${value} <- $scaleY(data_m\$${value})
}

###########                   scales                   ###########

###########  level of x-axis, color, linetype and facet variable ###########

### x_val levels
if(${x_type}){
    x_level <- c(${x_level})
    if (length(x_level)>1){
        data_m\$${xvariable} <- factor(data_m\$${xvariable},levels=x_level)
    }else{
        # data_m\$${xvariable} <- ordered(data_m\$${xvariable}, levels=naturalsort(unique(data_m\$${xvariable})))
        data_m\$${xvariable} <- factor(data_m\$${xvariable})
    }
}else{
    data_m\$${xvariable} <- as.numeric(data_m\$${xvariable})
}

### color levels
level_i <- c(${color_level})
if (length(level_i) >1){
    data_m\$${variable} <- factor(data_m\$${variable}, levels=level_i)
}else{
    data_m\$${variable} <- factor(data_m\$${variable})
}

### linetype levels
level_linetype <- c(${linetype_level})
if("$linetype" != "aAbBcC"){
    if (length(level_linetype) >1){
        data_m\$${linetype} <- factor(data_m\$${linetype}, levels=level_linetype)
    }else{
        data_m\$${linetype} <- factor(data_m\$${linetype})
    }
}

facet_level <- c(${facet_level})
if ("$facet" != "NoMeAnInGTh_I_n_G_s"){
    if (length(facet_level) >1){
        data_m\$${facet} <- factor(data_m\$${facet}, levels=facet_level, ordered=T)
    }else{
        data_m\$${facet} <- factor(data_m\$${facet})
    }
}

### value columns
data_m\$$value <- as.numeric(as.character(data_m\$$value))
data_m <- data_m[!is.na(data_m\$$value),]

###########  level of x-axis, color, linetype and facet variable ###########


############   calculate mean and sd about data_m    ############

s_mean_sd <- function(dat,group_va){
    mean_sd_dat <- dat %>% group_by_(.dots=group_va) %>% dplyr::summarise(sd=sd(${value}),${value}=mean(${value}))
    return(mean_sd_dat)
}

if ("${error_bar_coloumn}" == "ctCTct"){
    if ("$linetype" != "aAbBcC"){
        if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}","${linetype}","${facet}"))
        }else{
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}","${linetype}"))
        }
    }else{
        if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}","${facet}"))
        }else{
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}"))
        }
    }

}else if("${error_bar_coloumn}" != "ctCTct" && "${error_bar}" != "FALSE"){
    colnames(data_m)[which(colnames(data_m) =="${error_bar_coloumn}")] ='sd'
}

############   calculate mean and sd about data_m    ############

if(!${x_type}){
    data_m\$${xvariable} <- as.numeric(as.character(data_m\$${xvariable}))
}

###########                geom layer                  ###########
## basic
if("$xvariable" == "$variable"){
    p <- ggplot(data_m, aes($xvariable, $value))
}else{
    p <- ggplot(data_m, aes($xvariable, $value, color=${variable}))
}
p <- p + xlab("${xlab}") + ylab("${ylab}")+labs(title="${title}")


## layer

pd <- position_dodge($width)

## error_bar
if("${error_bar}" != "FALSE"){
    if("$xvariable" == "$variable"){
        p <- p + geom_errorbar(aes(ymin=${value}-sd,ymax=${value}+sd, group=1), colour="black", width=0.2, position=pd)
    }else{
        p <- p + geom_errorbar(aes(ymin=${value}-sd,ymax=${value}+sd, group=${variable}), colour="black", width=0.2, position=pd)
    }
}

## line
if (${smooth}){
    if("$linetype" == 'aAbBcC'){
        if("$xvariable" == "$variable"){
            p <- p + geom_smooth(aes(group=1),method="${smooth_method}",se=FALSE,size=${line_size},position=pd)
        }else{
            p <- p + geom_smooth(aes(group=${variable}),method="${smooth_method}",se=FALSE,size=${line_size},position=pd)
        }
    }else{
        if("$xvariable" == "$variable"){
            p <- p + geom_smooth(aes(linetype=${linetype},group=${linetype}),method="${smooth_method}",se=FALSE,size=${line_size},position=pd)
        }else{
            p <- p + geom_smooth(aes(linetype=${linetype},group=interaction(${variable},${linetype})),method="${smooth_method}",se=FALSE,size=${line_size},position=pd)
        }
    }
}else{
    if("$linetype" == 'aAbBcC'){
        if("$xvariable" == "$variable" ){
            p <- p + geom_line(aes(group=1),size=${line_size},position=pd)
        }else{
            p <- p + geom_line(aes(group=${variable}),size=${line_size},position=pd)
        }
    }else{
        if("$xvariable" == "$variable" ){
            p <- p + geom_line(aes(linetype=${linetype},group=${linetype}),size=${line_size},position=pd)
        }else{
            p <- p + geom_line(aes(linetype=${linetype},group=interaction(${variable},${linetype})),size=${line_size},position=pd)
        }
    }
}

## Whether add point
if("$point" != "FALSE"){
    if("$linetype" == 'aAbBcC'){
        if("$xvariable" == "$variable"){
            p <- p+geom_point(shape=${point_shape},position=pd)
        }else{
            p <- p+geom_point(aes(shape=${variable}),position=pd)
        }
    }else{
        if("$xvariable" == "$variable"){
            p <- p+geom_point(aes(shape=${linetype}),position=pd)
        }else{
            p <- p+geom_point(aes(shape=${variable}),position=pd)
        }
    }
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

# else{
#   if("$xtics_limits" != "FALSE" && "$xtics_labels" != "FALSE"){
#       p <- p + scale_x_discrete(limits=xtics_limit, labels=xtics_label)
#   }else if("$xtics_limits" != "FALSE" && "$xtics_labels" == "FALSE"){
#       p <- p + scale_x_discrete(limits=xtics_limit)
#   }else if("$xtics_limits" == "FALSE" && "$xtics_labels" != "FALSE"){
#       p <- p + scale_x_discrete(labels=xtics_label)
#   }
# }


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

### manunal change color
if("${color_v}" != "FALSE"){
    p <- p + scale_color_manual(values=c(${color_v}))
}

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

###########                geom layer                ###########

###########                  output                  ###########

#Correcting location of x-aixs label
p <- Xlable_angle_correct(p, ${xtics_angle})

#Set the position of legend
p <- LegendPos(p, "${legend_pos}")

#control margin of plot
p <- PLot_margin(p, '$title', '$xlab', '$ylab')

#plot width and height auto calculate
w_h_auto <-Plot_size_final(length(unique(data_m\$${xvariable})),"${rotate_plot}","$legend_pos","${variable}", "${xvariable}", "${facet}", "${facet_direction}", nlevels(data_m\$${facet}),plottype="line")
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

