#!/bin/bash

# set -x

usage()
{
cat <<EOF
${txtcyn}

***CREATED BY Chen Tong (chentong_biology@163.com)***
***MODIFIED BY Lin Dechun (lindechun@genomics.com)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This script is used to draw bar plot using ggplot2.

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
    -q  Giving one gene ID to do barplot specifically for this gene.(only one row in data file)
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
    -c  Name for color variable.
        If no-subclass of X-variavle(-a), this will be used X-axis variable.
        ${bldred}[Default "variable" represents the column named "variable".
        This parameter can only be set when -m is TRUE or -Q be seted.]${txtrst}
    -W  Name for alpha-legend column, meaning alpha legend variable.[Default: FALSE]. When you set -W is not same with -a and -F, -a must have only one Properties.
    -L  Levels for x-axis variable.
        [${txtred}Default data order,accept a string like
        "'g','a','j','x','s','c','o','u'". This will only be considered when -A is TRUE. ***for <Set> column***.${txtrst}]
    -l  Levels for color-legend variable.
        [${txtred}Default columns order,accept a string like
        "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.[Default legend_pos for <xvariable> column].${txtrst}]
    -k  Levels for alpha-legend variable. Default columns order,accept a string like "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.[Default NULL. Only be used when -W be set
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
    -C  Manually set colors for each bar. Default FALSE, meaning using ggplot2 default Color for each bar.
        [${txtred}Or, you can giving str format, 
        ususlly the number of colors should
        be equal to the number of lines. example
        "'red','pink','blue','cyan','green','yellow'" or
        "rgb(255/255,0/255,0/255),rgb(255/255,0/255,255/255),rgb(0/255,0/255,255/255),
        rgb(0/255,255/255,255/255),rgb(0/255,255/255,0/255),rgb(255/255,255/255,0/255)"
        ${txtrst}]

<facet>
    -G  facet grid plots by given column. This is used to put multiple plot
        in one picture. Used when -m TRUE or -Q be seted, normally a string <set>
        should be suitable for this parameter.
    -g  The levels of wrapping to set the order of each group.
        ${txtred}Normally the unique value of the column given to B in
        a format like <"'a','b','c','d'">.${txtrst}
    -M  The direction of facet when -G is used. give the following h (horizontal,Default), v (vertical).

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
    -E  The format of output figures.[${txtred}Default pdf, accept
        eps/ps, tex (pictex), pdf, jpeg, tiff, bmp, svg and wmf)${txtrst}]
    -T  The self-definited theme for ggplot2, give the followding,theme_classic2[Default], theme_classic3, theme_cin, theme_bar.${txtrst}

<other ggplot code>
    -p  [Lowercase p] Other legal R codes for gggplot2 will be given here.
        [${txtres}Begin with '+' ${txtrst}]

<output>
    -e  Execute or not[${bldred}Default TRUE${txtrst}]
    -o  Path of output.[Default path of data file(-f), Optinal]

<self-barplot>
<<sort value>>
    -X  Whether sort the value. FALSE [Default] or you can giving the following '-'(dscending) or '+'(ascending) order. '-' and '+' Only be used when -A TURE and -L no set.
    -Y  Whether sort inside each group. Default FALSE. Only be used when -a xvariable have only one Property, -A TRUE and -X TRUE.

<<width of sub_bar>>
    -K  The width of bar.[${txtred}Default 0.75${txtrst}]. Only be used when -A TRUE

<<Position of each bar>>
    -D  The ways to show the height of bars.
        The height of bars represent the numerical values in each group
        by default('identity')(normally in value column of melted data). 
        One can also give 'count' to let the program count the number of 
        items in each group (Normally the 'variable' column is used to group 
        'xvariable' colum after melt).
        if you give 'count', will ignore -d.
        [${txtred}Default identity, accept count when categorial data
        are given. ${txtrst}]
    -I  The ways to place multiple bars for one group if there are. 
        Multiple bars in same place will be stacked together by
        default.
        Giving "fill" to get stacked percent barplot.
        Giving "dodge" to arrange multiple bars side-by-side. 
        [${txtred}Default dodge, accept stack, fill.${txtrst}]

<<error bar>>
    -Z  Whether show error-bar. Default FALSE. Be used when -I dodge and -D identity.
    -j  If show error-bar is TRUE, giving the Name for error-bar column in data file(sd value)[optinal].
        Specify the column containing sd value.
        should be set when -m TRUE, -I dodge and -D identity.

<<text in bar>>
    -U  Whether show text(mean) at each bar. Default FALSE.

<<vhline>>
    -v  Add vertical lines. [Default FALSE, accept a series of
        numbers in following format '1,2,3,4,5' or 1].
    -V  Add  horizontal lines. [Default FALSE, accept a series of
        numbers in following format '1,2,3,4,5' or 1].
    -N  Color for vline or hline. [Default black]
    -O  linetype for vline or hline. Only be used when -v or -V is seted.give the followding dashed[Default], solid, dotted, dotdash, longdash, twodash.${txtrst}
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
alpha_var='aAbBcC'

x_level=""
color_level=""
alpha_level=""
color_v='FALSE'
x_type='TRUE'

#labs
title=''
xlab=' '
ylab=' '

#scales
xtics_angle=0
scaleY='FALSE'
y_add=0

#facet
facet='NoMeAnInGTh_I_n_G_s'
facet_direction='h'
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

#self-barplot

##sort value
sort_var='FALSE'
sort_by_group='FALSE'

##width of sub_bar
sub_bar=0.75

## Position of each bar
stat='identity'
position='dodge'

##error bar
error_bar='FALSE'
error_bar_coloumn='ctCTct'

##text in bar
show_text='FALSE'

##vhline
vhline_color='black'
linetype_vh='dotted'
vline='FALSE'
hline='FALSE'


while getopts "hf:m:Q:z:q:H:a:d:c:W:L:l:k:A:t:x:y:b:s:S:C:G:g:M:R:w:u:P:r:E:T:p:e:o:X:Y:K:D:I:Z:j:U:v:V:N:O:" OPTION
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
        W)
            alpha_var=$OPTARG
            ;;
        L)
            x_level=$OPTARG
            ;;
        l)
            color_level=$OPTARG
            ;;
        k)
            alpha_level=$OPTARG
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
        #self-boxplot
        ##sort value
        X)
            sort_var=$OPTARG
            ;;
        Y)
            sort_by_group=$OPTARG
            ;;
        ##width of sub-bar
        K)
            sub_bar=$OPTARG
            ;;
        ## Position of each bar
        D)
            stat=$OPTARG
            ;;
        I)
            position=$OPTARG
            ;;
        ##error bar
        Z)
            error_bar=$OPTARG
            ;;
        j)
            error_bar_coloumn=$OPTARG
            ;;
        ##text in bar
        U)
            show_text=$OPTARG
            ;;
        ## vhline
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

if [ -z $file ]; then
    usage
    exit 1
fi

## Variable inspection

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

if test "$melted" == "TRUE" && test "$xvariable" == ""; then
    echo "If '-m TRUE' be set, -a should be set"
    exit 1
fi

if test "$melted" == "TRUE" && test "$stat" != "count" && test "$value" == ""; then
    echo "If -m TRUE and -D identity be set, -d should be set"
    exit 1
fi

######################### sort value ################################
### sort value
if test "$x_level" != '' && test "$sort_var" != "";then
    echo "Warning: you have set -L(x-axis level), so we will ignore -X (sort value)"
fi
### sort by group
if test "$sort_by_group" != "FALSE" && test "$sort_var" != "FALSE" && test "$xvariable" == "$variable";then
    echo " You set -X"${sort_var}", but -a is same with -c, so we ignore -Y TRUE"
    sort_by_group='FALSE'
fi
######################### sort value ################################

######################### error bar ################################
if test "$stat" != "identity" || test "$position" == "fill";then
    if test "$error_bar_coloumn" != 'ctCTct' || test "$error_bar" != "FALSE"; then
        echo "Warining, stat: count or position: fill be used. We will ignore -j or -Z and not show error bar"
        error_bar_coloumn="ctCTct"
        error_bar="FALSE"
    fi
fi

if test "$melted" == "TRUE"; then
    if test "$error_bar" == "FALSE" && test "$error_bar_coloumn" != 'ctCTct';then
        echo "Error, Beauce this data file is melted, please set -Z TRUE or don't set -j. We will exit."
        exit 1
    fi
    if test "$error_bar" != "FALSE" && test "$error_bar_coloumn" != 'ctCTct';then
        echo "Hint: if you data file already calcualte mean and sd, pleace set -j"
    fi
fi

if test "$melted" == "FALSE"  && test "$error_bar_coloumn" != 'ctCTct' && "$stat" == "identity"; then
    echo "Warining, data is unmelted, so don't need to set -j. We will ignore this and show error bar."
    error_bar_coloumn="ctCTct"
fi
######################### error bar ################################

if test "$position" == "fill" && test "$scaleY" != 'FALSE'; then
    echo "Warining, -s(scaleY) only be used when -I(position) is not fill. We will ignore -I."
fi

if test -z "$variable"; then
    variable=$xvariable
fi

if test "$xlab" == " ";then
    xlab=$xvariable
fi

if test "$ylab" == " ";then
    if test "${stat}" == "identity";then
        ylab=$value
    else
        ylab="Number"
    fi
fi

if test "$x_type" == "FALSE"; then
    sub_bar='NULL'
fi

if test "$position" == "fill" && test "$xvariable" == "$variable"; then
    if test "$alpha_var" == "$variable" || test "$alpha_var" == "$xvariable" || test "$alpha_var" == "aAbBcC";then
        echo "Eroor: Because you set -I fill, but do not set -F that is different with -a or do not set -W that is different with -a and -F. so the grapha is no significant. please re-run script after add other paraments."
        exit 1
    fi
fi
## Variable inspection

## Prefix of picture file
mid=$mid'.barplot'

if test "$melted" == 'TRUE' || (test "$melted" != 'TRUE' && test "$xvariable" != 'variable'); then
    if test "$stat" == "identity";then
        if test "$xvariable" != "$variable";then
            mid=$mid'.x_'$xvariable'.y_'$value'.color_'$variable
        else
            mid=$mid'.x_'$xvariable'.y_'$value
        fi
    else
        if test "$xvariable" != "$variable";then
            mid=$mid'.x_'$xvariable".color_"$variable
        else
            mid=$mid'.x_'$xvariable
        fi
    fi
fi

mid=$mid'.'$stat'.'$position

if test "$error_bar" != "FALSE" && test "$stat" == "identity"; then
    mid=$mid'.errorBar'
fi

if test "$show_text" != "FALSE"; then
    mid=$mid'.show_text'
fi

if test "$vline" != "FALSE"; then
    mid=$mid'.vline'
fi

if test "$hline" != "FALSE"; then
    mid=$mid'.hline'
fi

if test "$alpha_var" != 'aAbBcC';then
    mid=$mid'.alpha_'$alpha_var
fi

if test "$facet" != "NoMeAnInGTh_I_n_G_s"; then
    mid=$mid'.facet_'$facet
fi

if test "${sort_var}" != 'FALSE';then
    mid=$mid'.sort_values'
fi

if test "$melted" == "FALSE" && test "$gene" == "FALSE" ; then
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
library(plyr)
library(dplyr)
library(reshape2)
library(grid)
library(naturalsort)

###########                  read file                ###########

if ("${sampleGroup}" != "FALSE") {
        sampleGroup <- read.table("${sampleGroup}",sep="\t",header=1,check.names=F,row.names=1)
        sampleGroup\$variable<- factor(rownames(sampleGroup))
}

if(! $melted){
    if ("${gene}" != "FALSE") {

        # data <- read.table(file="${file}", sep="\t", header=$header, row.names=1, quote="", check.names=F)
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

        if ("$xvariable" %in% colnames(data) & "$x_type" == 'TRUE'){
            data\$$xvariable <- ordered(data\$$xvariable,unique(as.character(data\$$xvariable)))
        }

        if ("$variable" %in% colnames(data)){
            data\$$variable <- ordered(data\$$variable,unique(as.character(data\$$variable)))
        }

        data_m <- melt(data)
        # data_m\$variable <- ordered(data_m\$variable,rev(levels(data_m\$variable)))

        if ("${sampleGroup}" != "FALSE") {
            data_m <-left_join(data_m,sampleGroup,by="variable")
        }
    }
} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="")
}

###########                  read file                 ###########

if (${y_add} != 0){
    data_m\$${value} <- data_m\$${value} + ${y_add}
}
if("$scaleY" !="FALSE"){
    data_m\$${value} <- $scaleY(data_m\$${value})
}


###########  level of x-axis, legend and tag of facet  ###########
if(${x_type}){
    x_level <- c(${x_level})
    if (length(x_level)>1){
        data_m\$${xvariable} <- factor(data_m\$${xvariable},levels=x_level)
    }else{
        # data_m\$${xvariable} <- ordered(data_m\$${xvariable}, levels=naturalsort(unique(data_m\$${xvariable})))

        data_m\$$xvariable <- ordered(data_m\$$xvariable,unique(as.character(data_m\$$xvariable)))
    }
}else{
    data_m\$${xvariable} <- as.numeric(data_m\$${xvariable})
}

level_i <- c(${color_level})
if (length(level_i) >1){
    data_m\$${variable} <- factor(data_m\$${variable}, levels=level_i)
}else{
    data_m\$${variable} <- factor(data_m\$${variable})
}

alpha_level <- c(${alpha_level})
if (length(alpha_level)>1){
    data_m\$${alpha_var} <- factor(data_m\$${alpha_var},levels=alpha_level)
}

if("$facet" !="NoMeAnInGTh_I_n_G_s"){
    if ("${facet_level}" != "NA") {
        facet_level <- c(${facet_level})
        data_m\$${facet} <- factor(data_m\$${facet},
        levels=facet_level, ordered=T)
    }else{
        data_m\$${facet} <- factor(data_m\$${facet})
    }
}

### value columns
data_m\$$value <- as.numeric(as.character(data_m\$$value))
data_m <- data_m[!is.na(data_m\$$value),]

### remove zero
data_m <- data_m[data_m\$$value !=0,]

###########  level of x-axis, legend and tag of facet  ###########

###########   calculate mean and sd about data_m    ###########

s_mean_sd <- function(dat,group_va, stat="identity"){
    if (stat == "identity"){
        mean_sd_dat <- dat %>% group_by_(.dots=group_va) %>% dplyr::summarise(sd=sd(${value}),${value}=mean(${value}))
    }else{
        mean_sd_dat <- dat %>% group_by_(.dots=group_va) %>% dplyr::summarise(${value} = n())
    }
    return(mean_sd_dat)
}

s_mean_sd_stack <- function(dat,group_va){
    mean_sd_dat <- dat %>% group_by_(.dots=group_va) %>% dplyr::summarise(sd=sd(${value}),${value}=mean(${value}))

    mean_sd_dat1 <- mean_sd_dat %>% dplyr::group_by_(.dots=c("${xvariable}")) %>% dplyr::summarise(${value}=sum(${value}))
    temp_error<- dat %>% dplyr::group_by_(.dots=c("${xvariable}")) %>% dplyr::summarise(sd=sd(${value}),${value}=mean(${value}))
    mean_sd_dat1\$sd <- temp_error\$sd

    return(mean_sd_dat1)
}

if ("${error_bar_coloumn}" == "ctCTct"){

    data_m_alpha <- unique(data_m[,c('$xvariable','$variable')])

    if("${alpha_var}" != 'aAbBcC' & sum(xtabs(~${xvariable}+$variable,data_m_alpha))/length(unique(data_m_alpha\$$xvariable)) == 1 & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
        if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
            data_m_temp <- data_m
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}","$alpha_var","${facet}"),"$stat")

            if ("${position}" == "stack" & "${facet}" !="${variable}" & "$stat" == "identity"){
                mean_sd_dat <- s_mean_sd_stack(data_m_temp,c("${variable}","${xvariable}","$alpha_var","${facet}"))
            }
        }else{
            data_m_temp <- data_m
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}","$alpha_var"),"$stat")
            if ("${position}" == "stack" & "$stat" == "identity"){
                mean_sd_dat <- s_mean_sd_stack(data_m_temp,c("${variable}","${xvariable}","$alpha_var"))
            }
        }
    }else{
        if("$alpha_var" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
            stop("Error: because each $xvariable do not corresponds to only one $variable and $alpha_var is not same with $variable, so igeore alpha legend setting")
        }
        if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
            data_m_temp <- data_m
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}","${facet}"))

            if ("${position}" == "stack" && "${facet}" !="${variable}"){
                mean_sd_dat <- s_mean_sd_stack(data_m_temp,c("${variable}","${xvariable}","${facet}"))
            }

        }else{
            data_m_temp <- data_m
            data_m <- s_mean_sd(data_m,c("${variable}","${xvariable}"),"$stat")

            if ("${position}" == "stack" & "$stat" == "identity"){
                mean_sd_dat <- s_mean_sd_stack(data_m_temp,c("${variable}","${xvariable}"))
            }
        }
    }
}else if("${error_bar_coloumn}" != "ctCTct" && "${error_bar}" != "FALSE"){
    colnames(data_m)[which(colnames(data_m) =="${error_bar_coloumn}")] ='sd'
}


###########   calculate sd(error bar) about barplot    ###########

###########                 sort value                ###########

sortValueBySubgroup <- function(dat,var1,var2,var3,type='stack',sortGroup=F,alpha_levels=NULL){

    #### select xlevel
    if (type == 'dodge'){
        data_m_sort_temp <- dat %>% dplyr::group_by_(.dots=c(var1)) %>% dplyr::summarise_(maxValue=paste('max(',var2,')',sep="")) %>% dplyr::arrange_("${sort_var}maxValue")
        data_m_sort_temp1 <- dat %>% dplyr::group_by_(.dots=c(var3)) %>% dplyr::summarise_(countT=paste('sum(',var2,')',sep="")) %>% dplyr::arrange_("${sort_var}countT")
        # data_m_sort_temp1 <- dat %>% dplyr::group_by_(.dots=c(var3)) %>% dplyr::summarise_(countT=paste('sum(',var2,')',sep="")) %>% dplyr::arrange_("countT")

        if(sortGroup){
            return(unique(as.character(data_m_sort_temp[,var1][[1]])))
        }else{
            return(list(xa=unique(as.character(data_m_sort_temp[,var1][[1]])),a=unique(as.character(data_m_sort_temp1[,var3][[1]]))))
        }

    } else if( type == "stack"){
        # data_m_sort_temp1 <- dat %>% dplyr::group_by_(.dots=c(var3)) %>% dplyr::summarise_(countT=paste('sum(',var2,')',sep="")) %>% dplyr::arrange_("${sort_var}countT")
        data_m_sort_temp1 <- dat %>% dplyr::group_by_(.dots=c(var3)) %>% dplyr::summarise_(countT=paste('sum(',var2,')',sep="")) %>% dplyr::arrange_("countT")

        ##等高柱子的相对顺序
        data_m_sort_temp2 <- dat %>% dplyr::arrange_(paste(var2,sep="\t"))
        if ("$sort_var" == '+'){
            temp_var1_sort=rev(unique(as.character(data_m_sort_temp2[,var1][[1]])))
        }else{
            temp_var1_sort=unique(as.character(data_m_sort_temp2[,var1][[1]]))
        }
        dat[[var1]] <- ordered(dat[[var1]],levels=temp_var1_sort)
        ##等高柱子的相对顺序

        data_m_sort_temp <- dat %>% dplyr::group_by_(.dots=c(var1)) %>% dplyr::summarise_(countT=paste('sum(',var2,')',sep="")) %>% dplyr::arrange_("${sort_var}countT")

        if(sortGroup){
            return(unique(as.character(data_m_sort_temp[,var1][[1]])))
        }else{
            return(list(xa=unique(as.character(data_m_sort_temp[,var1][[1]])),a=unique(as.character(data_m_sort_temp1[,var3][[1]]))))
        }

    } else{

        cal_legend_order <- function(dat,var1,var3,alpha=NULL){

            data_m_sort_temp <- dat %>% dplyr::group_by_(.dots=c(var1)) %>% dplyr::mutate_(countT=paste('sum(',var2,')',sep="")) %>% dplyr::group_by_(.dots=c(var3),add=TRUE) %>% dplyr::mutate_(percent=paste('round(100*',var2,'/countT,2)',sep=""))

            sort_var3 <- data_m_sort_temp %>% dplyr::group_by_(.dots=c(var3)) %>% dplyr::summarise(all=sum(percent)) %>% dplyr::arrange(-all)
            sort_var3 <- sort_var3[,var3][[1]]

            if (is.null(alpha)){
                return_sort <- rev(sort_var3)
            }else{
                alpha <- alpha[!is.na(match(alpha,sort_var3))]
                sort_var3<- alpha
                return_sort <- rev(alpha)
            }

            ###1
            data_m_all <- expand.grid(var1=unique(as.character(data_m_sort_temp[,var1][[1]])), var3=unique(as.character(data_m_sort_temp[,var3][[1]]))) %>% data.frame
            colnames(data_m_all) <- c(var1,var3)
            data_m_all <- data_m_all %>% left_join(data_m_sort_temp)
            ###1

            d_te1 <- data_m_all %>% dplyr::filter_(paste(var3,'==','"',sort_var3[1],'"',sep="")) %>% dplyr::arrange_(var3,"${sort_var}percent")

            d_te2 <- data_m_all %>% dplyr::filter_(paste(var3,'!=','"',sort_var3[1],'"',sep=""))

            temp_xaxis_sort <- d_te1[!is.na(d_te1\$percent),][,var1]

            anything_remain <- d_te2[d_te2[,var1] %in% d_te1[is.na(d_te1\$percent),][,var1],]

            if(sortGroup & "$sort_var" == "+"){
                return(list(x=rev(as.character(temp_xaxis_sort)),y=anything_remain,z=return_sort))
            }else{
                return(list(x=as.character(temp_xaxis_sort),y=anything_remain,z=return_sort))
            }
        }


        xaxis_sort <- c()

        xy <- cal_legend_order(dat,var1,var3,alpha_levels)
        xaxis_sort <- c(xaxis_sort, xy\$x)

        result_sort_var3 <- xy\$z

        if(!nrow(xy\$y)){
            xy <- cal_legend_order(xy\$y,var1,var3,alpha_levels)
            xaxis_sort <- c(xaxis_sort, xy\$x)
        }

        if(sortGroup){
            return(xaxis_sort)
        }else{
            return(list(xa=xaxis_sort, a=result_sort_var3))
        }
    }
}

##### sort value
if (${x_type} & "${sort_var}" !='FALSE'){
    data_m_alpha <- unique(data_m[,c('$xvariable','$variable')])

    if("$position" != "fill"){
        ## position: stack and dodge

        if ( $sort_by_group & "${xvariable}" != "$variable"){

            if (sum(xtabs(~${xvariable}+$variable,data_m_alpha))/length(unique(data_m_alpha\$$xvariable)) == 1){

                if ("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){

                    ##### xvariable
                    temp_xva <- c()

                    data_m_sort_temp10 <- data_m %>% dplyr::group_by($variable) %>% dplyr::summarise(mean1=mean($value)) %>% dplyr::arrange(${sort_var}mean1)

                    # for (i in levels(data_m\$${variable})){
                    for (i in unique(as.character(data_m_sort_temp10\$${variable}))){
                        temp_d<- dplyr::filter(data_m, $variable == i)
                        temp_d\$$variable <- droplevels(temp_d\$$variable)
                        temp_d\$$xvariable <- droplevels(temp_d\$$xvariable)
                        temp_d\$$alpha_var <- droplevels(temp_d\$$alpha_var)

                        sort_results <- sortValueBySubgroup(temp_d,"$xvariable","$value","$alpha_var",type='$position',sortGroup=T)
                        temp_xva <- c(temp_xva,sort_results)

                    }
                    data_m\$$xvariable <-ordered(data_m\$$xvariable,levels=temp_xva)
                    
                    ##### alpha_var
                    if("$position" == "dodge"){
                        data_m_sort_temp1 <- data_m %>% dplyr::group_by($alpha_var) %>% dplyr::summarise(countT=sum($value)) %>% dplyr::arrange(${sort_var}countT)
                    }else{
                        data_m_sort_temp1 <- data_m %>% dplyr::group_by($alpha_var) %>% dplyr::summarise(countT=sum($value)) %>% dplyr::arrange(countT)
                    }

                    data_m\$$alpha_var <-ordered(data_m\$$alpha_var,levels=unique(as.character(data_m_sort_temp1\$${alpha_var})))

                }else{
                    data_m_sort_temp10 <- data_m %>% dplyr::group_by($variable) %>% dplyr::summarise(mean1=mean($value)) %>% dplyr::arrange(${sort_var}mean1)
                    data_m\$$variable <- ordered(data_m\$$variable,unique(as.character(data_m_sort_temp10\$${variable})))

                    data_m <- dplyr::arrange(data_m, ${variable}, ${sort_var}${value})

                    # data_m <- dplyr::arrange(data_m, ${sort_var}${value},${variable})
                    # data_m\$$variable <- ordered(data_m\$$variable,unique(as.character(data_m\$$variable)))
                    data_m\$$xvariable <- ordered(data_m\$$xvariable,unique(as.character(data_m\$$xvariable)))
                }
                if ('mean_sd_dat' %in% ls()) {
                    mean_sd_dat\$$xvariable <- ordered(mean_sd_dat\$$xvariable, unique(as.character(data_m\$$xvariable)))
                }
            }else{
                print("Warning: because each $xvariable do not corresponds to only one $variable, so igeore sort by group and do not sort x-axis")
                fill_guides_reverse <- 1
            }
        }else{

            if("${facet}" == "${xvariable}"){
                if ("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" &"$alpha_var" != "$xvariable"){
                    data_m\$${alpha_var} <- reorder(data_m\$${alpha_var},${sort_var}data_m\$${value}, FUN=mean)

                    if ('mean_sd_dat' %in% ls()) {
                        mean_sd_dat\$$alpha_var <- ordered(mean_sd_dat\$${alpha_var}, levels(data_m\$${alpha_var}))
                    }

                }else{
                    data_m\$${variable} <- reorder(data_m\$$variable,${sort_var}data_m\$${value}, FUN=mean)
                    if ('mean_sd_dat' %in% ls()) {
                        mean_sd_dat\$$variable <- ordered(mean_sd_dat\$$variable, levels(data_m\$$variable))
                    }
                }
            }else{
                ### sort only by xvariable
                # data_m\$${xvariable} <- reorder(data_m\$$xvariable,${sort_var}data_m\$${value}, FUN=mean)

                if ("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){

                    sort_x_list=sortValueBySubgroup(data_m,"$xvariable","$value","$alpha_var",type='$position')
                    data_m\$$xvariable <-ordered(data_m\$$xvariable,levels=sort_x_list\$xa)
                    data_m\$$alpha_var <-ordered(data_m\$$alpha_var,levels=sort_x_list\$a)

                }else{

                    if ("$xvariable" == "$variable" || sum(xtabs(~${xvariable}+$variable,data_m_alpha))/length(unique(data_m_alpha\$$xvariable)) == 1){

                        data_m\$${xvariable} <- reorder(data_m\$$xvariable,${sort_var}data_m\$${value}, FUN=mean)

                    }else{

                        sort_x_list=sortValueBySubgroup(data_m,"$xvariable","$value","$variable",type='$position')
                        data_m\$$xvariable <-ordered(data_m\$$xvariable,levels=sort_x_list\$xa)
                        data_m\$$variable <-ordered(data_m\$$variable,levels=sort_x_list\$a)
                       
                    }

                    if ('mean_sd_dat' %in% ls()) {
                        mean_sd_dat\$$xvariable <- ordered(mean_sd_dat\$$xvariable, levels(data_m\$$xvariable))
                    }
                }
            }
        }
    }else{
        ## position: fill

        if ( $sort_by_group & "${xvariable}" != "$variable"){

            if (sum(xtabs(~${xvariable}+$variable,data_m_alpha))/length(unique(data_m_alpha\$$xvariable)) == 1){
                
                if ("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" &"$alpha_var" != "$xvariable"){

                    ##### alpha_var

                    data_m_sort_temp <- data_m %>% dplyr::group_by($xvariable) %>% dplyr::mutate(countT=sum($value)) %>% dplyr::group_by($alpha_var,add=TRUE) %>% dplyr::mutate(percent=round(100*${value}/countT,2))

                    sort_var3 <- data_m_sort_temp %>% dplyr::group_by($alpha_var) %>% dplyr::summarise(all=sum(percent)) %>% dplyr::arrange(${sort_var}all)

                    if("$sort_var" == "-"){
                        data_m\$$alpha_var <-ordered(data_m\$$alpha_var,levels=rev(sort_var3\$$alpha_var))
                    }else{
                        data_m\$$alpha_var <-ordered(data_m\$$alpha_var,levels=sort_var3\$$alpha_var)
                    }

                    ##### xvariable
                    temp_xva <- c()
                    for (i in levels(data_m\$${variable})){
                        temp_d<- dplyr::filter(data_m, $variable == i)
                        temp_d\$$variable <- droplevels(temp_d\$$variable)
                        temp_d\$$xvariable <- droplevels(temp_d\$$xvariable)
                        temp_d\$$alpha_var <- droplevels(temp_d\$$alpha_var)

                        temp_xva <- c(temp_xva,sortValueBySubgroup(temp_d,"$xvariable","$value","$alpha_var",type='$position',sortGroup=T,alpha_levels=sort_var3\$$alpha_var))

                    }
                    data_m\$$xvariable <-ordered(data_m\$$xvariable,levels=temp_xva)


                }else{
                    data_m <- dplyr::arrange(data_m, ${variable}, ${sort_var}${value})
                    data_m\$$xvariable <- ordered(data_m\$$xvariable,unique(as.character(data_m\$$xvariable)))
                }

            }else{
                print("Warning: because each $xvariable do not corresponds to only one $variable, so igeore sort by group and do not sort x-axis")
                fill_guides_reverse <- 1
            }
        }else{
            if("${facet}" == "${xvariable}"){
                if ("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" &"$alpha_var" != "$xvariable"){
                    data_m\$${alpha_var} <- reorder(data_m\$${alpha_var},${sort_var}data_m\$${value}, FUN=mean)
                }else{
                    data_m\$${variable} <- reorder(data_m\$$variable,${sort_var}data_m\$${value}, FUN=mean)
                }
            }else{

                if ("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" &"$alpha_var" != "$xvariable"){

                    sort_x_list=sortValueBySubgroup(data_m,"$xvariable","$value","$alpha_var",type='$position')
                    data_m\$$xvariable <-ordered(data_m\$$xvariable,levels=sort_x_list\$xa)
                    data_m\$$alpha_var <-ordered(data_m\$$alpha_var,levels=sort_x_list\$a)

                }else{
                    if (sum(xtabs(~${xvariable}+$variable,data_m_alpha))/length(unique(data_m_alpha\$$xvariable)) == 1){
                        data_m\$${xvariable} <- reorder(data_m\$$xvariable,${sort_var}data_m\$${value}, FUN=mean)
                    }else{
                        sort_x_list=sortValueBySubgroup(data_m,"$xvariable","$value","$variable",type='$position')
                        data_m\$$xvariable <-ordered(data_m\$$xvariable,levels=sort_x_list\$xa)
                        data_m\$$variable <-ordered(data_m\$$variable,levels=sort_x_list\$a)
                    }
                }
            }
        }
    }
}

###########                sort value                  ###########
### 如果x设定为数值型,但使用dplyr计算后返回的数据框此列会变为因子型，所以强制改为数值
if(!${x_type}){
    data_m\$${xvariable} <- as.numeric(as.character(data_m\$${xvariable}))
}

###########                geom layer                  ###########
## basic
p <- ggplot(data_m, aes($xvariable, ${value}))
p <- p + xlab("${xlab}") + ylab("${ylab}")+labs(title="${title}")

## layer
if ("$xvariable"=="$variable" && length(unique(data_m\$$xvariable))>8){
    if ("$alpha_var" != 'aAbBcC'){
        alpha_n <- seq(1,0, by = -1/nlevels(data_m\$$alpha_var))[1:nlevels(data_m\$$alpha_var)]

        p <- p + geom_bar(aes(alpha=${alpha_var}),fill="grey50",stat="identity", position="${position}",width=${sub_bar})
        if("$sort_var" == "+" & "$position" == "dodge"){
            p <- p+scale_alpha_manual(values = rev(alpha_n))
        }else{
            p <- p+scale_alpha_manual(values = alpha_n)
        }

    }else{
        p <- p + geom_bar(color="grey50",fill="grey50",stat="identity", position="${position}",width=${sub_bar})
    }
}else{
    if ("$alpha_var" != 'aAbBcC'){

        alpha_n <- seq(1,0, by = -1/nlevels(data_m\$$alpha_var))[1:nlevels(data_m\$$alpha_var)]

        p <- p + geom_bar(aes(fill=${variable},alpha=${alpha_var}), stat="identity", position="${position}",width=${sub_bar})
        if("$sort_var" == "+" & "$position" == "dodge"){
            p <- p+scale_alpha_manual(values = rev(alpha_n))
        }else{
            p <- p+scale_alpha_manual(values = alpha_n)
        }

    }else{
        p <- p + geom_bar(aes(fill=${variable}, color=${variable}), stat="identity", position="${position}",width=${sub_bar})
    }
}

if ("$position" == "fill"){
    p <- p+scale_y_continuous(labels = scales::percent)+ylab("Percent")
}

## error_bar
if ("${error_bar}" != "FALSE" && "${position}" != "fill" && "${stat}"=="identity") {

    if ("${position}" == 'dodge' || "${facet}" == "${variable}"){
        if("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
            p <- p + geom_errorbar(aes(ymin=${value}-sd,ymax=${value}+sd, group=${alpha_var}), colour="black", width=0.2, position=position_dodge(width=${sub_bar}))
        }else{
            p <- p + geom_errorbar(aes(ymin=${value}-sd,ymax=${value}+sd, group=${variable}), colour="black", width=0.2, position=position_dodge(width=${sub_bar}))

        }
    } else {
        p <- p + geom_errorbar(data=mean_sd_dat,aes(ymin=${value}-sd, ymax=${value}+sd), colour="black", width=0.2, position=position_dodge(width=${sub_bar}))
    }
}

## text in bar
if ("${show_text}" == 'TRUE'){

    if ("$position" == 'stack'){
        if("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
            p <- p+geom_text(aes(label=round(${value},1),group=${alpha_var}),size=2,position=position_stack(vjust=0.5))
        }else{
            p <- p+geom_text(aes(label=round(${value},1),group=${variable}),size=2,position=position_stack(vjust=0.5))
        }

    }else if ("$position" == 'fill'){

        if ("$facet" != "NoMeAnInGTh_I_n_G_s"){
        data_m_text <- data_m %>% dplyr::group_by(${xvariable},${facet}) %>% dplyr::mutate(countT=sum(${value})) %>% dplyr::group_by(${variable},add=TRUE) %>% dplyr::mutate(percent=round(100*${value}/countT,2))

        }else{
            data_m_text <- data_m %>% dplyr::group_by(${xvariable}) %>% dplyr::mutate(countT=sum(${value})) %>% dplyr::group_by(${variable},add=TRUE) %>% dplyr::mutate(percent=round(100*${value}/countT,2))
        }

        data_m_text\$labels=round(data_m_text\$percent, 1)

        if("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
            p <- p+geom_text(data=data_m_text,aes(label=labels,group=${alpha_var}), size=2, position=position_fill(vjust=0.5))
        }else{
            p <- p+geom_text(data=data_m_text,aes(label=labels,group=${variable}), size=2, position=position_fill(vjust=0.5))
        }


    }else if ("$position" == 'dodge'){
        if ("${error_bar}" != "FALSE"){
            data_m_err_text <- data_m
            data_m_err_text\$label <- round(data_m_err_text\$$value,1)

            data_m_err_text\$temp <- data_m_err_text\$$value
            data_m_err_text\$$value <- -0.025*(max(data_m_err_text\$$value)-min(data_m_err_text\$$value))

            ## 负值和正值的柱子上的text位置校正
            data_m_err_text[data_m_err_text\$temp < 0,]\$$value <- -data_m_err_text[data_m_err_text\$temp < 0,]\$$value

            data_m\$sd1 <- data_m\$sd
            data_m\$sd1[is.na(data_m\$sd1)] <- 0

            if("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
                p <- p+geom_text(data=data_m_err_text, aes(label=label,group=${alpha_var}),size=2,position=position_dodge(width=${sub_bar}))+coord_cartesian(ylim=c(min(data_m\$${value}+data_m\$sd1,na.rm=T),max(data_m\$$value+data_m\$sd1,na.rm=T)),expand=TRUE)
            }else{
                p <- p+geom_text(data=data_m_err_text, aes(label=label,group=${variable}),size=2,position=position_dodge(width=${sub_bar}))+coord_cartesian(ylim=c(min(data_m\$${value}+data_m\$sd1,na.rm=T),max(data_m\$${value}+data_m\$sd1,na.rm=T)),expand=TRUE)
            }
        }else{
            ## 负值和正值的柱子上的text位置校正
            data_m_text_p <- data_m[data_m\$$value >=0,]
            data_m_text_n <- data_m[data_m\$$value <0,]

            if("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
                if (${rotate_plot}){
                    p <- p+geom_text(data=data_m_text_n, aes(label=round(${value},1),group=${alpha_var}),size=2,position=position_dodge(width=${sub_bar}),hjust=1.1)
                    p <- p+geom_text(data=data_m_text_p, aes(label=round(${value},1),group=${alpha_var}),size=2,position=position_dodge(width=${sub_bar}),hjust=-0.1)

                }else{
                    p <- p+geom_text(data=data_m_text_n, aes(label=round(${value},1),group=${alpha_var}),size=2,position=position_dodge(width=${sub_bar}),vjust=1.3)
                    p <- p+geom_text(data=data_m_text_n, aes(label=round(${value},1),group=${alpha_var}),size=2,position=position_dodge(width=${sub_bar}),vjust=-0.3)
                }
            }else{
                if (${rotate_plot}){
                    p <- p+geom_text(data=data_m_text_n, aes(label=round(${value},1),group=${variable}),size=2,position=position_dodge(width=${sub_bar}),hjust=1.1)
                    p <- p+geom_text(data=data_m_text_p, aes(label=round(${value},1),group=${variable}),size=3,position=position_dodge(width=${sub_bar}),hjust=-0.1)
                }else{
                    p <- p+geom_text(data=data_m_text_n, aes(label=round(${value},1),group=${variable}),size=2,position=position_dodge(width=${sub_bar}),vjust= 1.3)
                    p <- p+geom_text(data=data_m_text_p, aes(label=round(${value},1),group=${variable}),size=2,position=position_dodge(width=${sub_bar}),vjust=-0.3)
                }
            }
        }
    }
}

if("${sort_var}" == '+' & ! 'fill_guides_reverse' %in% ls()){
    if("${alpha_var}" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
        p <-p+guides(fill = guide_legend(reverse = TRUE))
    }else{
        p <-p+guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = TRUE))
    }
}

### coord_cartesian
if (!("$position" == 'dodge' & "${stat}"=="identity" & "${show_text}" == 'TRUE' & "${error_bar}" != "FALSE")){
    if ("$position" == "fill"){
        if($show_text){
            if($rotate_plot){
                p <- p+ scale_y_continuous(expand=c(0.040,0),labels = scales::percent)
            }else{
                p <- p+ scale_y_continuous(expand=c(0.040,0),labels = scales::percent)
            }
        }else{
            p <- p+ scale_y_continuous(expand=c(0.015,0),labels = scales::percent)
        }
    }else{
        if("$show_text" == "TRUE"){
            if($rotate_plot){
                p <- p+ scale_y_continuous(expand=c(0.15,0))
            }else{
                p <- p+ scale_y_continuous(expand=c(0.040,0))
            }
        }else{
            p <- p+ scale_y_continuous(expand=c(0.015,0))
        }
    }
}

### save data_m
if ("${error_bar_coloumn}" == "ctCTct"){
    write.table(data_m,file="$output/$(basename $file)${mid}.txt",sep="\t", quote=FALSE, row.names = F)
}


### vline
vline_vector <- c(${vline})
if("${vline}" != 'FALSE'){
    p <- p + geom_vline(xintercept=hline_vector,
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
    if("$alpha_var" != 'aAbBcC'){
        p <- p + scale_fill_manual(values=c(${color_v}))
    }else{
        p <- p + scale_fill_manual(values=c(${color_v}))+scale_colour_manual(values=c(${color_v}))
    }
}

### coord_flip
if (${rotate_plot}) {
    p <- p + coord_flip()
}

### facet
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
w_h_auto <-Plot_size_final(length(unique(data_m\$${xvariable})),"${rotate_plot}","$legend_pos","${variable}", "${xvariable}", "${facet}", "${facet_direction}", nlevels(data_m\$${facet}), "bar",dodge_status="$position")

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
