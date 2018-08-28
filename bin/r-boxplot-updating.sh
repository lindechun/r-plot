#!/bin/bash

# set -x

usage()
{
cat <<EOF
${txtcyn}

***CREATED BY Chen Tong (chentong_biology@163.com)***
***MODIFY BY Lin Dechun (lindechun@genomics.cn)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This script is used to do boxplot using ggplot2.

${txtbld}OPTIONS${txtrst}:

<input>
	-f	Data file (with header line, the first row is the
 		colname, tab seperated. Multiple formats are allowed and described in r-plot_tutorial.md)
		${bldred}[NECESSARY]${txtrst}
	-m	When true, it will skip preprocess. But the format must be
		the same as listed before (Matrix_melted).
		${bldred}[Default FALSE, accept TRUE]${txtrst}
	-Q	Giving a sampleGroup file with format specified above to
   		tell the group information for each sample.	only can be used when <-m FALSE>. When <-Q> is given, <-F> and <-a> should be one of the column
		names of sampleGrp file.${bldred}[Default FALSE, accept a file name]${txtrst}
	-z	Is there a header. Only be used when -m FALSE.[${bldred}Default TRUE${txtrst}]
	-q	Giving one gene ID to do boxplot specifically for this gene(only one row in data file).
		${bldred}[Default FALSE, accept a string,like: "Name:A"]${txtrst}
	-H	When you set -q, but you normal data file have some non-numerical colmuns，so you must skip these. [Default 1]
<aes>
	-a	Name for x-axis variable
		[${txtred}Default variable, which is an inner name, suitable 
		for data without 'Set' column. For the given example, 
		'Group' which represents groups of each gene should be 
		supplied to this parameter.
	-d	The column represents the digital values, such as Expr.
		${bldred}[Default "value" represents the column named "value".
		This parameter can only be set when -m is TRUE.]${txtrst}
	-F	Name for legend column, meaning legend_variable.
		If no-subclass of X-variavle(-a), this will be used X-axis variable.
		${bldred}[Default "variable" represents the column named "variable". 
		This parameter can only be set when -m is TRUE or -Q be seted.]${txtrst}
	-W	Name for alpha-legend column, meaning alpha legend variable.[Default: FALSE]. When you set -W is not same with -a and -F, -a must have only one Properties.
	-L	Levels for x-axis variable
		[${txtred}Default data order,accept a string like
		"'g','a','j','x','s','c','o','u'" ***for <Set> column***.${txtrst}]
	-l	Levels for legend variable
		[${txtred}Default data order,accept a string like
		"'TP16','TP22','TP23'" [Default legend_pos for <xvariable> column].${txtrst}]
	-k	Levels for alpha-legend variable. Default columns order,accept a string like "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.[Default NULL. Only be used when -W be set

<labs>
	-t	Title of picture[${txtred}Default empty title${txtrst}]
	-x	xlab of picture[${txtred}Default name for -a${txtrst}]
	-y	ylab of picture[${txtred}Default name for -d${txtrst}]

<scales>
	-b	Rotation angle for x-axis value(anti clockwise)
		${bldred}[Default 0]${txtrst}
	-s	Scale y axis
		[${txtred}Default null. Accept TRUE.
		Also if the supplied number after -S is not 0, this
		parameter will be set to TRUE${txtrst}]
	-S	A number to add y axis（to -d)
		[${txtred}Default 0. If a non-zero number is given, -s is
		TRUE.${txtrst}]
	-c	Manually set colors for each box.[${txtred}Default FALSE,
		meaning using ggplot2 default.${txtrst}]
	-C	Color for each box.[${txtred}When -c is TRUE, str in given
		format must be supplied, ususlly the number of colors should
		be equal to the number of lines.
		"'red','pink','blue','cyan','green','yellow'" or
		"rgb(255/255,0/255,0/255),rgb(255/255,0/255,255/255),rgb(0/255,0/255,255/255),
		rgb(0/255,255/255,255/255),rgb(0/255,255/255,0/255),rgb(255/255,255/255,0/255)"
		${txtrst}]

<facet>
	-G	facet grid plots by given column. This is used to put multiple plot
		in one picture. Used when -m TRUE or -Q be seted, normally a string <set>
		should be suitable for this parameter.
	-g	The levels of wrapping to set the order of each group.
		${txtred}Normally the unique value of the column given to B in
		a format like <"'a','b','c','d'">.${txtrst}
	-M	The direction of facet when -G is used. give the following h (horizontal,Default), v (vertical).

<coord>
	-R	Rotate the plot from vertical to horizontal. 
		Usefull for plots with many values or very long labels at X-axis.
		${bldred}[Default FALSE]${txtrst}

<panel>
	-w	The width of output picture.[${txtred}Default auto calculate${txtrst}]
	-u	The height of output picture.[${txtred}Default auto calculate${txtrst}]
	-P	[Uppercase P] Legend position[${txtred}Default right. Accept
		top,bottom,left,none, or c(0.08,0.8) (relative to left-bottom).${txtrst}]
	-r	The resolution of output picture.[${txtred}Default 300 ppi${txtrst}]
	-E	The type of output figures.[${txtred}Default pdf, accept
		eps/ps, tex (pictex), pdf, jpeg, tiff, bmp, svg and wmf)${txtrst}]
	-T	The self-definited theme for ggplot2, give the followding theme_classic2 [Default], theme_classic3, theme_cin.${txtrst}

<other ggplot code>
	-p	[Lowercase p] Other legal R codes for gggplot2 will be given here.
		[${txtres}Begin with '+' ${txtrst}]

<output>
	-e	Execute or not[${bldred}Default TRUE${txtrst}]
	-o	Path of output.[Default path of data file(-f), Optinal]

<self-boxplot>
<<sort value>>
	-X	Whether sort the value. FALSE [Default] '-'(dscending)or '+'(ascending) order. Only be used when -A TURE.
	-Y 	Whether sort inside each group. Default FALSE. Only be used when -a xvariable have only one Properties and -A TURE.

<<notch of boxplot>>
	-n	Using notch (sand clock shape) or not.${txtred}[Default FALSE]${txtrst}

<<width of sub-box>>
	-K	The width of box or violin.[${txtred}Default 0.75${txtrst}]. Only be used when -A TRUE

<<plot type>>
	-v	Do violin plot instead of boxplot.${txtred}[Default FALSE]${txtrst}
	-V	Do violin plot overlay with jitter.${txtred}[Default FALSE]${txtrst}
	-j	Do jitter plot instead of boxplot.${txtred}[Default FALSE]${txtrst}
	-J	Do boxplot plot overlay with jitter.${txtred}[Default FALSE]${txtrst}
	-A	The value given to scale for violin plot.
		if "area", all violins have the same area (before trimming the tails). 
		If "count", areas are scaled proportionally to the number of observations. 
		If "width", all violins have the same maximum width. 
		'equal' is also accepted.
		${txtred}[Default 'width']${txtrst}

<<cut x-axis or legend columns>>
	-D	Self-define intervals for legend variable when legend is
		continuous numbers. Accept either a
		numeric vector of two or more cut points or a single number
		(greater than or equal to 2) giving the number of intervals
		into what 'x' is to be cut. This has higher priority than -l.
		[10 will generate 10 intervals or 
		"c(-1, 0, 1, 2, 5, 10)" will generate (-1,0],(0,1]...(5,10]]
	-B	Self-define intervals for x-axis variable. Accept either a
		numeric vector of two or more cut points or a single number
		(greater than or equal to 2) giving the number of intervals
		into what 'x' is to be cut. This has higher priority than -L.
		[10 will generate 10 intervals or 
		"c(-1, 0, 1, 2, 5, 10)" will generate (-1,0],(0,1]...(5,10]]

<<outlier>>
	-i	Exclude outliers.
		[${txtred}Exclude outliers or not, default FALSE means not.${txtrst}]
	-O	The scales for you want to zoom in to exclude outliers.
		[${txtred}Default 1.05. No recommend to change unless you know
		what you are doing.${txtrst}]
EOF
}

# 待修改：
# ${bldred}[Default empty string, accept comma separated strings
# like "'Id1','Id2','Id3'" or single string "id1"]${txtrst}
# This parameter must set when -q is not FALSE or -m is TRUE.${txtrst}]

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
color='FALSE'
color_v=''

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

# other ggplot code
par=''

#output
execute='TRUE'
output=''

#self-boxplot
##sort value
sort_var='FALSE'
sort_by_group='FALSE'

##notch of boxplot
notch='FALSE'

##width of sub_box
sub_box=0.75

##plot type
violin='FALSE'
violin_jitter='FALSE'
jitter='FALSE'
boxplot_jitter='FALSE'
scale_violin='width'

##cut x-aixs or legend columns
legend_cut=""  ##待修改
x_cut=""       ##待修改

##outlier
outlier='FALSE'
out_scale=1.05

while getopts "hf:m:Q:z:q:H:a:d:F:L:l:t:x:y:b:s:S:W:c:C:G:g:M:R:w:u:P:r:E:T:p:e:o:X:Y:n:K:v:V:j:J:A:D:B:i:O" OPTION
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
		F)
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
		c)
			color=$OPTARG
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
		##notch of boxplot
		n)
			notch=$OPTARG
			;;
		##width of sub-box
		K)
			sub_box=$OPTARG
			;;
		##plot type
		v)
			violin=$OPTARG
			;;
		V)
			violin_jitter=$OPTARG
			;;
		j)
			jitter=$OPTARG
			;;
		J)
			boxplot_jitter=$OPTARG
			;;
		A)
			scale_violin=$OPTARG
			;;
		##cut x-axis or legend columns
		B)
			x_cut=$OPTARG
			;;
		D)
			legend_cut=$OPTARG
			;;
		##outlier
		i)
			outlier=$OPTARG
			;;
		O)
			out_scale=$OPTARG
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

if test "${melted}" == "FALSE"; then
	if [ -z $xvariable ]; then
		xvariable="variable"
		echo "Warning, when you give data(-f) file is unmelted, and you don't set -a, so there is set -a variable."
	fi
	if [ -z $value ]; then
		value="value"
		echo "Warning, when you give data(-f) file is unmelted, and you don't set -d, so there is set -d value."
	fi
fi

if test "${melted}" == "FALSE" && test "${gene}" != "FALSE";then
	if test "${xvariable}" == "variable";then
		xvariable="Sample"
	fi
fi

if test "${melted}" == "TRUE"  && test "${xvariable}" == ""; then
	echo "If '-m TRUE' be set, -a should be set"
	exit 1
fi

if test "${melted}" == "TRUE" && test "${value}" == ""; then
	echo "If '-m TRUE' be set, -d should be set"
	exit 1
fi

# if test "${melted}" != "FALSE" && test "${gene}" != "FALSE";then
# 	if test "${xvariable}" != "";then
# 		xvariable=${ID_var}
# 	fi
# fi

######################### sort value
## sort value
if test "$x_level" != '' && test "$sort_var" != "";then
	echo "Warning: you have set -L(x-axis level), so we will ignore -X (sort value)"
fi
## sort by group
if test "$sort_by_group" != "FALSE" && test "$sort_var" != "FALSE" && test "$xvariable" == "$variable";then
	echo " You set -X"${sort_var}", but -a is same with -F, so we ignore -Y TRUE"
	sort_by_group='FALSE'
fi
######################### sort value

if test -z "${variable}"; then
	variable=${xvariable}
fi

if test "${xlab}" == " ";then
	xlab=${xvariable}
fi

if test "${ylab}" == " ";then
	ylab=${value}
fi

### 判断图形输入类型
b=0
plot_judge=($violin $violin_jitter $jitter $boxplot_jitter)

for i in ${plot_judge[@]}
do
    if [ $i != 'FALSE' ];then
        ((b++))
    fi
done

if [ $b -gt 1 ];then
	echo "You can only choose one of these (-V -W -j -J)"
	exit 1
fi
## Variable inspection

## Prefix of picture file
if [ $b -eq 0 ];then
	mid=${mid}'.boxplot'
fi

if test "${violin}" == "TRUE"; then
	mid=${mid}'.violin'
fi

if test "${violin_jitter}" == "TRUE"; then
	mid=${mid}'.violin_jitter'
fi

if test "${jitter}" == "TRUE"; then
	mid=${mid}'.jitter'
fi

if test "${boxplot_jitter}" == "TRUE"; then
	mid=${mid}'.boxplot_jitter'
fi

# if test "${gene}" != "FALSE" && test ${skip} -gt 1; then
	# value="${gene}"
# fi

if test "${melted}" == 'TRUE' || (test "${melted}" != 'TRUE' && test "${xvariable}" != 'variable'); then
	if test "${xvariable}" != "${variable}";then
		mid=${mid}'.'${xvariable}_${value}"_legend-"${variable}
	else
		mid=${mid}'.'${xvariable}_${value}
	fi
fi

if test "${melted}" == "FALSE" && test "${gene}" == "FALSE" ; then
	mid=${mid}'.allRows'
fi

if test "${outlier}" == "TRUE"; then
	mid=${mid}'.noOutlier'
fi

#if test "${gene}" != "FALSE"; then
#	mid=${mid}".${gene}"
#fi

if test "${scaleY}" != "FALSE"; then
	mid=${mid}'.scaleY_'${scaleY}
fi

if test "${facet}" != "NoMeAnInGTh_I_n_G_s"; then
	mid=${mid}'.facet_grid'
fi
## Prefix of picture file

if test -z "$output";then
	output=$(cd `dirname $file`;pwd)
fi

cat <<END >$output/$(basename $file)${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

# installp(c("ggplot2", "reshape2", "scales","ggbeeswarm","dplyr"))

if(${boxplot_jitter} || ${violin_jitter} || ${jitter}){
	library(ggbeeswarm)
}

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

###########                 read file                ###########

if ("${sampleGroup}" != "FALSE") {
        sampleGroup <- read.table("${sampleGroup}",sep="\t",header=1,check.names=F,row.names=1)
        sampleGroup\$variable <- factor(rownames(sampleGroup))
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

	    if ("$xvariable" %in% colnames(data)){
	    	data\$$xvariable <- ordered(data\$$xvariable,unique(as.character(data\$$xvariable)))
	    }

	    if ("$variable" %in% colnames(data)){
	    	data\$$variable <- ordered(data\$$variable,unique(as.character(data\$$variable)))
	    }

		data_m <- melt(data)

        if ("${sampleGroup}" != "FALSE") {
        	# print(data_m\$variable)
			data_m <-left_join(data_m,sampleGroup,by="variable")
        }
    }
} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="")
}

###########                 read file                ###########


if (${y_add} != 0){
    data_m\$${value} <- data_m\$${value} + ${y_add}
}
if("$scaleY" != 'FALSE'){
    data_m\$${value} <- $scaleY(data_m\$${value})
}

########### level of x-axis, legend and tag of facet ###########


if ("${legend_cut}" != "") {
    data_m\$${variable} <- cut(data_m\$${variable}, ${legend_cut})
}

if ("${x_cut}" != "") {
    data_m\$${xvariable} <- cut(data_m\$${xvariable},${x_cut})
}

data_m\$${xvariable} <- factor(data_m\$${xvariable})
data_m\$${variable} <- factor(data_m\$${variable})

####sort value
if ("${sort_var}" !='FALSE'){

	if ($sort_by_group & "${xvariable}" != "$variable"){
		if (sum(xtabs(~${xvariable}+$variable, data_m))/length(unique(data_m\$$xvariable)) == 1){

			temp_sort <- data_m %>% dplyr::group_by(${xvariable}) %>% dplyr::summarise(mean=mean($value))
			temp_sort <- dplyr::arrange(temp_sort, ${sort_var}mean)
			data_m\$$xvariable <- ordered(data_m\$$xvariable, as.character(temp_sort\$$xvariable))
		}else{
			print("Warning: because each $xvariable do not corresponds to only one $variable, so igeore sort by group")
		}

	}else{
		if ("${facet}" == "${xvariable}"){
			data_m\$${variable} <- reorder(data_m\$$variable,${sort_var}data_m\$${value}, FUN=mean)
		}else{
			data_m\$${xvariable} <- reorder(data_m\$$xvariable,${sort_var}data_m\$${value}, FUN=mean)
		}
	}
}

level_i <- c(${color_level})
if (length(level_i)>1){
    data_m\$${variable} <- factor(data_m\$${variable}, levels=level_i)
}

x_level <- c(${x_level})
if (length(x_level)>1){
    data_m\$${xvariable} <- factor(data_m\$${xvariable},levels=x_level)
}

alpha_level <- c(${alpha_level})
if (length(alpha_level)>1){
    data_m\$${alpha_var} <- factor(data_m\$${alpha_var},levels=alpha_level)
}

####sort value

if ("${facet_level}" != "NA") {
	facet_level <- c(${facet_level})
    data_m\$${facet} <- factor(data_m\$${facet},
        levels=facet_level, ordered=T)
}

if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
	data_m\$${facet} <- factor(data_m\$${facet})
}

########### level of x-axis, legend and tag of facet ###########

###########   calculate median line about boxplot    ###########

s_boxplot <- function(dat,group_va){

    median_dat <- dat %>% group_by_(.dots=group_va) %>% summarise(median=median(${value}),mean=mean($value),sd=sd($value))
    return(median_dat)

}
if("${alpha_var}" != 'aAbBcC' & sum(xtabs(~${xvariable}+$variable,data_m_alpha))/length(unique(data_m_alpha\$$xvariable)) == 1 & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
	if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
		data_bp_median <- s_boxplot(data_m,c("${variable}","${xvariable}","$alpha_var","${facet}"))
	} else {
		data_bp_median <- s_boxplot(data_m,c("${variable}","${xvariable}","$alpha_var"))
	}
else{
	if("$alpha_var" != 'aAbBcC' & "$alpha_var" != "$variable" & "$alpha_var" != "$xvariable"){
		stop("Error: because each $xvariable do not corresponds to only one $variable and $alpha_var is not same with $variable, so igeore alpha legend setting")
	}

	if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
		data_bp_median <- s_boxplot(data_m,c("${variable}","${xvariable}","${facet}"))
	} else {
		data_bp_median <- s_boxplot(data_m,c("${variable}","${xvariable}"))
	}
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

p <- ggplot(data_m, aes(${xvariable}, ${value})) + xlab("$xlab") +
    ylab("$ylab") + labs(title="$title")

if (${violin}) {
    p <- p + geom_violin(aes(color=${variable},fill=${variable}), 
    stat = "ydensity", position = "dodge", trim = TRUE, 
    scale = "${scale_violin}",width=${sub_box})

	if ("$variable" != "$xvariable"){
	    p <- p+geom_boxplot(aes(fill=${variable}),color="black",size=0.4,
	            outlier.colour='NA', width=${sub_box}/3,position=position_dodge(width=${sub_box}))
	}else{
	    p <- p+geom_boxplot(fill='white',color="black",size=0.4,
	            outlier.colour='NA', width=${sub_box}/3,position=position_dodge(width=${sub_box}))
	}

} else if (${violin_jitter}) {
    p <- p + geom_violin(aes(color=${variable}),size=0.5, 
    stat = "ydensity", position = "dodge", trim = TRUE, 
    scale = "${scale_violin}",width=${sub_box})+geom_quasirandom(size=jitter_size,alpha=0.7)

    p <- p+geom_point(data=data_bp_median,aes(x=${xvariable},y=median,group=${variable},color=${variable}),position=position_dodge(width=${sub_box}),size=1.5,shape=17)

} else if (${jitter}) {

    p <- p + geom_quasirandom(aes(color=${variable}),size=jitter_size)
    # p <- p + geom_dotplot(aes(color=${variable},fill=${variable}), stackdir = 'center',size=jitter_size, binaxis ='y')

    p <- p + stat_summary(fun.y = "mean", geom = "text", label="----", size= 5, color= "black")


} else {
    if (${notch}){
        if (${outlier}){
            p <- p + geom_boxplot(aes(fill=${variable},color=${variable}), notch=TRUE,width=$sub_box, 
            notchwidth=0.3, outlier.colour='NA')
        }else{
            p <- p + geom_boxplot(aes(fill=${variable},color=${variable}), notch=TRUE,outlier.size=0.5, width=$sub_box, 
            notchwidth=0.3)
        }
    } else {
        if (${outlier}){
            p <- p + geom_boxplot(aes(fill=${variable},color=${variable}),
            outlier.colour='NA', width=${sub_box})
        }else{
            p <- p + geom_boxplot(aes(fill=${variable},color=${variable}),outlier.size=0.5, width=${sub_box})
        }
    }

	p <- p+geom_crossbar(data=data_bp_median,aes(x=${xvariable},y=median,ymin=median,ymax=median,group=${variable}),position=position_dodge(width=${sub_box}),width=${sub_box},fatten=0,size=0.5,color="white")
}

if (${boxplot_jitter}) {
    p <- p + geom_quasirandom(aes(group=${variable}),color='black',size=jitter_size,alpha=0.7)
}

### outlier
if (${outlier}) {
    #ylim_zoomin <- boxplot.stats(ddata_m\$${value})\$stats[c(1,5)]
    stats <- boxplot.stats(data_m\$${value})\$stats
    ylim_zoomin <- c(stats[1]/${out_scale}, stats[5]*${out_scale})
    p <- p + coord_cartesian(ylim = ylim_zoomin)
}


### save data_m and mean_sd_dat data

write.table(data_bp_median,file="$output/$(basename $file)${mid}.txt",sep="\t", quote=FALSE, row.names = F)


### manunal change color
if ($color) {
    p <- p + scale_fill_manual(values=c(${color_v}))+scale_colour_manual(values=c(${color_v}))+scale_fill_manual(values=c(${color_v}))
}
### coord_flip
if (${rotate_plot}) {
    p <- p + coord_flip()
}

### facet
if ("${facet}" != "NoMeAnInGTh_I_n_G_s") {
	if("${facet_direction}" == 'h') {
	    p <- p + facet_grid( .~ ${facet},scales="free")
	}else if("${facet_direction}" == 'v') {
	    p <- p + facet_grid( ${facet}~. ,scales="free")
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
p<- PLot_margin(p, '$title', '$xlab', '$ylab')

#plot width and height auto calculate
w_h_auto <-Plot_size_final(length(unique(data_m\$${xvariable})),"${rotate_plot}","$legend_pos","${variable}", "${xvariable}", "${facet}", "${facet_direction}", nlevels(data_m\$${facet}))
w_auto <- w_h_auto[1]
h_auto <- w_h_auto[2]

#output
Plot_savefig('$output/$(basename $file)${mid}.${ext}', p, $uwid, $vhig, w_auto, h_auto, $res, "${rotate_plot}",ftype='${ext}')

###########                  output                  ###########
END

if [ "$execute" == "TRUE" ]; then
	Rscript $output/$(basename $file)${mid}.r
    if [ "$?" == "0" ]; then
        /bin/rm -f $output/$(basename $file)${mid}.r
    fi
fi
