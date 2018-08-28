#!/bin/bash

#Only for debugging
#set -x

usage()
{
cat <<EOF
${txtcyn}


***CREATED BY Chen Tong (chentong_biology@163.com)***
***MODIFY BY Lin Dechun (lindechun@genomics.com)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This program is designed to test if two boxes have statistically
significant differences.

Currently multiple groups among one set are supported.

fileformat for -f (suitable for data extracted from one sample, the
number of columns is unlimited. Column 'Set' is not necessary unless
you have multiple groups)

Gene	hmC	expr	Set
NM_001003918_26622	0	83.1269257376101	TP16
NM_001011535_3260	0	0	TP16
NM_001012640_14264	0	0	TP16
NM_001012640_30427	0	0	TP16
NM_001003918_2662217393_30486	0	0	TP16
NM_001017393_30504	0	0	TP16
NM_001025241_30464	0	0	TP16
NM_001017393_30504001025241_30513	0	0	TP16

For file using "Set" column, you can use 
$0 -f file -a Set 

fileformat when -m is true
#The name "value" and "variable" shoud not be altered.
#The "Set" column is optional.
#If you do have several groups, they can put at the "Set" column 
#with "Set" or other string as labels. The label should be given
#to parameter -a.
#Actually this format is the melted result of last format.
value	variable	Set
0	hmC	g
1	expr	g
2	hmC	a
3	expr	a

${txtbld}OPTIONS${txtrst}:
<input>
	-f	Data file (with header line, the first column is the
 		colname, tab seperated)${bldred}[NECESSARY]${txtrst}
	-m	When true, it will skip preprocess. But the format must be
		the same as listed before.
		${bldred}[Default FALSE, accept TRUE]${txtrst}
	-Q	Giving a sampleGroup file with format specified above to
   		tell the group information for each sample.	only can be used when <-m FALSE>. When <-Q> is given, <-F> and <-a> should be one of the column
		names of sampleGrp file.${bldred}[Default FALSE, accept a file name]${txtrst}
	-q	Giving one gene ID to do boxplot specifically for this gene.
		${bldred}[Default FALSE, accept a string,like: "Name:A"]${txtrst}
	-H	When you set -q, but you normal data file have some non-numerical colmuns，so you must skip these. [Default 1]
<aes>
	-a	Name for x-axis variable
		[${txtred}Default variable, which is an inner name, suitable 
		for data without 'Set' column. For the given example, 
		'Set' which represents groups of each gene, and should be 
		supplied to this parameter.
		${txtrst}]
	-d	The column represents the digital values, such as 'H3K27ac'.
		${bldred}[Default "value" represents the column named "value".
		This parameter can only be set when -m is TRUE.]${txtrst}
	-F	Name for sub-x-axis-variable.meaning group information
		${bldred}[Default "variable" represents the column named "variable". 
		This parameter can only be set when -m is TRUE or -Q be seted.]${txtrst}
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
		into what 'x' is to be cut. 
		[10 will generate 10 intervals or 
		"c(-1, 0, 1, 2, 5, 10)" will generate (-1,0],(0,1]...(5,10]]
<method>
	-s	The statistical method you want to use.${bldred}[Default
		shapiro.test, accept lillieTest, ***]${txtrst}
<execute>
	-e	Execute or not[${bldred}Default TRUE${txtrst}]
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
level=""
legend_cut=""
x_level=""
x_cut=""

#output
output=''
execute='TRUE'
method='shapiro.test'

while getopts "hf:m:Q:z:q:H:a:d:F:D:B:s:e:" OPTION
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
		#method
		s)
			method=$OPTARG
			;;
		B)
			x_cut=$OPTARG
			;;
		D)
			legend_cut=$OPTARG
			;;
		e)
			execute=$OPTARG
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

if test "${melted}" == "TRUE"  && test "${xvariable}" == ""; then
	echo "If '-m TRUE' be set, -a should be set"
	exit 1
fi

if test "${melted}" == "TRUE" && test "${value}" == ""; then
	echo "If '-m TRUE' be set, -d should be set"
	exit 1
fi

if test -z "${variable}"; then
	variable=${xvariable}
fi

mid=".boxplot.${method}"


cat <<END >${file}${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

# installp("fBasics")

library(reshape2)
library(dplyr)
if ("${method}" == "lillieTest"){
	library('fBasics')
}

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
		data_m <- melt(data)

        if ("${sampleGroup}" != "FALSE") {
			data_m <-left_join(data_m,sampleGroup,by="variable")

        }
    }
} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="")
}

if ("${legend_cut}" != ""){
	data_m\$$variable <- cut(data_m\$$variable, ${legend_cut})
} 
if ("${x_cut}" != ""){
	data_m\$${xvariable} <- cut(data_m\$${xvariable},${x_cut})
}

if ("$xvariable" == "$variable"){
	#No Group information
	variableL <- unique(data_m\$$xvariable)
	len_var <- length(variableL)
	for(i in 1:len_var){
		var1 <- variableL[i]
		new_data <- data_m[data_m\$$xvariable == var1,]\$$value
		if ("${method}" == "lillieTest"){
			if (length(new_data) > 4) {
				print(paste("### Compute normality for", var1,  "###"))
				print(${method}(new_data))
			} else {
				print(paste("### No enough data for lillieTest:", var1, "###"))
			}
		} else if ("${method}" == "shapiro.test") {
			if (length(new_data) > 2) {
				print(paste("### Compute normality for", var1, "###"))
				print(${method}(new_data))
			} else {
				print(paste("### No enough data for shapiro.test:",
				var1, "###"))
			}
		} else {
			print(paste("### Compute normality for", var1,  "###"))
			print(${method}(new_data))
		}
	}
} else {
	#Compute several groups
	group <- names(summary(data_m\$${xvariable}))

	for (i in group){
		tmp <- data_m[data_m\$${xvariable}==i,]
		print(paste("*** Compute for Group ", i, " ***"))

		variableL <- unique(tmp\$$variable)
		len_var <- length(variableL)
		for(i in 1:len_var){
			var1 <- variableL[i]
			new_data <- tmp[tmp\$$variable == var1,]\$$value
			if ("${method}" == "lillieTest"){
				if (length(new_data) > 4) {
					print(paste("### Compute normality for", var1, "###"))
					print(${method}(new_data))
				} else {
					print(paste("### No enough data for lillieTest:",
					var1, "###"))
				}
			} else if ("${method}" == "shapiro.test") {
				if (length(new_data) > 2) {
					print(paste("### Compute normality for", var1, "###"))
					print(${method}(new_data))
				} else {
					print(paste("### No enough data for shapiro.test:",
					var1, "###"))
				}
			}else {
				print(paste("### Compute normality for", var1,  "###"))
				print(${method}(new_data))
			}
		}
	}
}

END

if [ "$execute" == "TRUE" ]; then
	Rscript ${file}${mid}.r
	if [ "$?" == "0" ]; then
		/bin/rm -f ${file}${mid}.r
	fi
fi

