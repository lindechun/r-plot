#!/bin/bash

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

This script is used to get a color scale legend for heatmap or other
types of pcitures.


${txtbld}OPTIONS${txtrst}:
	-f	Data points like '0,1,4' or '0,4'. The difference between 
		minimu and midpoint, maximum and midpoint predicts the length
		of [xcol-mcol], [mocl-ycol] in color legend. This is not the
		real value for legend, but roughly a length estimation for
		legend.
		${bldred}[NECESSARY]${txtrst}
	-l	Length for each interval like '20,100' if there are three
		points given to -f or '100' if there are two points given to -f. 
		Any number is approaite and may not affect output. 
		${blrred}[Default '20,20', optional]${txtrst}
	-P	Legend position[${txtred}Default right. Accept
		top,bottom,left,none, or c(0.08,0.8).${txtrst}]
		command for ggplot2)${txtrst}]
	-H	The height of legend.[${txred}Default system default (cm)${txtrst}]
	-W	The width of legend.[${txred}Default system default (cm)${txtrst}]
	-x	The color for representing low value.[${txtred}Default
		green${txtrst}]
	-y	The color for representing large value.[${txtred}Default
		red${txtrst}]
	-m	The color for representing mid-value.[${txtred}Default
		yellow${txtrst}]
	-w	The width of output picture.[${txtred}Default 5${txtrst}]
	-u	The height of output picture.[${txtred}Default 5${txtrst}] 
	-E	The type of output figures.[
		${txtred}Default eps (since it has the best effect), accept
		eps/ps, pdf, tex (pictex), png, jpeg, tiff, bmp, svg and wmf)${txtrst}]
	-r	The resolution of output picture.[${txtred}Default 100 ppi${txtrst}]
	-e	Execute or not[${bldred}Default TRUE${txtrst}]
	-i	Install depended packages[${bldred}Default FALSE${txtrst}]
EOF
}

points=
len_bins='20,20'
execute='TRUE'
ist='FALSE'
uwid=5
vhig=5
legend_width=0
legend_height=0
res=100
legend_pos='top'
xcol='green'
mcol='yellow'
ycol='red'
xlab='NULL'
ylab='NULL'
ext='eps'

while getopts "hf:l:P:x:m:E:y:H:W:w:u:r:e:i:" OPTION
do
	case $OPTION in
		h)
			usage
			exit 1
			;;
		f)
			points=$OPTARG
			;;
		l)
			len_bins=$OPTARG
			;;
		P)
			legend_pos=$OPTARG
			;;
		x)
			xcol=$OPTARG
			;;
		m)
			mcol=$OPTARG
			;;
		y)
			ycol=$OPTARG
			;;
		H)
			legend_height=$OPTARG
			;;
		W)
			legend_width=$OPTARG
			;;
		E)
			ext=$OPTARG
			;;
		w)
			uwid=$OPTARG
			;;
		u)
			vhig=$OPTARG
			;;
		r)
			res=$OPTARG
			;;
		e)
			execute=$OPTARG
			;;
		i)
			ist=$OPTARG
			;;
		?)
			usage
			exit 1
			;;
	esac
done

if [ -z $points ]; then
	usage
	exit 1
fi

file="colorBar"

cat <<END >${file}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

library(ggplot2)
library(reshape2)
library(grid)

points <- c(${points})
binSize <- c(${len_bins})

len_points = length(points)
len_binSize = length(binSize)

data <- c(seq(points[1], points[2], length=binSize[1]))

if (len_points == len_binSize+1){
	for(i in 2:len_binSize){
		tmp <- c(seq(points[i], points[i+1], length=binSize[i]))
		data <- c(data, tmp)
	}
} else {
	for(i in 2:len_points-1){
		tmp <- c(seq(points[i], points[i+1], length=binSize[1]))
		data <- c(data, tmp)
	}
	print("Unconsistent points and len_bins, using the the first
	element in bins as interval.")
	#quit()
}

data <- matrix(data, ncol=1)
colnames(data) <- 'value'
data <- as.data.frame(data)
data\$id <- rownames(data)
data.m <- melt(data, c('id'))
data.m\$id <- factor(data.m\$id, levels=data\$id, ordered=T) 
#print(data.m)

p <- ggplot(data.m, aes(x=variable, y=id)) + 
	geom_tile(aes(fill=value)) + xlab($xlab) + ylab($ylab) + theme_bw() +
	theme(legend.title=element_blank(),
   	panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p <- p + theme(axis.ticks.x = element_blank(),
legend.key=element_blank(), axis.text.y=element_blank()) 

if (len_points >2){
	p <- p+scale_fill_gradient2(low="$xcol", mid="$mcol",
	high="$ycol", midpoint=points[2])
}else{
	p <- p+scale_fill_gradient(low="$xcol", high="$ycol")
}

top='top'
botttom='bottom'
left='left'
right='right'
none='none'
legend_pos_par <- ${legend_pos}

p <- p + theme(legend.position=legend_pos_par)

if(${legend_width} != 0) {
	p <- p + theme(legend.key.width=unit(${legend_width}, "cm"))
} 

if(${legend_height} != 0) {
	p <- p + theme(legend.key.height=unit(${legend_height}, "cm"))
} 

ggsave(p, filename="${file}.${ext}", dpi=$res, width=$uwid,
height=$vhig, units=c("cm"))
END

if [ "$execute" == "TRUE" ]; then
	Rscript ${file}.r
if [ "$?" == "0" ]; then /bin/rm -f ${file}${mid}.r; fi
fi

