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

This script is used to do hcluster using package ggplot2 and hclust fucntion.

The parameters for logical variable are either TRUE or FALSE.

${txtbld}OPTIONS${txtrst}:
	-f	Data file (with header line, the first column is the
 		rowname, tab seperated. Colnames must be unique unless you
		know what you are doing.)${bldred}[NECESSARY]${txtrst}
	-t	Title of picture[${txtred}Default empty title${txtrst}]
		[Scatter plot of horizontal and vertical variable]
	-H	Cluster tree shown in horizontal format.
		${bldred}Default TRUE meaning vertical tree, accept FALSE${txtrst}
	-c	Clustering method, Default "complete". 
		Accept "ward.D", "ward.D2","single", "average" (=UPGMA), 
		"mcquitty" (=WPGMA), "median" (=WPGMC) or "centroid" (=UPGMC)
	-C	Color vector. 
		${bldred}Default pheatmap_default. Aceept a vector containing
		multiple colors such as <c("white", "blue")> or 
		a R function generating a list of colors.${txtrst}
	-I	Clustering distance method for cols.
		${bldred}Default 'correlation', accept 'euclidean', 
		"manhattan", "maximum", "canberra", "binary", "minkowski". ${txtrst}
	-L	First get log-value, then do other analysis.
		Accept an R function log2 or log10. 
		${bldred}[Default FALSE]${txtrst}
	-d	Scale the data or not for clustering and visualization.
		[Default 'none' means no scale, accept 'row', 'column' to 
		scale by row or column.]
	-Q	A file to specify col-annotation.[${txtred}Default NA${txtrst}]
	-u	The width of output picture.[${txtred}Default 20${txtrst}]
	-v	The height of output picture.[${txtred}Default 20${txtrst}] 
	-E	The type of output figures.[${txtred}Default pdf, accept
		eps/ps, tex (pictex), png, jpeg, tiff, bmp, svg and wmf)${txtrst}]
	-r	The resolution of output picture.[${txtred}Default 300 ppi${txtrst}]
	-F	Font size [${txtred}Default 14${txtrst}]
	-e	Execute or not[${bldred}Default TRUE${txtrst}]
	-i	Install the required packages[${bldred}Default FALSE${txtrst}]

Example: sp_hcluster_gg.sh -f matrix.pearson.xls -Q cluster.txt

EOF
}

file=''
title=''
horizontal='TRUE'
clustering_distance_cols='correlation'
clustering_method='complete'
legend_breaks='NA'
color_vector='colorRampPalette(rev(brewer.pal(n=7, name="RdYlBu")))(100)'
width=''
label=''
logv='FALSE'
kclu='NA'
scale='none'
execute='TRUE'
ist='FALSE'
legend=' '
na_color='grey'
uwid=6
vhig=4
res=300
fontsize=14
ext='pdf'
colormodel='srgb'
xcol='green'
ycol='red'
mcol='yellow'
mid_value_use='FALSE'
mid_value='Inf'
xtics='TRUE'
xtics_angle=270
ytics='TRUE'
gradient=1
givenSepartor=''
gradientC="'green','yellow','red'"
generateNA='FALSE'
digits='FALSE'
annotation_row='NA'
annotation_col='NA'
preprocess='TRUE'

while getopts "hf:t:a:A:b:H:R:c:D:p:I:L:d:k:u:v:E:r:F:P:Q:x:y:M:Z:X:s:m:N:Y:G:C:O:e:i:" OPTION
do
	case $OPTION in
		h)
			echo "Help mesage"
			usage
			exit 1
			;;
		f)
			file=$OPTARG
			;;
		t)
			title=$OPTARG
			;;
		a)
			xtics=$OPTARG
			;;
		A)
			xtics_angle=$OPTARG
			;;
		b)
			ytics=$OPTARG
			;;
		H)
			horizontal=$OPTARG
			;;
		R)
			cluster_rows=$OPTARG
			;;
		c)
			clustering_method=$OPTARG
			;;
		D)
			clustering_distance_rows=$OPTARG
			;;
		I)
			clustering_distance_cols=$OPTARG
			;;
		p)
			preprocess=$OPTARG
			;;
		L)
			logv=$OPTARG
			;;
		P)
			annotation_row=$OPTARG
			;;
		Q)
			annotation_col=$OPTARG
			;;
		d)
			scale=$OPTARG
			;;
		k)
			kclu=$OPTARG
			;;
		u)
			uwid=$OPTARG
			;;
		v)
			vhig=$OPTARG
			;;
		E)
			ext=$OPTARG
			;;
		r)
			res=$OPTARG
			;;
		F)
			fontsize=$OPTARG
			;;
		x)
			xcol=$OPTARG
			;;
		y)
			ycol=$OPTARG
			;;
		M)
			mcol=$OPTARG
			;;
		K)
			logv_pos=$OPTARG
			;;
		Z)
			mid_value_use=$OPTARG
			;;
		X)
			mid_value=$OPTARG
			;;
		s)
			small=$OPTARG
			;;
		m)
			maximum=$OPTARG
			;;
		N)
			generateNA=$OPTARG
			;;
		Y)
			na_color=$OPTARG
			;;
		G)
			gradient=$OPTARG
			;;
		C)
			color_vector=$OPTARG
			;;
		O)
			givenSepartor=$OPTARG
			;;
		e)
			execute=$OPTARG
			;;
		i)
			ist=$OPTARG
			;;
		?)
			usage
			echo "Unknown parameters"
			exit 1
			;;
	esac
done

mid=".hcluster"

if [ -z $file ] ; then
	echo 1>&2 "Please give filename."
	usage
	exit 1
fi


if test "$log" != ''; then
	mid=${mid}".$log"
fi

if test "${scale}" == "TRUE"; then
	mid=${mid}".scale"
fi

cat <<END >${file}${mid}.r

library(ggplot2)
library(ggdendro)
library(amap)

if($gradient){
	library(RColorBrewer)
}

data <- read.table(file="$file", sep="\t", header=T, row.names=1,
	check.names=F, quote="", comment="")

if ("${annotation_col}" != "NA") {
	annotation_col <- read.table(file="${annotation_col}", header=T,
		row.names=1, sep="\t", quote="", check.names=F, comment="")
	levs <- unique(unlist(lapply(annotation_col, unique)))
	annotation_col <- data.frame(lapply(annotation_col, factor,
		levels=levs), row.names=rownames(annotation_col))
} else {
	annotation_col <- NA
}

hc <- hcluster(data)
dhc <- as.dendrogram(hc)
dhc_data <- dendro_data(dhc, type="rectangle")

p <- ggplot(segment(dhc_data)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))

if ("${annotation_col}" != "NA") {
	if (${horizontal}){
		p <- p + geom_text(data=dhc_data\$labels, aes(x,y,label=label, 
			color=annotation_col[,1]), angle=0, size=3, hjust=0) + coord_flip() + scale_y_reverse(expand=c(0.2, 0))
	}else {
		p <- p + geom_text(data=dhc_data\$labels, aes(x,y,label=label, 
			color=annotation_col[,1]), angle=270, size=3, vjust=0.5,hjust=-0.1)
	}
} else {
	if (${horizontal}){
		p <- p + geom_text(data=dhc_data\$labels, aes(x,y,label=label), 
			angle=0, size=3, hjust=0) + coord_flip() + scale_y_reverse(expand=c(0.2, 0))
	}else {
		p <- p + geom_text(data=dhc_data\$labels, aes(x,y,label=label), 
			angle=270, size=3, vjust=0.5,hjust=-0.1)
	}
}

p <- p+ theme(axis.line.y=element_blank(),
	axis.ticks=element_blank(), 
	axis.text=element_blank(), 
	axis.title.y=element_blank(), 
	panel.background=element_rect(fill="white"), 
	panel.grid=element_blank(), 
	legend.position="top"
	)+labs(x='',y='')


if ("${ext}" == "pdf") {
	if (${horizontal}){
		ggsave(p, filename="${file}${mid}.${ext}", dpi=$res, width=$uwid, height=$vhig, units=c("in"),colormodel="${colormodel}")
	} else{
		ggsave(p, filename="${file}${mid}.${ext}", dpi=$res, width=$vhig, height=$uwid, units=c("in"),colormodel="${colormodel}")
	}
} else {
	if(${horizontal}){
		ggsave(p, filename="${file}${mid}.${ext}", dpi=$res, width=$uwid, height=$vhig, units=c("in"))
	}else{
		ggsave(p, filename="${file}${mid}.${ext}", dpi=$res, width=$vhig, height=$uwid, units=c("in"))
	}
}


END


if [ "$execute" == "TRUE" ]; then
	Rscript ${file}${mid}.r
	if [ "$?" == "0" ]; then 
		/bin/rm -f ${file}${mid}.r
		/bin/rm -f Rplots.pdf	
	fi
fi