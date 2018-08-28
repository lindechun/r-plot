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

This script is used to draw venn plot using VennDiagram.

${txtbld}OPTIONS${txtrst}:
<<input>>
    -f  Data file. fileName(Default) or FALSE
    -z  Is there a header. Only be used when -m FALSE.[${bldred}Default TRUE${txtrst}]
    -F  Which file format that you input(-f). 'long'(Default) or 'wide'.
        long format: the first column is the
                     feature such as genes, microorganism, the second
                     column is sample name
        wide format: the first column is the
                     feature such as genes, microorganism, the second and thricolumn is sample name,
                     The second to last column is matrix of the presence of the first columns.
<aes>
    -a  Name for sample variable
        [${txtred}when -F long, -a will be used. when -F wide, it is variable, which is an inner name. 
    -d  Name for feature that compared among each sample.
        This parameter can only be set when -F long, 'value'(-F wide).
    -A  The order sample variable
        Default columns order, accept a string like
        "'TP16','TP22','TP23'". Pay attention to the usage of two types of quotes.

<scales>
    -n  List of numbers for venn plot (used when -f FALSE).

        For two-set venn, the format is "100, 110, 50" represents 
        (length_a, length_b,
        a_b_overlap).

        For three-set venn, the format is "100, 110, 90, 50, 40, 40, 20" 
        represents (length_a, length_b, length_c, 
        a_b_overlap,  b_c_overlap, a_c_overlap, a_b_c_overlap).  

        For four-set venn, the format is "100, 110, 90, 50, 40, 40, 20" 
        represents (length_a, length_b, length_c, 
        a_b_overlap, a_c_overlap, a_d_overlap, b_c_overlap, 
        b_d_overlap, c_d_overlap, abc_overlap, abd_overlap, 
        acd_overlap, bcd_overlap, abcd_overlap).

    -l  List of labels for venn plot (used when -z is FALSE or -f FALSE).

        Format: "'a', 'b'" for two-set and "'a', 'b', 'c'" for three-set.

        Pay attention to the order.
        Both double-quotes and single-quotes are needed.

    -c  giving the colours of the circles' areas.  Default 'rainbow'.
        1. colorspace. ['rainbow',colorspace::sequential_hcl','colorspace::','colorspace::heat_hcl','rainbow_hcl','colorspace::terrain_hcl','colorspace::diverge_hcl']
        2. single color such that "red","black","blue","transparent".
        3. Manually set color for each area.
            colorName: "'red','green','pink','blue','cyan','green','yellow'"
            RGB: "'#023FA5','#7D87B9','#BEC1D4','#E2E2E2'"
    -C  should set colours of the circles' areas type, if set -c. [1,2,3]

    -g  giving the alpha transparency of the circles' areas. Default 0.5
    -j  giving the size of the areas' labels. Default 1
    -J  Which function than set the size of the areas' labels varies with the size of area. Default NULL. ['log10','lin','NULL'].

    -k  giving the colours of the circles' circumferences. Default 'black'. you can give 'transparent' to set no color of outline.
    -m  giving the line dash pattern of the circles's circumferences. Only be used when -k is no 'transparent'. give the followding solid[Default], dashed, dotted, dotdash, longdash, twodash.${txtrst}
    -V  the number giving the line width of the circles' circumferences
        Default 1.
        Only be used when -k is no 'transparent'
    -K  the size of the category names. Default 2

<labs>
    -t  Title for the picture.

<panel>
    -w  The width of output picture.
        [${txtred}Default 6${txtrst}]
    -u  The height of output picture.
        [${txtred}Default 5${txtrst}]
    -r  The resolution of output picture.[${txtred}Default 300 ppi${txtrst}]
    -E  The format of output figures. [${txtred}Default pdf, accept eps/ps, tex (pictex), pdf, jpeg, tiff, bmp, svg and wmf)${txtrst}]

<output>
    -e  Execute or not[${bldred}Default TRUE${txtrst}]
    -o  Path of output.[Default path of data file(-f), Optinal]

Example:
    r-plot vennDiagram -f prefix -F TRUE -n "120, 110, 50"  -l "'a','b'"
    r-plot vennDiagram -f file -a h3k27ac -b ctcf

    r-plot venn -f ../../data/vennData_wide.txt -F wide -o ./ -m dotted -k transparent
    
EOF
}

#input
file='FALSE'
header='TRUE'
dataFormat='long'

#aes
sample='aAbBcC'
feature='aAbBcC'
sample_level=''

#scales
numList=''
labelList=''
# color_v='"dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"'

fill_color='rainbow'
fill_type='1'
alpha=0.5
area_label_size=1
areas_labels_function='NULL'
outline_color="black"
outline_type="solid"
lwd_size=1
category_label_size=2

#labs
title=""

#panel
uwid=8
vhig=6
res=300
ext='pdf'

#output
execute='TRUE'
output=''

line_size=1

while getopts "hf:z:F:a:d:A:n:l:c:C:g:j:J:k:m:V:K:t:w:u:r:E:e:o:" OPTION
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
        z)
            header=$OPTARG
            ;;
        F)
            dataFormat=$OPTARG
            ;;
        #aes
        a)
            sample=$OPTARG
            ;;
        d)
            feature=$OPTARG
            ;;
        A)
            sample_level=$OPTARG
            ;;
        #scales
        n)
            numList=$OPTARG
            ;;
        l)
            labelList=$OPTARG
            ;;
        c)
            fill_color=$OPTARG
            ;;
        C)
            fill_type=$OPTARG
            ;;
        g)
            alpha=$OPTARG
            ;;
        j)
            area_label_size=$OPTARG
            ;;
        J)
            areas_labels_function=$OPTARG
            ;;
        k)
            outline_color=$OPTARG
            ;;
        m)
            outline_type=$OPTARG
            ;;
        V)
            lwd_size=$OPTARG
            ;;
        K)
            category_label_size=$OPTARG
            ;;
        #labs
        t)
            title=$OPTARG
            ;;
        #panel
        w)
            uwid=$OPTARG
            ;;
        u)
            vhig=$OPTARG
            ;;
        r)
            res=$OPTARG
            ;;
        E)
            ext=$OPTARG
            ;;
        e)
            execute=$OPTARG
            ;;
        #output
        o)
            output=$OPTARG
            ;;
        ?)
            usage
            exit 1
            ;;
    esac
done

if test "$file" == "FALSE" && test "$numList" == ""; then
    echo "If -f FALSE, -n should be set."
    usage
    exit 1
fi

if test "$file" != "FALSE" && test "$dataFormat" == "long" && test "$sample" == "aAbBcC"; then
    echo "If -f $file and -F long, -a should be set"
    exit 1
fi

if test "$file" != "FALSE" && test "$dataFormat" == "long" && test "$feature" == "aAbBcC"; then
    echo "If -f $file and -F long, -d should be set"
    exit 1
fi

if test "$file" == "FALSE";then
    if test "$fill_color" == "rainbow";then
        fill_type=1
    fi

    if test "$fill_color" != "rainbow" && test "$fill_type" == "1"; then
        echo "Because -f FALSE, -C isn't allow set 1"
        exit 1
    fi
fi

if test "$areas_labels_function" != "NULL";then
    cexPropFlag="'"
else
    cexPropFlag=
fi

## Prefix of picture file
if test "$file" == "FALSE";then
	mid='vennDiagram'
else
	mid='.vennDiagram'
fi

if test "$fill_type" == '1';then
    mid=$mid'.fillType_colorspace'
elif test "$fill_type" == '2';then
    mid=$mid'.fillType_singleColor'
elif test "$fill_type" == '3';then
    mid=$mid'.fillType_moreColor'
fi

if test "$outline_color" != "transparent";then
    mid=$mid'.lineColor_'$outline_color
    mid=$mid'.lineType_'$outline_type
fi

if test "$areas_labels_function" != "NULL";then
    mid=$mid'.areaLabelFunction_'$areas_labels_function
fi

mid=$mid'.areaLabelSize_'$area_label_size

# if test "$file" == "FALSE";then
#     mid=$mid'.'$fill_type
# fi

if test -z "$output";then
    output=$(cd `dirname $file`;pwd)
fi

cat <<END >$output/$(basename $file)${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

library(VennDiagram)
library(reshape2)

if ("$areas_labels_function" != 'NULL'){
    source('$(cd `dirname $0`; pwd)/VennDiagram_revision.R')
}

# don't write log file for VennDiagram
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

### width and height of output
w_auto <- 0; h_auto <- 0

## print width and height of picture
PrintWD($uwid,$vhig,w_auto,h_auto,FALSE)

if ("${file}" != "FALSE") {
    #####   venn plot for given numbers   #####

    data_m <- read.table(file="$file", sep="\t", header=$header, quote="",stringsAsFactors=F)

    if ("$dataFormat" == "wide"){
        data_m\$ID <- rownames(data_m)
        data_m <- melt(data_m)
        data_m <- subset(data_m,value!=0)

    } else if ("$dataFormat" == "long"){
        data_m\$variable <- data_m\$$sample
        data_m\$ID <- data_m\$$feature
    }

    if("$sample_level" != ""){
        data_m\$variable <- factor(data_m\$variable, levels=c($sample_level), ordered=T)
    }

    ## set number
    setNumber <- nlevels(data_m\$variable)

    if(nlevels(data_m\$variable) > 5){
        stop("This script only support 5-set.")
    }else{
        venn_vector_list <- list()
        for (i in unique(data_m\$variable)){
            temp_data <- subset(data_m,variable == i)
            venn_vector_list[[i]]<-temp_data\$ID
        }
    }

    ##output
    if ("${ext}" == "png") {
        png(filename='$output/$(basename $file)${mid}.${ext}', width=$uwid, height=$vhig,
        res=$res, units="cm")
    } else if ("${ext}" == "eps") {
        postscript(file='$output/$(basename $file)${mid}.${ext}', onefile=FALSE, horizontal=FALSE, 
        paper="special", width=$uwid, height = $vhig, pointsize=10)
    } else if ("${ext}" == "pdf") {
        pdf(file='$output/$(basename $file)${mid}.${ext}',width=$uwid,$vhig, onefile=FALSE, 
        paper="special")
    }else if ("${ext}" == "svg") {
        svg(filename='$output/$(basename $file)${mid}.${ext}', width=$uwid, height=$vhig,
        pointsize=10)
    } else {
        print("This format is currently unsupported. Please check the file <Rplots.pdf> in current directory.")
    }

    ## layer
    if($fill_type == 1){
        p <- venn.diagram(venn_vector_list,
                filename=NULL,
                fill=${fill_color}(setNumber),
                col="$outline_color",
                lwd=$lwd_size,alpha=$alpha,
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = $area_label_size,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
            )
    }else if($fill_type == 2){
        p <- venn.diagram(venn_vector_list,
                filename=NULL,
                fill="$fill_color",
                col="$outline_color",
                lwd=$lwd_size,
                alpha=$alpha,
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = $area_label_size,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
            )
    }else if($fill_type == 3){
        p <- venn.diagram(venn_vector_list,
                filename=NULL,
                fill=c($fill_color),
                col="$outline_color",
                lwd=$lwd_size,alpha=$alpha,
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = $area_label_size,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
            )
    }

    grid.draw(p)
    dev.off()

    #output
	# Plot_savefig('$output/$(basename $file)${mid}.${ext}', p, $uwid, $vhig, w_auto, h_auto, $res, FALSE,ftype='${ext}')

} else {
    #####   venn plot for given numbers   #####

    numList <- c(${numList})
    labelList <- c(${labelList})

    num <- length(labelList)

    if(length(num) > 5){
        stop("This script only support 5-set.")
    }

    if ("${ext}" == "png") {
        png(filename=paste("${mid}_",num,"Set.png",sep=""), width=$uwid, height=$vhig,
        res=$res, units="cm")
    } else if ("${ext}" == "eps") {
        postscript(file=paste("${mid}_",num,"Set.eps",sep=""), onefile=FALSE, horizontal=FALSE, 
        paper="special", width=$uwid, height = $vhig, pointsize=10)
    } else if ("${ext}" == "pdf") {
        pdf(file=paste("${mid}_",num,"Set.pdf",sep=""),width=$uwid,$vhig, onefile=FALSE, 
        paper="special")
    }else if ("${ext}" == "svg") {
        svg(filename=paste("${mid}_",num,"Set.svg",sep=""), width=$uwid, height=$vhig,
        pointsize=10)
    } else {
        print("This format is currently unsupported. Please check the file <Rplots.pdf> in current directory.")
    }

    if (num==2) {
        if ($fill_type == 1){
            draw.pairwise.venn(area1=numList[1],
                area2=numList[2],
                cross.area=numList[3],
                category=labelList,
                col="$outline_color",
                lwd=$lwd_size,
                alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
                )
        } else if($fill_type == 2){
            draw.pairwise.venn(area1=numList[1],
                area2=numList[2],
                cross.area=numList[3],
                category=labelList,
                fill="${fill_color}",
                col="$outline_color",
                lwd=$lwd_size,
                alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
                )
        } else if($fill_type == 3){
            draw.pairwise.venn(area1=numList[1],
                area2=numList[2],
                cross.area=numList[3],
                category=labelList,
                fill=c(${fill_color}),
                col="$outline_color",
                lwd=$lwd_size,
                alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
                )
        }

    } else if (num==3) {
        if ($fill_type == 1){
            draw.triple.venn(area1=numList[1],
                area2=numList[2],
                area3=numList[3],
                n12=numList[4],
                n23=numList[5],
                n13=numList[6],
                n123=numList[7],
                category=labelList,
                col="$outline_color",
                lwd=$lwd_size,
                alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                reverse=FALSE,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
            )
        } else if($fill_type == 2){
            draw.triple.venn(area1=numList[1],
                area2=numList[2],
                area3=numList[3],
                n12=numList[4],
                n23=numList[5],
                n13=numList[6],
                n123=numList[7],
                category=labelList,
                col="$outline_color",
                fill="$fill_color",
                lwd=$lwd_size,
                alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                reverse=FALSE,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
            )
        } else if($fill_type == 3){
            draw.triple.venn(area1=numList[1],
                area2=numList[2],
                area3=numList[3],
                n12=numList[4],
                n23=numList[5],
                n13=numList[6],
                n123=numList[7],
                category=labelList,
                col="$outline_color",
                fill=c($fill_color),
                lwd=$lwd_size,
                alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                reverse=FALSE,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
            )
        }

    }else if (num==4) {
        if ($fill_type == 1){
            draw.quad.venn(area1=numList[1],
                area2=numList[2],
                area3=numList[3], 
                area4=numList[4], 
                n12=numList[5], 
                n13=numList[6],
                n14=numList[7], 
                n23=numList[8],
                n24=numList[9], 
                n34=numList[10], 
                n123=numList[11], 
                n124=numList[12], 
                n134=numList[13],
                n234=numList[14], 
                n1234=numList[15],
                category=labelList,
                col="$outline_color",
                lwd=$lwd_size,alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                reverse=FALSE,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
                )
        } else if ($fill_type == 2){
            draw.quad.venn(area1=numList[1],
                area2=numList[2],
                area3=numList[3], 
                area4=numList[4], 
                n12=numList[5], 
                n13=numList[6],
                n14=numList[7], 
                n23=numList[8],
                n24=numList[9], 
                n34=numList[10], 
                n123=numList[11], 
                n124=numList[12], 
                n134=numList[13],
                n234=numList[14], 
                n1234=numList[15],
                category=labelList,
                col="$outline_color",
                fill="c($fill_color)",
                lwd=$lwd_size,alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                reverse=FALSE,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
                )
        } else if($fill_type == 3){
            draw.quad.venn(area1=numList[1],
                area2=numList[2],
                area3=numList[3], 
                area4=numList[4], 
                n12=numList[5], 
                n13=numList[6],
                n14=numList[7], 
                n23=numList[8],
                n24=numList[9], 
                n34=numList[10], 
                n123=numList[11], 
                n124=numList[12], 
                n134=numList[13],
                n234=numList[14], 
                n1234=numList[15],
                category=labelList,
                col="$outline_color",
                fill=c($fill_color),
                lwd=$lwd_size,alpha="$alpha",
                lty="$outline_type",
                fontfamily="Times",
                cat.fontfamily = "Times",
                label.col = c("black"),
                cat.cex=$category_label_size,
                cex = "$area_label_size",
                reverse=FALSE,
                cex.prop=${cexPropFlag}${areas_labels_function}${cexPropFlag}
                )
        }
    }
    dev.off()

}



END

if [ "$execute" == "TRUE" ]; then
    Rscript $output/$(basename $file)${mid}.r
    if [ "$?" == "0" ]; then
        /bin/rm -f $output/$(basename $file)${mid}.r
    fi
fi
