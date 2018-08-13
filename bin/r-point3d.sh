#!/bin/bash

#set -x

usage()
{
cat <<EOF
${txtcyn}

***CREATED BY Lin Dechun (lindechun@genomics.com)***

Usage:

$0 options${txtrst}

${bldblu}Function${txtrst}:

This script is used to do 3D scatter plot using scatterplot3d. 

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
        tell the group information for each sample. only can be used when <-m FALSE>. When <-Q> is given, <-F> and <-a> should be one of the column
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
    -l  The order for x-axis variable

    -d  Name for y-axis variable the digital values, such as 'H3K27ac'.
        ${bldred}[Default "value" represents the column named "value".
        This parameter can only be set when -m is TRUE.]${txtrst}
    -z  Name for z-axis variable the digital values.

    -c  Name of color variable in data to map to plot.
    -C  Manually set the order of color variable
    -s  Name of size variable in data to map to plot.
    -S  Manually set the order of size variable
    -i  Name of shape variable in data to map to plot.
    -I  Manually set the order of shape variable

<scales>
    # -b  Rotation angle for x-axis value(anti clockwise)
    #     [Default 0]
    # -s  Scale y axis
    #     [${txtred}Default FALSE. Accept the following
    #     log10[default], log2, or other formula.
    #     Also if the supplied number after -S is not 0, this
    #     parameter will be set to TRUE${txtrst}]
    # -S  A number to add y axis（to -d) before log-transform to avoid log(0).
    #     [${txtred}Default 0. If a non-zero number is given, -s is
    #     TRUE.${txtrst}]
    -C  Manually specified colors.
        [Default system default.
        Accept string in format like <"'green', 'red'"> (both types of quotes needed).]
    -b  Colors of points in the plot. Default 'black'. if you set -c, -b isn't use.
    -D  Size of points in the plot. Default 1. if you set -c, -D isn't use.
    -g  Shape of points in the plot. Default 16. if you set -c, -g isn't use.

<labs>
    -t  Title of picture[${txtred}Default empty title${txtrst}]
        [Scatter plot of horizontal and vertical variable]
    -x  xlab of picture[${txtred}Default name for -a${txtrst}]
    -y  ylab of picture[${txtred}Default nane for -d${txtrst}]

	-F	Scale data for plot analysis.
		Often, we would normalize data.
		Only when we care about the real number changes other than the trends, 
		we do not need scale. When this happens, we expect the changin ranges 
		of data is small for example log-transformed data.	
		${bldred}[Default FALSE.]${txtrst}

	-A	Transparency value for points.
		${bldred}[Optional, such as a number or 
		a variable indicating one data column, 
		normally should be number column]${txtrst}
	-L	Label points (using geom_text_repel).
		${bldred}[Default no-label (FALSE), accept TRUE]${txtrst}
	-N	Label font size.
		${bldred}[Default system default. Accept numbers less than 5 to shrink fonts.]${txtrst}
	-Q	Point hjust.
		${bldred}[Default 0, accept a positive (at leftt) and negative value (at right)]${txtrst}
	-d	If you may want to specifize the order of
		other variables (default alphabetically), please supply a string like below.
		[${txtred}Accept sth like 
		(one level one sentence, separate by';') 
		data\$size <- factor(data\$size, levels=c("l1",
		"l2",...,"l10"), ordered=T) ${txtrst}]

<panel>
    -w  The width of output picture.
        [${txtred}Default auto calculate${txtrst}]
    -u  The height of output picture.
        [${txtred}Default auto calculate${txtrst}]
    -P  [Uppercase P] Legend position
        Default right. Accept top, bottom, left,none.
    -p  Legend inset. Default 0. Aceept -1~1.
    -r  The resolution of output picture.[${txtred}Default 300 ppi${txtrst}]
    -E  The format of output figures. [${txtred}Default pdf, accept eps/ps, tex (pictex), pdf, jpeg, tiff, bmp, svg and wmf)${txtrst}]

<output>
	-e	Execute or not[${bldred}Default TRUE${txtrst}]
	-o	Path of output.[Default path of data file(-f), Optinal]

<<self-point3d>>
    -T character indicating the type of plot: "p" for points, "l" for lines, "h" for vertical lines to x-y-plane, etc.
    -B whether add box of plot. Default TRUE.
    -G logical value indicating whether y and z grid should be drawn on the plot.[x(Default), FALSE, xyz]
    -F whether add fitted plane. Default FALSE. [lm, glm, FALSE]
    -J whether add Residual Lines. Default FALSE. if you set -F lm/glm, it will be used
    -R angle between x and y axis (Attention: result depends on scaling). Default 40

example:
    r-plot point3d -f ../../data/mtcars.txt -m TRUE -a wt -d disp -z mpg -o ./ -t "mtcars" -G xyz -B FALSE
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
y_variable=''
z_variable=''

x_level=""

color_variable='c_t_c_t0304'
color_level=""

size_variable="c_t_c_t0304"
size_level=""

shape_variable="c_t_c_t0304"
shape_level=""

alpha_variable="c_t_c_t0304"
alpha_level=""

#labs
title=''
xlab=' '
ylab=' '
zlab=' '

#scales
color_val_manual='FALSE'
color_var="'black'"
size_var='1'
shape_var='16'

#panel
uwid='7'
vhig='7'
legend_position='right'
legend_inset=0
ext='pdf'
res=300

#output
execute='TRUE'
output=''

## self-point3d
plot_type='p'
whether_box='TRUE'
grid_x_xyz='x'
fitted_plane="c_t_c_t0304"
segment='FALSE'
angle='40'

while getopts "hf:m:Q:H:q:K:a:d:l:z:c:C:s:S:i:I:t:x:y:j:b:D:g:w:u:P:p:r:E:e:o:T:B:G:F:J:R:" OPTION
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
        H)
            header=$OPTARG
            ;;
        q)
            gene=$OPTARG
            ;;
        K)
            skip=$OPTARG
            ;;
        #aes
        a)
            x_variable=$OPTARG
            ;;
        d)
            y_variable=$OPTARG
            ;;
        l)
            x_level=$OPTARG
            ;;
        z)
            z_variable=$OPTARG
            ;;
        c)
            color_variable=$OPTARG
            ;;
        C)
            color_level=$OPTARG
            ;;
        s)
            size_variable=$OPTARG
            ;;
        S)
            size_level=$OPTARG
            ;;
        i)
            shape_variable=$OPTARG
            ;;
        I)
            shape_level=$OPTARG
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
        # scales
        j)
            color_val_manual=$OPTARG
            ;;
        b)
            color_var=$OPTARG
            ;;
        D)
            size_var=$OPTARG
            ;;
        g)
            shape_var=$OPTARG
            ;;
        #panel
        w)
            uwid=$OPTARG
            ;;
        u)
            vhig=$OPTARG
            ;;
        P)
            legend_position=$OPTARG
            ;;
        p)
            legend_inset=$OPTARG
            ;;
        r)
            ext=$OPTARG
            ;;
        E)
            res=$OPTARG
            ;;
        #output
        e)
            execute=$OPTARG
            ;;
        o)
            output=$OPTARG
            ;;
        ## self-point3d
        T)
            plot_type=$OPTARG
            ;;
        B)
            whether_box=$OPTARG
            ;;
        G)
            grid_x_xyz=$OPTARG
            ;;
        F)
            fitted_plane=$OPTARG
            ;;
        J)
            segment=$OPTARG
            ;;
        R)
            angle=$OPTARG
            ;;
		?)
			usage
			exit 1
			;;
	esac
done

if [ -z $file ] ; then
	echo 1>&2 "Please give filename."
	usage
	exit 1
fi

if test "$melted" == "FALSE"; then
    if [ -z $x_variable ]; then
        x_variable="variable"
    fi
    if [ -z $y_variable ]; then
        y_variable="value"
    fi
    if [ -z $z_variable ]; then
        echo "Error: -z isn't NULL"
        exit 1
    fi
fi

if test "$melted" == "TRUE" && test "${x_variable}" == ""; then
    echo "If '-m TRUE', -a should be set"
    exit 1
fi

if test "$melted" == "TRUE" && test "${y_variable}" == ""; then
    echo "If '-m TRUE', -d should be set"
    exit 1
fi

if test "$melted" == "TRUE" && test "${z_variable}" == ""; then
    echo "If '-m TRUE', -z should be set"
    exit 1
fi

if test "$grid_x_xyz" != 'FALSE' && test "$grid_x_xyz" != 'x' && test "$grid_x_xyz" != 'xyz';then
    echo "grid shoulb be one of FALSE, x, xyz"
    exit 1
fi


if test "$xlab" == " ";then
    xlab=$x_variable
fi

if test "$ylab" == " ";then
    ylab=$y_variable
fi

if test "$zlab" == " ";then
    zlab=$z_variable
fi

######### temp_level_relative_val
if test "$color_variable" != "c_t_c_t0304";then
    color_var="as.numeric(data_m\$$color_variable)"
fi

if test "$shape_variable" != "c_t_c_t0304";then
    shape_var="as.numeric(data_m\$$shape_variable)"
fi

if test "$size_variable" != "c_t_c_t0304";then
    size_var="as.numeric(data_m\$$size_variable)"
fi

mid=$mid".point3d"

mid=$mid".x_"${x_variable}".y_"${y_variable}".z_"${z_variable}

if test "$color_variable" != "c_t_c_t0304";then
    mid=$mid'.color_'${color_variable}
fi

if test "$size_variable" != "c_t_c_t0304";then
    mid=$mid'.size_'${size_variable}
fi

if test "$shape_variable" != "c_t_c_t0304";then
    mid=$mid'.shape_'${shape_variable}
fi

if test "$fitted_plane" != "c_t_c_t0304";then
    mid=$mid".fittedModel_"$fitted_plane
fi

if test "$segment" != "FALSE";then
    mid=$mid".Residual_Lines"
fi

if test "$plot_type" != 'p';then
    mid=$mid".element_"$plot_type
fi

if test "$whether_box" != 'TRUE';then
    mid=$mid".nobox"
fi

if test "$grid_x_xyz" == 'FALSE';then
    mid=$mid".noGrid"
elif test "$grid_x_xyz" == 'xyz';then
    mid=$mid".Grid_xyz"
fi

## Prefix of picture file
if test -z "$output";then
	output=$(cd `dirname $file`;pwd)
fi

cat <<END >$output/$(basename $file)${mid}.r
source('$(cd `dirname $0`; pwd)/rFunction.R')

library(scatterplot3d)

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
    data_m <- data_m[data_m\$$y_variable !=0 & data_m\$$x_variable & data\$$z_variable,]

} else {
    data_m <- read.table(file="$file", sep="\t",
    header=$header, quote="")
}

###########                  read file                 ###########

############### x_variable, color, size, shape ###############

## x_variable levels
x_level <- c(${x_level})
if (length(x_level) >1){
    data_m\$${x_variable} <- factor(data_m\$${x_variable}, levels=x_level, ordered=T)
}

## color levels
color_level <- c(${color_level})
if ("$color_variable" != "c_t_c_t0304"){
    if (length(color_level) >1){
        data_m\$${color_variable} <- factor(data_m\$${color_variable}, levels=color_level, ordered=T)
    }else{
        data_m\$${color_variable} <- factor(data_m\$${color_variable})
    }
}

## size levels
size_level <- c(${size_level})
if ("$size_variable" != "c_t_c_t0304"){
    if (length(size_level) >1){
        data_m\$${size_variable} <- factor(data_m\$${size_variable}, levels=size_level, ordered=T)
    }else{
        data_m\$${size_variable} <- factor(data_m\$${size_variable})
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
}

############### x_variable, color, size, shape ###############

###############        layer       ###############

# pdf("$output/$(basename $file)${mid}.${ext}")
if ("${ext}" == "png") {
    png(filename="$output/$(basename $file)${mid}.${ext}", width=$uwid, height=$vhig,
    res=$res, units="cm")
} else if ("${ext}" == "eps") {
    postscript(file="$output/$(basename $file)${mid}.${ext}", onefile=FALSE, horizontal=FALSE, 
    paper="special", width=$uwid, height = $vhig, pointsize=10)
} else if ("${ext}" == "pdf") {
    pdf(file="$output/$(basename $file)${mid}.${ext}",width=$uwid,$vhig, onefile=FALSE,
    paper="special")
}else if ("${ext}" == "svg") {
    svg(filename="$output/$(basename $file)${mid}.${ext}", width=$uwid, height=$vhig,
    pointsize=10)
} else {
    print("This format is currently unsupported. Please check the file <Rplots.pdf> in current directory.")
}

# basic

if ("$grid_x_xyz" == "FALSE"){
    s3d <- scatterplot3d(
        x=data_m\$$x_variable,
        y=data_m\$$y_variable,
        z=data_m\$$z_variable,
        pch=$shape_var,
        color=$color_var,
        cex.symbols=$size_var,
        cex.lab=1.2,
        main="$title",
        xlab="$xlab",
        ylab="$ylab",
        zlab="$zlab",
        type='$plot_type',
        grid=FALSE,
        box=$whether_box,
        angle=$angle
        )

}else if("$grid_x_xyz" == "x"){
    s3d <- scatterplot3d(
        x=data_m\$$x_variable,
        y=data_m\$$y_variable,
        z=data_m\$$z_variable,
        pch=$shape_var,
        color=$color_var,
        cex.symbols=$size_var,
        cex.lab=1.2,
        main="$title",
        xlab="$xlab",
        ylab="$ylab",
        zlab="$zlab",
        type='$plot_type',
        grid=TRUE,
        box=$whether_box,
        angle=$angle,
        )
}else if ("$grid_x_xyz" == "xyz"){
    # 1. Empty 3D scatter plot using pch=""
    source('$(cd `dirname $0`; pwd)/addgrids3d.R')

    s3d <- scatterplot3d(
        x=data_m\$$x_variable,
        y=data_m\$$y_variable,
        z=data_m\$$z_variable,
        color=$color_var,
        pch=$shape_var,
        cex.symbols=$size_var,
        cex.lab=1.2,
        main="$title",
        xlab="$xlab",
        ylab="$ylab",
        zlab="$zlab",
        type='$plot_type',
        grid=FALSE,
        box=$whether_box,
        angle=$angle
        )

    # 2. Add grids
    addgrids3d(
        x=data_m\$$x_variable,
        y=data_m\$$y_variable,
        z=data_m\$$z_variable,
        grid = c("xy", "xz", "yz"),
        angle=$angle
        )

    # 3. Add points
    # s3d\$points3d(
    #     x=data_m\$$x_variable,
    #     y=data_m\$$y_variable,
    #     z=data_m\$$z_variable,
    #     type='$plot_type'
    #     )
}

# Add regression plane
Fitted <- function(dat){
        fitted_plane_temp <- $fitted_plane(dat\$$z_variable ~ dat\$$x_variable + dat\$$y_variable)

        s3d\$plane3d(fitted_plane_temp)

        ## add segment from points to fitted plane
        if ("$segment" != "FALSE"){
            obser2d <- s3d\$xyz.convert(dat\$$x_variable, dat\$$y_variable, dat\$$z_variable)
            pred2d <- s3d\$xyz.convert(dat\$$x_variable, dat\$$y_variable, fitted(fitted_plane_temp))
            segments(obser2d\$x, obser2d\$y, pred2d\$x, pred2d\$y, lty = 4)
        }
}

if("$fitted_plane" != "c_t_c_t0304"){
    if ("$color_variable" == "c_t_c_t0304"){
        Fitted(data_m)
    }else{
        for (i in unique(data_m\$$color_variable)){
            dat_temp <- subset(data_m, $color_variable == i)
            Fitted(dat_temp)
        }
    }
}

# legend

if("$legend_position" != "none" & "$color_variable" != "c_t_c_t0304"){
    legend("$legend_position",
        legend = levels(data_m\$$color_variable),
        col =  $color_var,
        pch = $shape_var,
        inset = $legend_inset,
        xpd = TRUE,
        horiz = TRUE
        )
}

dev.off()


## print width and height of picture
w_auto <- 0; h_auto <- 0
PrintWD($uwid,$vhig,w_auto,h_auto,FALSE)

###############        layer       ###############

END

if [ "$execute" == "TRUE" ]; then
    Rscript $output/$(basename $file)${mid}.r
    if [ "$?" == "0" ]; then
     /bin/rm -f $output/$(basename $file)${mid}.r
    fi
fi
