#!/bin/bash

# set -x 

filename=`basename $0`

usage()
{
cat <<EOF
${txtcyn}
Usage:

${filename} options${txtrst}

${bldblu}Function${txtrst}:

This software is designed to simply the process of plotting and help
researchers focus more on data rather than technology.

Currently, the following types of plot are supported.

#### Bars
${filename} barplot
${filename} colorLegend

#### Dots
${filename} point
${filename} point3d
${filename} pca

#### Boxplot
${filename} boxplot

#### Histogram
${filename} histogram

#### Areaplot
${filename} areaplot

#### Density
${filename} density
${filename} density2d

#### Cluster
${filename} hcluster

${filename} pheatmap

#### Pie
${filename} pie

#### Venn
${filename} venn

#### Other
${filename} enrichmentPlot
${filename} png2eps

EOF
}

if test $# -lt 1; then
	usage
	exit 1
fi

program="r-"$1".sh"
type ${program} >/dev/null 2>&1
error=$?
if test $error != 0; then
	usage
	echo "**Please check the program name input**"
	exit 1
else
	shift
	${program} "$@"
fi
