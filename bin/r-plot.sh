#!/bin/bash

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

#### Dots
${filename} point3d

#### Histogram
${filename} histogram

#### Areaplot
${filename} areaplot

#### Pie
${filename} pie

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
