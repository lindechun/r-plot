#currentPath=$(cd `dirname $0`; pwd)
#dataPath=${currentPath}/../../data/

# basic
## melt data
s-plot boxplot -f ../../data/ToothGrowth.txt -m TRUE -a dose -d len -o ./
s-plot boxplot -f ../../data/ToothGrowth.txt -m TRUE -a dose -d len -c supp -R TRUE -o ./
## normal data
