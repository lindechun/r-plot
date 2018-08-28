# basic
## melt data
s-plot barplot -f ../../data/uspopchange.txt -m TRUE -a Abb -d Change -c Region -U TRUE -X + -Y TRUE -o ./
s-plot barplot -f ../../data/cabbage_exp.txt -m TRUE -a Date -d Weight -c Cultivar -U TRUE -o ./
### http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/80-bar-plots-and-modern-alternatives/,下面这个命令参考，但有bug，需修改
s-plot barplot -f ../../data/mtcars.txt -m TRUE -a name -d mpg_z -c mpg_grp -U TRUE -X + -Y TRUE -o ./ -b 90
s-plot barplot -f ../../data/mtcars.txt -m TRUE -a name -d mpg -c cyl -U TRUE -X + -Y TRUE -o ./ -b 90 -R TRUE
## normal data

### atgc_precent_lineage_3rd
s-plot barplot -m TRUE -f ../../data/ATGC_percent_3rd.txt -a minor -d value -c major -G variable -I fill -b 45 -w 9 -u 5 -U TRUE -L "'African','South_Eastern_Asian','Oceania-American'" -l "'A3','T3','C3','G3'" -g "'NS4A','NS2A','C','NS5','prM','NS1','NS4B','E','NS2B','NS3'" -x 'Lineage' -o ./
