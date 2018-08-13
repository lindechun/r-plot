# one sample
### 1%以下的数值作为other组,标签显示名称和百分比
r-plot pie -f ../../data/pie_Species_report.txt -m TRUE -d Percent -c Species -o ./ -M plotly -D 0.01 -L label+percent -l inside
r-plot pie -f ../../data/pie_Species_report.txt -m TRUE -d Percent -c Species -o ./ -M plotly -L percent

# more sample
## 饼图上不显示标签,0.5%以下的数值都作为other
r-plot pie -f ../../data/twoSample_pie_Species_report.txt -m TRUE -a Sample -d Percent -c Species -o ./ -M ggplot -L FALSE -D 0.005
r-plot pie -f ../../data/twoSample_pie_Species_report.txt -m TRUE -a Sample -d Percent -c Species -o ./ -M plotly -L percent -D 0.005
