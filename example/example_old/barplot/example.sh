## 矩阵数据+样本分组数据
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType

## 显示数据
s-plot barPlot -f boxplot.normal.data -U TRUE

#只统计样本
s-plot barPlot -f barplot.melt.data -m TRUE -a variable -d value

#统计每个坐标上两类样本的平均值,按值升序排序
s-plot barPlot -f barplot.melt.data -m TRUE -a Pos -c variable -d value -A TRUE -X +

## 只统计样本出现个数
s-plot barPlot -f barplot.melt.data -m TRUE -a Pos -c variable -d value -D count

## errorbar-堆叠图形-改变x的因子水平
 s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c GeType -a GeType -Z TRUE -I dodge -L "'B','C','A'"

## errorbar-并排bar
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c GeType -a GeType -Z TRUE -I dodge

## 显示text,分组的bar并排放置
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType -Z TRUE -U TRUE

## 显示text，bar为百分比堆叠图
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType -Z TRUE -I fill -U TRUE
##显示text，bar堆叠
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType -Z TRUE -I stack -U TRUE

## 只统计计样本出现次数
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType  -D count -U TRUE -I fill
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType  -D count -U TRUE -I dodge
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c Group -a GeType  -D count -U TRUE -I stack

## y轴数据变换
s-plot barPlot -f boxplot.normal.data -Q sampleGroup -c GeType -a GeType -Z TRUE -I dodge -s log10
