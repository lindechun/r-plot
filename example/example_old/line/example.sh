s-plot lines -m TRUE -a Pos -f line.melt.data -c variable -d value
s-plot lines -m TRUE -a Pos -f line.melt.data -c variable -d value -w 8 -u 6 -A TRUE -W TRUE -X variable

### 误差棒，加point，point shape，x坐标属性是text
s-plot lines -m TRUE -a Pos -f line.melt.data_temp -d value -Z TRUE -A TRUE  -W TRUE -Y 15
####使用xtics limits--scale_x_continuous()
s-plot lines -m TRUE -a Pos -f line.melt.data_temp -d value -Z TRUE -W TRUE -Y 15 -A FALSE -U '-4000,4000' -K '-4000,4000'

# 使用linetype
## color 和 linetype相同
s-plot lines -m TRUE -a Pos -f line.melt.data_temp -X variable -d value -A TRUE -W TRUE -c variable
## color 和 linetype不同
s-plot lines -m TRUE -a Pos -f line.melt.data_temp -X type -d value -A TRUE  -W TRUE -c variable
#### 使用smooth平滑曲线
s-plot lines -m TRUE -a Pos -f line.melt.data_temp -X type -d value -A TRUE  -W TRUE -c variable -J TRUE

## 只有linetype，没有color,设定linetype 因子水平
s-plot lines -m TRUE -a Pos -f line.melt.data_temp -X variable -d value -A TRUE -W TRUE -C "'h3k27ac','ctcf'"
