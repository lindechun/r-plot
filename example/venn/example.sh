## 从文件中求集合关系
r-plot venn -f ../../data/vennData_wide.txt -F wide -o ./ -g 0.8 -k transparent
### 使用颜色主题，area label size 根据面积大小而变化
r-plot venn -f ../../data/vennData_wide.txt -F wide -o ./ -c colorspace::diverge_hcl -J log10 -j 2
### 使用long format数据 outlier使用虚线
r-plot venn -f ../../data/vennData_long.txt -F long -o ./ -a sample -d feature -m dashed

### 手动输入数值
#颜色类型3, 一组颜色
r-plot venn -n "10,15,5" -l "'A','B'" -u 6 -w 6 -c "'red','blue'" -C 3
#颜色类型2，单个颜色
r-plot venn -n "10,15,5" -l "'A','B'" -u 6 -w 6 -c 'red' -C 2
#outline null，area label size 根据面积大小而变化
r-plot venn -n "10,15,5" -l "'A','B'" -u 6 -w 6 -c transparent -C 2 -J log10
