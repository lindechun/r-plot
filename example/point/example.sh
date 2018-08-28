s-plot point -f ../../data/heightweight.txt -m TRUE -a ageYear -d heightIn -o ./
s-plot point -f ../../data/heightweight.txt -m TRUE -a ageYear -d heightIn -c sex -o ./ -F TRUE -k lm
### -A 除了可以控制x轴是数字还是字符串外，还能控制默认输出图形宽度
s-plot point -f ../../data/mtcars.txt -m TRUE -a name -d mpg -c cyl -o ./ -b 90  -W TRUE -p mpg -A TRUE -w 7 -u 6
s-plot point -f ../../data/mtcars.txt -m TRUE -a name -d mpg -c cyl -o ./ -b 90  -p mpg
#排序-组内排序
s-plot point -f ../../data/mtcars.txt -m TRUE -a name -d mpg -c cyl -o ./ -b 90  -A TRUE -w 7 -u 6 -X + -Y TRUE
## add segment,排序
s-plot point -f ../../data/mtcars.txt -m TRUE -a name -d mpg -c cyl -o ./ -b 90  -X + -Y TRUE -A TRUE -W TRUE
s-plot point -f ../../data/mtcars.txt -m TRUE -a name -d mpg -c cyl -o ./ -b 90  -X + -Y TRUE -W TRUE -A TRUE -T theme_point1 -w 8 -u 6
