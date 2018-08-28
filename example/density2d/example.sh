s-plot density2d -f ../../data/faithful.txt -m TRUE -a eruptions -d waiting -o ./

## 加点，并对点设置分组颜色
s-plot density2d -f ../../data/faithful.txt -m TRUE -a eruptions -d waiting -o ./ -L TRUE -c group

## 使用fill，并控制bandwidth
s-plot density2d -f ../../data/faithful.txt -m TRUE -a eruptions -d waiting -o ./ -Y fill -D 'c(0.5,5)'
