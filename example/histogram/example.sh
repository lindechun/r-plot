### histogram
r-plot histogram -f ../../data/birthwt.txt -m TRUE -a bwt -c smoke -o ./ -J 10 -L TRUE -S TRUE
## alpha，密度
r-plot histogram -f ../../data/birthwt.txt -m TRUE -a bwt -o ./ -J 10 -L TRUE -j density -i smoke -S density
## 计数,无边框
r-plot histogram -f ../../data/birthwt.txt -m TRUE -a bwt -o ./ -J 10 -L FALSE -j count -X mean -Z TRUE -J 40 -S density -c smoke

### area
r-plot histogram -f ../../data/birthwt.txt -m TRUE -a bwt -c smoke -o ./ -J 10 -L TRUE -S TRUE -D area
## alpha，密度
r-plot histogram -f ../../data/birthwt.txt -m TRUE -a bwt -o ./ -J 10 -L TRUE -j density -i smoke -S density -D area
## 计数,无边框
r-plot histogram -f ../../data/birthwt.txt -m TRUE -a bwt -o ./ -J 10 -L FALSE -j count -X mean -Z TRUE -J 40 -S density -c smoke -D area

# 另一数据
r-plot histogram -f ../../data/wdata.txt -m TRUE -a weight -c sex -o ./ -D area
