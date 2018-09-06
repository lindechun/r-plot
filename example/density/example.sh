#r-plot density -f ../../data/birthwt.txt -m TRUE -a bwt -c smoke -o ./ -D density
#r-plot density -f ../../data/wdata.txt -m TRUE -a weight -c sex -o ./ -D density
r-plot density -f ../../data/wdata.txt -m TRUE -a weight -c sex -o ./ -C "'#00AFBB','#E7B800'" -X mean -P top -w 6 -u 5 -D density -F TRUE -Z TRUE

## x是数值型的离散值
r-plot density -f ../../data/density_x_numtype.txt -m TRUE -a pctOAC -d NoP -c Age -o ./ -C "'#00AFBB','#E7B800'" -P top -w 6 -u 5 -D identity -Z TRUE
