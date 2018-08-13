r-plot point3d -f ../../data/mtcars.txt -m TRUE -a wt -d disp -z mpg -o ./ -t "Mtcars" -G xyz -B TRUE -R 60 -F lm -J TRUE
r-plot point3d -f ../../data/mtcars.txt -m TRUE -a wt -d disp -z mpg -o ./ -B FALSE -G xyz -c cyl -p 0.05 -T h -D 1.5
r-plot point3d -f ../../data/mtcars.txt -m TRUE -a wt -d disp -z mpg -o ./ -G FALSE -c cyl -i vs -s am  -P bottom -p -0.15
