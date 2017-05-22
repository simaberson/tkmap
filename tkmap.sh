rm out*
rm hurrloc*
rm points*
xtkmap
ls -1 points1 points2 points3 > points
ls -1 hurrloc1 hurrloc2 hurrloc3 > hurrloc
/adaptive/applications/grads2.1/bin/grads -l -g 900x675+50+50 -c plotdrops.gs
