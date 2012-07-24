import os
import networkx as net
import matplotlib.pyplot as plot
os.chdir('C://My Projects//Zheng - Poland FaceBook')
file = open('fs.csv','r')
g = net.Graph()
line = file.readline()
i = 1
while True:
	line = file.readline()
	
	[fid,appid,uid] = line.split(',')
	g.add_edge(uid,fid)
	i = i+1;
	if not line: break
	if i>100000: break
file.close()
net.draw(g)
plot.show()
