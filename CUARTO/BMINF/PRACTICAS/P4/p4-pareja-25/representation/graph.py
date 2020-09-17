import matplotlib as mpl
import matplotlib.pyplot as plt
import networkx as nx
import sys

def print_graph(filename):
	g = nx.read_edgelist(filename, create_using=nx.Graph(), nodetype=int)
	nx.draw(g)
	plt.show()

def point_representation(filename, scale):
	file = open(filename, "r")
	lines = file.readlines()
	graph = {}

	for line in lines:
		values = line.split(' ')
		x = values[0].strip()
		y = values[1].strip()

		if x in graph:
			graph[x].append(y)
		else:
			graph[x] = []
			graph[x].append(y)

	final = {}
	for key, value in graph.items():
		if(len(value) in final):
			final[len(value)] += 1
		else:
			final[len(value)] = 1 

	plt.plot(final.keys(), final.values(), 'ro')
	plt.xlabel('Grado')
	plt.ylabel('Num nodos')

	if(scale == 'log'):
		plt.xscale('log')
		plt.yscale('log')

	plt.show()



if __name__ == '__main__':
	# print_graph(sys.argv[1])
	if(len(sys.argv) == 3):
		point_representation(sys.argv[1], sys.argv[2])
	else:
		point_representation(sys.argv[1], None)