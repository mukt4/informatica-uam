import numpy as np
import matplotlib.pyplot as plt


def make_plot(file, name):
	with open(file) as f:
			data = f.read()

	data = data.split('\n')

	x = []
	y_f = []
	y_fmedio = []
	for row in data[1:]:
		if row == "":
			break
		x.append(float(row.split('\t')[0]))
		y_f.append(float(row.split('\t')[1]))
		y_fmedio.append(float(row.split('\t')[2]))


	fig = plt.figure()

	ax1 = fig.add_subplot(111)

	ax1.set_title(name)    
	ax1.set_xlabel("Generación")
	ax1.set_ylabel('Fitness')

	ax1.plot(x,y_f, c='r', label='Best fitness')
	ax1.plot(x,y_fmedio, c='b', label='Fitness medio')

	start, end = ax1.get_xlim()
	ticks = np.arange(0, end+1, 10)
	plt.xticks(ticks)

	leg = ax1.legend()

	plt.show()
