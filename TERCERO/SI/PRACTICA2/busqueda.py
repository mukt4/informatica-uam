from flask import request

def filtrado(peliculas, request):
	newDict = []
	try:
		texto = request.form['texto_busqueda'].lower()
		if(len(texto) != 0 and texto != ""):
			for pelicula in peliculas:
				nombre = pelicula['titulo'].lower()
				if(nombre.find(texto) != -1):
					newDict.append(pelicula)
			return newDict
		else:
			return peliculas
	except Exception:
		genero = request.form['genero_busqueda'].lower()
		valoracion = request.form['valoracion_busqueda'].lower()
		if(len(genero) != 0 and genero != "genero"):
			for pelicula in peliculas:
				gen = pelicula['categoria'].lower()
				if(gen.find(genero) != -1):
					newDict.append(pelicula)
			return newDict

		elif(len(valoracion) != 0 and valoracion != "valoracion"):
			for pelicula in peliculas:
				val = str(pelicula['valoracion'])
				if(valoracion.find(val) != -1):
					newDict.append(pelicula)
			return newDict
		else:
			return peliculas