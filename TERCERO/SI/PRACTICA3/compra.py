import os
import datetime
import json
import uuid

#Excepcion que se lanza cuando el usuario no tiene dinero
class Dinero(Exception):
	pass

def procesarCompra(session, peliculas):
	if ('user' not in session):
		raise Exception("Es necesario que te registres para poder realizar una compra")

	if(('carrito' not in session) or (len(session['carrito']) == 0)):
		raise Exception("No es posible procesar una compra con el carrito vacio")

	if(('precio' not in session) or (session['precio'] <= 0)):
		raise Exception("Caclulo del precio total erroneo, abortando compra")

	USER_DIR = 'usuarios/' + session['user']
	if(not(os.path.exists(USER_DIR))):
		raise Exception("Error con los directorios del usuario, contacte con webmaster@localhost para mas informacion")

	file = open(os.path.join(USER_DIR, 'datos.dat'), "r+")
	data = file.readlines()
	usr = data[0]
	cash = data[4]

	if(float(cash) < session['precio']):
		raise Dinero("No tiene suficiente dinero en la cuenta")
	else:
		data[4] = str(float(cash) - session['precio']) + '\n'
		file.seek(0)
		file.truncate()
		file.writelines(data)
		file.close()
		file_historial = open(os.path.join(USER_DIR, 'historial.json'), "r+")
		if(os.stat(os.path.join(USER_DIR, 'historial.json')).st_size == 0):
			historial = []
			info_pelicula = session['carrito']
			info = {"id": len(historial), "peliculas": info_pelicula , "precio": session['precio'], "fecha": datetime.datetime.now().strftime("%Y-%m-%d %H:%M")}
			historial.append(info)
			json.dump(historial, file_historial)
		else:
			a = json.load(file_historial)
			info = {"id": len(a), "peliculas": session['carrito'], "precio": session['precio'], "fecha": datetime.datetime.now().strftime("%Y-%m-%d %H:%M")}
			a.append(info)
			file_historial.seek(0)
			file_historial.truncate()
			json.dump(a, file_historial)

