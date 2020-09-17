import os, hashlib, random

def crearUsuario(userName, user, password, email, cuenta):
	if(len(userName) == 0 or len(user) == 0 or len(password) == 0 or len(email) == 0 or len(cuenta) == 0):
		raise Exception("Alguno de los campos no han sido rellenados correctamente")

	USER_DIR = 'usuarios/' + userName 

	if(os.path.exists(USER_DIR)):
		raise Exception("Este usuario ya existe")
	else:
		os.mkdir(USER_DIR)

	DIR = os.path.join(USER_DIR, 'datos.dat')
	file = open(DIR, 'w')


	m = hashlib.md5()
	m.update(password.encode('utf-8'))
	file.write(userName + '\n')
	file.write(m.hexdigest() + '\n')
	file.write(user + '\n')
	file.write(cuenta + '\n')
	file.write(str(random.randint(0, 100)))
	file.close()
	DIR = os.path.join(USER_DIR, 'historial.json')
	file = open(DIR, 'w')
	file.close()

def comprobarUsuario(userName, password):
	USER_DIR = 'usuarios/' + userName +'/datos.dat'

	if(len(userName) == 0 or len(password) == 0):
		raise Exception("Alguno de los campos no ha sido rellenado correctamente")

	if(not(os.path.exists(USER_DIR))):
		raise Exception("Nombre de usuario incorrecto")

	f = open(USER_DIR, "r")
	m = hashlib.md5()
	m.update(password.encode('utf-8'))
	usr = f.readline()
	pwd = f.readline()

	if(usr.find(userName) == -1 or pwd.find(m.hexdigest()) == -1):
		raise Exception("La contrasenia y el nombre de usuario no coinciden")
		

	







