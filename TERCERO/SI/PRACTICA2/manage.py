from flask import Flask, render_template, url_for, request, redirect, session, flash
import os, json
from usuarios import crearUsuario, comprobarUsuario
from busqueda import filtrado
from compra import procesarCompra, Dinero

app = Flask(__name__)

catalogo_data = json.loads(open(os.path.join(app.root_path,'json/catalogo.json')).read())
peliculas = catalogo_data['peliculas']

# Ruta de la pagina index de la aplicacion
@app.route("/", methods = ['GET','POST'])
def index():
	if(request.method == 'POST'):
		peliculasFiltradas = filtrado(peliculas, request)
		return render_template('index.html', peliculas = peliculasFiltradas)
	else:
		return render_template('index.html', peliculas = peliculas)

# Ruta de la informacion de una pelicula
@app.route("/info/<pelicula>", methods = ['GET', 'POST'])
def informacionPelicula(pelicula):
	peliculas = catalogo_data['peliculas']
	if(request.method == 'POST'):
		return redirect(url_for('index'), code = 307)
	for aux in peliculas:
		if aux['titulo'] == pelicula.replace("%20", " "):
			return render_template('informacion-pelicula.html', pelicula = aux)
	return redirect(url_for('index'))

# Ruta de la pagina html de login
@app.route("/login", methods=['GET','POST'])
def login():
	if ('user' in session):
		return redirect(url_for('index'))
	else:
		if(request.method == 'POST'):
			try:
				comprobarUsuario(request.form['nombre-usuario'], request.form['contrasenia'])
				session['user'] = request.form['nombre-usuario']
				if 'last_user' in session:
					print(session['last_user'])
					if session['last_user'] == session['user']:
						if (('last_carrito' in session) and ('last_precio' in session)):
							session['carrito'] = session['last_carrito']
							session['precio'] = session['last_precio']
				return redirect(url_for('index'))
			except Exception as error:
				return render_template('login.html', error = error)
		else:
			return render_template('login.html')

# Ruta de la pagina html de registro
@app.route("/register", methods = ['GET', 'POST'])
def register():
	if ('user' in session):
		return redirect(url_for('index'))
	else:
		if(request.method == 'POST'):
			try:
				crearUsuario(request.form['cuestionario_nombre'], request.form['cuestionario_nombreCompleto'], request.form['cuestionario_contrasenia'], request.form['cuestionario_correo'], request.form['cuestionario_cuenta'])
				session['user'] = request.form['cuestionario_nombre']
				return redirect(url_for('index'))
			except Exception as error:
				return render_template('register.html', error = error)
		else:
			return render_template('register.html')

#Ruta para cerrar sesion
@app.route('/logout')
def logout():
	if('user' in session):
		if(('carrito' in session) and ('precio' in session)):
			session['last_user'] = session['user']
			session['last_carrito'] = session['carrito']
			session['last_precio'] = session['precio']
		session.pop('user', None)
		session.pop('carrito', None)
		session.pop('precio', None)
		return redirect(url_for('index'))
	else:
		return redirect(url_for('index'))

#Ruta para ver el carrito
@app.route('/carrito', methods = ['GET', 'POST'])
def carrito():
	if(request.method == 'POST'):
		return redirect(url_for('index'), code = 307)
	else:
		return render_template('carrito.html', peliculas = peliculas)

#Metodo get para aniadir una pelicula al carrito de compra
@app.route("/info/<pelicula>/aniadir-carrito", methods = ['GET'])
def aniadir(pelicula):
	if 'precio' not in session:
		session['precio'] = 0
	for aux in peliculas:
		if aux['titulo'] == pelicula.replace("%20", " "):
			if 'carrito' in session:
				for deter in session['carrito']:
					if(deter['nombre'] == pelicula.replace("%20", " ")):
						deter['cantidad'] = deter['cantidad'] + 1
						session['precio'] = session['precio'] + aux['precio']
						return redirect(url_for('index'))
				add = {"nombre" : pelicula.replace("%20", " "), "cantidad": 1}
				session['carrito'].append(add)
				session['precio'] = session['precio'] + aux['precio']
				return redirect(url_for('index'))
			else:
				session['carrito'] = []
				add = {"nombre" : pelicula.replace("%20", " "), "cantidad": 1}
				session['carrito'].append(add)
				session['precio'] = session['precio'] + aux['precio']
				return redirect(url_for('index'))
	return redirect(url_for('index'))

#Metodo get para eliminar una pelicula del carrito de compra
@app.route("/info/<pelicula>/del-carrito", methods = ['GET'])
def delete(pelicula):
	if 'precio' not in session:
		return redirect(url_for('carrito'))
	for aux in peliculas:
		if aux['titulo'] == pelicula.replace("%20", " "):
			if 'carrito' in session:
				#Quitar pelicula del carrito
				i = 0
				for deter in session['carrito']:
					if(deter['nombre'] == pelicula.replace("%20", " ")):
						if(deter['cantidad'] == 1):
							del(session['carrito'][i])
						else:
							deter['cantidad'] = deter['cantidad'] - 1
					i = i + 1

				#Modificar dinero del carrito
				if (len(session['carrito']) == 0):
					session['precio'] = 0
				else:
					session['precio'] = session['precio'] - aux['precio']
					if(session['precio'] < 0):
						session['precio'] = 0
			else:
				return redirect(url_for('carrito'))
	return redirect(url_for('carrito'))

#Pagina de cofirmacion del carrito de compra
@app.route("/confirmacion", methods = ['GET', 'POST'])
def confirmacion():
	if('user' in session):
		if(('carrito' in session) and (len(session['carrito']) > 0)):
			return render_template('confirmacion.html', peliculas = peliculas)
		else:
			return redirect(url_for('index'))
	else:
		return redirect(url_for('index'))

@app.route("/confirmacion/push")
def push():
	try:
		procesarCompra(session, peliculas)
		session.pop('carrito', None)
		session.pop('precio', None)
		return redirect(url_for('index'))
	except Dinero as error:
		flash("Parece que no tienes dinero suficiente")
		return redirect(url_for('index'))
	except Exception as error:
		flash(error)
		return redirect(url_for('index', error = error))

# Ruta del historial de compra de un usuario
@app.route("/historial", methods = ['GET', 'POST'])
def historial():
	if('user' not in session):
		return redirect(url_for('index'))
	else:
		PATH = 'usuarios/' + session['user'] + '/historial.json'
		historial = json.loads(open(PATH).read())

		if(request.method == 'POST'):
			return redirect(url_for('index'), code = 307)
		else:
			return render_template('historial.html', historial = historial)

#Mantener cookies despues de cerrar el navegador
@app.before_request
def session_management():
	session.permanent = True

if __name__ == "__main__":
	app.secret_key = os.urandom(24)
	app.run(host='0.0.0.0', port=5001, debug=True)
    
