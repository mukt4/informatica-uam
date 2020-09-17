from flask import Flask, render_template, url_for, request, redirect, session, flash
import os, json
from compra import procesarCompra, Dinero
from sqlalchemy import create_engine
from sqlalchemy import Table, Column, Integer, String, MetaData, ForeignKey
from sqlalchemy.sql import select
from sqlalchemy import func
from sqlalchemy.orm import sessionmaker
from ast import literal_eval

app = Flask(__name__)

# configurar el motor de sqlalchemy, igual hay que aniadir 5432 a localhost
db_engine = create_engine("postgresql://alumnodb:alumnodb@localhost/si1", echo=False)
# cargar las tablas desde una base de datos existente
db_meta = MetaData(bind=db_engine, reflect = True)
# conexion a la base de datos
db_conn = db_engine.connect()
# create a configured "Session" class
Session = sessionmaker(bind=db_engine)
# create a Session
session_sql = Session()

#Creamos el array de top_ventas al inciar la aplicacion
top_ventas = session_sql.query(func.public.gettopventas(2015)).all()
list_top_ventas = []
for top in top_ventas:
	top_ventas_tuple = literal_eval(top[0])
	result = db_conn.execute("select * from movies where movietitle = '" + top_ventas_tuple[1] + "'")
	for row in result:
		list_top_ventas.append(row)

# Ruta de la pagina index de la aplicacion
@app.route("/", methods = ['GET','POST'])
def index():
	if(request.method == 'POST'):
		#Aplicar filtrado en todas las peliculas
		lista_peliculas = db_conn.execute("select * from movies")
		return render_template('index.html', peliculas = lista_peliculas)
	else:
		#Creamos una session para utlizar las funciones declaradas en la base de datos
		return render_template('index.html', peliculas = list_top_ventas)

# Ruta de la informacion de una pelicula
@app.route("/info/<pelicula>", methods = ['GET', 'POST'])
def informacionPelicula(pelicula):
	informacion = {}
	if(request.method == 'POST'):
		return redirect(url_for('index'), code = 307)
	aux =  db_conn.execute("select * from movies where movietitle = '" + pelicula.replace("%20", " ") +"'")
	for row in aux:
		informacion['pelicula'] = row
		aux =  db_conn.execute("select * from products where movieid = " + str(row['movieid']))
		informacion['articulos'] = []
		for product in aux:
			informacion['articulos'].append(product)
		return render_template('informacion-pelicula.html', pelicula = informacion)
	return redirect(url_for('index'))


# Ruta de la pagina html de login
@app.route("/login", methods=['GET','POST'])
def login():
	if ('user' in session):
		return redirect(url_for('index'))
	else:
		if(request.method == 'POST'):
			##try:
			# Obtenemos la tabla de customers
			db_customers = db_meta.tables['customers']
			# Creamos la consulta para comprobar si el usuario con la contrasenia especificada esta registrado
			query_usario = "username = '" + request.form['nombre-usuario'] + "'" + "AND password = '" + request.form['contrasenia'] + "'"
			query = select([db_customers]).where(query_usario)
			db_result = db_conn.execute(query)
			result = list(db_result)
			if(result):
				session['user'] = request.form['nombre-usuario']
				#Comenzamos el proceso para setear las cookies
				query_usuario = db_conn.execute("select customerid from customers where username = '" + request.form['nombre-usuario'] +"'")
				customerid = query_usuario.first()
				if not customerid:
					session.pop('user', None)
					return redirect(url_for('index'))
				query_carrito = db_conn.execute("select prod_id, quantity, subtotal from orders natural join orderdetail where customerid = " + str(customerid[0]) + "and status=1")
				lista_compra = list(query_carrito)
				#Si el resultado de la query no es una lista vacia significara que nuestro usuario tiene un carrito anterior que tenemos que setear
				if(len(lista_compra) != 0):
					#Borramos las cookies anteriores
					session.pop('carrito', None)
					session.pop('precio', None)
					#Actualizamos la base de datos con el carrito anterior del usuario
					session['carrito'] = []
					session['precio'] = 0
					for elemento in lista_compra:
						add = {"nombre" : elemento['prod_id'], "cantidad": elemento['quantity']}
						session['carrito'].append(add)
						session['precio'] = session['precio'] + int(elemento['subtotal']) 
				#Si el usuario no tiene ningu carrito guardado actualizamos la base de datos con el carrito que estaba creando
				else:
					if 'carrito' in session:
						#Comenzamos con un order nuevo
						db_conn.execute("insert into orders(customerid, status) values(" + str(customerid[0]) + ", 1)")
						#Comprobamos que hemos creado el nuevo order correctamente
						query_order = db_conn.execute("select orderid from orders where customerid = " + str(customerid[0]) + " and status = 1")
						orderid = query_order.first()
						if not orderid:
							return redirect(url_for('index'))
						#Aniadimos el nuevo producto al carrito
						for deter in session['carrito']:
							db_conn.execute("insert into orderdetail(orderid, prod_id, quantity) values(" + str(orderid[0]) + "," + str(deter['nombre']) + "," + str(deter['cantidad']) + ")")

				return redirect(url_for('index'))
			else:
				return render_template('login.html', error = Exception("La contrasenia y el nombre de usuario no coinciden"))
			##except Exception:
				# Crear una pagina de error para cuando se producen errores criticos como este
				#return redirect(url_for('index'))
		else:
			return render_template('login.html')

# Ruta de la pagina html de registro
@app.route("/register", methods = ['GET', 'POST'])
def register():
	if ('user' in session):
		return redirect(url_for('index'))
	else:
		if(request.method == 'POST'):
			#try:
			# Obtenemos la tabla de customers
			db_customers = db_meta.tables['customers']
			# Creamos la consulta para comprobar si el usuario con la contrasenia especificada esta registrado
			query_usario = "username = '" + request.form['cuestionario_nombre'] + "'"
			query = select([db_customers]).where(query_usario)
			db_result = db_conn.execute(query)
			result = list(db_result)
			if(result):
				return render_template('register.html', error = Exception("Este usuario ya existe"))
			else:
				# Creacion de usuario en la  base de datos
				query = db_customers.insert().values(name = request.form['cuestionario_nombreCompleto'], 
													email = request.form['cuestionario_correo'],
													creditcard = request.form['cuestionario_cuenta'],
													username = request.form['cuestionario_nombre'],
													password = request.form['cuestionario_contrasenia'])
				db_conn.execute(query)
				session['user'] = request.form['cuestionario_nombre']

				#Metemos el carrito que el usuario staba haciendo en la base de datos
				if 'carrito' in session:
					query_usuario = db_conn.execute("select customerid from customers where username = '" + request.form['cuestionario_nombre'] + "'")
					customerid = query_usuario.first()
					print(customerid)
					if not customerid:
						session.pop('user', None)
						return redirect(url_for('index'))
					#Comenzamos con un order nuevo
					db_conn.execute("insert into orders(customerid, status) values(" + str(customerid[0]) + ", 1)")
					#Comprobamos que hemos creado el nuevo order correctamente
					query_order = db_conn.execute("select orderid from orders where customerid = " + str(customerid[0]) + " and status = 1")
					orderid = query_order.first()
					if not orderid:
						return redirect(url_for('index'))
					#Aniadimos el nuevo producto al carrito
					for deter in session['carrito']:
						db_conn.execute("insert into orderdetail(orderid, prod_id, quantity) values(" + str(orderid[0]) + "," + str(deter['nombre']) + "," + str(deter['cantidad']) + ")")

				return redirect(url_for('index'))
			#except Exception as error:
				# Crear una pagina de error para cuando se producen errores criticos como este
				#print(error)
				#return redirect(url_for('index'))
		else:
			return render_template('register.html')

#Ruta para cerrar sesion
@app.route('/logout')
def logout():
	if('user' in session):
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
		if 'carrito' in session:
			peliculas = []
			for deter in session['carrito']:
				query_producto = db_conn.execute("select * from products where prod_id = " + str(deter['nombre']))
				for row in query_producto:
					producto = row
					query_pelicula = db_conn.execute("select * from movies where movieid = " + str(row['movieid']))
					for row2 in query_pelicula:
						pelicula = row2
				carrito = {}
				carrito['pelicula'] = pelicula
				carrito['producto'] = producto
				carrito['cantidad'] = deter['cantidad']
				peliculas.append(carrito)
			return render_template('carrito.html', peliculas = peliculas)
		else:
			peliculas = []
			return render_template('carrito.html', peliculas = peliculas)

#Metodo get para aniadir una pelicula al carrito de compra
@app.route("/info/<pelicula>/aniadir-carrito", methods = ['GET'])
def aniadir(pelicula):
	#Comprobacion de que la id del producto que vamos a borrar se encuentra en la base de datos
	movie_result = db_conn.execute("select * from products where prod_id = " + pelicula)
	producto = movie_result.first()

	if not producto:
		return redirect(url_for('index'))
	
	movie_result = db_conn.execute("select * from movies where movieid = " + str(producto['movieid']))
	pelicula_sql = movie_result.first()

	if not pelicula:
		return redirect(url_for('index'))

	#Actulizamos las cookies del carrito para no cambiar el flujo de nuestro programa
	if 'precio' not in session:
		session['precio'] = 0

	if 'carrito' in session:
		flag = 0
		for deter in session['carrito']:
			if(deter['nombre'] == producto['prod_id']):
				deter['cantidad'] = deter['cantidad'] + 1
				session['precio'] = session['precio'] + int(producto['price'])
				flag = flag + 1
		if flag == 0:
			add = {"nombre" : producto['prod_id'], "cantidad": 1}
			session['carrito'].append(add)
			session['precio'] = session['precio'] + int(producto['price'])
	else:
		session['carrito'] = []
		add = {"nombre" : producto['prod_id'], "cantidad": 1}
		session['carrito'].append(add)
		session['precio'] = session['precio'] + int(producto['price'])

	#Si hay un usuario con la sesion iniciada realizamos los respectivos cambios en la base de datos
	if 'user' in session:
		#Gestionarlo en base de datos
		customerid_query = db_conn.execute("select customerid from customers where username = '" + session['user'] +"'")
		customerid = customerid_query.first()
		if not customerid:
			session.pop('user', None)
			return redirect(url_for('index'))

		query_order = db_conn.execute("select orderid from orders where customerid = " + str(customerid[0]) + " and status = 1")
		orderid = query_order.first()
		if orderid:
			#Se aniade una pelicula a un carrito ya creado
			query_producto = db_conn.execute("select quantity from orderdetail where orderid = " + str(orderid[0]) + " and prod_id = " + pelicula)
			cantidad = query_producto.first()

			if cantidad:
				#Sumamos una en la cantidad de procductos de ese orderdetail
				db_conn.execute("update orderdetail set quantity = " + str(int(cantidad[0]) + 1) + "where orderid = " + str(orderid[0]) + " and prod_id = " + pelicula)
			else:
				#Creamos un nuevo orderdetail
				db_conn.execute("insert into orderdetail(orderid, prod_id, quantity) values(" + str(orderid[0]) + "," + pelicula + ", 1)")

			return redirect(url_for('index'))
		else:
			#print(customerid)
			#Comenzamos con un order nuevo
			db_conn.execute("insert into orders(customerid, status) values(" + str(customerid[0]) + ", 1)")
			#Comprobamos que hemos creado el nuevo order correctamente
			query_order = db_conn.execute("select orderid from orders where customerid = " + str(customerid[0]) + " and status = 1")
			orderid = query_order.first()
			if not orderid:
				return redirect(url_for('index'))
			#Aniadimos el nuevo producto al carrito
			db_conn.execute("insert into orderdetail(orderid, prod_id, quantity) values(" + str(orderid[0]) + "," + pelicula + ", 1)")
			return redirect(url_for('index'))

	return redirect(url_for('index'))

#Metodo get para eliminar una pelicula del carrito de compra
@app.route("/info/<pelicula>/del-carrito", methods = ['GET'])
def delete(pelicula):
	#Comprobacion de que la id del producto que vamos a borrar se encuentra en la base de datos
	movie_result = db_conn.execute("select * from products where prod_id = " + pelicula)
	producto = movie_result.first()

	if not producto:
		return redirect(url_for('index'))

	movie_result = db_conn.execute("select * from movies where movieid = " + str(producto['movieid']))
	pelicula_sql = movie_result.first()

	if not pelicula:
		return redirect(url_for('index'))

	#Actualizamos las cookeis para no cambiar el flujo de nuestro programa
	if 'precio' not in session:
		return redirect(url_for('carrito'))

	if 'carrito' in session:
		#Quitar pelicula del carrito
		i = 0
		for deter in session['carrito']:
			if(int(deter['nombre']) == int(pelicula)):
				if(deter['cantidad'] == 1):
					del(session['carrito'][i])
				else:
					deter['cantidad'] = deter['cantidad'] - 1
			i = i + 1

		#Modificar dinero del carrito
		if (len(session['carrito']) == 0):
			session['precio'] = 0
		else:
			session['precio'] = session['precio'] - int(producto['price'])
			if(session['precio'] < 0):
				session['precio'] = 0
	else:
		return redirect(url_for('carrito'))

	#Realizamos los cambios respectivos en la base de datos
	if 'user' in session:
		#Gestionarlo en base de datos
		customerid_query = db_conn.execute("select customerid from customers where username = '" + session['user'] +"'")
		customerid = customerid_query.first()
		if not customerid:
			session.pop('user')
			return redirect(url_for('index'))

		query_order = db_conn.execute("select orderid from orders where customerid = " + str(customerid[0]) + " and status = 1")
		orderid = query_order.first()
		if orderid:
			#Se elimina una pelicula de un carrito ya creado
			query_producto = db_conn.execute("select quantity from orderdetail where orderid = " + str(orderid[0]) + " and prod_id = " + pelicula)
			cantidad = query_producto.first()
			if not cantidad:
				return redirect(url_for('carrito'))
			if int(cantidad[0]) > 1:
				#Restamos uno en la cantidad de procductos de ese orderdetail si hay mas de 1 producto
				db_conn.execute("update orderdetail set quantity = " + str(int(cantidad[0]) - 1) + "where orderid = " + str(orderid[0]) + " and prod_id = " + pelicula)
			else:
				#Eliminamos el orderdetail
				db_conn.execute("delete from orderdetail where orderid = " + str(orderid[0]) + " and prod_id = " + pelicula)

			return redirect(url_for('carrito'))
		else:
			#Si no existe ningun order no hacemos nada
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