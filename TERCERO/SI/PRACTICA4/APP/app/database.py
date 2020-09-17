# -*- coding: utf-8 -*-

import os
import sys, traceback, time

from sqlalchemy import create_engine

# configurar el motor de sqlalchemy
db_engine = create_engine("postgresql://alumnodb:alumnodb@localhost/si1", echo=False, execution_options={"autocommit":False})

def dbConnect():
    return db_engine.connect()

def dbCloseConnect(db_conn):
    db_conn.close()

def getListaCliMes(db_conn, mes, anio, iumbral, iintervalo, use_prepare, break0, niter):

    # TODO: implementar la consulta; asignar nombre 'cc' al contador resultante
    consulta = "SELECT COUNT(customerid) FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) =" + str(anio) + " AND extract (MONTH FROM orderdate) = " + str(mes) + " AND  totalamount >= "
    
    # Si use_prepare esta a true realizamos el prepare de la consulta
    if use_prepare == True:
        consulta2 = "SELECT COUNT(customerid) FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = $1 AND extract (MONTH FROM orderdate) = $2 AND  totalamount >= $3"
        query = "PREPARE query(int, int, int) AS " + consulta2
        db_conn.execute(query)

    # TODO: ejecutar la consulta
    # - mediante PREPARE, EXECUTE, DEALLOCATE si use_prepare es True
    # - mediante db_conn.execute() si es False
    res = {"cc" : 0}

    # Array con resultados de la consulta para cada umbral
    dbr=[]

    for ii in range(niter):
        
        # TODO: ...
        if use_prepare == False:
            query = consulta + str(iumbral)
            clientes = db_conn.execute(query)
        else:
            query = "EXECUTE query(" + str(anio) + "," + str(mes) + "," + str(iumbral) + ")"
            clientes = db_conn.execute(query)

        for rsl in clientes:
            res['cc'] = rsl[0]

        # Guardar resultado de la query
        dbr.append({"umbral":iumbral,"contador":res['cc']})

        # TODO: si break0 es True, salir si contador resultante es cero
        if break0 == True:
            if res['cc'] == 0:
                break

        # Actualizacion de umbral
        iumbral = iumbral + iintervalo

    # Si use prepare esta a true quitamos el prepare realizado anteriormente de la consulta al finalizar el programa
    if use_prepare == True:
        query = "DEALLOCATE PREPARE query"
        db_conn.execute(query)

    return dbr

def getMovies(anio):
    # conexion a la base de datos
    db_conn = db_engine.connect()

    query="select movietitle from imdb_movies where year = '" + anio + "'"
    resultproxy=db_conn.execute(query)

    a = []
    for rowproxy in resultproxy:
        d={}
        # rowproxy.items() returns an array like [(key0, value0), (key1, value1)]
        for tup in rowproxy.items():
            # build up the dictionary
            d[tup[0]] = tup[1]
        a.append(d)
        
    resultproxy.close()  
    
    db_conn.close()  
    
    return a
    
def getCustomer(username, password):
    # conexion a la base de datos
    db_conn = db_engine.connect()

    query="select * from customers where username='" + username + "' and password='" + password + "'"
    res=db_conn.execute(query).first()
    
    db_conn.close()  

    if res is None:
        return None
    else:
        return {'firstname': res['firstname'], 'lastname': res['lastname']}
    
def delCustomer(customerid, bFallo, bSQL, duerme, bCommit):
    
    # Array de trazas a mostrar en la página
    dbr=[]

    # TODO: Ejecutar consultas de borrado
    # - ordenar consultas según se desee provocar un error (bFallo True) o no
    # - ejecutar commit intermedio si bCommit es True
    # - usar sentencias SQL ('BEGIN', 'COMMIT', ...) si bSQL es True
    # - suspender la ejecución 'duerme' segundos en el punto adecuado para forzar deadlock
    # - ir guardando trazas mediante dbr.append()

    begin = "BEGIN"
    commit = "COMMIT"
    rollback = "ROLLBACK"

    # Lo primero que hacemos es ejecutar la sentencia BEGIN para poder utilizar el mecanismo ROLLBACK
    # Nos conectamos a la base de datos
    db_conn = db_engine.connect()

    # Lista en la que guardaremos las consultas que vamos a realizar
    consultas = []

    # Creamos el array de consultas que vamos a ejecutar secuencialmente
    # Primero borrariamos los datos de orderdetail
    consulta = "DELETE FROM orderdetail WHERE orderid IN (SELECT orderid FROM orders WHERE customerid = " + str(customerid) + ")"
    consultas.append(consulta)
    # A continuacion borramos los datos de orders
    consulta = "DELETE FROM orders WHERE customerid = " + str(customerid)
    consultas.append(consulta)
    # Por ultimo se borraria de la tabla customers
    consulta = "DELETE FROM customers WHERE customerid = " + str(customerid)
    consultas.append(consulta)
    
    try:
        # TODO: ejecutar consultas
        if(bFallo):
            # Eliminacion de manera incorrecta
            traza = "Ejecutando BEGIN"
            dbr.append(traza)
            db_conn.execute(begin)
            traza = "BEGIN ejecutado correctamente"
            dbr.append(traza)
            # COMMIT intermedio despues de BEGIN
            if(bCommit):
                traza = "Ejecutando COMMIT intermedio despues de BEGIN"
                dbr.append(traza)
                db_conn.execute(commit)
                traza = "COMMIT ejecutado correctamente"
                dbr.append(traza)

            traza = "Ejecutamos consulta '" + consultas[1] + "'"
            dbr.append(traza)
            db_conn.execute(consultas[1])
            traza = "Consulta ejecutada correctamente"
            dbr.append(traza)
            traza = "Ejecutando COMMIT"
            dbr.append(traza)
            db_conn.execute(commit)
            traza = "COMMIT ejecutado correctamente"
            dbr.append(traza)
        else:
            for consulta in consultas:
                traza = "Ejecutando BEGIN"
                dbr.append(traza)
                db_conn.execute(begin)
                traza = "BEGIN ejecutado correctamente"
                dbr.append(traza)
                # COMMIT intermedio despues de BEGIN
                if(bCommit):
                    traza = "Ejecutando COMMIT intermedio despues de BEGIN"
                    dbr.append(traza)
                    db_conn.execute(commit)
                    traza = "COMMIT ejecutado correctamente"
                    dbr.append(traza)
                traza = "Ejecutamos consulta '" + consulta + "'"
                dbr.append(traza)
                db_conn.execute(consulta)
                traza = "Consulta ejecutada correctamente"
                dbr.append(traza)
                traza = "Ejecutando COMMIT"
                dbr.append(traza)
                db_conn.execute(commit)
                traza = "COMMIT ejecutado correctamente"
                dbr.append(traza)
            
    except Exception as e:
        traza = "Error al ejecutar la consulta"
        dbr.append(traza)
        traza = "Ejecutando ROLLBACK"
        dbr.append(traza)
        db_conn.execute(rollback)
        traza = "ROLLBACK ejecutado correctamente"
        dbr.append(traza)

    else:
        # TODO: confirmar cambios si todo va bien
        traza = "Borrado realizado con exito"
        dbr.append(traza)
        db_conn.close()
        
    return dbr

