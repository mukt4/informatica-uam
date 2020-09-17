--SENTENCIA QUE SELECCIONA LOS CLINTES QUE EN EL ANIO Y MES INTRODUCIDOS HAYAN SUPERADO 100 EN GASTOS
SELECT customerid, username, orderdate FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = 2015 AND extract (MONTH FROM orderdate) = 04 AND  totalamount >= 100;

--SENTENCIA ANTERIOR CON LA SENTENCIA EXPLAIN
EXPLAIN SELECT customerid, username, orderdate FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = 2015 AND extract (MONTH FROM orderdate) = 04 AND  totalamount >= 100;

--CREAMOS INDICE PARA MEJORAR LA CONSULTA
CREATE INDEX index_totalamount on orders(totalamount);

--VOLVEMOS A EJECUTAR CON EL COMANDO EXPLAIN
EXPLAIN SELECT customerid, username, orderdate FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = 2015 AND extract (MONTH FROM orderdate) = 04 AND  totalamount >= 100;

--PROBAMOS OTRO INDICE
--PRIMERO DROPEAMOS EL INDICE ANTERIOR
DROP INDEX index_totalamount;
--CREAMOS UN NUEVO INDICE CON LA FECHA
CREATE INDEX index_orderdate on orders(orderdate);
--VOLVEMOS A EJECUTAR LA CONSULTA CON EL COMANDO EXPLAIN
EXPLAIN SELECT customerid, username, orderdate FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = 2015 AND extract (MONTH FROM orderdate) = 04 AND  totalamount >= 100;

--PROBAMOS AHORA A CREAR UN INDICE EN EL CAMPO ANIO DE ORDERDATE
--PRIMERO DROPEAMOS EL INDICE ANTERIOR
DROP INDEX index_orderdate;
--CREAMOS UN NUEVO INDICE CON LA FECHA
CREATE INDEX index_orderdate_year on orders(extract(YEAR from orderdate));
--VOLVEMOS A EJECUTAR LA CONSULTA CON EL COMANDO EXPLAIN
EXPLAIN SELECT customerid, username, orderdate FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = 2015 AND extract (MONTH FROM orderdate) = 04 AND  totalamount >= 100;

--PROBAMOS AHORA A CREAR VARIOS INDICES Y EJECUTAR LA CONSULTA JUNTO CON LA SENTENCIA EXPLAIN
--CREAMOS UN INDICE CON EL MES DE ORDERDATE
CREATE INDEX index_orderdate_month on orders(extract(MONTH from orderdate));
--CREAMOS OTRO INDICE CON TOTALAMOUNT
CREATE INDEX index_totalamount on orders(totalamount);
--VOLVEMOS A EJECUTAR LA CONSULTA CON EL COMANDO EXPLAIN
EXPLAIN SELECT customerid, username, orderdate FROM customers NATURAL JOIN orders WHERE extract (YEAR FROM orderdate) = 2015 AND extract (MONTH FROM orderdate) = 04 AND  totalamount >= 100;
