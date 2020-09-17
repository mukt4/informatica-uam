----------------------------------------------------------------------
-----------------ARREGLO DE LOS NOMBRES DE LAS TABLAS-----------------
----------------------------------------------------------------------

ALTER TABLE imdb_actormovies RENAME TO actormovies;
ALTER TABLE imdb_actors RENAME TO actors;
ALTER TABLE imdb_directormovies RENAME TO directormovies;
ALTER TABLE imdb_directors RENAME TO directors;
ALTER TABLE imdb_moviecountries RENAME TO moviecountries;
ALTER TABLE imdb_moviegenres RENAME TO moviegenres;
ALTER TABLE imdb_movielanguages RENAME TO movielanguages;
ALTER TABLE imdb_movies RENAME TO movies;

----------------------------------------------------------------------

-------------------------------------------------------------------------
---------CREACION DE TABLAS PARA ATRIBUTOS CON MULTIPLES VALORES---------
-------------------------------------------------------------------------

--CREACION DE LA TABLA STATUS PARA EL ESTADO DEL PEDIDO
CREATE TABLE status(
	status_id SERIAL PRIMARY KEY,
	status varchar(14)
);

INSERT INTO status (status)
VALUES (NULL),('Paid'),('Processed'),('Shipped');

--CREACION DE UNA TABLA AUXILIAR EN LA QUE GUARDAREMOS TODOS LOS VALORES DE ORDERS
--Y EN LA QUE RELACIONAREMOS EL ESTADO CON EL STATUS_ID
CREATE TABLE aux AS 
	SELECT orderid, orderdate, customerid, netamount, tax, totalamount, status_id AS status
	FROM status NATURAL JOIN orders;
	
DROP TABLE orders;
ALTER TABLE aux RENAME TO orders;

--CREACION DE TABLA DE GENEROS
CREATE TABLE genres(
	genre_id SERIAL PRIMARY KEY,
	genre varchar(20) NOT NULL
);

INSERT INTO genres (genre) 
SELECT genre FROM moviegenres GROUP BY genre ORDER BY genre ASC;

ALTER TABLE moviegenres
    ADD COLUMN genre_id INTEGER,
    ADD FOREIGN KEY (genre_id) REFERENCES genres(genre_id);

UPDATE moviegenres
SET genre_id = genres.genre_id
FROM genres
WHERE genres.genre = moviegenres.genre;

ALTER TABLE moviegenres
    DROP COLUMN genre;

--CREACION DE TABLA PARA EL ATRIBUTO COUNTRY
CREATE TABLE countries(
	country_id SERIAL PRIMARY KEY,
	country varchar(32) NOT NULL
);

INSERT INTO countries (country) 
SELECT country FROM moviecountries GROUP BY country ORDER BY country ASC;

ALTER TABLE moviecountries
    ADD COLUMN country_id INTEGER,
    ADD FOREIGN KEY (country_id) REFERENCES countries(country_id);

UPDATE moviecountries
SET country_id = countries.country_id
FROM countries 
WHERE countries.country = moviecountries.country;

ALTER TABLE moviecountries
    DROP COLUMN country;

--CREACION DE TABLA PARA EL ATRBUTO LANGUAGE
CREATE TABLE language(
	language_id SERIAL PRIMARY KEY,
	language varchar(32) NOT NULL
);

INSERT INTO language (language) 
SELECT language FROM movielanguages GROUP BY language ORDER BY language ASC;

ALTER TABLE movielanguages
    ADD COLUMN language_id INTEGER,
    ADD FOREIGN KEY (language_id) REFERENCES language(language_id);

UPDATE movielanguages
SET language_id = language.language_id
FROM language 
WHERE language.language = movielanguages.language;

ALTER TABLE movielanguages
    DROP COLUMN language;

-------------------------------------------------------------------------

----------------------------------------------------------
----------ARREGLO DE PRIMARY KEYS Y FOREIGN KEYS----------
----------------------------------------------------------

-- AÑADIENDO FOREIGN KEYS A ACTORMOVIES
ALTER TABLE actormovies 
	ADD FOREIGN KEY (actorid) REFERENCES actors(actorid);

ALTER TABLE actormovies 
	ADD FOREIGN KEY (movieid) REFERENCES movies(movieid);

-- AÑADIENDO FOREIGN KEYS A INVENTORY
ALTER TABLE inventory 
	ADD FOREIGN KEY (prod_id) REFERENCES products(prod_id);

-- AÑADIENDO PRIMARY KEY A MOVIECOUNTRIES
ALTER TABLE moviecountries
	ADD PRIMARY KEY (movieid, country_id);

-- AÑADIENDO PRIMARY KEY A MOVIEGENRES
ALTER TABLE moviegenres
	ADD PRIMARY KEY (movieid, genre_id);

-- AÑADIENDO PRIMARY KEY A MOVIELANGUAGES
ALTER TABLE movielanguages
	ADD PRIMARY KEY (movieid, language_id, extrainformation);

-- AÑADIENDO PRIMARY KEY A ORDERDETAIL
-- PRIMERO HAY QUE JUNTAR LOS PEDIDOS QUE TENGAN MAS DE PRODUCTO DEL MISMO TIPO
CREATE TABLE aux AS 
	SELECT orderid, prod_id, SUM(quantity) AS quantity 
	FROM orderdetail 
	GROUP BY orderid, prod_id;
	
DROP TABLE orderdetail;
ALTER TABLE aux RENAME TO orderdetail;

ALTER TABLE orderdetail
    ADD COLUMN subtotal NUMERIC(10, 2);

ALTER TABLE orderdetail
	ADD PRIMARY KEY (orderid, prod_id);

-- AÑADIMOS PRIMARY KEY A ORDERS
ALTER TABLE orders
	ADD PRIMARY KEY (orderid);

-- AÑADIMOS FOREIGN KEY A ORDERS
ALTER TABLE orders 
	ADD FOREIGN KEY (customerid) REFERENCES customers(customerid);

-- AÑADIMOS FOREIGN KEYS A ORDERDETAIL
ALTER TABLE orderdetail 
	ADD FOREIGN KEY (prod_id) REFERENCES products(prod_id);

-- BORRAMOS LOS DATOS INCONSISTENTES DE ORDERDETAIL
DELETE FROM orderdetail
WHERE orderid = 88699;

DELETE FROM orderdetail
WHERE orderid = 146760;

DELETE FROM orderdetail
WHERE orderid = 147769;

DELETE FROM orderdetail
WHERE orderid = 161010;

DELETE FROM orderdetail
WHERE orderid = 174528;

ALTER TABLE orderdetail 
	ADD FOREIGN KEY (orderid) REFERENCES orders(orderid);

-- CONCATENAMOS EL USERNAME CON EL CUSTOMERID PARA PODER GESTIONAR LOS USUARIOS CON EL MISMO NOMBRE DE USUARIO
UPDATE customers
SET username = username || customerid
WHERE username IN (SELECT username
	  FROM customers
	  GROUP BY username
	  HAVING COUNT(username) > 1);

--BORRAMOS LAS FILAS QUE TIENEN DOS ANIOS EN MOVIES
UPDATE movies
SET year = '1998'
WHERE year = '1998-1999';

-------------------------------------------------------------------------

-------------------------------------
-- Creación de la tabla de alertas --
-------------------------------------

CREATE TABLE alerts(
	prod_id integer,
	fecha date DEFAULT now(),
	alerta text DEFAULT 'Stock ha llegado a 0'
);

ALTER TABLE alerts
ADD FOREIGN KEY(prod_id) REFERENCES products(prod_id);

---------------------------------------------------------------
-- Inserción de productos que faltaban en la tabla inventory --
---------------------------------------------------------------

-- EL STOCK SE GENERA DE FORMA ALEATORIA ENTRE 1 Y 900
INSERT INTO inventory
SELECT prod_id, ceil(random() * 900) AS stock, sales
FROM (SELECT prod_id, SUM(quantity) AS sales 
	  FROM orderdetail 
	  GROUP BY prod_id ) AS orderdetail
WHERE orderdetail.prod_id NOT IN (SELECT prod_id FROM inventory);

------------------------------------------------
-- Cambio de algunos datatypes inconsistentes --
------------------------------------------------

ALTER TABLE movies 
	ALTER COLUMN year TYPE integer USING year::integer;

ALTER TABLE orders
	ALTER COLUMN totalamount TYPE numeric(10,2);

ALTER TABLE orders
	ALTER COLUMN netamount TYPE numeric(10,2);	

---------------------------------
-- Creacion de algunos indices --
---------------------------------

CREATE INDEX idx_movietitle ON movies(movietitle);

CREATE INDEX idx_username ON customers(username);

CREATE INDEX idx_genre_id ON moviegenres(genre_id);

----------------------------------------------------------------------------------------------
-- Cambiamos algunos valores por defecto para que sean más aproximados a nuestra aplicacion --
----------------------------------------------------------------------------------------------

ALTER TABLE orders
	ALTER COLUMN totalamount SET DEFAULT 0;

ALTER TABLE orders
	ALTER COLUMN netamount SET DEFAULT 0;

ALTER TABLE orders
	ALTER COLUMN status SET DEFAULT 1;

ALTER TABLE orders
	ALTER COLUMN tax SET DEFAULT 21;

ALTER TABLE orders
	ALTER COLUMN orderdate SET DEFAULT now();	

ALTER TABLE orderdetail
	ALTER COLUMN quantity SET DEFAULT 1;

-- COMO EN NUESTRA APLICACION NO GESTIONAREMOS MUCHOS DE LOS DATOS QUE SE NOS PROPORCIONAN DE LOS USUARIOS
-- ACEPTAREMOS LOS VALORES QUE NO VAMOS A UTILIZAR COMO NOT NULL
-- PENDIENTE DE MANDAR CORREO AL PIBE PARA PREGUNTAR SI PODEMOS QUITAR ESOS VALORES
ALTER TABLE customers
	RENAME COLUMN firstname TO name;

ALTER TABLE customers
	ALTER COLUMN lastname DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN address1 DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN city DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN country DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN region DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN email SET NOT NULL;

ALTER TABLE customers
	ALTER COLUMN creditcardtype DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN creditcardexpiration DROP NOT NULL;

ALTER TABLE customers
	ALTER COLUMN gender DROP NOT NULL;

-- CREAMOS UNA SEQUENCIA PARA orderid DESDE EL ULTIMO VALOR
CREATE SEQUENCE orders_orderid_seq
    START WITH 181791 -- Ultimo valor de orderid en orders
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE orders
	ALTER COLUMN orderid SET DEFAULT nextval('orders_orderid_seq');

-------------------------------------------------------------------------------------------
-----------ARREGLO PARA MEJORAR LA ELIMINACION DE DATOS EN NUESTRA BASE DE DATOS-----------
-------------------------------------------------------------------------------------------
-- ARREGLO ACTORMOVIES
ALTER TABLE actormovies DROP CONSTRAINT actormovies_actorid_fkey;
ALTER TABLE actormovies ADD CONSTRAINT actormovies_actorid_fkey FOREIGN KEY (actorid) REFERENCES public.actors(actorid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;
ALTER TABLE actormovies DROP CONSTRAINT actormovies_movieid_fkey;
ALTER TABLE actormovies ADD CONSTRAINT actormovies_movieid_fkey FOREIGN KEY (movieid) REFERENCES public.movies(movieid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO ALERTS
ALTER TABLE alerts DROP CONSTRAINT alerts_prod_id_fkey;
ALTER TABLE alerts ADD CONSTRAINT alerts_prod_id_fkey FOREIGN KEY (prod_id) REFERENCES public.products(prod_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO DIRECTORMOVIES
ALTER TABLE directormovies DROP CONSTRAINT imdb_directormovies_directorid_fkey;
ALTER TABLE directormovies ADD CONSTRAINT directormovies_directorid_fkey FOREIGN KEY (directorid) REFERENCES public.directors(directorid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;
ALTER TABLE directormovies DROP CONSTRAINT imdb_directormovies_movieid_fkey;
ALTER TABLE directormovies ADD CONSTRAINT directormovies_movieid_fkey FOREIGN KEY (movieid) REFERENCES public.movies(movieid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO INVENTORY
ALTER TABLE inventory DROP CONSTRAINT inventory_prod_id_fkey;
ALTER TABLE inventory ADD CONSTRAINT inventory_prod_id_fkey FOREIGN KEY (prod_id) REFERENCES public.products(prod_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO MOVIECOUNTRIES
ALTER TABLE moviecountries DROP CONSTRAINT imdb_moviecountries_movieid_fkey;
ALTER TABLE moviecountries ADD CONSTRAINT moviecountries_movieid_fkey FOREIGN KEY (movieid) REFERENCES public.movies(movieid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;
ALTER TABLE moviecountries DROP CONSTRAINT moviecountries_country_id_fkey;
ALTER TABLE moviecountries ADD CONSTRAINT moviecountries_country_id_fkey FOREIGN KEY (country_id) REFERENCES public.countries(country_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO MOVIEGENRES
ALTER TABLE moviegenres DROP CONSTRAINT imdb_moviegenres_movieid_fkey;
ALTER TABLE moviegenres ADD CONSTRAINT moviegenres_movieid_fkey FOREIGN KEY (movieid) REFERENCES public.movies(movieid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;
ALTER TABLE moviegenres DROP CONSTRAINT moviegenres_genre_id_fkey;
ALTER TABLE moviegenres ADD CONSTRAINT moviegenres_genre_id_fkey FOREIGN KEY (genre_id) REFERENCES public.genres(genre_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO MOVIELANGUAGES
ALTER TABLE movielanguages DROP CONSTRAINT imdb_movielanguages_movieid_fkey;
ALTER TABLE movielanguages ADD CONSTRAINT movielanguages_movieid_fkey FOREIGN KEY (movieid) REFERENCES public.movies(movieid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;
ALTER TABLE movielanguages DROP CONSTRAINT movielanguages_language_id_fkey;
ALTER TABLE movielanguages ADD CONSTRAINT movielanguages_language_id_fkey FOREIGN KEY (language_id) REFERENCES public.language(language_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO ORDERDETAIL
ALTER TABLE orderdetail DROP CONSTRAINT orderdetail_prod_id_fkey;
ALTER TABLE orderdetail ADD CONSTRAINT orderdetail_prod_id_fkey FOREIGN KEY (prod_id) REFERENCES public.products(prod_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;
ALTER TABLE orderdetail DROP CONSTRAINT orderdetail_orderid_fkey;
ALTER TABLE orderdetail ADD CONSTRAINT orderdetail_orderid_fkey FOREIGN KEY (orderid) REFERENCES public.orders(orderid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO ORDERS
ALTER TABLE orders DROP CONSTRAINT orders_customerid_fkey;
ALTER TABLE orders ADD CONSTRAINT orders_customerid_fkey FOREIGN KEY (customerid) REFERENCES public.customers(customerid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO PRODUCTS
ALTER TABLE products DROP CONSTRAINT products_movieid_fkey;
ALTER TABLE products ADD CONSTRAINT products_movieid_fkey FOREIGN KEY (movieid) REFERENCES public.movies(movieid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE CASCADE;

-- ARREGLO DE SECUENCIAS
ALTER SEQUENCE customers_customerid_seq RESTART WITH 14094;

-- ARREGLO DE LA TABLA DE PELICULAS PARA GUARDAR EL NOMBRE DE LA IMAGEN QUE SE CORRESPONDE CON LA PELICULA
ALTER TABLE movies ADD COLUMN imagen varchar(100);

--ANIADIMOS LAS PELICULAS DEL TOP VENTAS Y AL RESTO DE PELICULAS LE PONEMOS UNA IMAGEN PREDETERMINADA
UPDATE movies SET imagen = 'imagenStandar.jpeg';
UPDATE movies SET imagen = 'illtown.jpeg' WHERE movietitle = 'Illtown (1996)';
UPDATE movies SET imagen = 'mago-de-oz.jpeg' WHERE movietitle = 'Wizard of Oz, The (1939)';
UPDATE movies SET imagen = 'life-less.jpeg' WHERE movietitle = 'Life Less Ordinary, A (1997)';
UPDATE movies SET imagen = 'gang-related.jpeg' WHERE movietitle = 'Gang Related (1997)';


