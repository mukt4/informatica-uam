CREATE OR REPLACE FUNCTION getTopVentas(integer) 
RETURNS TABLE(
	año int, 
	pelicula varchar(255), 
	ventas numeric) 
AS $$
BEGIN
	RETURN QUERY 
	SELECT DISTINCT ON (year) EXTRACT(YEAR FROM orderdate)::int AS year, movietitle, SUM(quantity) AS sales
	FROM movies NATURAL JOIN products NATURAL JOIN orderdetail NATURAL JOIN orders
	WHERE EXTRACT(YEAR FROM orderdate)::int >= $1
	GROUP BY movies.movieid, EXTRACT(YEAR FROM (DATE(orders.orderdate)))
	ORDER BY year, sales DESC;
END;
$$ LANGUAGE plpgsql;

SELECT * FROM getTopVentas(2015);