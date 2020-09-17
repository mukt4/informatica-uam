CREATE OR REPLACE FUNCTION getTopMonths(integer, integer) 
RETURNS TABLE(
	año int,
	mes int, 
	articulos numeric(10,0), 
	euros numeric(10,2)) 
AS $$
BEGIN
	RETURN QUERY SELECT extract('year' from orderdate)::int AS anio, extract('month' from orderdate)::int AS mes, SUM(quantity) as productos, SUM(subtotal) as importe
			FROM orders NATURAL JOIN orderdetail
			GROUP BY anio, mes
			HAVING SUM(quantity) > $1 OR SUM(subtotal) > $2
			ORDER BY anio;
END;
$$ LANGUAGE plpgsql;

SELECT * FROM getTopMonths(19000, 320000)