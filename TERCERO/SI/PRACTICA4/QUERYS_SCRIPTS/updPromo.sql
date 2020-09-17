# Aniadimos la columna promo a la tabla customers
ALTER TABLE customers ADD COLUMN promo decimal(5,2);

# Primero creamos la funcion que ejecutara nuestro trigger
CREATE OR REPLACE FUNCTION upd_products()
   BEGIN
      UPDATE orderdetail SET price = price - b.price * promo 
      FROM (customers AS a NATURAL JOIN PRODUCTS AS b NATURAL JOIN ORDERDETAIL AS c NATURAL JOIN orders AS d) 
      WHERE a.customerid = d.customerid AND d.orderid = c.orderid AND c.orderid = b.orderid
   END;

# Pasamos ahora a crear el trigger sobre la tabla customers
CREATE TRIGGER trigger_upd_products AFTER UPDATE OF promo ON customers FOR EACH ROW EXECUTE PROCEDURE upd_products;