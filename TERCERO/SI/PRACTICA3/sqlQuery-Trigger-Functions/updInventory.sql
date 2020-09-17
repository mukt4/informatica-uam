CREATE OR REPLACE FUNCTION updInventory()
RETURNS TRIGGER
AS $$
DECLARE
	temp1 RECORD;
BEGIN
	IF NEW.status = 2 THEN

		FOR temp1 IN    SELECT prod_id, quantity, stock
				FROM orders NATURAL JOIN orderdetail NATURAL JOIN products NATURAL JOIN inventory
				WHERE orderid = NEW.orderid
		LOOP
			IF (temp1.stock - temp1.quantity) < 0 THEN
				RAISE NOTICE 'No hay suficientes items en stock de ese producto';
				INSERT INTO alerts
				VALUES(temp1.prod_id, 'No hay suficientes items en stock de ese producto');
				RETURN NULL;
			
			ELSIF (temp1.stock - temp1.quantity) = 0 THEN
				INSERT INTO alerts(prod_id)
				VALUES(temp1.prod_id);
			END IF;
		END LOOP;
		
		UPDATE inventory
		SET (stock, sales) = (stock - quantity, sales + quantity)
		FROM (  SELECT prod_id, quantity
			FROM orders NATURAL JOIN orderdetail NATURAL JOIN products
			WHERE orderid = NEW.orderid  ) AS natural_join
		WHERE inventory.prod_id = natural_join.prod_id;
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER t_updInventory BEFORE UPDATE 
ON orders FOR EACH ROW 
WHEN (OLD.status IS DISTINCT FROM NEW.status)
EXECUTE PROCEDURE updInventory();