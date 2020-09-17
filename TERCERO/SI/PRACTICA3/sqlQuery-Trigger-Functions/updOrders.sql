CREATE OR REPLACE FUNCTION updOrders()
RETURNS TRIGGER
AS $$
BEGIN
	-- CASO DE INSERCION EN LA TABLA ORDERDETAIL
	IF (TG_OP = 'INSERT') THEN
		NEW.subtotal = (SELECT price FROM products WHERE products.prod_id = NEW.prod_id) * NEW.quantity;

		UPDATE orders
		SET netamount = netamount + NEW.subtotal
		WHERE orders.orderid = NEW.orderid;

		UPDATE orders
		SET totalamount = netamount + (netamount * (tax/100))
		WHERE orders.orderid = NEW.orderid;
		RETURN NEW;
		
	-- CASO DE BORRADO EN LA TABLA ORDERDETAIL
	ELSIF (TG_OP = 'DELETE') THEN
		UPDATE orders
		SET netamount = netamount - OLD.subtotal
		WHERE orders.orderid = OLD.orderid;
		
		UPDATE orders
		SET totalamount = netamount + (netamount * (tax/100))
		WHERE orders.orderid = OLD.orderid;
		RETURN OLD;
		
	-- CASO DE UPDATE EN LA TABLA ORDERDETAIL
	ELSE
		NEW.subtotal = (SELECT price FROM products WHERE products.prod_id = OLD.prod_id) * NEW.quantity;

		UPDATE orders
		SET netamount = netamount + (NEW.subtotal - OLD.subtotal)
		WHERE orders.orderid = NEW.orderid;

		UPDATE orders
		SET totalamount = netamount + (netamount * (tax/100))
		WHERE orders.orderid = NEW.orderid;
		RETURN NEW;		
	END IF;
	
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER t_updOrders BEFORE INSERT OR UPDATE OR DELETE
ON orderdetail FOR EACH ROW 
EXECUTE PROCEDURE updOrders();