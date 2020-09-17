CREATE OR REPLACE FUNCTION setAmountOrder() RETURNS VOID
AS $$
BEGIN
	UPDATE orders
	SET netamount = sum
	FROM 	(SELECT orderid, SUM(subtotal)
			 FROM orderdetail GROUP BY orderid) AS aux
	WHERE orders.orderid = aux.orderid;

	UPDATE orders
	SET totalamount = netamount + (netamount * (tax/100));	
END;
$$ LANGUAGE plpgsql;

SELECT setAmountOrder();