#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>
#include "odbc.h"

int main(int argc, char** argv){
	SQLHENV env;
    SQLHDBC dbc;
    SQLHSTMT stmt;
    SQLRETURN ret; /* ODBC API return status */
    char consulta[300];
	int i;
	int id_usuario;
	int id_edicion;
	int id_venta;
	double precio;
	double precio_total=0;
	

	
	/* CONNECT */
    ret = odbc_connect(&env, &dbc);

    if (!SQL_SUCCEEDED(ret)) {
        return EXIT_FAILURE;
    }
	
	if(argc<3){
		printf("ERROR EN LOS ARGUMENTOS\n");	
		printf("compra add screen_name fecha isbn isbn .... isbn \n");
		printf("compra del factura_id\n");
		/* DISCONNECT */
    	ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}
		return EXIT_FAILURE;
	}
	
	if(strcmp(argv[1],"del")){
		sprintf(consulta,"DELETE FROM venta where venta.id_venta='%s'",argv[2]);
		SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
		if (!SQL_SUCCEEDED(ret)) {
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
	}
	else if(strcmp(argv[1],"add")){
		sprintf(consulta,"SELECT id_usuario from usuario where usuario.nombre='%s'",argv[2]);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
		if (!SQL_SUCCEEDED(ret)) {
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
		ret = SQLGetData(stmt, 1, SQL_C_SLONG, &id_usuario, sizeof(SQLINTEGER), NULL);
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);

		if (!SQL_SUCCEEDED(ret)) {
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
		sprintf(consulta,"INSERT INTO venta(id_usuario,fecha_venta) values (%d,%s)",id_usuario,argv[3]);
		SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
		if (!SQL_SUCCEEDED(ret)) {
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		sprintf(consulta,"SELECT max(id_venta) from venta");
		SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
		if (!SQL_SUCCEEDED(ret)) {
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
		ret = SQLGetData(stmt, 1, SQL_C_SLONG, &id_venta, sizeof(SQLINTEGER), NULL);
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		if (!SQL_SUCCEEDED(ret)) {
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
		for(i=4;i<argc;i++){
			sprintf(consulta,"SELECT precio from edicion where edicion.id_libro=%s",argv[i]);
			SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
			ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
			if (!SQL_SUCCEEDED(ret)) {
				SQLFreeHandle(SQL_HANDLE_STMT, stmt);
				/* DISCONNECT */
				ret = odbc_disconnect(env, dbc);
    			if (!SQL_SUCCEEDED(ret)) {
        			return EXIT_FAILURE;
    			}
        		return EXIT_FAILURE;
			}
			ret = SQLGetData(stmt, 1, SQL_C_SLONG, &precio, sizeof(SQLINTEGER), NULL);
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			if (!SQL_SUCCEEDED(ret)) {
				/* DISCONNECT */
				ret = odbc_disconnect(env, dbc);
    			if (!SQL_SUCCEEDED(ret)) {
        			return EXIT_FAILURE;
    			}
        		return EXIT_FAILURE;
			}
			sprintf(consulta,"SELECT id_edicion from edicion where edicion.id_libro=%s",argv[i]);
			SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
			ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
			if (!SQL_SUCCEEDED(ret)) {
				SQLFreeHandle(SQL_HANDLE_STMT, stmt);
				/* DISCONNECT */
				ret = odbc_disconnect(env, dbc);
    			if (!SQL_SUCCEEDED(ret)) {
        			return EXIT_FAILURE;
    			}
        		return EXIT_FAILURE;
			}
			ret = SQLGetData(stmt, 1, SQL_C_SLONG, &id_edicion, sizeof(SQLINTEGER), NULL);
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			if (!SQL_SUCCEEDED(ret)) {
				/* DISCONNECT */
				ret = odbc_disconnect(env, dbc);
    			if (!SQL_SUCCEEDED(ret)) {
        			return EXIT_FAILURE;
    			}
        		return EXIT_FAILURE;
			}
			precio_total+=precio;
			sprintf(consulta,"INSERT INTO vendes(id_venta,id_edicion) values (%d,%d)",id_venta,id_edicion);
			SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
			ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
			if (!SQL_SUCCEEDED(ret)) {
				SQLFreeHandle(SQL_HANDLE_STMT, stmt);
				/* DISCONNECT */
				ret = odbc_disconnect(env, dbc);
    			if (!SQL_SUCCEEDED(ret)) {
        			return EXIT_FAILURE;
    			}
        		return EXIT_FAILURE;
			}
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		}
		sprintf(consulta,"INSERT INTO venta(p_ini,p_final) values (%f,%f)",precio_total,(precio_total*0.9));
		SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
		if (!SQL_SUCCEEDED(ret)) {
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
		}
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	}
	else{
		printf("ERROR EN LOS ARGUMENTOS\n");
		printf("add o del\n");
		return EXIT_FAILURE;	
	}

	SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	/* DISCONNECT */
    ret = odbc_disconnect(env, dbc);
    if (!SQL_SUCCEEDED(ret)) {
        return EXIT_FAILURE;
    }
	return EXIT_SUCCESS;
}
