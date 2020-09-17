#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "alfabeto.h"
#include "estado.h"
#include "afnd.h"
#include "palabra.h"


int main(int argc, char ** argv)
{

	AFND * p_afnd_l0;
	AFND * p_afnd_l1;
	AFND * p_afnd_l2;
	AFND * p_afnd_l3;
	AFND * p_afnd_l4;
	AFND * p_afnd_l5;
	AFND * p_afnd_l6;


/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESION REGULAR “1” */
        p_afnd_l1 = AFND1ODeSimbolo("1");
/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESION REGULAR “0” */
        p_afnd_l0 = AFND1ODeSimbolo("0");
/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESION REGULAR “1”.”1” */
        p_afnd_l2 = AFND1OConcatena(p_afnd_l1, p_afnd_l1);
/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESION REGULAR “0”+“1” */
        p_afnd_l4 = AFND1OUne(p_afnd_l0, p_afnd_l1);
/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESIÓN ( “0”+”1” ) *  */
        p_afnd_l5 = AFND1OEstrella(p_afnd_l4);
/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESIÓN “1”.”1”.( “0”+”1” )*   */
        p_afnd_l3 = AFND1OConcatena(p_afnd_l2, p_afnd_l5);


/* SE CREA UN AUTÓMATA FINITO PARA LA EXPRESIÓN "1" * */
	p_afnd_l6 = AFND1OEstrella(p_afnd_l1);
	
/* SE CALCULA EL CIERRE REFLEXIVO-TRANSITIVO DE TODOS LOS AUTÓMATAS */
      AFNDCierraLTransicion(p_afnd_l0);
      AFNDCierraLTransicion(p_afnd_l1);
      AFNDCierraLTransicion(p_afnd_l2);
      AFNDCierraLTransicion(p_afnd_l3);
      AFNDCierraLTransicion(p_afnd_l4);
      AFNDCierraLTransicion(p_afnd_l5);
      AFNDCierraLTransicion(p_afnd_l6);

	
/********************************************************/
	fprintf(stdout,"EJEMPLO DE AUTÓMATA DE UNA EXPRESIÓN CORRESPONDIENTE A UN SÍMBOLO: \"1\"\n");
	AFNDImprime(stdout,p_afnd_l1);


	fprintf(stdout,"EJEMPLO DE AUTÓMATA DE UNA EXPRESIÓN CORRESPONDIENTE A LA CONCATENACIÓN DE OTROS DOS, JUSTAMENTE CONCATENA EL ANTERIOR CONSIGO MISMO\n");
	AFNDImprime(stdout,p_afnd_l2);


	fprintf(stdout,"A CONTINUACIÓN SE VA A MOSTRARA LA UNIÓN DE DOS AUTÓMATAS QUE VIENEN DE EXPRESIONES REGULARES, UNO DE ELLOS ES EL CORRESPONDIENTE A LA EXPRESION \"1\" QUE YA SE MOSTRÓ ANTERIORMENTE, EL OTRO ES EL DE LA EXPRESIÓN \"0\" QUE SE MUESTRA A CONTINUACIÓN\n"); 
	AFNDImprime(stdout,p_afnd_l0);


	fprintf(stdout, "Y ESTA ES SU UNIÓN\n");
	AFNDImprime(stdout,p_afnd_l4);

	fprintf(stdout,"SE MUESTRA EL AUTÓMATA FINITO CORRESPONDIENTE A LA EXPRESION \"1\" * A PARTIR DEL AUTÓMATA ASOCIADO CON \"1\" QUE YA SE MOSTRÓ ANTERIORMENTE\n"); 
	AFNDImprime(stdout,p_afnd_l6);


/********************************************************/
	fprintf(stdout,"Y, A CONTINUACIÓN, ALGUNOS EJEMPLOS DE PROCESADO DE CADENAS DEL AUTÓMATA DE LA EXPRESIÓN 11(0+1)*\n"); 
	AFNDImprime(stdout,p_afnd_l3);
	fprintf(stdout,"\tLA CADENA 11 ES RECONOCIDA POR EL AUTOMATA DE 11(0+1)* POR SU PRINCIPIO\n"); 
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDImprimeCadenaActual(stdout,p_afnd_l3);
	AFNDInicializaEstado(p_afnd_l3);
	AFNDProcesaEntrada(stdout,p_afnd_l3);
	AFNDInicializaCadenaActual(p_afnd_l3);
	fprintf(stdout,"\tLA CADENA 110 TAMBIÉN ES RECONOCIDA POR EL AUTOMATA DE 11(0+1)* \n"); 
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"0");
	AFNDImprimeCadenaActual(stdout,p_afnd_l3);
	AFNDInicializaEstado(p_afnd_l3);
	AFNDProcesaEntrada(stdout,p_afnd_l3);
	AFNDInicializaCadenaActual(p_afnd_l3);
	fprintf(stdout,"\tLA CADENA 111 TAMBIÉN ES RECONOCIDA POR EL AUTOMATA DE 11(0+1)* \n"); 
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDImprimeCadenaActual(stdout,p_afnd_l3);
	AFNDInicializaEstado(p_afnd_l3);
	AFNDProcesaEntrada(stdout,p_afnd_l3);
	AFNDInicializaCadenaActual(p_afnd_l3);
	fprintf(stdout,"\tLA CADENA 1111001 TAMBIÉN ES RECONOCIDA POR EL AUTOMATA DE 11(0+1)* \n"); 
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDInsertaLetra(p_afnd_l3,"0");
	AFNDInsertaLetra(p_afnd_l3,"0");
	AFNDInsertaLetra(p_afnd_l3,"1");
	AFNDImprimeCadenaActual(stdout,p_afnd_l3);
	AFNDInicializaEstado(p_afnd_l3);
	AFNDProcesaEntrada(stdout,p_afnd_l3);
	AFNDInicializaCadenaActual(p_afnd_l3);
	fprintf(stdout,"\tLA CADENA VACIA SIN EMBARGO NO ES RECONOCIDA POR EL AUTOMATA DE 11(0+1)* \n"); 
	AFNDImprimeCadenaActual(stdout,p_afnd_l3);
	AFNDInicializaEstado(p_afnd_l3);
	AFNDProcesaEntrada(stdout,p_afnd_l3);
	AFNDInicializaCadenaActual(p_afnd_l3);

/********************************************************/
/********************************************************/
	/* PRUEBA IMPRESION A DOT */
	
	AFNDADot(p_afnd_l0);
	AFNDADot(p_afnd_l1);
	AFNDADot(p_afnd_l2);
	AFNDADot(p_afnd_l3);
	AFNDADot(p_afnd_l4);
	AFNDADot(p_afnd_l5);
	AFNDADot(p_afnd_l6);


/********************************************************/
	AFNDElimina(p_afnd_l0);
	AFNDElimina(p_afnd_l1);
	AFNDElimina(p_afnd_l2);
	AFNDElimina(p_afnd_l3);
	AFNDElimina(p_afnd_l4);
	AFNDElimina(p_afnd_l5);
	AFNDElimina(p_afnd_l6);
/********************************************************************************/
/********************************************************************************/

	return 0;

}


