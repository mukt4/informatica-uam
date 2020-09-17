#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "afnd.h"

/* ESTOS INCLUDES DEPENDEN DE LA IMPLEMENTACIÓN, TAL VEZ TÚ DISEÑES OTROS MÓDULOS */
#include "alfabeto.h"
#include "estado.h"
#include "palabra.h"


int main(int argc, char** argv)
{

/* DECLARACIÓN DE UN PUNTERO A UN AFND */
    AFND * p_afnd;


/* INICIALIZACIÓN DE UN NUEVO AFND DE NOMBRE afTestLetras Y CON 5 ESTADOS Y 4 SÍMBOLOS EN SU ALFABETO */
    p_afnd = AFNDNuevo("afTestLetras",5,4);
    

/* DEFINICIÓN DEL ALFABETO DEL AFND */
    AFNDInsertaSimbolo(p_afnd,"A");
    AFNDInsertaSimbolo(p_afnd,"b");
    AFNDInsertaSimbolo(p_afnd,"C");
    AFNDInsertaSimbolo(p_afnd,"d");

/* DEFINICIÓN DEL CONJUNTO DE ESTADOS */
    AFNDInsertaEstado(p_afnd,"q0",INICIAL);
    AFNDInsertaEstado(p_afnd,"q1",NORMAL);
    AFNDInsertaEstado(p_afnd,"q2",NORMAL);
    AFNDInsertaEstado(p_afnd,"q3",NORMAL);
    AFNDInsertaEstado(p_afnd,"qf",FINAL);

/* DEFINICIÓN DE LAS TRANSICIONES NO LAMBDA */
    AFNDInsertaTransicion(p_afnd, "q0", "A", "q0");
    AFNDInsertaTransicion(p_afnd, "q0", "b", "q0");
    AFNDInsertaTransicion(p_afnd, "q0", "C", "qf");
    AFNDInsertaTransicion(p_afnd, "q1", "d", "q2");
    AFNDInsertaTransicion(p_afnd, "q0", "A", "q1");
    AFNDInsertaTransicion(p_afnd, "q1", "b", "qf");
    AFNDInsertaTransicion(p_afnd, "q2", "d", "q3");
    AFNDInsertaTransicion(p_afnd, "q2", "C", "q2");
    AFNDInsertaTransicion(p_afnd, "q1", "d", "q1");
    AFNDInsertaTransicion(p_afnd, "q1", "C", "q1");

/* SE MUESTRA EL AFND DEFINIDO */
    fprintf(stdout,"\n****************** AFND *********************\n");
    AFNDImprime(stdout,p_afnd);
    fprintf(stdout,"\n*********************************************\n");

/* DEFINICIÓN DE LA CADENA DE ENTRADA [ 0 1 0 1 1 ] */
    p_afnd= AFNDInsertaLetra(p_afnd,"b");
    p_afnd= AFNDInsertaLetra(p_afnd,"b");
    p_afnd= AFNDInsertaLetra(p_afnd,"A");
    p_afnd= AFNDInsertaLetra(p_afnd,"d");
    p_afnd= AFNDInsertaLetra(p_afnd,"d");
    p_afnd= AFNDInsertaLetra(p_afnd,"C");
    p_afnd= AFNDInsertaLetra(p_afnd,"d");
    p_afnd= AFNDInsertaLetra(p_afnd,"d");
    p_afnd= AFNDInsertaLetra(p_afnd,"b");

/* SE ESTABLECE COMO ESTADO ACTUAL DEL AUTÓMATA EL INICIAL */

    AFNDInicializaEstado (p_afnd);

/* SE MUESTRA LA CADENA ACTUAL */

    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd);
    fprintf(stdout,"\n*********************************************\n");

/* SE PROCESA LA CADENA DE ENTRADA ACTUAL MOSTRANDO UNA TRAZA DEL FUNCIONAMIENTO DEL AUTOMATA: EN CADA PASO DE ANÁLISIS SE MUESTRA LA CADENA ACTUAL Y EL CONJUNTO DE ESTADOS EN LOS QUE SE ENCUENTRA EL AUTÓMATA */

    AFNDProcesaEntrada(stdout,p_afnd);

/* SE LIBERAN TODOS LOS RECURSOS ASOCIADOS CON EL AFND */
    AFNDElimina(p_afnd);

    return 0;
}
