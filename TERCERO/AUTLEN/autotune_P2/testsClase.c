#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "alfabeto.h"
#include "estado.h"
#include "afnd.h"
#include "palabra.h"


int main(int argc, char ** argv)
{

    AFND * p_afnd_l;

    p_afnd_l = AFNDNuevo("aflPruebaP.Vacia",2,1);

    AFNDInsertaEstado(p_afnd_l,"q0",INICIAL);
    AFNDInsertaEstado(p_afnd_l,"qf",FINAL);

    AFNDInsertaLTransicion(p_afnd_l, "q0", "qf");

    AFNDCierraLTransicion(p_afnd_l);
    
    fprintf(stdout,"\n******************* AFND1 *******************\n");

    AFNDImprime(stdout,p_afnd_l);

    p_afnd_l = AFNDInicializaEstado(p_afnd_l);

    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd_l);
    fprintf(stdout,"\n*********************************************\n");

    AFNDProcesaEntrada(stdout,p_afnd_l);


/*********************************************************************************/

    AFNDElimina(p_afnd_l);

/********************************************************************************/

    p_afnd_l = AFNDNuevo("aflPruebaFinConL",3,1);

    AFNDInsertaSimbolo(p_afnd_l,"A");

    AFNDInsertaEstado(p_afnd_l,"q0",INICIAL);
    AFNDInsertaEstado(p_afnd_l,"q1",NORMAL);
    AFNDInsertaEstado(p_afnd_l,"qf",FINAL);

    AFNDInsertaTransicion(p_afnd_l, "q0", "A", "q1");

    AFNDInsertaLTransicion(p_afnd_l, "q1", "qf");
    AFNDInsertaLTransicion(p_afnd_l, "q0", "qf");

    AFNDCierraLTransicion(p_afnd_l);
    
    fprintf(stdout,"\n******************* AFND2 *******************\n");

    AFNDImprime(stdout,p_afnd_l);

    AFNDInsertaLetra(p_afnd_l,"A");

    p_afnd_l = AFNDInicializaEstado(p_afnd_l);

    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd_l);
    fprintf(stdout,"\n*********************************************\n");

    AFNDProcesaEntrada(stdout,p_afnd_l);
    
    p_afnd_l = AFNDInicializaCadenaActual(p_afnd_l);

    p_afnd_l = AFNDInicializaEstado(p_afnd_l);

    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd_l);
    fprintf(stdout,"\n*********************************************\n");

    AFNDProcesaEntrada(stdout,p_afnd_l);


/*********************************************************************************/

    AFNDElimina(p_afnd_l);

/********************************************************************************/

    p_afnd_l = AFNDNuevo("afl1",6,2);

    AFNDInsertaSimbolo(p_afnd_l,"A");
    AFNDInsertaSimbolo(p_afnd_l,"A");
    AFNDInsertaSimbolo(p_afnd_l,"B");

    AFNDInsertaEstado(p_afnd_l,"q0",INICIAL);
    AFNDInsertaEstado(p_afnd_l,"q1",NORMAL);
    AFNDInsertaEstado(p_afnd_l,"q2",NORMAL);
    AFNDInsertaEstado(p_afnd_l,"q3",NORMAL);
    AFNDInsertaEstado(p_afnd_l,"q4",NORMAL);
    AFNDInsertaEstado(p_afnd_l,"qf",FINAL);

    AFNDInsertaTransicion(p_afnd_l, "q2", "A", "q3");
    AFNDInsertaTransicion(p_afnd_l, "q1", "B", "q4");
    AFNDInsertaTransicion(p_afnd_l, "q2", "B", "q4");

    AFNDInsertaLTransicion(p_afnd_l, "q0", "q1");
    AFNDInsertaLTransicion(p_afnd_l, "q1", "q2");
    AFNDInsertaLTransicion(p_afnd_l, "q3", "q0");
    AFNDInsertaLTransicion(p_afnd_l, "q4", "q3");
    AFNDInsertaLTransicion(p_afnd_l, "q2", "qf");

    AFNDCierraLTransicion(p_afnd_l);

    AFNDImprime(stdout,p_afnd_l);
    
    fprintf(stdout,"\n******************* AFND3 *******************\n");

    AFNDInsertaLetra(p_afnd_l,"A");

    p_afnd_l = AFNDInicializaEstado(p_afnd_l);

    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd_l);
    fprintf(stdout,"\n*********************************************\n");

    AFNDProcesaEntrada(stdout,p_afnd_l);
    
    p_afnd_l = AFNDInicializaCadenaActual(p_afnd_l);
    
    AFNDInsertaLetra(p_afnd_l,"A");
    AFNDInsertaLetra(p_afnd_l,"C");
    AFNDInsertaLetra(p_afnd_l,"B");

    p_afnd_l = AFNDInicializaEstado(p_afnd_l);

    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd_l);
    fprintf(stdout,"\n*********************************************\n");

    AFNDProcesaEntrada(stdout,p_afnd_l);


/*********************************************************************************/

    AFNDElimina(p_afnd_l);

/********************************************************************************/


    return 0;

}

