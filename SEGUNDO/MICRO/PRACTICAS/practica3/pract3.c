#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0
#define NUMEROINTENTOS 10

unsigned int comprobarNumeroSecreto(unsigned char* numero);

void rellenarIntento(unsigned int intento, unsigned char* intentoDigitos);

unsigned int calcularAciertos(unsigned char* numSecreto, unsigned char* intentoDigitos);

unsigned int calcularSemiaciertos(unsigned char* numSecreto, unsigned char* intentoDigitos);

//////////////////////////////////////////////////////////////////////////
///// -------------------------- MAIN ------------------------------ /////
//////////////////////////////////////////////////////////////////////////

int main( void ){
	int t;
	unsigned char numSecreto[4];
	unsigned char intentoDigitos[4];
	unsigned int numIntentos, intento, aciertos, semiaciertos, repetido, i;

	srand((unsigned) time(&t));

	do {
		for (i=0; i<4; i++)
			numSecreto[i] = rand() % 10;
		
		repetido = comprobarNumeroSecreto(numSecreto);
	} while (repetido == TRUE);
	
	numIntentos = 0;
	
	do {
		numIntentos++;
		do {
			printf("Introduzca intento %u [0000 - 9999]: ", numIntentos );
			scanf("%u", &intento);
		} while ( intento > 9999);

		rellenarIntento( intento, intentoDigitos );
		aciertos = calcularAciertos(numSecreto, intentoDigitos);
		semiaciertos = calcularSemiaciertos(numSecreto, intentoDigitos);
		printf("Numero de Aciertos: %u\t", aciertos);
		printf("Numero de Semiaciertos: %u\n", semiaciertos );
	} while ((aciertos != 4) && (numIntentos != NUMEROINTENTOS));

	if (aciertos == 4)
		printf("Combinacion correcta: HAS GANADO!!!\n");
	else
		printf("Numero de intentos excedido: HAS PERDIDO :(\n");

	printf("Numero secreto: %u%u%u%u\n", numSecreto[0], numSecreto[1], numSecreto[2], numSecreto[3]);

	return 0;
}