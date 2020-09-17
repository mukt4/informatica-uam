/**********************************************************************************************
 practica2.c                                                                                  *	
 Muestra las direciones Ethernet de la traza que se pasa como primer parametro.				  *	
 Debe complatarse con mas campos de niveles 2, 3, y 4 tal como se pida en el enunciado.       *
 Debe tener capacidad de dejar de analizar paquetes de acuerdo a un filtro.                   *
																							  *
 Compila: gcc -Wall -o practica2 practica2.c -lpcap, make                                     *
 Autor: Jose Luis Garcia Dorado, Jorge E. Lopez de Vergara Mendez, Rafael Leira, Javier Ramos *
 2018 EPS-UAM                                                                                 *
**********************************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <pcap.h>
#include <string.h>
#include <netinet/in.h>
#include <linux/udp.h>
#include <linux/tcp.h>
#include <signal.h>
#include <time.h>
#include <getopt.h>
#include <inttypes.h>
#include <stdint.h>

#include "practica2.h"

/*Definicion de constantes **************************************************************************************/
#define ETH_ALEN      6     					 	/* Tamanio de la direccion ethernet                         */
#define ETH_HLEN      14     						/* Tamanio de la cabecera ethernet                          */
#define ETH_TLEN      2      						/* Tamanio del campo tipo ethernet                          */
#define ETH_FRAME_MAX 1514   						/* Tamanio maximo la trama ethernet (sin CRC)               */
#define ETH_FRAME_MIN 60     						/* Tamanio minimo la trama ethernet (sin CRC)               */
#define ETH_DATA_MAX  (ETH_FRAME_MAX - ETH_HLEN)  	/* Tamano maximo de los datos de una trama ethernet         */
#define ETH_DATA_MIN  (ETH_FRAME_MIN - ETH_HLEN)	/* Tamano maximo de los datos de una trama ethernet         */
#define IP_ALEN 4									/* Tamanio de la direccion IP					            */
#define IP_HEADER 24 								/* Tamanio de la cabecera IP                                */
#define ROW_HEADER_IP 4								/* Tamanio de cada linea de la cabecera IP                  */ 
#define ROW_LEVEL_4_UDP 2							/* Tamanio de una fila en la cabecera de nivel 4 de tipo DUP*/
#define ROW_LEVEL_4_TCP 4							/* Tamanio de una fila en la cabecera de nivel 4 de tipo TCP*/
#define OK 0										/* Variables de control										*/
#define ERROR 1										/*															*/
#define PACK_READ 1									/*															*/
#define PACK_ERR -1									/*															*/
#define BREAKLOOP -2								/*															*/
#define NO_FILTER 0									/*															*/
#define NO_LIMIT -1									/*															*/
/****************************************************************************************************************/

/*Definicion de variables globales*******************************************************************************/
pcap_t *descr = NULL;																						  /**/
uint64_t contador = 0;																						  /**/
uint8_t ipsrc_filter[IP_ALEN] = {NO_FILTER};															      /**/
uint8_t ipdst_filter[IP_ALEN] = {NO_FILTER};																  /**/
uint16_t sport_filter= NO_FILTER;																			  /**/
uint16_t dport_filter = NO_FILTER;																			  /**/
/****************************************************************************************************************/

int main(int argc, char **argv)
{
	

	char errbuf[PCAP_ERRBUF_SIZE];
	
	int long_index = 0, retorno = 0;
	char opt;
	
	(void) errbuf; //indicamos al compilador que no nos importa que errbuf no se utilice. Esta linea debe ser eliminada en la entrega final.

	if (signal(SIGINT, handleSignal) == SIG_ERR) {
		printf("Error: Fallo al capturar la senal SIGINT.\n");
		exit(ERROR);
	}

	if (argc == 1) {
		printf("Ejecucion: %s <-f traza.pcap / -i eth0> [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]\n", argv[0]);
		exit(ERROR);
	}

	static struct option options[] = {
		{"f", required_argument, 0, 'f'},
		{"i",required_argument, 0,'i'},
		{"ipo", required_argument, 0, '1'},
		{"ipd", required_argument, 0, '2'},
		{"po", required_argument, 0, '3'},
		{"pd", required_argument, 0, '4'},
		{"h", no_argument, 0, '5'},
		{0, 0, 0, 0}
	};

	//Simple lectura por parametros por completar casos de error, ojo no cumple 100% los requisitos del enunciado!
	while ((opt = getopt_long_only(argc, argv, "f:i:1:2:3:4:5", options, &long_index)) != -1) {
		switch (opt) {
		case 'i' :
			if(descr) { // comprobamos que no se ha abierto ninguna otra interfaz o fichero
				printf("Ha seleccionado más de una fuente de datos\n");
				pcap_close(descr);
				exit(ERROR);
			}
		
			if ( (descr = pcap_open_live(optarg, ETH_DATA_MAX, 0, 100, errbuf)) == NULL){
				printf("Error: pcap_open_live(): Interface: %s, %s %s %d.\n", optarg,errbuf,__FILE__,__LINE__);
				exit(ERROR);
			}

			break;

		case 'f' :
			if(descr) { // comprobamos que no se ha abierto ninguna otra interfaz o fichero
				printf("Ha seleccionado más de una fuente de datos\n");
				pcap_close(descr);
				exit(ERROR);
			}

			if ((descr = pcap_open_offline(optarg, errbuf)) == NULL) {
				printf("Error: pcap_open_offline(): File: %s, %s %s %d.\n", optarg, errbuf, __FILE__, __LINE__);
				exit(ERROR);
			}

			break;

		case '1' :
			if (sscanf(optarg, "%"SCNu8".%"SCNu8".%"SCNu8".%"SCNu8"", &(ipsrc_filter[0]), &(ipsrc_filter[1]), &(ipsrc_filter[2]), &(ipsrc_filter[3])) != IP_ALEN) {
				printf("Error ipo_filtro. Ejecucion: %s /ruta/captura_pcap [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]: %d\n", argv[0], argc);
				exit(ERROR);
			}

			break;

		case '2' :
			if (sscanf(optarg, "%"SCNu8".%"SCNu8".%"SCNu8".%"SCNu8"", &(ipdst_filter[0]), &(ipdst_filter[1]), &(ipdst_filter[2]), &(ipdst_filter[3])) != IP_ALEN) {
				printf("Error ipd_filtro. Ejecucion: %s /ruta/captura_pcap [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]: %d\n", argv[0], argc);
				exit(ERROR);
			}

			break;

		case '3' :
			if ((sport_filter= atoi(optarg)) == 0) {
				printf("Error po_filtro.Ejecucion: %s /ruta/captura_pcap [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]: %d\n", argv[0], argc);
				exit(ERROR);
			}

			break;

		case '4' :
			if ((dport_filter = atoi(optarg)) == 0) {
				printf("Error pd_filtro. Ejecucion: %s /ruta/captura_pcap [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]: %d\n", argv[0], argc);
				exit(ERROR);
			}

			break;

		case '5' :
			printf("Ayuda. Ejecucion: %s <-f traza.pcap / -i eth0> [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]: %d\n", argv[0], argc);
			exit(ERROR);
			break;

		case '?' :
		default:
			printf("Error. Ejecucion: %s <-f traza.pcap / -i eth0> [-ipo IPO] [-ipd IPD] [-po PO] [-pd PD]: %d\n", argv[0], argc);
			exit(ERROR);
			break;
		}
	}

	if (!descr) {
		printf("No selecciono ningún origen de paquetes.\n");
		return ERROR;
	}

	//Simple comprobacion de la correcion de la lectura de parametros
	printf("Filtro:");
	if(ipsrc_filter[0]!=0)
		printf("ipsrc_filter:%"PRIu8".%"PRIu8".%"PRIu8".%"PRIu8"\t", ipsrc_filter[0], ipsrc_filter[1], ipsrc_filter[2], ipsrc_filter[3]);
	if(ipdst_filter[0]!=0)
		printf("ipdst_filter:%"PRIu8".%"PRIu8".%"PRIu8".%"PRIu8"\t", ipdst_filter[0], ipdst_filter[1], ipdst_filter[2], ipdst_filter[3]);

	if (sport_filter!= NO_FILTER) {
		printf("po_filtro=%"PRIu16"\t", sport_filter);
	}

	if (dport_filter != NO_FILTER) {
		printf("pd_filtro=%"PRIu16"\t", dport_filter);
	}

	printf("\n\n");

	retorno=pcap_loop(descr,NO_LIMIT,analizar_paquete,NULL);
	switch(retorno)	{
		case OK:
			printf("Traza leída\n");
			break;
		case PACK_ERR: 
			printf("Error leyendo paquetes\n");
			break;
		case BREAKLOOP: 
			printf("pcap_breakloop llamado\n");
			break;
	}
	printf("Se procesaron %"PRIu64" paquetes.\n\n", contador);
	pcap_close(descr);
	return OK;
}



void analizar_paquete(u_char *user,const struct pcap_pkthdr *hdr, const uint8_t *pack)
{
	(void)user;
	int flag_tamanio = 0;
	int flag_primero = 0;
	int flag_comparacion = 0;
	uint8_t aux8;
	uint16_t aux16;
	uint16_t desplazamiento;
	uint8_t protocolo;

	printf("***************************************************\n");
	printf("Nuevo paquete capturado el %s\n", ctime((const time_t *) & (hdr->ts.tv_sec)));
	contador++;
	int i = 0;
	printf("-->Cabecera Nivel 2\n\n");
	printf("Direccion ETH destino= ");
	printf("%02X", pack[0]);

	for (i = 1; i < ETH_ALEN; i++) {
		printf("-%02X", pack[i]);
	}

	printf("\n");
	pack += ETH_ALEN;

	printf("Direccion ETH origen= ");
	printf("%02X", pack[0]);

	for (i = 1; i < ETH_ALEN; i++) {
		printf("-%02X", pack[i]);
	}

	printf("\n");

	pack+=ETH_ALEN;

	/*IMPRIMIMOS EL TIPO ETHERNET Y COMPROBAMOS SI EL TIPO ETHERNET ES IP*/
	printf("Tipo Ethernet= ");
	printf("%04x\n", ntohs(pack[0]));
	if(ntohs(pack[0]) != 0x0800){
		printf("El tipo ethernet de este paquete no es de tipo ip, por lo que no se mostrara mas informacion sobre el resto de cabeceras del paquete\n");
		return;
	}

	pack+=ETH_TLEN;

	/*PASAMOS A COMPROBAR LA CABECERAS DEL NIVEL DE RED(IP)*/
	
	printf("\n-->Cabecera Nivel 3\n");
	printf("\nVersion: ");
	memcpy(&aux8, &pack[0], sizeof(uint8_t));
	/*DESPLAZAMOS 4 BITS HACIA LA DERECHA PARA PODER OBTENER EL VALOR DE LA VERSION EN DECIMAL*/
	printf("%d", aux8 >> 4);
	printf("\nLongitud de la cabecera: ");
	/*HACEMOS UNA AND CON EL 00001111 PARA OBTENER EL VALOR DE LA LONGITUD DE LA CABECERA*/
	/*MULTIPLICAMOS POR 4 YA QUE EL TAMANIO DE LA CABECERA QUE OBTENEMOS ES EL NUMERO DE PALABRAS, QUE HAY QUE MULTIPLICAR
	  POR 32 PARA OBTENER EL NUMERO DE BITS Y ACONTINUACION DIVIDIR ENTRE 8 PARA OBTENER LOS BYTES*/
	printf("%d bytes", (aux8 & 0x0F) * 4);
	if(((aux8 & 0x0F) * 4) != 20){
		/*FLAG CON LA QUE COMPROBAREMOS SI LA CABECERA IP TIENE EL CAMPO OPCIONES*/
		flag_tamanio++;
	}

	printf("\nLongitud total del paquete: ");
	/*COPIAMOS A PARTIR DEL 16 BIT DE LA CABECERA PARA OBTENER LA LONGITUD*/
	memcpy(&aux16, &pack[2], sizeof(uint16_t));
	printf("%d bytes", ntohs(aux16));

	/*NOS DESPLAZAMOS UNA LINEA EN LA CABECERA*/
	pack+=ROW_HEADER_IP;

	memcpy(&aux16, &pack[2], sizeof(uint16_t));
	/*NOS GUARDAMOS EL VALOR DEL DESPLAZAMIENTO Y DE LAS FLAGAS PARA FUTURAS ITERACIONES*/
	memcpy(&desplazamiento, &aux16, sizeof(uint16_t));
	/*COMPROBAMOS SI EL PAQUETE ESTA FRAGMENTADO*/
	if(ntohs(aux16) >> 13 & 0x0001){
		printf("\nHay mas fragmentos del paquete");
	}
	else{
		printf("\nNo quedan mas fragmentos del paquete");
	}
	printf("\nPosicion/desplazamiento: ");
	/*IMPRIMIMOS EL VALOR QUE QUEREMOS DESHACIENDONOS DE LOS 3 PRIMEROS BITS DE LAS FLAGS*/ 
	printf("%d", ntohs(aux16) & 0x1FFF);
	if((ntohs(aux16) & 0x1FFF) == 0){
		flag_primero++;
	}

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_HEADER_IP;

	printf("\nTiempo de vida: ");
	memcpy(&aux8, &pack[0], sizeof(uint8_t));

	printf("%d", aux8);
	printf("\nProtocolo: ");
	/*GUARDAMOS EL PROTOCOLO*/
	memcpy(&aux8, &pack[1], sizeof(uint8_t));
	switch(aux8){
		case(6):
			printf("TCP");
			break;
		case(17):
			printf("UDP");
			break;
		default:
			printf("No se trata de protocolo UDP ni TCP, por lo que no se mostrara mas informacion acerca de las siguientes cabeceras");
			return;
	}
	/*GUARDAMOS EL TIPO DE PROTOCOLO PARA FUTURAS ITERACIONES*/
	memcpy(&protocolo, &aux8, sizeof(uint8_t));

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_HEADER_IP;

	printf("\nDireccion ip origen: ");
	/*COMPROBAMOS SI LA DIRECCION ORIGEN SE CORRESPONDE A LA DIRECCION DEL FILTRO*/
	printf("%d", pack[0]);
	if((ipsrc_filter[0] != NO_FILTER) && (ipsrc_filter[0] != pack[0])){
		flag_comparacion++;
	}

	for (i = 1; i < IP_ALEN; i++) {
		printf(".%d", pack[i]);
		/*COMPROBAMOS EL RESTO DE VALORES DE LA DIRECCION ORIGEN*/
		if((ipsrc_filter[i] != NO_FILTER) && (ipsrc_filter[i] != pack[i]))
			flag_comparacion++;
	}
	
	/*SI LA DIRECCION ORIGEN ES DISTINTA A LA DE FILTRO DEJAMOS DE IMPRIMIR INFORMACION Y PASAMOS AL SIGUIENTE PAQUETE*/
	if(flag_comparacion != 0){
		printf("\nEl correspondiente paquete no tiene la direccion ip origen deseada\n\n");
		return;
	}

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_HEADER_IP;

	printf("\nDireccion ip destino: ");
	printf("%d", pack[0]);
	/*COMPROBAMOS SI LA DIRECCION DESTINO SE CORRESPONDE A LA DIRECCION DEL FILTRO*/
	if((ipdst_filter[0] != NO_FILTER) && (ipdst_filter[0] != pack[0]))
		flag_comparacion++;

	for (i = 1; i < IP_ALEN; i++) {
		printf(".%d", pack[i]);
		/*COMPROBAMOS EL RESTO DE VALORES DE LA DIRECCION ORIGEN*/
		if((ipdst_filter[i] != NO_FILTER) && (ipdst_filter[i] != pack[i]))
			flag_comparacion++;
	}

	/*SI LA DIRECCION ORIGEN ES DISTINTA A LA DE FILTRO DEJAMOS DE IMPRIMIR INFORMACION Y PASAMOS AL SIGUIENTE PAQUETE*/
	if(flag_comparacion != 0){
		printf("\nEl correspondiente paquete no tiene la direccion ip destino deseada\n\n");
		return;
	}

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_HEADER_IP;

	/*SI LA FLAG DE TAMANIO ES DISTINTA DE 0 SIGNIFICA QUE LA CABECERA DE NUESTRO PAQUETE TIENE CAMPO OPCIONES POR 
	  LO QUE HABRA QUE DESPLAZARSE UNA FILA MAS*/
	if(flag_tamanio != 0)
		pack+=ROW_HEADER_IP;

	/*PASAMOS A ANALIZAR LA CABECERA DE NIVEL 4 SI LA HAY*/
	if(flag_primero == 0){
		printf("\nSe trata de un fragmento de un paquete que no tiene informacion sobre la cabecera de nivel 4, por lo que no se mostrara mas informacion\n\n");
		return;
	}

	switch(protocolo){
		case(6):
			printf("\nPasamos a analizar la cabecera TCP");
			analizarCabeceraTcp(pack);
			break;
		case(17):
			printf("\nPasamos a anlizar la cabecera UDP");
			analizarCabeceraUdp(pack);
			break;
		default:
			printf("\nNo se trata de protocolo UDP ni TCP, por lo que no se mostrara informacion acerca de la cabecera de nivel 4");
			return;
	}

	printf("\n\n");
	
}

void handleSignal(int nsignal)
{
	(void) nsignal; // indicamos al compilador que no nos importa que nsignal no se utilice

	printf("Control C pulsado\n");
	pcap_breakloop(descr);
}

void analizarCabeceraUdp(const uint8_t *pack){
	uint16_t aux;

	printf("\n\n-->Cabecera Nivel 4(UDP)\n");
	printf("\nPuerto de origen: ");
	/*GUARDAMOS EN AUX EL VALOR DEL PUERTO DE ORIGEN*/
	memcpy(&aux, &pack[0], sizeof(uint16_t));
	printf("%d", ntohs(aux));

	/*COMPROBAMOS SI EL PUERTO DE ORIGEN SE CORRESPONDE AL DEL FILTRO*/
	if(sport_filter != NO_FILTER && sport_filter != ntohs(aux)){
		printf("\nEl puerto de origen no se corresponde al deseado\n\n");
		return;
	}

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_LEVEL_4_UDP;
	
	printf("\nPuerto de destino: ");
	/*GUARDAMOS EN AUX EL VALOR DEL PUERTO DE DESTINO*/
	memcpy(&aux, &pack[0], sizeof(uint16_t));
	printf("%d", ntohs(aux));
	
	/*COMPROBAMOS SI EL PUERTO DE DESTINO SE CORRESPONDE AL DEL FILTRO*/
	if(dport_filter != NO_FILTER && dport_filter != ntohs(aux)){
		printf("\nEl puerto de destino no se corresponde al deseado\n\n");
		return;
	}	

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_LEVEL_4_UDP;

	printf("\nLongitud: ");
	/*GUARDAMOS EN AUX LA LONGITUD*/
	memcpy(&aux, &pack[0], sizeof(uint16_t));
	printf("%d", ntohs(aux));
}

void analizarCabeceraTcp(const uint8_t *pack){
	uint16_t aux;
	uint16_t flags;

	printf("\n\n-->Cabecera Nivel 4(TCP)\n");
	printf("\nPuerto de origen: ");
	/*GUARDAMOS EN AUX EL VALOR DEL PUERTO DE ORIGEN*/
	memcpy(&aux, &pack[0], sizeof(uint16_t));
	printf("%d", ntohs(aux));

	/*COMPROBAMOS SI EL PUERTO DE ORIGEN SE CORRESPONDE AL DEL FILTRO*/
	if(sport_filter != NO_FILTER && sport_filter != ntohs(aux)){
		printf("\nEl puerto de origen no se corresponde al deseado\n\n");
		return;
	}

	/*NOS DESPLAZAMOS AL PUERTO DESTINO*/
	pack+=ROW_LEVEL_4_UDP;
	
	printf("\nPuerto de destino: ");
	/*GUARDAMOS EN AUX EL VALOR DEL PUERTO DE DESTINO*/
	memcpy(&aux, &pack[0], sizeof(uint16_t));
	printf("%d", ntohs(aux));

	/*COMPROBAMOS SI EL PUERTO DE DESTINO SE CORRESPONDE AL DEL FILTRO*/
	if(dport_filter != NO_FILTER && dport_filter != ntohs(aux)){
		printf("\nEl puerto de destino no se corresponde al deseado\n\n");
		return;
	}	

	/*NOS DESPLAZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_LEVEL_4_UDP;

	/*NOS DESPLZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_LEVEL_4_TCP;

	/*NOS DESPLZAMOS A LA SIGUIENTE FILA DE LA CABECERA*/
	pack+=ROW_LEVEL_4_TCP;

	/*COGEMOS LOS 16 BITS EN LOS QUE SE ENCUENTRAN LA PARTE DE LAS FLAGS DE LA CABECERA*/
	memcpy(&aux, &pack[0], sizeof(uint16_t));
	/*DE LOS 16 BIT OBTENIDOS LOS REDUCIMOS A LOS BITS DEDICADOS A LAS FLAGS SYN Y FIN*/
	flags = ntohs(aux) & 0x0003;
	printf("\nValor de las flags: ");
	printf("\n\tFlag SYN: ");
	/*IMPRIMIMOS SOLO EL BIT DE INFORMACION ACERCA DE LA FLAG SYN, REALIZANDO UN DESPLAZAMIENTO A LA DERECHA DE UN BIT*/
	printf("%d", flags >> 1);

	printf("\n\tFLAG FIN: ");
	/*IMPRIMIMOS SOLO EL BIT DE INFORMACION ACERCA DE LA FLAG FIN, UTILIZANDO UNA PUERTA AND*/
	printf("%d", flags & 0x0001);
}
