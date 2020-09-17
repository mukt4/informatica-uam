/******************************************************************************************************
 practica3.c                                                                                          *
 Inicio, funciones auxiliares y modulos de transmision implmentados y a implementar de la practica 4. *
 Compila con warning pues falta usar variables y modificar funciones                                  *
                                                                                                      *
 Compila: make                                                                                        *
 Autor: Jose Luis Garcia Dorado, Jorge E. Lopez de Vergara Mendez                                     *
 2018 EPS-UAM v1                                                                                      *
******************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include "interface.h"
#include "practica3.h"

/***************************Variables globales utiles****************************************************************/
pcap_t* descr, *descr2; 	/* Descriptores de la interface de red                                    			    */
pcap_dumper_t * pdumper;	/* y salida a pcap                                                        			    */
uint64_t cont=0;			/* Contador numero de mensajes enviados                                   			    */
char interface[10];			/* Interface donde transmitir por ejemplo "eth0"                           			    */
uint16_t ID=0;				/* Identificador IP                                                                     */
int flag_dontfrag = 0;		/* Flag que especifica si los paquetes se pueden fragmentar								*/       
							/* Variables globales para controlar el offset del paquete asi como tres flags          */
							/* para saber si necesitamos fragmentar el paquete y si este es el ultimo fragmento o   */
							/* el primero                                               			                */
int flag_frag = 0;			/* Flag para saber si el paquete esta fragmentado                         			    */
int flag_ultPaquete = 0;	/* Flag para saber si es el ultimo fragmento del paquete  							    */ 
uint16_t offset_actual = 0; 		/* Variable que contiene el offset del paquete que se va a enviar       		*/
int flag_mostrar = 0;		/* Variable que controla si el usuario quiere que se le muestren las trazas por pantalla*/
/********************************************************************************************************************/


void handleSignal(int nsignal){
	printf("Control C pulsado (%"PRIu64")\n", cont);
	pcap_close(descr);
	exit(OK);
}

int main(int argc, char **argv){	

	char errbuf[PCAP_ERRBUF_SIZE];
	char fichero_pcap_destino[CADENAS];
	uint8_t IP_destino_red[IP_ALEN];
	uint16_t MTU;
	uint16_t datalink;
	uint16_t puerto_destino;
	char data[IP_DATAGRAM_MAX];
	uint16_t pila_protocolos[CADENAS];
	FILE* pf;


	int long_index=0;
	char opt;
	char flag_iface = 0, flag_ip = 0, flag_port = 0, flag_file = 0, flag_dontfrag = 0;

	static struct option options[] = {
		{"if",required_argument,0,'1'},
		{"ip",required_argument,0,'2'},
		{"pd",required_argument,0,'3'},
		{"f",required_argument,0,'4'},
		{"d",no_argument,0,'5'},
		{"m",no_argument,0,'6'},
		{"h",no_argument,0,'7'},
		{0,0,0,0}
	};

	//Dos opciones: leer de stdin o de fichero, adicionalmente para pruebas si no se introduce argumento se considera que el mensaje es "Payload "
	while ((opt = getopt_long_only(argc, argv,"1:2:3:4:5:6:7", options, &long_index )) != -1) {
		switch (opt) {

			case '1' :

				flag_iface = 1;
				//Por comodidad definimos interface como una variable global
				sprintf(interface,"%s",optarg);
				break;

			case '2' : 

				flag_ip = 1;
				//Leemos la IP a donde transmitir y la almacenamos en orden de red
				if (sscanf(optarg,"%"SCNu8".%"SCNu8".%"SCNu8".%"SCNu8"",
				                   &(IP_destino_red[0]),&(IP_destino_red[1]),&(IP_destino_red[2]),&(IP_destino_red[3])) != IP_ALEN){
					printf("Error: Fallo en la lectura IP destino %s\n", optarg);
					exit(ERROR);
				}

				break;

			case '3' :

				flag_port = 1;
				//Leemos el puerto a donde transmitir y la almacenamos en orden de hardware
				puerto_destino=atoi(optarg);
				break;

			case '4' :

				if(strcmp(optarg,"stdin")==0) {
					if (fgets(data, sizeof data, stdin)==NULL) {
						  	printf("Error leyendo desde stdin: %s %s %d.\n",errbuf,__FILE__,__LINE__);
						return ERROR;
					}
					sprintf(fichero_pcap_destino,"%s%s","stdin",".pcap");
				} else {
					sprintf(fichero_pcap_destino,"%s%s",optarg,".pcap");
					//TODO Leer fichero en data [...]
					/*Abrimos el fichero en modo lectura*/
					pf = fopen(optarg, "r");
					/*Comprobamos si existe el fichero*/
					if(!pf){
						printf("No existe ningun fichero con ese nombre\n");
						return ERROR;
					}

					/*Guardamos los datos leidos en data*/
					if (fgets(data, sizeof data, pf) == NULL){
						printf("Error al abrir el fichero %s: %s %s %d", optarg, errbuf, __FILE__, __LINE__);
						fclose(pf);
						return ERROR;
					}
					fclose(pf);
				}
				flag_file = 1;
				break;
				
			case '5' :
				flag_dontfrag =1; // El usuario solicita que los paquetes se envien con el bit DF=1.
				break;

			case '6' :
				flag_mostrar =1; // El usuario solicita que se muestren en hexadecimal las tramas enviadas.
				break;

			case '7' : printf("Ayuda. Ejecucion: %s -if interface -ip direccion_IP -pd puerto [-f /ruta/fichero_a_transmitir o stdin] [-d] [-m]: %d\n",argv[0],argc); exit(ERROR);
				break;

			case '?' : 
			default: printf("Error. Ejecucion: %s -if interface -ip direccion_IP -pd puerto [-f /ruta/fichero_a_transmitir o stdin] [-d] [-m]: %d\n",argv[0],argc); exit(ERROR);
				break;
        }
    }

	if ((flag_iface == 0) || (flag_ip == 0) || (flag_port == 0)){
		printf("Error. Ejecucion: %s -if interface -ip direccion_IP -pd puerto [-f /ruta/fichero_a_transmitir o stdin] [-d] [-m]: %d\n",argv[0],argc);
		exit(ERROR);
	} else {
		printf("Interface:\n\t%s\n",interface);
		printf("IP:\n\t%"PRIu8".%"PRIu8".%"PRIu8".%"PRIu8"\n",IP_destino_red[0],IP_destino_red[1],IP_destino_red[2],IP_destino_red[3]);
		printf("Puerto destino:\n\t%"PRIu16"\n",puerto_destino);
		if (flag_dontfrag) printf("Se solicita enviar paquete con bit DF=1\n");
		if (flag_mostrar) printf("Se solicita mostrar las tramas enviadas en hexadecimal\n");
	}

	if (flag_file == 0) {
		sprintf(data,"%s","Payload "); //Deben ser pares!
		sprintf(fichero_pcap_destino,"%s%s","debugging",".pcap");
	}

	if(signal(SIGINT,handleSignal)==SIG_ERR){
		printf("Error: Fallo al capturar la senal SIGINT.\n");
		return ERROR;
	}
	//Inicializamos las tablas de protocolos
	if(inicializarPilaEnviar()==ERROR){
      	printf("Error leyendo desde stdin: %s %s %d.\n",errbuf,__FILE__,__LINE__);
		return ERROR;
	}
	//Leemos el tamano maximo de transmision del nivel de enlace
	if(obtenerMTUInterface(interface, &MTU)==ERROR)
		return ERROR;
	//Descriptor de la interface de red donde inyectar trafico
	if ((descr = pcap_open_live(interface,MTU+ETH_HLEN,0, 0, errbuf)) == NULL){
		printf("Error: pcap_open_live(): %s %s %d.\n",errbuf,__FILE__,__LINE__);
		return ERROR;
	}

	datalink=(uint16_t)pcap_datalink(descr); //DLT_EN10MB==Ethernet

	//Descriptor del fichero de salida pcap para debugging
	descr2=pcap_open_dead(datalink,MTU+ETH_HLEN);
	pdumper=pcap_dump_open(descr2,fichero_pcap_destino);

	//Formamos y enviamos el trafico, debe enviarse un unico segmento por llamada a enviar() aunque luego se traduzca en mas de un datagrama
	//Primero, un paquete ICMP; en concreto, un ping
	pila_protocolos[0]=ICMP_PROTO; pila_protocolos[1]=IP_PROTO; pila_protocolos[2]=ETH_PROTO;
	Parametros parametros_icmp; parametros_icmp.tipo=PING_TIPO; parametros_icmp.codigo=PING_CODE; parametros_icmp.bit_DF=flag_dontfrag; memcpy(parametros_icmp.IP_destino,IP_destino_red,IP_ALEN);
	if(enviar((uint8_t*)ICMP_DATA,strlen(ICMP_DATA),pila_protocolos,&parametros_icmp)==ERROR ){
		printf("Error: enviar(): %s %s %d.\n",errbuf,__FILE__,__LINE__);
		return ERROR;
	}
	else	cont++;
	printf("Enviado mensaje %"PRIu64", ICMP almacenado en %s\n\n", cont,fichero_pcap_destino);

	//Luego, un paquete UDP
	//Definimos la pila de protocolos que queremos seguir
	pila_protocolos[0]=UDP_PROTO; pila_protocolos[1]=IP_PROTO; pila_protocolos[2]=ETH_PROTO;
	//Rellenamos los parametros necesario para enviar el paquete a su destinatario y proceso
	Parametros parametros_udp; memcpy(parametros_udp.IP_destino,IP_destino_red,IP_ALEN); parametros_udp.bit_DF=flag_dontfrag; parametros_udp.puerto_destino=puerto_destino;
	//Enviamos
	if(enviar((uint8_t*)data,strlen(data),pila_protocolos,&parametros_udp)==ERROR ){
		printf("Error: enviar(): %s %s %d.\n",errbuf,__FILE__,__LINE__);
		return ERROR;
	}
	else	cont++;

	printf("Enviado mensaje %"PRIu64", almacenado en %s\n\n\n", cont,fichero_pcap_destino);

	//Cerramos descriptores
	pcap_close(descr);
	pcap_dump_close(pdumper);
	pcap_close(descr2);
	return OK;
}


/****************************************************************************************
 * Nombre: enviar                                                                       *
 * Descripcion: Esta funcion envia un mensaje                                           *
 * Argumentos:                                                                          *
 *  -mensaje: mensaje a enviar                                                          *
 *  -pila_protocolos: conjunto de protocolos a seguir                                   *
 *  -longitud: bytes que componen mensaje                                               *
 *  -parametros: parametros necesario para el envio (struct parametros)                 *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t enviar(uint8_t* mensaje, uint32_t longitud,uint16_t* pila_protocolos,void *parametros){
	uint16_t protocolo=pila_protocolos[0];
printf("Enviar(%"PRIu16") %s %d.\n",protocolo,__FILE__,__LINE__);
	if(protocolos_registrados[protocolo]==NULL){
		printf("Protocolo %"PRIu16" desconocido\n",protocolo);
		return ERROR;
	}
	else {
		return protocolos_registrados[protocolo](mensaje,longitud,pila_protocolos,parametros);
	}
	return ERROR;
}


/***************************TODO Pila de protocolos a implementar************************************/


/****************************************************************************************
 * Nombre: moduloICMP                                                                   *
 * Descripcion: Esta funcion implementa el modulo de envio ICMP                         *
 * Argumentos:                                                                          *
 *  -mensaje: mensaje a anadir a la cabecera ICMP                                       *
 *  -longitud: bytes que componen el mensaje                                            *
 *  -pila_protocolos: conjunto de protocolos a seguir                                   *
 *  -parametros: parametros necesario para el envio este protocolo                      *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/
uint8_t moduloICMP(uint8_t* mensaje, uint32_t longitud, uint16_t* pila_protocolos, void *parametros){
	uint8_t segmento[ICMP_DATAGRAM_MAX]={0};
	uint8_t aux8;
	uint16_t aux16;
	uint32_t pos=0;
	uint32_t pos_checkSum = 0;
	uint8_t protocolo_inferior = pila_protocolos[1];
	uint8_t* checksum;
	Parametros* parametros_icmp;

	if(!mensaje || !pila_protocolos || !parametros){
		printf("Error al crear la cabecera ICMP\n");
		return ERROR;
	}

	/*Comprobamos la interface MTU*/
	aux8 = obtenerMTUInterface(interface, &aux16);
	if(aux8 == ERROR || longitud + 20 > aux16){
		printf("Mensaje demsaiado grande para el MTU\n");
		return ERROR;
	}

	parametros_icmp = (Parametros*)parametros;

	printf("modulo ICMP(%"PRIu16") %s %d.\n",protocolo_inferior,__FILE__,__LINE__);
	
	aux8 = parametros_icmp->tipo;
	memcpy(segmento+pos,&aux8,sizeof(uint8_t));
	pos+=sizeof(uint8_t);
	//TODO rellenar el resto de campos de ICMP, incluyendo el checksum tras haber rellenado todo el segmento, incluyendo el mensaje
	// El campo de identificador se puede asociar al pid, y el de secuencia puede ponerse a 1.
	//[....]
	/*Seteamos el codigo*/
	aux8 = parametros_icmp->codigo;
	memcpy(segmento + pos, &aux8, sizeof(uint8_t));
	pos += sizeof(uint8_t);

	/*Guardamos la posicion del campo checksum y lo seteamos a 0 para establecerle un valor por defecto antes de calcular su valor*/
	pos_checkSum = pos;
	aux16 = 0;
	memcpy(segmento + pos, &aux16, sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Seteamos el identificador*/
	aux16 = htons(ID);
	memcpy(segmento + pos, &aux16, sizeof(uint16_t));
	pos+=sizeof(uint16_t);

	/*Seteamos el numero de secuencia con el mismo valor que el del identificador*/
	aux16 = htons(ID);
	memcpy(segmento + pos, &aux16, sizeof(uint16_t));
	pos+=sizeof(uint16_t);

	/*Rellenamos el campo de datos extra con el mensaje especificado*/
	/*No es necesario hacer ntohs ya que escribimos byte a byte*/
	memcpy(segmento + pos, mensaje, sizeof(uint8_t) * longitud);

	/*Una vez escritos todos los datos pasamos a calcular el campo checksum*/
	/*Reservamos memoria para dos bytes con un tipo uint8_t que se equivale a el tipo de valor de entrada de la funcion que calcula el checksum*/
	checksum = (uint8_t*)malloc(2 * sizeof(uint8_t));
	calcularChecksum(segmento, 10 + longitud, checksum);

	/*Una vez calculado el checksum lo guardamos en nuestro paquete*/
	/*Mejor copiarlo byte a byte para no tener que hacer transformaciones de formato*/
	memcpy(segmento + pos_checkSum, &checksum[0], sizeof(uint8_t));
	pos_checkSum += sizeof(uint8_t);
	memcpy(segmento + pos_checkSum, &checksum[1], sizeof(uint8_t));

	/*Una vez rellenomostrarHex(segmento, longitud + ICMP_HLEN); el campo checksum liberamos memoria*/
	free(checksum);

	/*Imprimimos por pantalla la cabecera que acabamos de crear*/
	if(flag_mostrar == 1){
		printf("\nModulo ICMP\n");
		/*Sumamos dos para mostrar tambien los datos extra de la cabecera ICMP*/
		mostrarHex(segmento, longitud + ICMP_HLEN);
	}

	//Se llama al protocolo definido de nivel inferior a traves de los punteros registrados en la tabla de protocolos registrados
	return protocolos_registrados[protocolo_inferior](segmento,longitud + pos,pila_protocolos,parametros);
}


/****************************************************************************************
 * Nombre: moduloUDP                                                                    *
 * Descripcion: Esta funcion implementa el modulo de envio UDP                          *
 * Argumentos:                                                                          *
 *  -mensaje: mensaje a enviar                                                          *
 *  -longitud: bytes que componen mensaje                                               *
 *  -pila_protocolos: conjunto de protocolos a seguir                                   *
 *  -parametros: parametros necesario para el envio este protocolo                      *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/
uint8_t moduloUDP(uint8_t* mensaje, uint32_t longitud, uint16_t* pila_protocolos, void *parametros){
	uint8_t segmento[UDP_SEG_MAX]={0};
	uint16_t puerto_origen = 0, suma_control=0, puerto_destino = 0;
	uint16_t aux16;
	uint32_t pos=0;
	uint8_t protocolo_inferior=pila_protocolos[1];
	uint8_t aux8;
	Parametros udpdatos;

	if(!mensaje  || !pila_protocolos || !parametros){
		printf("Error al crear la cabecera UDP\n");
		return ERROR;
	}

	printf("modulo UDP(%"PRIu16") %s %d.\n",protocolo_inferior,__FILE__,__LINE__);

	if (longitud > UDP_SEG_MAX){
		printf("Error: mensaje demasiado grande para UDP (%d).\n",UDP_SEG_MAX);
		return ERROR;
	}

	udpdatos=*((Parametros*)parametros);
	puerto_destino=udpdatos.puerto_destino;

	//TODO
	//[...] 
	//obtenerPuertoOrigen(...)
	/*Obtenemos el puerto origen*/
	aux8 = obtenerPuertoOrigen(&puerto_origen);
	if(aux8 == ERROR){
		printf("No se ha podido encontrar un puerto origen\n");
		return ERROR;
	}

	aux16=htons(puerto_origen);
	memcpy(segmento+pos,&aux16,sizeof(uint16_t));
	pos+=sizeof(uint16_t);
	//TODO Completar el segmento [...]
	//[...] 

	/*Escribimos en la cabecera UDP el puerto destino*/
	aux16 = htons(puerto_destino);
	memcpy(segmento + pos, &aux16, sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Escribimos la longitud del paquete*/
	/*Guardamos primero la longitud del paquete, que es la longitud + el tamanio de la cabecera UDP*/
	aux16 = htons(longitud + UDP_HLEN);
	memcpy(segmento + pos, &aux16, sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Rellenamos el campo checksum dejandolo a 0*/
	memcpy(segmento + pos, &suma_control, sizeof(uint16_t));
	pos+= sizeof(uint16_t);

	/*Una vez rellenada la cabecera guardamos los datos*/
	/*No nos importa el tamanio de los datos, ya que gestionaremos la fragmentacion en IP*/
	memcpy(segmento + pos, mensaje, longitud * sizeof(uint8_t));

	/*Una vez acabada la cabecera mostramos el paquete por pantalla*/
	if(flag_mostrar == 1){
		printf("\nModulo UDP\n");
		mostrarHex(segmento, longitud + UDP_HLEN);
	}

	//Se llama al protocolo definido de nivel inferior a traves de los punteros registrados en la tabla de protocolos registrados
	return protocolos_registrados[protocolo_inferior](segmento,longitud+UDP_HLEN,pila_protocolos,parametros);
}


/****************************************************************************************
 * Nombre: moduloIP                                                                     *
 * Descripcion: Esta funcion implementa el modulo de envio IP                           *
 * Argumentos:                                                                          *
 *  -segmento: segmento a enviar                                                        *
 *  -longitud: bytes que componen el segmento                                           *
 *  -pila_protocolos: conjunto de protocolos a seguir                                   *
 *  -parametros: parametros necesario para el envio este protocolo                      *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t moduloIP(uint8_t* segmento, uint32_t longitud, uint16_t* pila_protocolos, void *parametros){
	uint8_t datagrama[IP_DATAGRAM_MAX] = {0};
	uint16_t aux16, protocolo_actual = pila_protocolos[1], MTU, FlagsyPosicion = 0;
	uint8_t aux8;
	uint32_t pos=0, pos_checkSum, bytes_transmitidos, bytes_mensaje, bytes_ip;
	uint8_t IP_origen[IP_ALEN];
	uint8_t protocolo_superior=pila_protocolos[0];
	uint8_t protocolo_inferior=pila_protocolos[2];
	uint8_t mascara[IP_ALEN],IP_rango_origen[IP_ALEN],IP_rango_destino[IP_ALEN];
	uint8_t* IP_destino;
	uint8_t* checksum;
	Parametros ipdatos;
	int n_veces, i, flag_subred = 0;

	if(!segmento || !pila_protocolos || !parametros){
		printf("Error al crear la cabecera UDP\n");
		return ERROR;
	}

	printf("modulo IP(%"PRIu16") %s %d.\n",protocolo_inferior,__FILE__,__LINE__);

	ipdatos = *((Parametros*)parametros);
	IP_destino = ipdatos.IP_destino;

	/*Primero comprobamos la interface MTU, asi como la fragmentacion del paquete*/
	aux8 = obtenerMTUInterface(interface, &MTU);


	/*Si es necesario fragmentar el paquete entramos en esta sentencia*/
	if(aux8 == ERROR || longitud + IP_HLEN > MTU){
		/*Comprobamos si el usuario quiere que se fragmente el paquete*/
		if(ipdatos.bit_DF == 1){
			printf("No se puede enviar paquete porque el usuario ha especificado que no se puede fragmentar\n");
			return ERROR;
		}

		if(flag_frag == 1){
			printf("Se ha producido un error al especificar las longitudes de los paquetes\n");
			return ERROR;
		}

		/*Especificamos que se esta fragmentando un paquete poniendo la flag de fragmentacion a 1*/
		flag_frag = 1;

		/*Ponemos el offset del mensaje que vamos a enviar a 0, ya que es el primer paquete que enviamos*/
		offset_actual = 0;

		/*Calculamos el numero de bytes que podremos enviar en los paquetes intermedios*/
		bytes_ip = MTU - IP_HLEN;

		/*Mandamos el primer trozo de paquete que contiene la cabecera UDP, dejando hueco para la cabecera IP*/
		protocolos_registrados[protocolo_actual](segmento, bytes_ip, pila_protocolos, &ipdatos);

		/*Una vez enviado el primer paquete calculamos el numero de fragmentos que sera necesario enviar despues de haber enviado el primero*/
		/*Primero calculamos el numero de bytes que ya hemos transmitido*/
		bytes_transmitidos = MTU - UDP_HLEN - IP_HLEN;

		/*Calculamos los bytes de mensaje que quedan sin enviar*/
		bytes_mensaje = longitud - bytes_transmitidos - UDP_HLEN;

		/*Finalmente calculamos el numero de fragmentos que tenemos que enviar*/
		n_veces = (int)(bytes_mensaje/bytes_ip);

		/*Como es una division entera es necesario obtener el techo de la division para el correcto funcionamiento de nuestro programa*/
		if(n_veces * bytes_ip < bytes_mensaje)
			n_veces++;

		/*Seteamos el offset del segundo fragmento de paquete que se enviara*/
		/*Lo convertimos con htons para no tener problemas con el formato, ya que offset_actual tiene 16 bits*/
		offset_actual = bytes_transmitidos;

		/*Comenzamos el bucle de envio de paquetes, lo hacemos desde 1 para gestionar el ultimo paquete fuera del bucle*/
		for(i = 1; i < n_veces; i++){
			/*Enviamos el resto de paquetes con el tamanio de fragmento especifico*/
			protocolos_registrados[protocolo_actual](segmento + (bytes_ip * i) , bytes_ip, pila_protocolos, &ipdatos);

			/*Seteamos el offset actual del fragmento que se va a enviar*/
			offset_actual += bytes_ip;
		}

		/*Una vez enviados los fragmentos intermedios ponemos la flag de ultimo paquete a 1*/
		flag_ultPaquete = 1;

		/*Mandamos el ultimo paquete*/
		return protocolos_registrados[protocolo_actual](segmento + (bytes_ip * n_veces), longitud - (bytes_ip * n_veces), pila_protocolos, &ipdatos);
	}

	//TODO A implementar el datagrama y fragmentación, asi como control de tamano segun bit DF
	//[...] 

	/*Pasamos a rellenar los campos de la cabecera*/
	/*Guardamos en aux 8 el valor de la version(4) y de IHL(5) en hexadecimal*/
	aux8 = 0x45;
	memcpy(datagrama + pos, &aux8, sizeof(uint8_t));
	pos += sizeof(uint8_t);

	/*Pasamos a rellenar el campo de tipo de servicio*/
	/*Establecemos un tipo de servicio como un servicio de control de red prioritario(podriamos setearlo con otros valores)*/
	aux8 = 0x3F;
	memcpy(datagrama + pos, &aux8, sizeof(uint8_t));
	pos += sizeof(uint8_t);

	/*Pasamos a rellenar el campo de longitud*/
	/*Para rellenar este campo correctamente es necesario tener en cuenta la fragmentacion del paquete*/
	if (flag_frag == 1 && flag_ultPaquete == 0)
		aux16 = MTU;
	else
		aux16 = longitud + 20;

	/*Una vez obtenido el campo de la longitud pasamos a escribirlo en la cabecera*/
	aux16=htons(aux16);
	memcpy(datagrama+pos, &aux16,sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Pasamos a rellenar el campo de identificacion con un id valido*/
	/*Al ser 2 bytes es necesario hacer una transformacion htons*/
	aux16 = htons(ID);
	memcpy(datagrama + pos, &aux16, sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Comprobamos la flag de fragmentacion para saber si es necesario cambiar aumentar el valor de id*/
	if(flag_frag == 0)
		ID++;

	/*Pasamos a rellenar los campos de flag y de posicion*/
	/*Comprobamos se la flag de dont frag se encuentra a 1*/
	if(ipdatos.bit_DF == 1){
		FlagsyPosicion = 0x4000;
	}
	/*Si la flag de fragmentacion esta a 1 pasamos a comprobar si el paquete que se esta enviando es el ultimo*/
	else if(flag_frag == 1 && flag_ultPaquete == 1){
		FlagsyPosicion = FlagsyPosicion | offset_actual;
	}	
	/*Si no es el ultimo comprobamos si al menos es un paquete intermedio del fragmento*/
	else if(flag_frag == 1){
		FlagsyPosicion = 0x2000;
		FlagsyPosicion = FlagsyPosicion | offset_actual;
	}
	/*Si llegamos a esta sentencia significara que nos encontramos en un paquete normal sin fragmentar*/
	else{
		FlagsyPosicion = 0;
	}
	/*Al ser 2 bytes es necesario hacer una conversion htons*/
	aux16=htons(FlagsyPosicion);
	/*Guardamos en la cabecera la informacion*/
	memcpy(datagrama + pos, &aux16,sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Pasamos a rellenar el campo de tiempo de vida*/
	/*Rellenamos este campo con el valor 128 que es uno de los valores tipicos de este campo*/
	aux8 = 0x80;
	memcpy(datagrama + pos, &aux8, sizeof(uint8_t));
	pos += sizeof(uint8_t);

	/*Pasamos a rellenar el campo de protocolo*/
	aux8 = protocolo_superior;
	memcpy(datagrama + pos, &aux8, sizeof(uint8_t));
	pos += sizeof(uint8_t);

	/*Dejamos el campo checksum con un valor por defecto 0 para luego rellenarlo correctamente*/
	pos_checkSum = pos;
	aux16 = 0;
	memcpy(datagrama + pos, &aux16, sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Pasamos a rellenar el campo de direccion de origen*/
	/*Para rellenar este campo utilizamos la funcion que nos devuelve una ip en funcion de la interfaz en la que nos encontramos*/
	obtenerIPInterface(interface, IP_origen);
	/*Una vez obtenida la direccion ip la vamos incluyendo en el paquete byte a byte*/
	for(i = 0; i < IP_ALEN; i++){
		memcpy(datagrama + pos, &IP_origen[i], sizeof(uint8_t));
		pos += sizeof(uint8_t);
	}

	/*Pasamos a rellenar el campo de direccion destino*/
	/*Rellenamos este campo igual que lo hicimos con el campo de ip origen*/
	for(i = 0; i < IP_ALEN; i++){
		memcpy(datagrama + pos, &IP_destino[i], sizeof(uint8_t));
		pos += sizeof(uint8_t);
	}

	/*Una vez escrita la cabecera concatenamos con la cabecera superior*/
	memcpy(datagrama + pos, segmento, longitud * sizeof(uint8_t));
	
	/*Una vez rellenados todos los campos pasamos a settear el campo de checksum*/
	/*Resevervamos memoria para los dos bytes del campo checksum*/
	checksum = (uint8_t*)malloc(2 * sizeof(uint8_t));
	calcularChecksum(datagrama, 20, checksum);
	memcpy(datagrama + pos_checkSum, &checksum[0], sizeof(uint8_t));
	pos_checkSum += sizeof(uint8_t);
	memcpy(datagrama + pos_checkSum, &checksum[1], sizeof(uint8_t));
	pos_checkSum += sizeof(uint8_t);
	free(checksum);

	//TODO
	//Llamar a solicitudARP(...) adecuadamente y usar ETH_destino de la estructura parametros
	//[...] 

	/*Primero obtenemos la mascara de la interfaz*/
	obtenerMascaraInterface(interface, mascara);
	/*Aplicamos la mascara*/
	aplicarMascara(IP_origen, mascara, IP_ALEN, IP_rango_origen);
	aplicarMascara(IP_destino, mascara, IP_ALEN, IP_rango_destino);
	
	/*Pasamos a comprobar si el paquete se esta enviando a la misma subred*/
	for(i = 0; i < IP_ALEN; i++){
		if(IP_rango_origen[i] != IP_rango_destino[i])
			flag_subred++;
	}

	/*Si las dos ips se encuentran en la misma subred*/
	if(flag_subred == 0){
		/*Utilizamos MACdeInterface  para debuggear si nos mandamos paquetes al mismo equipo, mientras utilizamos el wifi eduroam*/
		//obtenerMACdeInterface(interface, ipdatos.ETH_destino);
		solicitudARP(interface, IP_destino, ipdatos.ETH_destino);
	}
	/*Si no estan en la misma subred es necesario saber la direccion por la que saldra el paquete de nuestro router*/
	else{
		obtenerGateway(interface, IP_rango_destino);
		solicitudARP(interface, IP_rango_destino, ipdatos.ETH_destino);
	}

	/*Imprimimos por pantalla si la flag de mostrar esta a 1*/
	if(flag_mostrar == 1){
		printf("\nModulo IP\n");
		mostrarHex(datagrama, longitud + pos);
	}

	/*Si el paquete ha sido fragmentado y es el utlimo ponemos todas las flags a 0 de nuevo*/
	if(flag_ultPaquete == 1){
		flag_frag = 0;
		flag_ultPaquete = 0;
	}

	//llamada/s a protocolo de nivel inferior [...]

	/*Pasamos el mensaje al protocolo inferior*/
	return protocolos_registrados[protocolo_inferior](datagrama, longitud + pos, pila_protocolos, &ipdatos);
}


/****************************************************************************************
 * Nombre: moduloETH                                                                    *
 * Descripcion: Esta funcion implementa el modulo de envio Ethernet                     *
 * Argumentos:                                                                          *
 *  -datagrama: datagrama a enviar                                                      *
 *  -longitud: bytes que componen el datagrama                                          *
 *  -pila_protocolos: conjunto de protocolos a seguir                                   *
 *  -parametros: Parametros necesario para el envio este protocolo                      *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t moduloETH(uint8_t* datagrama, uint32_t longitud, uint16_t* pila_protocolos,void *parametros){
	//TODO
	//[....]
	//[...] Variables del modulo
	uint8_t trama[ETH_FRAME_MAX]={0};
	uint8_t ETH_origen[ETH_ALEN];

	/*Deberia ser esta asignacion pero la hacemos abajo porque no crea nuestro paquete de manera correcta
	uint16_t protocolo_superior = pila_protocolos[0];
	*/

	uint16_t MTU, aux16;
	uint8_t retorno;
	uint32_t pos = 0;
	Parametros param = *(Parametros*)parametros;
	struct pcap_pkthdr cabecera;
	struct timeval time;
	int i;

	if(!datagrama || !pila_protocolos || !parametros){
		printf("No se ha podido crear la cabecera Ethernet\n");
		return ERROR;
	}

	printf("modulo ETH(fisica) %s %d.\n",__FILE__,__LINE__);

	//TODO
	//[...] Control de tamano

	retorno = obtenerMTUInterface(interface, &MTU);
	if(retorno == ERROR || longitud > MTU){
		printf("No se ha podido realizar la cabecera Ethernet\n");
		return ERROR;
	}

	//TODO
	//[...] Cabecera del modulo

	/*Rellenamos la parte de destino ethernet de la cabecera ethernet*/
	for(i = 0; i < ETH_ALEN; i++){
		memcpy(trama + pos, &(param.ETH_destino[i]), sizeof(uint8_t));
		pos += sizeof(uint8_t);
	}

	/*Obtenemos la ethernet de origen antes de escribirla en la cabecera*/
	obtenerMACdeInterface(interface, ETH_origen);
	/*Escirbimos la ethernet origen en la cabecera*/
	for(i = 0; i < ETH_ALEN; i++){
		memcpy(trama + pos, &ETH_origen[i], sizeof(uint8_t));
		pos += sizeof(uint8_t);
	}

	/*Guardamos el tipo de protocolo realizando la transformacion htons*/
	/*Como es un protocolo de tipo IP guardamos 0x0800 en nuestra variable*/
	/*El IP_PROTO es 4 y deberia ser 0x0800?*/
	aux16 = htons(0x0800);
	memcpy(trama + pos, &aux16, sizeof(uint16_t));
	pos += sizeof(uint16_t);

	/*Juntamos la trama y el datagrama*/
	memcpy(trama + pos, datagrama, longitud * sizeof(uint8_t));


	//TODO
	//Enviar a capa fisica [...]  

	/*Enviamos el paquete a la capa fisica*/
	retorno = pcap_inject(descr, trama, longitud + pos);
	if(retorno == PCAP_ERROR){
		printf("Error al enviar el paquete a la capa fisica\n");
		return ERROR;
	}

	//TODO
	//Almacenamos la salida por cuestiones de debugging [...]

	gettimeofday(&time, NULL);
	cabecera.ts.tv_sec = time.tv_sec;
	cabecera.ts.tv_usec = time.tv_usec;
	cabecera.caplen = longitud+pos;
	cabecera.len = longitud+pos;
	
	pcap_dump((u_char*)pdumper, &cabecera, trama);

	/*Imprimimos por pantalla si la flag de mostrar esta a 1*/
	if(flag_mostrar == 1){
		printf("\nModulo ETH\n");
		mostrarHex(trama, longitud + pos);
	}

	
	return OK;
}



/***************************Funciones auxiliares a implementar***********************************/

/****************************************************************************************
 * Nombre: aplicarMascara                                                               *
 * Descripcion: Esta funcion aplica una mascara a una vector                            *
 * Argumentos:                                                                          *
 *  -IP: IP a la que aplicar la mascara en orden de red                                 *
 *  -mascara: mascara a aplicar en orden de red                                         *
 *  -longitud: bytes que componen la direccion (IPv4 == 4)                              *
 *  -resultado: Resultados de aplicar mascara en IP en orden red                        *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t aplicarMascara(uint8_t* IP, uint8_t* mascara, uint8_t longitud, uint8_t* resultado){
	//TODO
	//[...]

	int i;

	if(!IP || !mascara || !resultado){
		printf("No se ha podido aplicar la mascara\n");
		return ERROR;
	}

	/*Aplicamos la mascara*/
	for(i = 0; i < longitud; i++){
		resultado[i] = IP[i] & mascara[i];
	}

	return OK;
}


/***************************Funciones auxiliares implementadas**************************************/

/****************************************************************************************
 * Nombre: mostrarHex                                                                   *
 * Descripcion: Esta funcion imprime por pantalla en hexadecimal un vector              *
 * Argumentos:                                                                          *
 *  -datos: bytes que conforman un mensaje                                              *
 *  -longitud: Bytes que componen el mensaje                                            *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t mostrarHex(uint8_t * datos, uint32_t longitud){
	uint32_t i;
	printf("Datos:\n");
	for (i=0;i<longitud;i++){
		printf("%02"PRIx8" ", datos[i]);
	}
	printf("\n");
	return OK;
}


/****************************************************************************************
 * Nombre: calcularChecksum                                                             *
 * Descripcion: Esta funcion devuelve el ckecksum tal como lo calcula IP/ICMP           *
 * Argumentos:                                                                          *
 *   -datos: datos sobre los que calcular el checksum                                   *
 *   -longitud: numero de bytes de los datos sobre los que calcular el checksum         *
 *   -checksum: checksum de los datos (2 bytes) en orden de red!                        *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t calcularChecksum(uint8_t *datos, uint16_t longitud, uint8_t *checksum) {
    uint16_t word16;
    uint32_t sum=0;
    int i;
    // make 16 bit words out of every two adjacent 8 bit words in the packet
    // and add them up
    for (i=0; i<longitud; i=i+2){
        word16 = (datos[i]<<8) + datos[i+1];
        sum += (uint32_t)word16;       
    }
    // take only 16 bits out of the 32 bit sum and add up the carries
    while (sum>>16) {
        sum = (sum & 0xFFFF)+(sum >> 16);
    }
    // one's complement the result
    sum = ~sum;      
    checksum[0] = sum >> 8;
    checksum[1] = sum & 0xFF;
    return OK;
}


/***************************Funciones inicializacion implementadas*********************************/

/****************************************************************************************
 * Nombre: inicializarPilaEnviar                                                        *
 * Descripcion: inicializar la pila de red para enviar registrando los distintos modulos*
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t inicializarPilaEnviar() {
	bzero(protocolos_registrados,MAX_PROTOCOL*sizeof(pf_notificacion));
	if(registrarProtocolo(ETH_PROTO, moduloETH, protocolos_registrados)==ERROR)
		return ERROR;
	if(registrarProtocolo(IP_PROTO, moduloIP, protocolos_registrados)==ERROR)
		return ERROR;
	
	//TODO
	//A registrar los modulos de ICMP y UDP [...]
	/*Registro del protocolo ICMP*/
	if(registrarProtocolo(ICMP_PROTO, moduloICMP, protocolos_registrados)==ERROR)
		return ERROR;

	/*Registro del protocolo UDP*/
	if(registrarProtocolo(UDP_PROTO, moduloUDP, protocolos_registrados)==ERROR)
		return ERROR; 

	return OK;
}


/****************************************************************************************
 * Nombre: registrarProtocolo                                                           *
 * Descripcion: Registra un protocolo en la tabla de protocolos                         *
 * Argumentos:                                                                          *
 *  -protocolo: Referencia del protocolo (ver RFC 1700)                                 *
 *  -handleModule: Funcion a llamar con los datos a enviar                              *
 *  -protocolos_registrados: vector de funciones registradas                            *
 * Retorno: OK/ERROR                                                                    *
 ****************************************************************************************/

uint8_t registrarProtocolo(uint16_t protocolo, pf_notificacion handleModule, pf_notificacion* protocolos_registrados){
	if(protocolos_registrados==NULL ||  handleModule==NULL){		
		printf("Error: registrarProtocolo(): entradas nulas.\n");
		return ERROR;
	}
	else
		protocolos_registrados[protocolo]=handleModule;
	return OK;
}


