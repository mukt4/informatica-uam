#ifndef PRACTICA2_H
#define PRACTICA2_H

#include <stdio.h>

void handleSignal(int nsignal);

void analizar_paquete(u_char *user,const struct pcap_pkthdr *hdr, const uint8_t *pack);

void analizarCabeceraUdp(const uint8_t *pack);

void analizarCabeceraTcp(const uint8_t *pack);

#endif