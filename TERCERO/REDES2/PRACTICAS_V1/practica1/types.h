/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#ifndef TYPES_H
#define	TYPES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/socket.h>
#include <resolv.h>
#include <arpa/inet.h>
#include <error.h>
#include <stdarg.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <resolv.h>
#include <arpa/inet.h>
#include <errno.h>
#include <error.h>
#include <stdarg.h>
#include <unistd.h>
#include <syslog.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <limits.h>

#define SA struct sockaddr

typedef enum{
    FALSE=0, TRUE=1
}Bool;

typedef enum{
    ERROR=0, OK=1
}Status;

typedef enum{
    OK200=200, ERROR404=404, ERROR400=400 , ERROR501=501
} Cod;

#endif
