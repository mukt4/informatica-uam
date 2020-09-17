;**************************************************************************
; SBM 2015. ESTRUCTURA BÁSICA DE UN PROGRAMA EN ENSAMBLADOR
;**************************************************************************
	; DEFINICION DEL SEGMENTO DE DATOS
	DATOS SEGMENT
		; Reservar memoria para una variable, CONTADOR, de un byte de tamaño.
		CONTADOR DB ?
		; Reservar memoria para una variable, TOME, de dos bytes de tamaño, e inicializarla con
		; el valor CAFEH
		TOME DW 0CAFEH
		; Reservar 100 bytes para una tabla llamada TABLA100
		TABLA100 DB 64H DUP (0)
		; Guardar en memoria la cadena de texto “Atención: Entrada de datos incorrecta.”, de
		; nombre ERROR1, para agilizar la salida de mensajes en un programa de corrección
		; automática de prácticas.
		ERROR1 DB "Atencion: Entrada de datos incorrecta."
	DATOS ENDS
	;**************************************************************************
	; DEFINICION DEL SEGMENTO DE PILA
	PILA SEGMENT STACK "STACK"
		DB 40H DUP (0) ;ejemplo de inicialización, 64 bytes inicializados a 0
	PILA ENDS
	;**************************************************************************
	; DEFINICION DEL SEGMENTO EXTRA
	EXTRA SEGMENT
		RESULT DW 0,0 ;ejemplo de inicialización. 2 PALABRAS (4 BYTES)
	EXTRA ENDS
	;**************************************************************************
	; DEFINICION DEL SEGMENTO DE CODIGO
	CODE SEGMENT
	ASSUME CS: CODE, DS: DATOS, ES: EXTRA, SS: PILA
	; COMIENZO DEL PROCEDIMIENTO PRINCIPAL
	INICIO PROC
	; INICIALIZA LOS REGISTROS DE SEGMENTO CON SU VALOR
	MOV AX, DATOS
	MOV DS, AX
	MOV AX, PILA
	MOV SS, AX
	MOV AX, EXTRA
	MOV ES, AX
	MOV SP, 64 ; CARGA EL PUNTERO DE PILA CON EL VALOR MAS ALTO
	; FIN DE LAS INICIALIZACIONES
	; COMIENZO DEL PROGRAMA
	; Copiar el sexto carácter de la cadena ERROR1 en la posición 63H de TABLA100
	; Primero guardamos en AL el sexto caracter de la cadena error1 que equivale a 69H
	MOV AL, ERROR1[SI+5]
	; Guardamos el sexto caracter en la posicion 98 de la tabla, es decir, la posicion 63H de TABLA100
	MOV TABLA100[SI+98],AL
	; Copiar el contenido de la variable TOME a partir de la posición 23H de TABLA100
	; Guardamos la variable TOME en AX, es decir, guardamos CAFEH en AX 
	MOV AX, TOME
	; Guardamos FEH en la posicion 34 de la tabla[equivalente a 23H en TABLA100] 
	MOV TABLA100[SI+34],AL
	; Guardamos CAH en la posicion 35 de la tabla[equivalente a 24H en TABLA100]
	MOV TABLA100[SI+35],AH
	; Copiar el byte más significativo de TOME a la variable CONTADOR 
	; Como ya tenemos TOME guardado en AX, simplemente tomamos el byte mas significativo utilizando AH
	MOV CONTADOR,AH
	; FIN DEL PROGRAMA
	MOV AX, 4C00H
	INT 21H
	INICIO ENDP
	; FIN DEL SEGMENTO DE CODIGO
	CODE ENDS
	; FIN DEL PROGRAMA INDICANDO DONDE COMIENZA LA EJECUCION
	END INICIO 