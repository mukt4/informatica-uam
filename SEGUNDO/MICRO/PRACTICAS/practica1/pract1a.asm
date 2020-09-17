;**************************************************************************
; SBM 2015. ESTRUCTURA BÁSICA DE UN PROGRAMA EN ENSAMBLADOR
;**************************************************************************
	; DEFINICION DEL SEGMENTO DE DATOS
	DATOS SEGMENT
		;-- rellenar con los datos solicitados
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
	; Cargar 15H en AX
	; Para comprobar que hemos cargado correctamente el 15 en AX
	; ejecutamos el programa con el compilador y revisamos si despues de la instruccion
	; se ha realizado correctamente la escritura
	MOV AX, 15H
	; Cargar BBH en BX 
	; Para comprobar que hemos cargado correctamente BB en BX realizamos la misma comprobacion
	; que en la instruccion anterior
	MOV BX, 0BBH
	; Cargar 3412H en CX 
	; Para comprobar que hemos cargado correctamente 3412 en CX realizamos la misma comprobacion
	; que en la instruccion anterior
	MOV CX, 3412H
	; Cargar el contenido de CX en DX
	; Para comprobar si hemos realizado esta instruccion correctamente, observamos si tras 
	; la instruccion hemos cargado 3412 en DX
	MOV DX, CX
	; Cargar en BL el contenido de la posición de memoria 65636H y en BH en contenido de
	; la posición 65637H
	; Primero cargamos en el registro AX el valor 6000 para poder inicializar correctamente
	; el data segment
	MOV AX, 6000H
	; Cargamos el contenido de AX en DS, lo que se traduce en inicializar el data segment en 
	; la posicion de memoria 60000
	MOV DS, AX
	; Cargamos en BX el contenido de la posicion de memoria 60000 + 5636 = 65636
	; Para comprobar si hemos realizado las instrucciones correctamente nos dirijimos a la 
	; posicion 65636 y comprobamos si tras la instruccion se realiza la escritura correctamente
	; en BX
	MOV BX, DS:[5636H]
	; Cargar en la posición de memoria 50005H el contenido de CH
	; Para realizar esta carga tenemos que seguir los mismos pasos que en el apartado enterior
	; Primero cargamos en AX 5000
	MOV AX,5000H
	; Despues inicializamos el data segment en la posicion 50000
	MOV DS, AX
	; Cargamos en la posicion de memoria 50000 + 5 = 50005 el contenido de CH
	MOV DS:[5H],CH
	; Cargar en AX el contenido de la dirección de memoria apuntada por DI
	MOV AX,[DI]
	; Cargar en BX el contenido de la dirección de memoria que está 10 bytes por encima de
	; la dirección apuntada por BP 
	MOV BX, 10[BP]
	; FIN DEL PROGRAMA
	MOV AX, 4C00H
	INT 21H
	INICIO ENDP
	; FIN DEL SEGMENTO DE CODIGO
	CODE ENDS
	; FIN DEL PROGRAMA INDICANDO DONDE COMIENZA LA EJECUCION
	END INICIO 