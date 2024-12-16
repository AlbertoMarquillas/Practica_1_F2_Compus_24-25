;**********
;CAP�ALERES   
;**********
    LIST P=PIC18F4321 F=INHX32
    #include <p18f4321.inc>

    ;**************
    ;CONFIGURACIONS 
    ;**************
    CONFIG OSC = HSPLL    ; 40MHz
    CONFIG PBADEN = DIG   ; PORTB
    CONFIG WDT = OFF 

;**********
;VARIABLES
;**********
FLAGS EQU 0x0001 ;Flags per avisar a les interrupcions
NUM_NOTES EQU 0x0002
LECTURA EQU 0x0003
COMPT_ESPERA_ACK EQU 0x0004;Comptador per generar ACK
NOTA_LLEGIDA EQU 0x0005
DURATION_LLEGIDA EQU 0x0006
COMPTADOR_3S_L EQU 0x0007
COMPTADOR_3S_H EQU 0x0008
FLAGS_INT EQU 0x0009 ;Flgs que venen de les interrupcions
COMPTADOR_500MS_L EQU 0x000A
COMPTADOR_500MS_H EQU 0x000B
COMPT_ESPERA_TRIGGER EQU 0x000C ;Comptador per generar trigger
COMPTADOR_2S_H EQU 0x000D
COMPTADOR_2S_L EQU 0x000E
COMPTADOR_1S_H EQU 0x000F
COMPTADOR_1S_L EQU 0x0010
    ;******************************
    ;VECTORS DE RESET I INTERRUPCI� 
    ;******************************
    ORG 0x000
    GOTO MAIN
    ORG 0X0008
    GOTO HIGH_RSI
    ORG 0X0018
    RETFIE FAST


;**********************
;CONFIGURACI� DE PORTS  
;**********************
CONFIG_PORTS
    ;PORT A -> Pins anal�gics: RA0, RA1, RA2, RA3, RA5; Pins d'oscil�lador: RA6, RA7
    BCF TRISA,0,0   ; Sortida  --  Length_0 (Led)
    BCF TRISA,1,0   ; Sortida  --  Length_1 (Led)
    BCF TRISA,3,0   ; Sortida  --  Answer Correct (Led RA3)
    BCF TRISA,4,0   ; Sortida  --  Answer Incorrect (Led RA4)
    
    BCF TRISA,2,0   ; Sortida  --  Trigger (Ultrasons)
    BCF TRISA,5,0   ; Sortida  --  GameScore (Servo)
    ;Pins RA6, RA7 no s'utilitzen per HSPLL
    
    ;PORT B - Entrades -> Pins d'interrupci�: RB0, RB1, RB2 ; Pins anal�gics: RB0, RB1, RB2, RB3, RB4
    BSF TRISB,0,0   ; Entrada --  NewNote (Ve de la F1)
    BSF TRISB,1,0   ; Entrada --  StartGame (Ve de la F1)
    BSF TRISB,2,0   ; Entrada --  Echo (Ultrasons)
    
    ;PORT C
    BSF TRISC,0,0   ; Entrada - Note_0 (ve de la F1)
    BSF TRISC,1,0   ; Entrada - Note_1 (ve de la F1)
    BSF TRISC,2,0   ; Entrada - Note_2 (ve de la F1)
    BSF TRISC,3,0   ; Entrada - Note_3 (ve de la F1)
    BSF TRISC,4,0   ; Entrada - Duration_0 (ve de la F1)
    BSF TRISC,5,0   ; Entrada - Duration_1 (ve de la F1)
    BCF TRISC,6,0   ; Sortida - ACK (va cap a la F1)
    BCF TRISC,7,0   ; Sortida - Speaker
    
    ;PORT D
    CLRF TRISD,0    ; Sortida - CurrentNote[7..0] (Seven segments)
    
    RETURN
    
;******************
;INICIALITZAR PORTS  
;******************
INIT_PORTS         ; Init sortides dels ports
    CLRF LATA,0    ; Netejem sortides PORTA (TOT SORTIDES)
    BCF LATC,6,0   ; Netejem el ACK
    BCF LATC,7,0   ; Netejem el PWM Speaker
    CLRF LATD,0    ; Netejem Seven Segments (CurrentNote[7..0])
    
    RETURN

;***********************
;INICIALITZAR VARIABLES 
;***********************
INIT_VARS
    BCF FLAGS, 0, 0 ;Posem a 0 el bit 0 de la variable Flags -> Indica quan el joc ha comen�at (START GAME ES ACTIU)
    BCF FLAGS, 1, 0 ;Posem a 0 el bit 1 de la variable Flags -> Indica que s'han de comptar 3 segons
    BCF FLAGS, 2, 0 ;Posem a 0 el bit 2 de la variable Flags -> Indica quan s'ha de fer algo amb el servo
    BCF FLAGS, 3, 0 ;Posem a 0 el bit 3 de la variable Flags -> Indica quan s'ha de fer algo amb l'altaveu
    BCF FLAGS, 4, 0 ;Posem a 0 el bit 4 de la variable Flags -> Indica quan s'han de comptar 500ms
    BCF FLAGS, 5, 0 ;Posem a 0 el bit 5 de la variable Flags -> Indica quan s'ha de comptar 1 segon
    BCF FLAGS, 6, 0 ;Posem a 0 el bit 6 de la variable Flags -> Indica quan s'han de comptar 2 segons

    CLRF NUM_NOTES
    CLRF LECTURA 
    CLRF COMPT_ESPERA_ACK 
    CLRF NOTA_LLEGIDA 
    CLRF DURATION_LLEGIDA 
    CLRF COMPTADOR_500MS_L
    CLRF COMPTADOR_500MS_H 
    CLRF COMPTADOR_1S_L
    CLRF COMPTADOR_1S_H 
    CLRF COMPTADOR_2S_L
    CLRF COMPTADOR_2S_H 
    CLRF COMPTADOR_3S_L
    CLRF COMPTADOR_3S_H 
    BCF FLAGS_INT, 0, 0 ;Posem a 0 el bit 0 de la variable Flags_Int -> Indica quan s'han comptat els 3 segons    
    BCF FLAGS_INT, 1, 0 ;Posem a 0 el bit 1 de la variable Flags_Int -> Indica quan s'ha comptat 1 segons
    BCF FLAGS_INT, 2, 0 ;Posem a 0 el bit 2 de la variable Flags_Int -> Indica quan s'han comptat els 2 segons
    BCF FLAGS_INT, 3, 0 ;Posem a 0 el bit 3 de la variable Flags_Int -> Indica quan s'han comptat els 500 milisegons

    SETF PORTD ;Netejem tot el port D per no encende el display 7-segments
    RETURN

;****************************
;CONFIGURACI� D'INTERRUPCIONS  
;****************************
CONFIG_INTERRUPTS
    ; Activar Interrupcions
    BSF INTCON,7,0   ; Unmasked
    BSF INTCON,6,0   ;High
    ; Activo interrupci� RB0 i indico que es per flanc de baixada (NewNote)
    BSF INTCON, INT0IE,0    ; Activo interrupci� RB0
    BCF INTCON2, INTEDG0,0  ; Indico flanc de baixada
    ;Activo interrupci� RBI (StartGame)
    BSF INTCON3, INT1IE, 0  ; Activo interrupci� RB1
    ;Activo interrupci� TMR0
    BSF INTCON, TMR0IE, 0   ; Activo interrupci� TMR0
    
    RETURN
    
;*****************
;INICIALITZAR RAM
;*****************
PUNTER_RAM
    MOVLW .1
    MOVWF FSR0H,0   ; Inicialitzar al banc 1 el punter d'escriptura de la RAM
    CLRF FSR0L,0    ; Inicialitzar el punter a l'adre�a 0
    MOVLW .1
    MOVWF FSR1H,0   ; Incialitzar al banc 1 el punter de lectura de la RAM
    CLRF FSR1L,0    ; Inicialitzar el punter a l'adre�a 0
    
    RETURN

;**************************
;CARREGAR EL VALOR DE TMR0
;**************************
CARREGA_TMR0
    
    MOVLW HIGH(.60536) ;Primer carreguem High
    MOVWF TMR0H, 0
    MOVLW LOW(.60536) ;Despres carreguem low
    MOVWF TMR0L, 0
    RETURN
    
;*****************
;INICIALITZAR TMR0
;*****************
INIT_TMR0
    ;Tint=(4/Fosc)Preescarler(2^Bits - L)
    ;500us=(4/40M) * 256 * (2^16 - L) = 65516,46875
    ;Preescaler=246, Bits=16
    
    ;500us=(4/40M) * 1 * (2^16 - L) -> L = 60536
    MOVLW b'10001000'
    ;Bit 7: Enables TMR0
    ;Bit 6: Timer 8/16 bit
    ;Bit 5: CLKO
    ;Bit 4: High-To-Low
    ;Bit 3: Not preescaler output
    ;Bit 2-0: Prescale value
    MOVWF T0CON, 0
    RETURN

;****************
;ACCIO DEL TMR0
;****************
ACTION_TMR0

    BCF INTCON, TMR0IF, 0 ;Netejem el flag
    CALL CARREGA_TMR0 ;Carreguem el valor del tmr0 de nou

    BTFSC FLAGS, 1, 0 ;Mirem el Flag de comptar 3 segons
    CALL COMPTAR_3S

    BTFSC FLAGS, 2, 0 ;Mirem el Flag d'activar el servo
    CALL SERVO
    
    BTFSC FLAGS, 3, 0 ;Mirem el Flag d'activar l'altaveu
    CALL ALTAVEU
    
    BTFSC FLAGS, 4, 0 ;Mirem el Flag de comptar 500ms
    CALL COMPTAR_500MS

    BTFSC FLAGS, 5, 0 ;Mirem el Flag de comptar 1 segon
    CALL COMPTAR_3S
    
    BTFSC FLAGS, 6, 0 ;Mirem el Flag de comptar 2 segons
    CALL COMPTAR_3S
    
    RETURN

;****************
;SERVOMOTOR
;****************
SERVO
    BCF FLAGS, 2, 0 ;Posem a 0 el bit 2 de la variable Flags, que indica que s'ha d'activar el servo
    RETURN
;****************
;ESPERA PER L'ACK
;****************
ALTAVEU
    BCF FLAGS, 3, 0 ;Posem a 0 el bit 3 de la variable Flags, que indica que s'ha d'activar l'altaveu
    RETURN

;****************
;ESPERA PER L'ACK
;****************
ESPERA_ACK ;Espera 2ms
    ;Tosc = 1 / 40M = 25ns
    ;Cicle m�quina = 4*Tosc = 4*25ns = 100ns
    ;Cicles=2m/100ns=20000
    MOVLW .255 ;1CICLE (De moment poso 255)
    MOVWF COMPT_ESPERA_ACK, 0 ;1CICLE
LOOP_ESPERA_ACK ;20000 - 4 = 19996
    ;3x = 19996 -> x=6665,3
    ;3x=19995 -> 6665 (+ 1 NOPs)
    DECFSZ COMPT_ESPERA_ACK, 1, 0 ;3CICLES
    GOTO LOOP_ESPERA_ACK ;2CICLES

    ;Es molt gran, puc fer servir dos comptadors (No se com es faria) o comptar un maxim de 255 cicles (2^8)
    ;255*100ns=25,5us
    ;Esperes de 25us per arribar a 2ms -> 2ms/25us = 1000

    RETURN

;************
;ACTIVAR ACK  
;************
ACTIVAR_ACK
    BSF LATC,6,0      ; Activem ACK (RC6)
    CALL ESPERA_ACK   ; Ens hem d'esperar perqu� la fase 1 detecti ACK ;2CICLES
    BCF LATC,6,0      ; Desactivem ACK (RC6)
    
    RETURN

;*****************
;GUARDAR NOTA RAM 
;*****************
GUARDAR_NOTA_RAM
    MOVFF NUM_NOTES, FSR0L    ;Movem el punter d'escriptura de la RAM per escriure una nota nova -> el banc sempre �s 1
    ; M�scara per guardar Nota[3..0] : Duration[1..0]
    MOVFF PORTC, LECTURA
    MOVLW b'00111111'
    ANDWF LECTURA,0,0           ; Multipliquem la m�scara pel portc i guardem el resultat a w
    ; Guardem la nota
    MOVWF POSTINC0,0         ; Escriure RAM (FSR1)
    
    RETURN

;***************
;PROCES NEWNOTE
;***************
NEW_NOTE
    BCF INTCON,INT0IF,0   ; Netejem el flag de la interrupci�
    ;Guardar la nota a la RAM
    CALL GUARDAR_NOTA_RAM
    ;Incrementar num notes
    INCF NUM_NOTES,1,0
    ;Activar ACK
    CALL ACTIVAR_ACK
    
    RETURN

;*********************************
;CONTROLAR INTERRUPCI� START GAME 
;*********************************
ACTIVAR_START_GAME
    BCF INTCON3, INT1IF,0   ; Netejar flanc d'interrupci�
    BSF FLAGS,0,0      ; Activem flag per comen�ar el joc (FLAG<0>=Start Game)
    
    RETURN

;******************************
;INTERRUPCIONS D'ALTA PRIORITAT
;******************************
HIGH_RSI
    ;Comprovem flag TMR0
    BTFSC INTCON, TMR0IF, 0
    CALL ACTION_TMR0
    ;Comprovem flag RB0 (NewNote)
    BTFSC INTCON, INT0IF, 0
    CALL NEW_NOTE ;Ho posem per interrupci� perque va per flanc
    ;Comprovem flag RB1 (StartGame)
    BTFSC INTCON3, INT1IF, 0
    CALL ACTIVAR_START_GAME
    
    RETFIE FAST
;****************************
;MOSTRAR NOTA PEL 7-SEGMENTS
;****************************
MOSTRAR_NOTA
	; M�scara per agafar nom�s la nota
	MOVLW b'00001111'
	ANDWF NOTA_LLEGIDA, 1, 0          ; Apliquem la m�scara i guardem la nota
	; Comprovar quina nota �s
	MOVLW b'00000000'          ; Posem un 0 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 0
	CALL NOTA_0

	MOVLW b'00000001'          ; Posem un 1 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 1
	CALL NOTA_1

	MOVLW b'00000010'          ; Posem un 2 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 2
	CALL NOTA_2

	MOVLW b'00000011'          ; Posem un 3 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 3
	CALL NOTA_3

	MOVLW b'00000100'          ; Posem un 4 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 4
	CALL NOTA_4

	MOVLW b'00000101'          ; Posem un 5 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 5
	CALL NOTA_5

	MOVLW b'00000110'          ; Posem un 6 al w
	SUBWF NOTA_LLEGIDA, 0      ; Restem per comparar
	BTFSC STATUS, Z, 0         ; Si STATUS = 0 -> SALTA; Si STATUS = 1 -> NOTA_LLEGIDA = 6
	CALL NOTA_6
	
	RETURN
	
;**************************
;MOSTRAR DURACI� PELS LEDS
;**************************
MOSTRAR_LEDS
	; Apliquem m�scara per quedar-nos amb la duraci�
	MOVLW b'00110000'		   ; M�scara
	ANDWF DURATION_LLEGIDA, 1, 0       ; Apliquem m�scara i guardem a la mateixa variable
	
	RRNCF DURATION_LLEGIDA, 1, 0
	RRNCF DURATION_LLEGIDA, 1, 0
	RRNCF DURATION_LLEGIDA, 1, 0
	RRNCF DURATION_LLEGIDA, 1, 0

	; Comprovo els valors del led
	MOVLW .1                      ; Comprovem si �s 1
	SUBWF DURATION_LLEGIDA, 0, 0             ; Restem la nota llegida i el w (comparar)
	BTFSC STATUS, Z, 0             ; si �s igual -> STATUS = 1
	CALL LED_1
	
	MOVLW .2                      ; Comprovem si �s 2
	SUBWF DURATION_LLEGIDA, 0, 0             ; Restem la nota llegida i el w (comparar)
	BTFSC STATUS, Z, 0             ; si �s igual -> STATUS = 1
	CALL LED_2
	
	MOVLW .3                      ; Comprovem si �s 3
	SUBWF DURATION_LLEGIDA, 0, 0             ; Restem la nota llegida i el w (comparar)
	BTFSC STATUS, Z, 0             ; si �s igual -> STATUS = 1
	CALL LED_3
	
	RETURN
	
;******************
;NOTES (7-SEGMENTS)
;******************
; El 7-segments funciona per l�gica negativa
NOTA_0
	MOVLW b'10000010'              ; Combinaci� per mostrar un 0 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_1
	MOVLW b'11001111'              ; Combinaci� per mostrar un 1 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_2
	MOVLW b'10010001'              ; Combinaci� per mostrar un 2 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_3
	MOVLW b'10000101'              ; Combinaci� per mostrar un 3 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_4
	MOVLW b'11001100'              ; Combinaci� per mostrar un 4 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_5
	MOVLW b'10100100'              ; Combinaci� per mostrar un 5 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_6
	MOVLW b'10100000'              ; Combinaci� per mostrar un 6 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
NOTA_7
	MOVLW b'10001111'              ; Combinaci� per mostrar un 7 pel 7-seg
	MOVWF LATD, 0                  ; Carreguem el valor al port de sortida
	RETURN                          ; Tornem a l'execuci� de StartGame per mirar els LEDS
	
;**************
;DURACI� (LEDS)
;**************
LED_1
    BSF LATA, 0, 0         ; Activem length_0
    BCF LATA, 1, 0         ; Desactivem lenght_1
    RETURN                  ; Tornem a l'execuci� de StartGame
	
LED_2
    BCF LATA, 0, 0         ; Activem length_0
    BSF LATA, 1, 0         ; Desactivem lenght_1
    RETURN                  ; Tornem a l'execuci� de StartGame
    
LED_3
    BSF LATA, 0, 0         ; Activem length_0
    BSF LATA, 1, 0         ; Desactivem lenght_1
    RETURN                  ; Tornem a l'execuci� de StartGame

;*********************************
;COMPROVAR SI HI HA NOTES A LA RAM  
;*********************************
COMPROVAR_NOTES
	; Mirem si la variable Num_Notes = 0
	MOVLW .0
	SUBWF NUM_NOTES,0,0
	BTFSC STATUS, Z, 0    ; Si STATUS = 0 -> Saltara; Si Num_Notes = 0 -> STATUS = 1
	GOTO RAM_BUIDA_END    ; Si la RAM �s buida no continuem amb el programa
	
	RETURN

;**********************************
;INICIALITZAR COMPTADOR DE 500MS
;**********************************
INIT_COMPT_500MS
	;Interrupcions de 500u per aribar a 500ms -> 500ms/500us = 1000
	;2 registres 65535
	;1000 d = 0011 1110 1000 b
	MOVLW HIGH(.1000)
	MOVWF COMPTADOR_500MS_H, 0
	MOVLW LOW(.1000)
	MOVWF COMPTADOR_500MS_L, 0
	
	BCF FLAGS_INT, 2, 0 ;Poso a 0 la variable que indica quan he comptat 2 segons
	BSF FLAGS, 1, 0 ;FLAGS<1>=Comptar 3 Segons
	RETURN
;*****************
;COMPTAR 500MS
;*****************
COMPTAR_500MS
    BCF FLAGS, 4, 0; Posem a 0 el flag que indica que s'han de comptar els 500ms
    DECF COMPTADOR_500MS_L,1,0
    BTFSC STATUS,C,0
    RETURN
    DECF COMPTADOR_500MS_H,1,0
    BTFSC STATUS,C,0
    RETURN
    BSF FLAGS_INT, 3, 0 ;Poso a 1 el bit 1 de la variable Flags_Int, indicant que ja he comptat 500usS

    RETURN

;**********************************
;INICIALITZAR COMPTADOR DE 1 SEGON
;**********************************
INIT_COMPT_1S
	;Interrupcuins de 500u per arribar a 3s -> 2s/500us = 2000
	;2 registres 65535
	MOVLW HIGH(.2000)
	MOVWF COMPTADOR_1S_H, 0
	MOVLW LOW(.2000)
	MOVWF COMPTADOR_1S_L, 0
	
	BCF FLAGS_INT, 1, 0 ;Poso a 0 la variable que indica quan he comptat 2 segons
	BSF FLAGS, 5, 0 ;FLAGS<5>=Comptar 1 Segon
    
	RETURN
	
;*****************
;COMPTAR 1 SEGON
;*****************
COMPTAR_1S   
	
    DECF COMPTADOR_1S_L,1,0
    BTFSC STATUS,C,0
    RETURN
    
    DECF COMPTADOR_1S_H,1,0
    BTFSC STATUS,C,0
    RETURN
    
    BSF FLAGS_INT, 1, 0 ;Poso a 1 el bit 1 de la variable Flags_Int, indicant que ja he comptat 1s
    BCF FLAGS, 5, 0 ;Poso a 0 el flag de comptar 3 segons
    RETURN
    
;**********************************
;INICIALITZAR COMPTADOR DE 2 SEGONS
;**********************************
INIT_COMPT_2S
	;Interrupcuins de 500u per arribar a 3s -> 2s/500us = 4000
	;2 registres 65535
	MOVLW HIGH(.4000)
	MOVWF COMPTADOR_2S_H, 0
	MOVLW LOW(.4000)
	MOVWF COMPTADOR_2S_L, 0
	
	BCF FLAGS_INT, 2, 0 ;Poso a 0 la variable que indica quan he comptat 2 segons
	BSF FLAGS, 6, 0 ;FLAGS<6>=Comptar 2 Segons
    
	RETURN
	
;*****************
;COMPTAR 2 SEGONS
;*****************
COMPTAR_2S   
	
    DECF COMPTADOR_2S_L,1,0
    BTFSC STATUS,C,0
    RETURN
    
    DECF COMPTADOR_2S_H,1,0
    BTFSC STATUS,C,0
    RETURN
    
    BSF FLAGS_INT, 2, 0 ;Poso a 1 el bit 0 de la variable Flags_Int, indicant que ja he comptat 3s
    BCF FLAGS, 6, 0 ;Poso a 0 el flag de comptar 3 segons
    RETURN
    
;**********************************
;INICIALITZAR COMPTADOR DE 3 SEGONS
;**********************************
INIT_COMPT_3S
	;Interrupcuins de 500u per arribar a 3s -> 3s/500us = 6000
	;2 registres 65535
        ;6000 d = 0001 0111 0111 0000 b
	MOVLW HIGH(.6000)
	MOVWF COMPTADOR_3S_H, 0
	MOVLW LOW(.6000)
	MOVWF COMPTADOR_3S_L, 0
	
	BCF FLAGS_INT, 0, 0
	BSF FLAGS, 1, 0 ;FLAGS<1>=Comptar 3 Segons
    
	RETURN
	
;*****************
;COMPTAR 3 SEGONS
;*****************
COMPTAR_3S   
	
    DECF COMPTADOR_3S_L,1,0
    BTFSC STATUS,C,0
    RETURN
    
    DECF COMPTADOR_3S_H,1,0
    BTFSC STATUS,C,0
    RETURN
    
    BSF FLAGS_INT, 0, 0 ;Poso a 1 el bit 0 de la variable Flags_Int, indicant que ja he comptat 3s
    BCF FLAGS, 1, 0 ;Poso a 0 el flag de comptar 3 segons
    RETURN
    
;************************************
;ESPERA PER LA GENERACI� DEL TRIGGER
;************************************
ESPERA_GENERAR_TRIGGER
    ;Tosc = 1 / 40M = 25ns
    ;Cicle m�quina = 4*Tosc = 4*25ns = 100ns
    ;Cicles= 10us/100ns=100
    MOVLW .32 ;1CICLE
    MOVWF COMPT_ESPERA_TRIGGER, 0 ;1CICLE
LOOP_ESPERA_TRIGGER ;100 - 4 = 96
    ;3x = 96 -> X = 32
    DECFSZ COMPT_ESPERA_TRIGGER, 1, 0 ;3CICLES
    GOTO LOOP_ESPERA_TRIGGER ;2CICLES
    RETURN
    
;******************************
;GENERAR TRIGGER PER ULTRASONS
;******************************
GENERAR_TRIGGER
    BSF LATA, 2, 0 ;Activo el bit 2 del port A, on esta conectat el trigger
    CALL ESPERA_GENERAR_TRIGGER ;M'ha d'esperar 10us
    BCF LATA, 2, 0
    RETURN 

;***********
;START GAME  
;***********
START_GAME
    ; Posicionem el punter de lectura de la RAM a la primera posici�
    CLRF FSR1L, 0
    ; Comprovar si hi ha notes
    CALL COMPROVAR_NOTES
LOOP_START_GAME
    ; Llegim la posici�
    MOVF POSTINC1       ; Movem el registre al work
    MOVWF NOTA_LLEGIDA, 0
    MOVWF DURATION_LLEGIDA, 0
    ; Mostrem la nota al 7-segments
    CALL MOSTRAR_NOTA
    ; Mostrem la duraci� als leds
    CALL MOSTRAR_LEDS
    ;Comptar 3s amb el tmr0 -> Indco amb un flag
    ;Posar un comptador a 0
    ;Activar una flag
    CALL INIT_COMPT_3S
    CALL ESPERAR_3S
    
    ; Comprovar Ultrasons
    ; Comprovar si hi ha notes per llegir
    MOVF NUM_NOTES, 0   ; Movem num_notes al w
    SUBWF FSR1L,0       ; Restem el n�mero de notes al punter de lectura de la RAM
    BTFSS STATUS, Z, 0   ; Comprovem -> STATUS = 1 : S�n iguals -> Si son iguals vol dir que s'ha acabat
    GOTO LOOP_START_GAME

RAM_BUIDA_END
    GOTO RAM_BUIDA_END
	
	RETURN
	
;********************
;COMPTAR 3S
;********************
ESPERAR_3S
    LOOP_MOSTRAR_3S
	BTFSS FLAGS_INT, 0, 0 ;Miro el flag que genera la meva interrupci�
	GOTO LOOP_MOSTRAR_3S
    BCF FLAGS_INT, 0, 0 ;Poso a 0 el flag per poder tornar a comptar
	RETURN
;********************
;MAIN 
;********************
MAIN
    CALL CONFIG_PORTS   ; CONFIGURAR ELS VO PORTS
    CALL INIT_PORTS     ; INICIALITZAR O PORTS
    CALL INIT_VARS      ; INICIALITZAR VARIABLES
    CALL CONFIG_INTERRUPTS    ; CONFIGURAR INTERRUCIONS
    CALL PUNTER_RAM     ; INICIALITZEM ELS PUNTERS DE LA RAM
    CALL INIT_TMR0      ; INICIALITZEM EL TMR0 PER INTERRUMPIR
    CALL CARREGA_TMR0   ; CARREGA EL TMR0 PER GENERAR LA INTERRUPCI�
LOOP
		    ; FEM NEWNOTE PER INTERRUPCIONS -> INT0IE
		    ;FEM STARTGAME PER INTERRUPCI� -> INT1IE
    BTFSC FLAGS,0,0     ; MIREM EL BIT 0 DEL REGISTRE QUE INDICA QUE START GAME ES ACTIU
    CALL START_GAME


    ENDsss