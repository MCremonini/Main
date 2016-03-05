;****************************************************************
;
;****************************************************************
TITLE MyFirstASM    ; programma ASM

COMMENT *Mio Commento del programma*


;****************************************************************
;
;****************************************************************
NOME_STACK_SEG SEGMENT PARA STACK 'STACK'
    DB 1024 DUP (0)
NOME_STACK_SEG ENDS

          
  
;Contact:= struc
;	Name            db	     (1)
;	IDNumber		dw       (0)
;ends        



;****************************************************************
;
;****************************************************************
NOME_DATA_MAIN_SEG SEGMENT PARA PUBLIC 'DATA'    
          
    
    
    
    ; da sostituire con struttura!!!!
    Contacts_Name           db	    4 dup     ('               $')
	Contacts_IDNumber		dw      4 dup     (0)
    
    wRecordSize              dw     18
 
    strMain       DB     16 DUP ('$')
;    Contacts	  Contact 

    wReturnAddr   dw (0) 
     
    bValDummy     DB  7 DUP  (255) 
    bCount DB 0
    strAst        db 1 DUP ('****************************************$') 
    strCmd        db 1 DUP ('1=Aggiungi 2=Rimuovi 3=Sfoglia 4=Carica 5=Salva 6=Modifica 7=Esci. Scegli e premi INVIO:$')  
    strBye        db 1 DUP ('BYE BYE$')   
    strName       db 16 DUP ('$')
    strInsNome        db 1 DUP ('Inserisci Nome:$') 
    strInsNumero        db 1 DUP ('Inserisci Numero:$') 
    byPos		db	(0)
    byTemp		db  (0)       
    strNoItems  db 1 dup ('No Items$')  
    strItemsDaModificare db 1 dup('Item da modificare: $') 
    strInfoDaModificare db 1 dup('1=Modifica nome 2=Modifica ID: $')  
    strInserireNuovaInfo db 1 dup('Inserire nuovaifo: $')   
NOME_DATA_MAIN_SEG ENDS 
               
               
      
NOME_DATA_PROC_SEG SEGMENT PARA PUBLIC 'DATA'
    bValProc      DB  12
NOME_DATA_PROC_SEG ENDS
                   
               
                   
CR  	            EQU 13          ; carriage return
LF 	                EQU 10          ; line feed
SPAZIO	            EQU 20h			; codifica ASCII del carattere 'spazio'
PRIMO	            EQU 48			; codifica ASCII del carattere "0"
DISPLAY_CHAR        EQU 2		    ; codice funzione per visualizzare carattere (in DL)
DISPLAY_STRING      EQU 9		    ; codice funzione per visualizzare stringa (in DS:DX)
IN_CHAR	            EQU 1			; codice funzione per input carattere (in AL)  
TAB                 EQU 9


;****************************************************************
;
;****************************************************************
NOME_CODE_SEG SEGMENT PARA PUBLIC 'CODE'
    ASSUME SS:NOME_STACK_SEG, CS:NOME_STACK_SEG, DS:NOME_DATA_MAIN_SEG, ES:nothing
   
      
   
;***************************************************************
; INIZIO PROCEDURE                                             
;***************************************************************
      
;***************************************************************      
; procedura InChar per l'acquisizione di un carattere da tastiera
;***************************************************************
InChar		PROC NEAR
		MOV AH, IN_CHAR
		INT 21H  	; AL riceve il carattere da tastiera
		RET
InChar		ENDP


;***************************************************************
; procedura InString per l'acquisizione di una stringa da tastiera
;***************************************************************
InString		PROC NEAR
		        
		PUSHA
    RETRIEVE_CHAR:
		MOV AH, IN_CHAR
		INT 21H  	; AL riceve il carattere da tastiera
		    
		cmp al,0dh
		JE EXIT_INPUT
		     
		mov [si],al
        inc si       
		
		JMP RETRIEVE_CHAR
		
	EXIT_INPUT:
		  
		mov [si], 0 
		
		
		POPA
		
		RET
InString		ENDP    

            
;***************************************************************
;
;***************************************************************        
readCharacter proc near
    mov ah,00
    int 16h
    ret
readCharacter endp   


;***************************************************************
;
;***************************************************************
readString proc near 
        
loop_Read_String:
    call readCharacter      
    mov [si],al
    inc si
    cmp al,0dh
    jne loop_Read_String
    mov [si],'$'
    ret
readString endp


;***************************************************************
; procedura InNum per l'acquisizione di un numero a 16 bit da tastiera
; (il numero è restituito in AX)
; NB: non effettua alcun controllo sull'input,  se i caratteri immessi
;     non corrispondono ad un numero il valore restituito è indefinito  
;***************************************************************
InNum		PROC NEAR
		MOV AX,0	; inizializza AX a 0		
 lopinn:	
        PUSH AX
		CALL InChar	; acquisisce carattere
		MOV BL,AL	; e lo salva in BL
		POP AX
		CMP BL,CR	; se CR termina acquisizione
		JE esclopinn
		MOV DX,10	; AX=AX*10
		MUL DX
		SUB BL,PRIMO	; AX=AX+cifra
		MOV BH,0
		ADD AX,BX
		JMP lopinn
 esclopinn:	
        RET			
InNum		ENDP


;***************************************************************
; procedura PutChar per la visualizzazione di un carattere a video
;***************************************************************
PutChar		PROC NEAR
		MOV AH, DISPLAY_CHAR
		INT 21H  	; DL contiene il carattere da visualizzare
		RET
PutChar		ENDP


;***************************************************************
; procedura PutString per la visualizzazione di una stringa a video
;***************************************************************
PutString	PROC NEAR
		MOV AH, DISPLAY_STRING
		INT 21H  	; DS:DX contengono puntatore a stringa da visualizzare
		RET
PutString	ENDP

   
;***************************************************************
; procedura PutNum per la visualizzazione di un numero intero positivo (a 16 bit, contenuto in AX)
;        NB: effettua una serie di divisioni succesive per 10
;            utilizza lo stack per stampare le cifre in ordine inverso a come sono generate
;***************************************************************
PutNum		PROC NEAR
		MOV DX, 0	; DX-AX contiene il numero a 32 bit
		MOV CX, 0	; contatore delle cifre salvate
		MOV BX, 10
looppn:		
        DIV BX		; divido il numero per 10
		PUSH DX		; salvo il resto nello stack
		INC CX
		CMP AX,0
		JZ looppn2	; se quoziente nullo esco dal ciclo
		MOV DX,0	; il quoziente in DX-AX
		JMP looppn
looppn2:	
        POP DX		; successiva cifra da stampare (contenuta in DL)
		ADD DL, PRIMO	; i numeri vengono stampati dal più significativo al meno
		PUSH CX		; salva ciò che non può essere modificato
		CALL PutChar	; ovvero in ordine inverso a quello di generazione
		POP CX
		loop looppn2
		RET
PutNum		ENDP
             

;***************************************************************
;
;***************************************************************   
NewLine      PROC NEAR
        PUSHA
        MOV DL, LF
        Call PutChar
        
        MOV DL, CR
        Call PutChar
        POPA
        RET  
NewLine     ENDP   


;***************************************************************
;
;***************************************************************   
PutTab     PROC NEAR
        PUSHA
        MOV DL, TAB
        Call PutChar
        POPA
        RET  
PutTab     ENDP
      
      
;***************************************************************
; procedura: GetStringLenght
;***************************************************************
GetStringLenght PROC NEAR

        L2:CMP BYTE PTR[SI],"$"
;        JE L1
        INC SI
        ADD AL,1
        JMP L2
        RET                                     
GetStringLenght ENDP
                     

;***************************************************************
; pushare prima la destinazione poi la sorgente
;***************************************************************
CopyString PROC NEAR

	;cld				;Required if you want to INC SI/DI
	  
	POP wReturnAddr 
	POP SI
	POP DI
	         
	        	
	
	mov BX, 0


  COPY_CHAR:	
	
	MOV AL, [SI]
	MOV [DI], AL   
	
	INC SI
	INC DI
	        
	INC BX
	CMP BX, 16
	JNE COPY_CHAR
	
	               
	PUSH wReturnAddr
	
	RET
CopyString ENDP


;***************************************************************
; Macro per la pulizia (nero) di tutto lo shermo
;***************************************************************
pulisci_Schermo  macro 
       Pusha
       mov cx,0
       mov dx,2476h
       mov bh,7
       mov ax,0600h
       int 10h
       mov ah,15
       int 10h
       mov dx,0
       mov ah,2
       int 10h
       popa
endm
      
      
;***************************************************************
;
;***************************************************************
aspetta macro
;Macro che attende la battitura di un tasto
;prima di ricedere il controllo al programma chiamante
        pusha
        mov ah,07h
        int 21h
        popa
endm



;***************************************************************
; pulisce stringhe di n caratteri
; pushare prima indirizzo stringa poi size
;***************************************************************
EmptyString PROC NEAR

	POP wReturnAddr 
	POP CX
	POP SI
	     
  EMPTY_CHAR:	
	
	MOV [SI], '$'
	INC SI
	
	LOOP EMPTY_CHAR
	
	               
	PUSH wReturnAddr
	
	RET
EmptyString ENDP

      
;***************************************************************
; MAIN
;***************************************************************
    MAIN:

        MOV AX, NOME_DATA_MAIN_SEG
        MOV DS, AX
                   
        
        ;*******************************************************
        ; PREPARAZIONE HEADER
        ;*******************************************************      
             
                    
    
    ;******************************************************
    RESTART:
    ;******************************************************
            
        pulisci_Schermo
        
        LEA SI, strAst
        MOV DX, SI
        Call PutString    
                    
        Call NewLine   
        
        LEA SI, strCmd
        MOV DX, SI
        Call PutString             
                                        
        Call NewLine   
        
        LEA SI, strAst
        MOV DX, SI
        Call PutString 
                    
        Call NewLine   
             
                         
                
                         
        ;*******************************************************
        ; scelta dell'azione
        ;*******************************************************
        Call InNum
        
        cmp AX, 1  
        JE AZIONE_AGGIUNGI 
                
        cmp AX, 2
        JE AZIONE_RIMUOVI
                      
        CMP AX, 3
        JE AZIONE_SFOGLIA
        
        CMP AX, 4
        JE AZIONE_CARICA
        
        CMP AX, 5
        JE AZIONE_SALVA
                 
        CMP AX, 6
        JE AZIONE_MODIFICA
        
        CMP AX, 7
        JE AZIONE_ESCI
        
           
    ;******************************************************  
    AZIONE_MODIFICA:
    ;******************************************************  
    
        Call NewLine
        
        LEA SI, strItemsDaModificare
        MOV DX, SI
        Call PutString 
        
        SUB AX, AX
        Call InNum
        
        ; salvo l'item selezionato
        MOV byTemp, AL
        
        
        Call NewLine
        
                                   
        LEA SI, strInfoDaModificare
        MOV DX, SI
        Call PutString    
        
        SUB AX, AX
        Call InNum    
                  
                  
        
                               
        
        CMP AX, 1
        JE AZIONE_MODIFICA_NOME   
        
        
        CMP AX, 2
        JE AZIONE_MODIFICA_ID
        
        
        
        
        
        AZIONE_MODIFICA_NOME:
                     
                     
            Call NewLine
                   
          
            LEA SI, strInserireNuovaInfo
            MOV DX, SI
            Call PutString 
        
                             
            lea si,  ds:strName         ; mov si, OFFSET ds:strName  
            PUSH SI
            SUB	 AX, AX
            MOV  AL, 10h
            PUSH AX
            call EmptyString
        
                  
            lea si,  ds:strName         ; mov si, OFFSET ds:strName
            PUSH SI
            call InString 
        
            
            SUB  aX, aX
    	    SUB  dx, dx
    	    MOV  aL, byTemp 
    	    MOV  dl, 10h
    	    IMUL dl               ; calcolo address e pusho
    	    SUB  dx, dx
    	    MOV  DX, offset Contacts_Name
    	    ADD  AX, DX
    	    PUSH AX   
    	
    	    SUB  aX, aX
    	    MOV  AX, offset strName   ;riesco ad ottenere un indirizzo a 16 bit
    	    PUSH AX
    	    
    	    call CopyString
    	
        
            JMP RESTART
                  
            
            
            
        AZIONE_MODIFICA_ID: 
                     
            Call NewLine
                   
          
            LEA SI, strInserireNuovaInfo
            MOV DX, SI
            Call PutString 
        
            call InNum 
            SUB  BX, BX
            MOV  BL, byTemp 
            ADD  BX, BX
            MOV  Contacts_IDNumber[BX], AX
        
            JMP RESTART               
        
        
    ;******************************************************    
    AZIONE_AGGIUNGI: 
    ;******************************************************
                         
        Call NewLine   
        
        LEA SI, strInsNome
        MOV DX, SI
        Call PutString 
     
        
        lea si,  ds:strName         ; mov si, OFFSET ds:strName  
        PUSH SI      
        SUB	 AX, AX
        MOV  AL, 10h
        PUSH AX
        call EmptyString
        
                  
        lea si,  ds:strName         ; mov si, OFFSET ds:strName
        PUSH SI
        call InString 
        
        
        Call NewLine   
        
        LEA SI, strInsNumero
        MOV DX, SI
        Call PutString 
         
         
        
        call InNum 
        SUB  BX, BX
        MOV  BL, byPos 
        ADD  BX, BX
        MOV  Contacts_IDNumber[BX], AX

                   
        
        
    	SUB  aX, aX
    	SUB  dx, dx
    	MOV  aL, byPos 
    	MOV  dl, 10h
    	IMUL dl               ; calcolo address e pusho
    	SUB  dx, dx
    	MOV  DX, offset Contacts_Name
    	ADD  AX, DX
    	PUSH AX   
    	
    	SUB  aX, aX
    	MOV  AX, offset strName   ;riesco ad ottenere un indirizzo a 16 bit
    	PUSH AX
    	    
    	call CopyString
    	
    	
    	
    	INC byPos
    	
	
        JMP RESTART
     


	;******************************************************
	AZIONE_RIMUOVI:   
    ;******************************************************
            
        Call NewLine
        
        
        SUB AX, AX
    	Call InNum		; indice base zero
    	
    	; byPos contiene il numero di items
    	
    	
    	; memorizzo la selezione in byTemp
    	MOV	byTemp, AL
    	
    	
    	;****************************************
    	; sovrascrivo gli item con i successivi fino
    	; all'ultimo (byPos), partendo da quello 
    	; selezionato 
    	;****************************************
    	; preparo il numero di cicli che devo fare   
    	SUB CX, CX
    	MOV CL, byPos 
        SUB CX, 1
    	SUB CX, AX
    	;****************************************
    	
    	; SE VIENE SELEZIONATO L'ULTIMO ITEM (ricordarsi che e'
    	; zero-index) A QUESTO PUNTO IN CX DOVREBBE ESSERCI ZERO
    	; QUINDI NON DOVREI ENTRARE NEL LOOP
    	   
    	CMP CX, 0  
    	JE  DELETE_LAST
    	             
    	SUB AX, AX
    	MOV AL, byPos
    	CMP CX, AX
    	JGE NO_ITEMS_TO_DELETE
    	
      SCROLL_ITEM: 
         	              
    	
		;****************************************
		; copio la stringa successiva a byTemp sullo
		; slot byTemp
		;****************************************    	              
    	SUB  aX, aX
        SUB  dx, dx
        MOV  aL, byTemp
        MOV  dl, 10h
        IMUL dl               ; calcolo address e pusho
        SUB  dx, dx
        MOV  DX, offset Contacts_Name
        ADD  AX, DX
        PUSH AX
    	
    	; lo start della sorgente e' 16 bytes prima della destinazione
    	ADD  AX, 10h
        PUSH AX
    	
    	Call CopyString
    	;****************************************
    	      
    	  
    	  
		;****************************************
		; copio il numero su successivo su byTemp
		;****************************************    	    	
    	SUB BX, BX
    	MOV BL, byTemp
    	ADD BL, 1
    	ADD BX, BX
    	 
    	MOV DX, Contacts_IDNumber[BX]
    	
    	
    	SUB BX, BX         
    	MOV BL, byTemp 
    	ADD BX, BX 
    	    	
    	MOV Contacts_IDNumber[bx], DX
    	;****************************************
    	
    	
    	INC byTemp
    	
    	
    	LOOP SCROLL_ITEM	
    	
    
    
    DELETE_LAST:	
    	
    	;****************************************
    	; azzero l'ultimo item
    	;****************************************
    	SUB  aX, aX
        SUB  dx, dx
        MOV  aL, byTemp
        MOV  dl, 10h
        IMUL dl               ; calcolo address e pusho
        PUSH AX
        
        SUB	 AX, AX
        MOV  AL, 10h
        PUSH AX
        call EmptyString
        
        
    	SUB BX, BX
    	MOV BL, byTemp
    	ADD BX, BX
    	
    	SUB AX, AX
    	MOV Contacts_IDNumber[bx], AX
    	;****************************************
    	
    	
    	; decremento contatore items
    	SUB byPos, 1
	
	            
	            
	  NO_ITEMS_TO_DELETE:
        JMP RESTART


	;******************************************************
    AZIONE_SFOGLIA:
    ;******************************************************     
           
        Call NewLine 
        
        CMP byPos, 0
        JE  NO_ITEMS_TO_BROWSE
        
        
          
        SUB CX, CX
        MOV CL, byPos
        SUB CX, 1  
        MOV BX, 0  
        
        
        CMP CX, 0
        JE NO_ITEMS_TO_BROWSE
        
        
     PRINT_ITEMS:       
                 
        MOV  byTemp, BL
        
        
        SUB  aX, aX
        SUB  dx, dx
        MOV  ax, BX
        MOV  dl, 10h
        IMUL dl               ; calcolo address e pusho
        ADD  AX, offset Contacts_Name
        MOV  DX, AX
        Call PutString 
        
        
        call PutTab             
                     
                     

    	ADD BX, BX    	 
    	MOV AX, Contacts_IDNumber[BX]
    	call PutNum
    	
    	Call NewLine
    	       
    	       
    	SUB BX, BX
    	MOV BL, byTemp
    	
        INC BX
           
        CMP BL, byPos
        JNE PRINT_ITEMS   
        
        
        JMP END_BROWSE
        
        
        
     NO_ITEMS_TO_BROWSE:
              
        LEA SI, strNoItems
        MOV DX, SI
        Call PutString 
        

       
     END_BROWSE:
                           
        call readCharacter             ; wait for keypress
        
        
        JMP RESTART


       
    ;******************************************************   
    AZIONE_CARICA:  
    ;******************************************************
                    
                    
        MOV byPos, 0
        
        
        LEA SI, strName
        Call InString 
    	
    	
    	mov dx, offset strName	;Points to filename
    	
    	mov ah, 3dh				;Open file func.
		mov al, 02h				;Write-read file access
		int 21h					;Do it
            
        mov bx, ax				;Move retrieved file handle into BX
        
          
    READ_DATA:
          
        ; lettura 2 bytes ID
        SUB DX, DX
        mov DL, byPos
        ADD DX, DX
        ADD dx, offset Contacts_IDNumber               ;indirizzo in cui viene salvata la lettura
        mov cx, 02h             ; numero di bytes da leggere
       
        mov ax, 3f00h             ; read directive
        int 21h 
        
        cmp CX, AX
        JNE END_READ_DATA    ; tmptmp: testare EOF?
        
          
        ; lettura 16 bytes name 
        SUB AX, AX 
        SUB DX, DX
        mov AL, byPos
        MOV DL, 10h
        IMUL DL
        mov dx, offset Contacts_Name               ;indirizzo in cui viene salvata la lettura
        ADD  DX, AX
        mov cx, 10h             ; numero di bytes da leggere
       
        mov ax, 3f00h             ; read directive
        int 21h
    
        INC byPos
          
        cmp CX, AX
        JE  READ_DATA 
        
           
   
   END_READ_DATA:
           
        mov ax, 3e00h			;Close file function
		int 21h					;Do it
		
		
                          
        JMP RESTART   
        


	;******************************************************
    AZIONE_SALVA:
    ;******************************************************
                  
    	LEA SI, strName
        Call InString 
    	
    	
    	;****************************************
    	; creazione/troncamento file
    	;****************************************
    	mov AX, 3c00h			;Create file func.
		mov cx, 2				;Archive file attribute
		mov dx, offset strName	;Points to filename
		int 21h					;Do it
                      
              
  		mov bx, ax				;Move retrieved file handle into BX
        
        mov ax, 3e00h			;Close file function
		int 21h					;Do it
		
		
		

		mov ah, 3dh				;Open file func.
		mov al, 02h				;Write-Only file access
		int 21h					;Do it


		mov bx, ax				;Move retrieved file handle into BX
		
		MOV	DX, 0
		MOV byTemp, 0
			
	WRITE_ENTRY:
	
		; scrivo l'ID
		SUB DX, DX
    	MOV DL, byTemp
    	ADD DX, DX
    	ADD DX, offset Contacts_IDNumber
    	mov cx,	2h			;Number of bytes to write
    	
    	mov ax, 4000h			;Write to File function
		int 21h					;Do it
    	
	
		; scrivo la stringa
		SUB  aX, aX
        SUB  dx, dx
        MOV  aL, byTemp
        MOV  dl, 10h
        IMUL dl               ; calcolo address e pusho
        SUB  dx, dx
        MOV  DX, offset Contacts_Name
        ADD  DX, AX				;Points to data to write
		mov  cx,	10h			;Number of bytes to write
		
		mov ax, 4000h			;Write to File function
		int 21h					;Do it
	
		
		INC byTemp
		SUB DX, DX
		MOV DL, byTemp
		cmp	DL, byPos
		JNE WRITE_ENTRY
		

		mov ax, 3e00h			;Close file function
		int 21h					;Do it

    	;****************************************

        JMP RESTART


	;******************************************************
    AZIONE_ESCI:  
    ;******************************************************
        Call NewLine   
        
        LEA SI, strBye
        MOV DX, SI
        Call PutString
                               
                          
                             
        ;*******************************************************
        ; uscita dal programma
        ;*******************************************************
        MOV AX, 4C00h
        int 21h      
        
        
NOME_CODE_SEG ENDS  
END MAIN       
    



                         
;***************************************************************
;
; DA PROVARE 
;       lea BX, offset ds:strMain
;       OFFSET    
;       SHL
;
;***************************************************************           