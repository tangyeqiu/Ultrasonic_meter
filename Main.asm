	errorlevel -302
	#include<p16f877a.inc>
	__CONFIG _DEBUG_OFF&_CP_ALL&_WRT_HALF&_CPD_ON&_LVP_OFF&_BODEN_OFF&_PWRTE_ON&_WDT_OFF&_HS_OSC
;************************************

Z       EQU 2    ;??0???????
RP0     EQU 5    ;?????RP0????
;*********************
RS EQU 2         ;LCD???????????RA.1?
RW EQU 3         ;LCD?/???????RA.2?
E  EQU 5         ;LCD????????RA.3?
COUNT EQU 24H    ;?????????
TMP1  EQU 25H    ;?????????
       udata 0x30
temp res 1
 
 ;Delay Variable
n11 res 1
n12 res 1
n21 res 1
n22 res 1
n31 res 1
n32 res 1
n33 res 1
n41 res 1
n42 res 1
n43 res 1
 
 ;Multiply 
ValueAH res 1
ValueAL res 1
ValueBH	res 1
ValueBL	res 1
ValueCH	res 1
ValueCL	res 1
ValueDH res 1
ValueDL res 1
temp_shift res 1
Flags res 1
 
ScaleH res 1
ScaleL res 1
 
;CodeSystmConvertion
BCD0 res 1
BCD1 res 1
BCD2 res 1
BCD3 res 1
BCD4 res 1
Bin0 res 1
Bin1 res 1
Bin2 res 1
Bin3 res 1
BCD_Value res 1
ASCII_H res 1
ASCII_L res 1
 
temp_shift2 res 1
temp_BCD res 1
temp_ASCII res 1

;Range
UpLimit2 res 1
UpLimit1 res 1
UpLimit0 res 1
LoLimit2 res 1
LoLimit1 res 1
LoLimit0 res 1
 
ULimit2 res 1
ULimit1 res 1
ULimit0 res 1
LLimit2 res 1
LLimit1 res 1
LLimit0 res 1
;Scene Protection 
w_temp res 1
status_temp res 1
pclath_temp res 1
 
;testnumber
testnumberH res 1
testnumberL res 1
;resultadjust
BCD21 res 1
BCD22 res 1
 ;******************************************************************************
    org	    00H
    goto    INIT
   org	    04H
    goto    ISR

;******************************   ROUTINE   ************************************
;*******************************************************************************
INIT
Variable_INIT	
 ;multiplication variable
	movlw 0
	movwf ScaleH
	movlw 55H
	movwf ScaleL
	;movlw 22	;759
	;movwf testnumberH
	;movlw 0EDH
	;movwf testnumberL
	movlw 0x1F	;680
	movwf testnumberH
	movlw 0x40
	movwf testnumberL
;range variable
	movlw 0
	movwf LoLimit2
	movlw 73H
	movwf LoLimit1
	movlw 5H
	movwf LoLimit0
	movlw 0
	movwf UpLimit2
	movlw 81H
	movwf UpLimit1
	movlw 0
	movwf UpLimit0
	
    CALL LED_INIT
    CALL LCD_INIT
    CALL Timer_INIT
INT_Init
	bsf STATUS, RP0
	movlw 31H
	movwf TRISB
	bsf INTCON, 7	;GIE
	bcf INTCON, 4	;INTE
	bsf OPTION_REG, 6 ;INTEDG
	bcf INTCON, 0	;RBIF
	bsf INTCON, 3	;RBIE
	bcf STATUS, RP0
	
MAIN
    	clrf TMR1H
	clrf TMR1L
	bcf STATUS, RP0
	bcf STATUS, RP1
	bsf T1CON, 0
	bsf PORTC, 7
Wait	btfss PORTB, 0
	goto Wait 
	bcf T1CON, 0
	call loadAB
	call multiply
	call LoadBin
	call BtoBCD
	call Result_Adjust
	call BCD_Display
	call Range_Display
	call Compare
	bcf PORTC, 7
	call DELAY_80ms
	goto MAIN
    
;*******************************	ISR	********************************
;*******************************************************************************
ISR	
	movwf w_temp
	swapf STATUS, w
	clrf STATUS
	movwf status_temp
	movf PCLATH, w
	movwf pclath_temp
	clrf PCLATH
	;****************  start   *****************
	;btfsc PORTB, 4
	;goto PIN5
	call DELAY_20ms
	btfss PORTB, 5
	goto decrease
PIN5	;btfsc PORTB, 5
	;goto retrieve
	call DELAY_20ms
	btfss PORTB, 4
	goto increase
	goto retrieve
	
decrease
	movf LoLimit1
	btfsc STATUS, Z
	call adjust0
	decf LoLimit1, f
	btfss LoLimit1, 3
	goto $+9
	btfss LoLimit1, 2
	goto $+7
	decf LoLimit1, f
	decf LoLimit1, f
	decf LoLimit1, f
	decf LoLimit1, f
	decf LoLimit1, f
	decf LoLimit1, f
	
	movf UpLimit1
	btfsc STATUS, Z
	call adjust1
	decf UpLimit1, f
	btfss UpLimit1, 3
	goto $+9
	btfss UpLimit1, 2 
	goto $+7
	decf UpLimit1, f
	decf UpLimit1, f
	decf UpLimit1, f
	decf UpLimit1, f
	decf UpLimit1, f
	decf UpLimit1, f
	goto retrieve

	
adjust0
	decf LoLimit2, f
	movlw 9AH
	movwf LoLimit1
	return
adjust1
	decf UpLimit2, f
	movlw 9AH
	movwf UpLimit1
	return
	
increase
	incf LoLimit1, f
	btfss LoLimit1, 3
	goto $+5
	btfss LoLimit1, 1
	goto $+3
	movlw 6H
	addwf LoLimit1,f
	
	btfss LoLimit1, 7
	goto $+5
	btfss LoLimit1, 5
	goto $+3
	clrf LoLimit1
	incf LoLimit2
	
	incf UpLimit1, f
	btfss UpLimit1, 3
	goto $+5
	btfss UpLimit1, 1
	goto $+3
	movlw 6H
	addwf UpLimit1, f
	
	btfss UpLimit1, 7
	goto $+5
	btfss UpLimit1, 5
	goto $+3
	clrf UpLimit1
	incf UpLimit2
	
	;****************   end   ******************
retrieve
	bcf INTCON, 0
	movf pclath_temp, w
	movwf PCLATH
	swapf status_temp, w
	movwf STATUS
	movf w_temp
	retfie
;***********************  Timer_Init  ******************************************
;*******************************************************************************
Timer_INIT
    bcf STATUS, RP0
    bcf STATUS, RP1
    bcf T1CON, 0    ;disable Timer1
    clrf TMR1H
    clrf TMR1L
    bcf T1CON, 1    ;using internal CLK
    movlw b'11001111';prescaler
    andwf T1CON, f
    return
 ;******************************* LCD *******************************************
;*******************************************************************************
TABLE                  
           ADDWF PCL,1   
           RETLW 'R'
           RETLW 'a'
           RETLW 'n'
           RETLW 'g'
           RETLW 'e' 
           RETLW '<'
           RETLW ' '
           RETLW ' '
           RETLW ' '
           RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW '>'
           RETLW 00H
;-------------------------------------------------------------------------------
TABLE1                   
	   ADDWF PCL,1
	   RETLW 'D' 
	   RETLW 'I'                           
	   RETLW 'S' 	
	   RETLW 'T' 	  
	   RETLW 'A' 	   
	   RETLW 'N' 	
	   RETLW 'C' 	
	   RETLW 'E'	 
	   RETLW ':'
	   RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW ' '
	   RETLW 'm'
	   RETLW 'm'
	   RETLW 00H

LCD_INIT
  BSF STATUS,RP0          ;??1
  MOVLW 07H
  MOVWF ADCON1            ;??RA????????IO?
  MOVLW 00H 
  MOVWF TRISA             ;??A????
  MOVWF TRISD             ;??D????
  BCF STATUS,RP0          ;??0
  CALL DELAY              ;????,???LCD??????PIC?
  MOVLW 01H
  MOVWF PORTD             ;????
  CALL ENABLE             ;???????
  MOVLW 38H
  MOVWF PORTD             ;8?2?5x7??
  CALL ENABLE
  MOVLW 0EH               ;????????????
  MOVWF PORTD
  CALL ENABLE
  MOVLW 06H               ;???????????
  MOVWF PORTD             
  CALL ENABLE
  MOVLW 80H
  MOVWF PORTD             ;??????
  CALL ENABLE
  CALL WRITE1             ;???????????WWW.PIC16.COM"
  MOVLW 0C0H
  MOVWF PORTD            ;??????       
  CALL ENABLE
  CALL WRITE2            ;???????????TEL0755-83643980" 
  RETURN 
;****************** ????????***************
WRITE1
  CLRF COUNT             ;?0??????
WRITE_A
  MOVFW COUNT
  CALL TABLE             ;?????www.pic16.com
  MOVWF TMP1             ;????
  
  INCF COUNT,1           ;?????1
  MOVFW TMP1             
  XORLW 00H              ;????????????????????00H???????
  BTFSC STATUS,Z
  GOTO  $+4              ;??????
  MOVFW TMP1
  CALL SEND_DATA             
  GOTO WRITE_A           ;??????
  RETURN                 ;?????

;*******************????????**************
WRITE2                 
  CLRF COUNT             ;?0??????
WRITE2_A
  MOVFW COUNT
  CALL TABLE1           ;?????0755-83643980
  MOVWF TMP1
  CALL SEND_DATA            ;??????
  INCF COUNT,1          ;?????1
  MOVFW TMP1
  XORLW 00H             ;???????????
  BTFSS STATUS,Z
  GOTO WRITE2_A         ;????????
  RETURN                ;??????

;*******************Write Comand***************
SEND_DATA                
  MOVWF PORTD           
  BSF PORTA,RS
  BCF PORTA,RW
  BCF PORTA,E
  CALL DELAY_20ms
  BSF PORTA,E
  RETURN

CLEAR
  MOVLW 01H
  MOVWF PORTD
  CALL ENABLE
  RETURN
  
Locate1
  movlw 80H
  movwf PORTD     
  call ENABLE
  return
Locate2
  MOVLW 0C0H
  MOVWF PORTD            ;locate at the second row   
  CALL ENABLE
  return
;*******************Instruction Command****************
ENABLE
  BCF PORTA,RS
  BCF PORTA,RW
  BCF PORTA,E
  CALL DELAY
  BSF PORTA,E
  RETURN

;***************************      DELAY       **********************************
;*******************************************************************************
DELAY                 
 MOVLW   0FFH        
 MOVWF   n11         
LP0
 MOVLW   0FFH        
 MOVWF   n12       
LP1
 DECFSZ  n12,1     
 GOTO     LP1         
 DECFSZ   n11,1       
 GOTO     LP0    
 RETURN               
 
 
DELAY_20ms     
  MOVLW   006H  ;OUTER   
  MOVWF   n22      
  MOVLW   0CCH  ;INNER 
  MOVWF   n21      
  INCFSZ  n21
  GOTO	$-1
  INCFSZ  n22
  GOTO	$-5    
  RETURN  
  
DELAY_80ms
  movlw	0x06	;
  movwf n33
  movlw	0xFA	;
  movwf n32
  movlw	0xDE	;
  movwf n31
  incfsz	n31
  goto	$-1
  incfsz	n32
  goto	$-5
  incfsz	n33
  goto	$-9
  return

DELAY_1s
  MOVLW	 38H	;LP1
  MOVWF n43
  MOVLW	 34H	;LP2
  MOVWF n42
  MOVLW	 0F1H	;LP3
  MOVWF	 n41
  INCFSZ n41
  GOTO	$-1
  INCFSZ n42
  GOTO	$-5
  INCFSZ n43
  GOTO	$-9
  RETURN
 ;************************** LED **********************************************
 ;*****************************************************************************
LED_INIT
  BSF     STATUS,RP0
  MOVLW   0FH
  MOVWF   TRISC                  
  BCF     STATUS,RP0
  BSF PORTC,4
  BSF PORTC,5
  BSF PORTC,6
  CALL DELAY_80ms
  BCF PORTC,4
  CALL DELAY_80ms
  CALL DELAY_80ms
  BCF PORTC,5
  CALL DELAY_80ms
  CALL DELAY_80ms
  BCF PORTC,6
  CALL DELAY_20ms
  CALL DELAY_1s
  BSF PORTC,6
  CALL DELAY_20ms
  CALL DELAY_20ms
  BSF PORTC,5
  CALL DELAY_80ms
  CALL DELAY_80ms
  BSF PORTC,4
  CALL DELAY_1s
  return
  
  
;*************************** Multiply  *****************************************
;*******************************************************************************
  ;*******************************************************************************
; Load time value to ValueA and Scaling factor values to ValueB
loadAB
	movf	TMR1H, w
	;movf testnumberH, w
	movwf	ValueAH
	
	movf	TMR1L, w
	;movf testnumberL, w
	movwf	ValueAL
	
	movf	ScaleH, w
	movwf	ValueBH
	movf	ScaleL, w
	movwf	ValueBL
	retlw	0

;*******************************************************************************
;32-bit outcome with higher significant value in ValueBH:ValueBL and lower 
;significant value in ValueCH:ValueCL
multiply
	call	setup
mloop	rrf	ValueDH, f
	rrf	ValueDL, f
	btfsc	STATUS, C
	call	add
	rrf	ValueBH, f
	rrf	ValueBL, f
	rrf	ValueCH, f
	rrf	ValueCL, f
	decfsz	temp_shift, f
	goto	mloop
	retlw	0

;-----------------------------------------
setup	movlw	.16
	movwf	temp_shift
	movf	ValueBH, w
	movwf	ValueDH
	movf	ValueBL, w
	movwf	ValueDL
	clrf	ValueBH
	clrf	ValueBL
	retlw	0
	
;-----------------------------------
;addtion value ValueB + ValueA ->ValueB
add	bcf	Flags, 0
	movf	ValueAL, w
	addwf	ValueBL, f
	btfss	STATUS, C
	goto	add1
	incf	ValueBH
	btfsc	STATUS, Z
	bsf	Flags, 0
add1	
	movf	ValueAH, w
	addwf	ValueBH, f
	btfsc	Flags, 0
	bsf	STATUS, C
	retlw	0
;************************************************
	return
;****************************  CodeSystemConvertion   **************************
;*******************************************************************************
BtoBCD
    bcf STATUS, C
    movlw .32
    movwf temp_shift2
    clrf BCD4
    clrf BCD3
    clrf BCD2
    clrf BCD1
    clrf BCD0
loop32
    rlf Bin0, f
    rlf Bin1, f
    rlf Bin2, f
    rlf Bin3, f
    rlf BCD0, f
    rlf BCD1, f
    rlf BCD2, f
    rlf BCD3, f
    rlf BCD4, f
    decfsz temp_shift2, f
    goto adjDEC
    retlw 0
    
    
adjDEC
    movlw BCD0
    movwf FSR
    call adjBCD
    movlw BCD1
    movwf FSR
    call adjBCD
    movlw BCD2
    movwf FSR
    call adjBCD
    movlw BCD3
    movwf FSR
    call adjBCD
    movlw BCD4
    movwf FSR
    call adjBCD
    goto loop32
    
    
adjBCD
    movlw 0x03
    addwf INDF, w 
    movwf temp_BCD
    btfsc temp_BCD, 3
    movwf INDF
    movlw 0x30
    addwf INDF, w
    movwf temp_BCD
    btfsc temp_BCD,7
    movwf INDF
    retlw 0
    
ASCII
    clrf ASCII_H
    clrf ASCII_L
    movfw BCD_Value
    andlw 0x0f
    iorlw 0x30
    movwf ASCII_L
    movfw BCD_Value
    andlw 0xf0
    movwf temp_ASCII
    swapf temp_ASCII, w
    iorlw 0x30
    movwf ASCII_H
    return
;**************************** BCD Display  *************************************
;*******************************************************************************
LoadBin
	movfw ValueBH
	movwf Bin3
	movfw ValueBL
	movwf Bin2
	movfw ValueCH
	movwf Bin1
	movfw ValueCL
	movwf Bin0
	return
BCD_Display
	;movfw BCD4
	;movwf BCD_Value
	;call ASCII
	;movfw ASCII_H
	;call SEND_DATA
	;movfw ASCII_L
	;call SEND_DATA
	MOVLW 0C9H
	MOVWF PORTD            ;locate at the second row   
	CALL ENABLE
	
	movfw BCD3
	movwf BCD_Value
	call ASCII
	;movfw ASCII_H
	;call SEND_DATA
	movfw ASCII_L
	call SEND_DATA	
	
	movfw BCD2
	movwf BCD_Value
	call ASCII
	movfw ASCII_H
	call SEND_DATA
	movfw ASCII_L
	call SEND_DATA
	
	movfw BCD1
	movwf BCD_Value
	call ASCII
	movfw ASCII_H
	call SEND_DATA
	

	;movlw '.'
	;call SEND_DATA
	;movfw ASCII_L
	;call SEND_DATA
	
	;movfw BCD0
	;movwf BCD_Value
	;call ASCII
	;movfw ASCII_H
	;call SEND_DATA
	;movfw ASCII_L
	;call SEND_DATA
	
	movlw 'm'
	call SEND_DATA
	movlw 'm'
	call SEND_DATA
	movlw ' '
	call SEND_DATA
	movlw 80H
	movwf PORTD
	call ENABLE
	return
	
;************************ The range display ************************************
;*******************************************************************************
Range_Display	movlw 86H
	movwf PORTD     
	call ENABLE
	movfw LoLimit2
	addlw 30H
	call SEND_DATA
	swapf LoLimit1, w
	andlw 0FH
	addlw 30H
	call SEND_DATA
	movfw LoLimit1
	andlw 0FH
	addlw 30H
	call SEND_DATA
	movfw LoLimit0
	addlw 30H
	call SEND_DATA
	movlw '-'
	call SEND_DATA
	movfw UpLimit2
	addlw 30H
	call SEND_DATA
	swapf UpLimit1, w
	andlw 0FH
	addlw 30H
	call SEND_DATA
	movfw UpLimit1
	andlw 0FH
	addlw 30H
	call SEND_DATA
	movfw UpLimit0
	addlw 30H
	call SEND_DATA
	return	
	
;************************* Complare  *******************************************
;*******************************************************************************
adjustC
	movlw 0xA0
	movwf LLimit1
	decf LLimit2
	return
Compare
	movfw LoLimit2
	movwf LLimit2
	movfw LoLimit1
	btfsc STATUS, Z
	call adjustC
	movlw 0x10
	subwf LoLimit1, w
	movwf LLimit1	
	movfw LoLimit0
	movwf LLimit0
	
	movfw UpLimit2
	movwf ULimit2
	movfw UpLimit1
	btfss UpLimit1, 7
	goto $+5
	btfss UpLimit1, 4
	goto $+3
	incf ULimit2
	movlw 0x0F1
	addlw 0x10
	movwf ULimit1
	movfw UpLimit0
	movwf ULimit0
	
	swapf BCD1, w
	andlw 0x0f
	movwf BCD1
	
	movfw LoLimit2
	subwf BCD3, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C	    ;
	goto  Compare2	    ;C=0
	goto UpSide
	
	movfw LoLimit1
	subwf BCD2, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C
	goto  Compare2
	goto UpSide
	
	movfw LoLimit0
	subwf BCD1, w
	btfss STATUS, C
	goto  Compare2

UpSide
	movfw BCD3
	subwf UpLimit2, w
	btfsc STATUS, Z
	goto  $+4
	btfss STATUS, C
	goto  Compare2	    ;C=0
	goto GREEN
	
	movfw BCD2
	subwf UpLimit1, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C
	goto  Compare2
	goto GREEN
	
	movfw BCD1
	subwf UpLimit0, w
	btfss STATUS, C
	goto  Compare2
	goto  GREEN
	
Compare2
	movfw LLimit2
	subwf BCD3, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C	    ;
	goto  RED
	goto USide
	
	movfw LLimit1
	subwf BCD2, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C
	goto  RED
	goto USide
	
	movfw LLimit0
	subwf BCD1, w
	btfss STATUS, C
	goto  RED

USide
	movfw BCD3
	subwf ULimit2, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C
	goto  RED	    ;C=0
	goto YELLOW
	
	movfw BCD2
	subwf ULimit1, w
	btfsc STATUS, Z
	goto $+4
	btfss STATUS, C
	goto  RED
	goto YELLOW
	
	movfw BCD1
	subwf ULimit0, w
	btfss STATUS, C
	goto  RED
	goto  YELLOW
	

GREEN	movlw 70H
	iorwf PORTC, f
	bcf PORTC, 6
	return
YELLOW	movlw 70H
	iorwf PORTC, f
	bcf PORTC, 5
	return	
RED	movlw 70H
	iorwf PORTC, f
	bcf PORTC, 4 
	return
;*******************************************************************************
;*******************************************************************************
Result_Adjust
	movfw BCD2
	andlw 0x0f
	movwf BCD21
	swapf BCD2, w
	andlw 0x0f
	movwf BCD22
	movfw BCD21
	btfsc STATUS, Z
	call resultadjust1
	decf BCD21
	swapf BCD22, w
	iorwf BCD21, w
	movwf BCD2
	return
resultadjust1
	movfw BCD22
	btfsc STATUS, Z
	call resultadjust2
	decf BCD22
	movlw 0x0A
	movwf BCD21
	return
	
resultadjust2
	decf BCD3
	movlw 0x0A
	movwf BCD22
	return
	
	
	
	
	
	
	
	
	
 END                  



