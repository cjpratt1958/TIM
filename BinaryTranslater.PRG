PUBLIC binarynumber as integer
public x1,x2,x3,x4,x5,x6,x7,x8 as integer
CLEAR
STORE '        ' TO binarynumber
STORE 1 TO x
DO WHILE x < 5
@ 5,10 say 'Enter 8 digit Binary Number: ' get binarynumber
read
	IF LEN(binarynumber) <> 8 
		@ 7,10 say '8 bit only please'
		loop
	ELSE		
		STORE VAL(SUBSTR(binarynumber,1,1)) TO x1
		STORE VAL(SUBSTR(binarynumber,2,1)) TO x2
		STORE VAL(SUBSTR(binarynumber,3,1)) TO x3
		STORE VAL(SUBSTR(binarynumber,4,1)) TO x4
		STORE VAL(SUBSTR(binarynumber,5,1)) TO x5
		STORE VAL(SUBSTR(binarynumber,6,1)) TO x6
		STORE VAL(SUBSTR(binarynumber,7,1)) TO x7
		STORE VAL(SUBSTR(binarynumber,8,1)) TO x8
		@ 7,10 say INT((x1*1)+(x2*2)+(x3*4)+(x4*8)+(x5*16)+(x6*32)+(x7*64)+(x8*128))
	ENDIF
	STORE x+1 TO x
	loop
ENDDO
