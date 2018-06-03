SET DELETED ON
STORE ' ' TO CURRENT
STORE '       ' TO m.invoice
STORE DBF()TO CURRENT
USE TIMTRACK 
BROWSE 
DO WHILE INVOICE = '*AVAIL*'
	@ 23,1 CLEAR
	@ 23,5 say 'Please enter a workorder to assign to ' + Technician get m.invoice
	read
	STORE Technician to m.technician
	if m.invoice = '       '
		exit
	endif
	USE timwork
	SET INDEX TO w_num,w_phone,w_bphone,w_wphone,w_incident,w_tech,w_ref
	seek m.invoice
	if eof()
		? CHR(7)
		wait 'THIS WORKORDER DOES NOT EXIST  ** press any key to condinue'
		exit
	else
		store name to m.name
		store city to m.city
	endif
	use timtrack
	SET ORDER TO 1
	seek m.technician
	REPLACE DATE WITH DATE()
	REPLACE technician WITH m.technician
	REPLACE invoice WITH m.invoice
	REPLACE name WITH m.name
	REPLACE city WITH m.city
	REPLACE timein WITH TIME()
	REPLACE dispatched WITH date()
	REPLACE disp_by with OPERATOR
	REPLACE timeout with ' '
	REPLACE closed with CTOD('  /  /    ')
	REPLACE closed_by with ' '
		
ENDDO
IF CURRENT > ' '
   USE CURRENT
ENDIF   
RETURN