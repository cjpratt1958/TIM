*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMSRCWK.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/14/94 at 16:44:22
*:
*:      Called by: TIM1MENU.PRG                      
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : LOGO               (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF        
*:
*:      Documented 12:29:53                                FoxDoc version 3.00a
*:*****************************************************************************

   DIMENSION chce(22)

   STORE '1 - QUIT          ' TO chce(1)
STORE '2 - W.O. NUMBER   ' TO chce(2)
STORE '3 - DATE          ' TO chce(3)
STORE '4 - JOB NAME      ' TO chce(4)
STORE '5 - JOB STREET    ' TO chce(5)
STORE '6 - JOB CITY      ' TO chce(6)
STORE '7 - BILL NAME     ' TO chce(7)
STORE '8 - BILL STREET   ' TO chce(8)
STORE '9 - BILL CITY     ' TO chce(9)
STORE '10 - HOME PHONE   ' TO chce(10)
STORE '11 - WORK PHONE   ' TO chce(11)
STORE '12 - MATERIAL     ' TO chce(12)
STORE '13 - AMOUNT       ' TO chce(13)
STORE '14 - AMOUNT - TAX ' TO chce(14)
STORE '15 - MORE WORK    ' TO chce(15)
STORE '16 - PREFERRED    ' TO chce(16)
STORE '17 - DISCOUNT RATE' TO chce(17)
STORE '18 - COMMENTS     ' TO chce(18)
STORE '19 - CONTACT      ' TO chce(19)
STORE '20 - JOB DESCRIP  ' TO chce(20)
STORE '21 - XSTREET      ' TO chce(21)
STORE '22 - REFERRAL     ' TO chce(22)

   STORE ' ' TO string
STORE 0 TO chse
STORE 'TIMSRCWK.PRG' TO prg_name
STORE '*****  S E A R C H   W O R K O R D E R S  *****' TO TITLE
DO addtitle
DO LOGO
STORE 40 - (LEN(chce(1))/2) TO C
SET COLOR TO &statusatr
@ 3,C MENU chce,22 TITLE 'Search W.O.'
READ MENU TO chse
SET COLOR TO &screenatr

   DO CASE
CASE  chse = 1
CLOSE DATABASES
RETURN
CASE chse = 2
STORE 'NO' TO string
CASE chse = 3
STORE 'DATE'TO string
CASE chse = 4
STORE 'NAME' TO string
CASE chse = 5
STORE 'STREET' TO string
CASE chse = 6
STORE 'CITY' TO string
CASE chse = 7
STORE 'BNAME' TO string
CASE chse = 8
STORE 'BSTREET' TO string
CASE chse = 9
STORE 'BCITY' TO string
CASE chse = 10
STORE 'PHONE' TO string
CASE chse = 11
STORE 'WPHONE' TO string
CASE chse = 12
STORE 'MATERIAL' TO string
CASE chse = 13
STORE 'AMOUNT' TO string
CASE chse = 14
STORE 'AMOUNT-TAX' TO string
CASE chse = 15
STORE 'WORK' TO string
CASE chse = 16
STORE 'PREFERRED' TO string
CASE chse = 17
STORE 'PREFDISC' TO string
CASE chse = 18
STORE 'COMMENTS' TO string
CASE chse = 19
STORE 'CONTACT' TO string
CASE chse = 20
STORE 'JOBDESC' TO string
CASE chse = 21
STORE 'XSTREET' TO string
CASE chse = 22
STORE 'REF' TO string
   
   
   
   
   
   ENDCASE
@ 23,0 CLEAR
@ 23,1 SAY 'CHOOSE DATA TO SEARCH FOR'
STORE '         ' TO DATA
IF chse = 12.or.chse = 13.or.chse = 17.or.chse = 16.or.chse = 14
   STORE 0.00 TO num
   @ 24,1 SAY 'BE SURE IT IS A NUMBER' GET num PICTURE '999999.99'
   READ
   STORE 'Y' TO DATA
   STORE string + '=' + STR(num,9,2) TO string
ELSE
   @ 23,0 CLEAR
   @ 23,1 SAY 'Enter Data to Search for ' GET DATA PICTURE '!!!!!!!!!'
   READ
   STORE "'" + TRIM(DATA) + "'$" + string TO string
ENDIF
@ 23,0 CLEAR
STORE 'N' TO printch
@ 23,5 SAY 'Print Results?  (Y/N)' GET printch PICTURE 'A'
READ

   IF DATA = ' '
SET COLOR TO &promptatr
@ 22,0 CLEAR
@ 22,5 SAY 'NO COMPARISON DATA ENTERED'
@ 23,5 SAY 'PRESS RETURN TO CONTINUE'
WAIT
@ 23,0 CLEAR
@ 23,0 SAY '*****  Please Wait  *****'
SET COLOR TO &screenatr
CLOSE DATABASES
RETURN
ENDIF
@ 1,0 CLEAR
IF printch = 'Y'
   SET PRINT ON
ENDIF



IF USED("timwork")
	SELECT timwork
	SET ORDER TO TAG "no"
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER TAG "no"
ENDIF


GOTO TOP
@ 1,0 CLEAR
@ 2,3 SAY 'W.O.#    INCIDENT  NAME                        PHONE                  AMOUNT'
SET COLOR TO &promptatr
@ 23,0 CLEAR
@ 23,5 SAY '*****  Searching  *****'
LOCATE ALL FOR &string
DO WHILE .NOT. EOF()
   STORE 3 TO C
   DO WHILE C < 22
      SET COLOR TO &hiliteatr
      IF EOF()
         @ C,0 CLEAR
         EXIT
      ENDIF
      @ 0,9 SAY RECNO()
      @ C,3 SAY no
      @ C,12 SAY incident
      @ C,22 SAY name
      @ C,46 SAY area
      @ C,52 SAY phone
      @ C,70 SAY amount
      STORE C+1 TO C
      SET COLOR TO &promptatr
      @ 23,0 CLEAR
      @ 23,5 SAY '*****  Searching  *****'
      SET COLOR TO &hiliteatr
      CONTINUE
   ENDDO C
   STORE 'N' TO ky
   @ 23,0 CLEAR
   SET COLOR TO &promptatr
   @ 23,1 SAY 'Press [E] to Exit   or '
   WAIT TO ky
   SET COLOR TO &hiliteatr
   IF ky = 'E'
      @ 23,0 CLEAR
      SET COLOR TO &promptatr
      @ 23,5 SAY '***** Please Wait *****'
      SET COLOR TO &screenatr
      CLOSE DATABASES
      RETURN
   ENDIF
ENDDO EOF()

   SET PRINT OFF
@ 23,0 CLEAR
SET COLOR TO &promptatr
@ 23,5 SAY '*****  End of Search  *****'
WAIT
@ 22,0 CLEAR
@ 23,5 SAY '***** Please Wait *****'
SET COLOR TO &screenatr
CLOSE DATABASES
RETURN

   
   
   
   
   
   
   
   
   
   
   *: EOF: TIMSRCWK.PRG
*: EOF: TIMSRCWK.PRG
