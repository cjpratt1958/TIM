*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMSRCHD.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/14/94 at 17:13:02
*:
*:      Called by: TIM1MENU.PRG                      
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : LOGO               (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMSTOCK.DBF       
*:
*:      Documented 12:30:05                                FoxDoc version 3.00a
*:*****************************************************************************
IF USED("TimStock")
	SELECT TimStock
	SET ORDER TO descrip
ELSE
	SELECT 3
	USE (LOCFILE("TimStock.dbf","DBF","Where is TimStock?"));
		AGAIN ALIAS TimStock ;
		ORDER TAG descrip
ENDIF


   STORE 'TIMSRCHD.PRG' TO prg_name
STORE '*****  S E A R C H   P A R T S  *****' TO TITLE
DO addtitle
DO LOGO
DIMENSION choce(10)

   STORE 'X - QUIT         ' TO choce(1)
STORE 'A - PART NUMBER  ' TO choce(2)
STORE 'B - DESCRIPTION  ' TO choce(3)
STORE 'C - PRICE        ' TO choce(4)
STORE 'D - COST         ' TO choce(5)
STORE 'E - SUPPLIER     ' TO choce(6)
STORE 'F - ONHAND       ' TO choce(7)
STORE 'G - ORDER        ' TO choce(8)
STORE 'H - SOLD         ' TO choce(9)
STORE 'I - NAED NUMBER  ' TO choce(10)
STORE 38 - LEN(choce(1))/2 TO y

   STORE ' ' TO string
STORE 0 TO chse

   SET COLOR TO &statusatr
@ 3,y MENU choce,10 TITLE ' Search Stock '
READ MENU TO chse
SET COLOR TO &screenatr

   DO CASE
CASE chse = 2
STORE 'PART_NO' TO string
CASE chse = 3
STORE 'DESCRIP'TO string
CASE chse = 4
STORE 'PRICE' TO string
CASE chse = 5
STORE 'COST' TO string
CASE chse = 6
STORE 'SUPPLIER' TO string
CASE chse = 7
STORE 'ONHAND' TO string
CASE chse = 8
STORE 'ORDER' TO string
CASE chse = 9
STORE 'SOLD' TO string
CASE chse = 10
STORE 'NAED_NO' TO string
CASE  chse = 1
CLOSE DATABASES
RETURN
ENDCASE
@ 23,0 CLEAR
@ 23,1 SAY 'CHOOSE DATA TO SEARCH FOR'
STORE '         ' TO DATA
IF chse = 4.or.chse = 5.or.chse = 7.or.chse = 8.or.chse = 9
   STORE '       ' TO num
   @ 24,1 SAY 'BE SURE IT IS A NUMBER' GET num PICTURE '999999999'
   READ
   STORE TRIM(num) TO DATA
   STORE string + '=' + DATA TO string
ELSE
   @ 23,0 CLEAR
   @ 23,1 SAY 'ENTER DATA TO SEARCH FOR ' GET DATA PICTURE 'XXXXXXXXX'
   READ
   STORE "'" + TRIM(DATA) + "'$" + string TO string
ENDIF

   @ 23,0 CLEAR
STORE 'N' TO printch
@ 23,5 SAY 'PRINT RESULTS?  (Y/N)' GET printch PICTURE 'A'
READ

   IF DATA = ' '
SET COLOR TO &promptatr
@ 23,0 CLEAR
@ 23,5 SAY 'NO COMPARISON DATA ENTERED'
WAIT
@ 23,0 CLEAR
@ 23,5 SAY '*****  Please Wait  *****'
SET COLOR TO &screenatr
CLOSE DATABASES
RETURN
ENDIF
@ 2,0 CLEAR
IF printch = 'Y'
   SET PRINT ON
ENDIF

IF USED("TimStock")
	SELECT TimStock
	SET ORDER TO descrip
ELSE
	SELECT 3
	USE (LOCFILE("TimStock.dbf","DBF","Where is TimStock?"));
		AGAIN ALIAS TimStock ;
		ORDER TAG descrip
ENDIF

   GOTO TOP
@ 1,0 CLEAR
@ 2,0 SAY 'PART NO.        NAED NO.   DESCRIPTION                             COL 3'
SET COLOR TO &promptatr
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
      @ C,0 SAY part_no
      @ C,16 SAY naed_no
      @ C,27 SAY descrip
      @ C,63 SAY col3 PICTURE '999999.99'
      STORE C+1 TO C
      SET COLOR TO &promptatr
      @ 23,0 CLEAR
      @ 23,5 SAY '*****  Searching*****'
      SET COLOR TO &hiliteatr
      CONTINUE
   ENDDO C
   STORE 'N' TO ky
   @ 23,0 CLEAR
   SET COLOR TO &promptatr
   @ 23,1 SAY 'Press [E] to Exit or '
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
@ 23,0 CLEAR
@ 24,5 SAY '*****  Please Wait  *****'
SET COLOR TO &screenatr
CLOSE DATABASES
RETURN

   
   
   
   
   
   
   
   
   
   
   *: EOF: TIMSRCHD.PRG
*: EOF: TIMSRCHD.PRG
