*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMAPCHK.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/15/94 at 21:37:04
*:
*:      Called by: TIMPAYRO.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : CHKRMBRS           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF
*:               : TIMPAYRO.DBF
*:
*:        Indexes: TPR_TEC.IDX
*:               : TPR_WO.IDX
*:
*:      Documented 12:31:00                                FoxDoc version 3.00a
*:*****************************************************************************
SET ECHO OFF
SET DELIMITERS ON
SET DELIMITERS TO '[]'
SET TALK OFF
STORE 'TIMAPCHK.PRG' TO prg_name
STORE '***** A P P R O V E   W O R K O R D E R *****' TO TITLE
STORE 'N' TO cl
STORE 0 TO dt
STORE 0 TO wge
STORE 2 TO C
STORE 'N' TO yn
STORE 'Y' TO ok
STORE DATE() TO ppe
STORE 0 TO tw
STORE DTOC(DATE()) TO mdate_paid
@ 5,5 SAY 'Please Enter Payroll date for this period' GET mdate_paid PICTURE '  /  /    '
READ
CLEAR
SET COLOR TO &windowatr
@ 0,1 TO 19,80 DOUBLE
SET COLOR TO W/N
@ 1,5 SAY 'W O #    HOURS   TOTAL-TAX    WAGE%       WAGES PD    R/F?     PAID?   '
@ 18,30 SAY 'Total'
SET COLOR TO &screenatr
DO addtitle
@ 24,0 CLEAR

*	SELECT 1
IF USED("timwork")
	SELECT timwork
	SET ORDER TO no
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER no
ENDIF

*	SELECT 2
IF USED("timpayro")
	SELECT timpayro
	SET ORDER TO 1
ELSE
	SELECT 2
	USE (LOCFILE("timpayro.dbf","DBF","Where is timpayro?"));
		AGAIN ALIAS timpayro ;
		ORDER 1
ENDIF





DO WHILE .T.
	SELECT timwork
	SET ORDER TO no
	STORE '       ' TO m.no
	SET COLOR TO &hiliteatr
	IF C = 17
		STORE 2 TO C
		CLEAR
		SET COLOR TO &windowatr
		@ 0,1 TO 19,80 DOUBLE
		SET COLOR TO W/N
		@ 1,5 SAY 'W O #    HOURS   TOTAL-TAX    WAGE%       WAGES PD    R/F?     PAID?   '
		@ 18,30 SAY 'Total'
		SET COLOR TO &screenatr
		DO addtitle
	ENDIF
	@ 21,0 CLEAR
	@ 23,5 SAY 'ENTER JOB NUMBER'GET m.no PICTURE '!!!!!!!'
	SET COLOR TO &screenatr
	@ 24,5 SAY 'ENTER [RETURN] TO RETURN TO MAIN MENU'
	READ
	IF m.no = '       '
		SET DELIMITERS OFF
		RETURN
	ENDIF
	seek ALLTRIM(m.no)
	IF EOF()
		@ 22,0 CLEAR
		SET COLOR TO &promptatr
		@ 22,5 SAY '****** THIS WORKORDER DOES NOT EXIST ******'
		SET COLOR TO &screenatr
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		WAIT
		SAVE SCREEN TO SCREEN
	*	DO chkrmbrs
		RESTORE SCREEN FROM SCREEN
		LOOP
	ENDIF
	@ C,9 SAY RECNO()
	STORE (amount-tax)*(wage/100) TO wge
	STORE (amount-tax) TO dt
	STORE amount TO mamount
	IF payroll < DATE
		@ C,5 SAY no
		@ C,12 SAY HOURS PICTURE '999.99'
		@ C,20 SAY amount-tax
		@ C,33 SAY wage
		@ C,45 SAY wageextens PICTURE '99999.99'
		STORE 'NO' TO rf
		IF (mat_cost*4) > (amount-tax)
			STORE 'YES' TO rf
		ENDIF
		@ C,59 SAY rf
		SET COLOR TO &hiliteatr
		@ 22,5 SAY 'CAN THIS JOB BE PAID  (Y/N)????' GET ok PICTURE '!'
		READ
		SET COLOR TO &screenatr
		IF ok = 'Y'
			@ C,63 SAY '****  paid  ****'
			STORE tw + wageextens TO tw
			@ 18,40 SAY tw
			REPLACE cleared WITH 'Y'
			REPLACE payroll WITH CTOD(mdate_paid)

			SELECT 2
			SET ORDER TO 1
			seek m.no
			DO WHILE .NOT. EOF()
				IF wo = m.no
					REPLACE date_apprv WITH DATE()
				ELSE
					EXIT
				ENDIF
				SKIP 1
			ENDDO
			SELECT 1
		ENDIF
		IF ok = 'N'
			@ 22,0 SAY 'ENTER REASON' GET comments PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
			READ
		ENDIF
		STORE C+1 TO C

		SAVE SCREEN TO SCREEN
*		DO chkrmbrs
		RESTORE SCREEN FROM SCREEN
	ELSE
		@ C,5 SAY no
		@ C,12 SAY HOURS PICTURE '999.99'
		@ C,20 SAY amount-tax
		@ C,33 SAY wage
		@ C,45 SAY (amount-tax)*(wage/100) PICTURE '99999.99'
		STORE 'NO' TO rf
		IF (mat_cost*4) > (amount-tax)
			STORE 'YES' TO rf
		ENDIF
		@ C,59 SAY rf
		@ 22,0 CLEAR
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		SET COLOR TO &promptatr
		@ 21,5 SAY '********* THIS JOB HAS ALREADY BEEN APPROVED *********'
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		@ 22,5 SAY "BACK THIS JOB OUT OF ELECTRICIAN'S PAY ??"
		SET COLOR TO &hiliteatr
		@ 23,5 SAY '                    ENTER  Y  or  N ' GET cl PICTURE '!'
		READ
		SET COLOR TO &screenatr
		IF cl = 'Y'
			SET COLOR TO &hiliteatr
			@ C,63 SAY '** subtracted **'
			@ 22,0 SAY 'ENTER REASON' GET comments PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
			READ
			SET COLOR TO &screenatr
			STORE tw - wageextens TO tw
			@ 18,40 SAY tw
			REPLACE cleared WITH 'N'
			REPLACE payroll WITH CTOD('  /  /    ')
			@ 20,0 TO 23,78 CLEAR
			SELECT 2
			SET ORDER TO 1
			seek m.no
			DO WHILE .NOT. EOF()
				IF wo = m.no
					REPLACE date_apprv WITH CTOD('  /  /    ')
				ELSE
					EXIT
				ENDIF
				SKIP 1
			ENDDO
			SELECT 1
		ENDIF
		SAVE SCREEN TO SCREEN
*		DO chkrmbrs
		RESTORE SCREEN FROM SCREEN
		STORE C+1 TO C
		LOOP
	ENDIF
ENDDO
SET DELIMITERS OFF
RETURN



*: EOF: TIMAPCHK.PRG
*: EOF: TIMAPCHK.PRG
