*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMEFFIC.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/21/00, CARL PRATT
*:  Last modified: 02/28/00 at 17:13:20
*:
*:      Called by: TIMWREPO.PRG
*:
*:
*:           Uses: TIMWORK.DBF
*:               : TIMTIME.DBF
*:
*:   Report Forms: TIMEFFIC.FRX
*:
*:
*:      Documented 12:29:57                                FoxDoc version 3.00a
*:*****************************************************************************
SET CENTURY ON
SET TALK OFF



IF USED("timwork")
	SELECT timwork
	SET ORDER TO incident
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER incident
ENDIF

IF USED("timtime")
	SELECT timtime
	SET ORDER TO incident
ELSE
	SELECT 2
	USE (LOCFILE("timtime.dbf","DBF","Where is timtime?"));
		AGAIN ALIAS timtime ;
		ORDER incident
ENDIF

SELECT 4
USE timeffic EXCL
ZAP
USE TIMEFFIC

STORE DATE() TO edate
STORE DATE()-7 TO bdate
STORE '   ' TO emp


CLEAR

STORE RIGHT(DTOC(DATE()),4) TO YEAR
@ 19,5 SAY 'Enter Employee Number               ' GET emp PICTURE '999'
@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99/9999'
@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99/9999'
READ

STORE RIGHT(DTOC(bdate),4) TO YEAR


SELECT 1
SET ORDER TO INCIDENT

@ 5,5 SAY 'SORTING WORKORDERS'
SET FIELDS ON
SET FIELDS TO no, closedate, technician, paid
COPY ALL FOR technician = emp  .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO tempwork
COUNT ALL FOR YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO M.NO_CALLS
STORE EDATE - BDATE TO M.NO_DAYS


SET FIELDS OFF

SELECT 3
USE tempwork
INDEX ON no TO temp_no
GOTO TOP
STORE no TO m.no
DO WHILE .T.

	SELECT 1
	LOCATE FOR no = m.no
	SCATTER FIELDS EXCEPT WORK MEMVAR MEMO

	SELECT 2
	@ 5,5 SAY 'SORTING TIMECARDS '
IF M.NO <> '535'
	SUM ALL HOURS FOR wo = m.no .AND. ot = 'N' TO m.hours_paid
	SUM ALL HOURS FOR wo = m.no .AND. ot = 'Y' TO m.ot_paid
	SUM ALL miles FOR wo = m.no TO	m.miles_paid
	SUM ALL amount FOR wo = m.no TO m.wage_paid
	COUNT ALL FOR wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO M.NO_trips
ELSE
	SUM ALL HOURS FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate .AND. ot = 'N' TO m.hours_paid
	SUM ALL HOURS FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. ot = 'Y' TO m.ot_paid
	SUM ALL miles FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO	m.miles_paid
	SUM ALL amount FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO m.wage_paid
*	COUNT ALL FOR TECHNICINA = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO M.NO_trips
ENDIF




	SELECT 4
	GOTO BOTTOM
	APPEND BLANK
	GATHER MEMVAR MEMO

	IF M.NO <> '535'
		SELECT 3
		LOCATE FOR no = m.no
		SKIP 1
		IF .NOT. EOF()
			STORE no TO m.no
		ELSE
			store '535' to m.no
		ENDIF
	ELSE
		EXIT
	ENDIF
ENDDO

SELECT 4

REPORT FORM TIMEFFIC preview

store 'Y' TO YN
@ 24,5 SAY 'Would you like to print this report to the printer?  Y / N'  GET YN PICTURE 'X'
READ

IF YN = 'Y'
REPORT FORM TIMEFFIC to print
endif

RETURN
