*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMEFFIC.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/21/00, CARL PRATT
*:  Last modified: 01/21/00 at 17:13:20
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


SELECT 1
USE timwork
SET INDEX TO w_num,w_phone,w_bphone,w_wphone,w_incident,w_tech,w_ref,w_pri
SET ORDER TO 5
SELECT 2
USE timtime share

SELECT 4
USE timeffic EXCL
ZAP
USE TIMEFFIC

*** SELECT DATE PERIOD

STORE DATE() TO edate
STORE DATE()-7 TO bdate


CLEAR

STORE RIGHT(DTOC(DATE()),4) TO YEAR
@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99/9999'
@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99/9999'
READ

STORE RIGHT(DTOC(bdate),4) TO YEAR


******************************************************************
***	COPY ALL RECORDS TO 'TEMPWORK' FILE WHICH QUALIFY FOR DATE ***
******************************************************************

SELECT 1
SET ORDER TO 6
@ 5,5 SAY 'SORTING WORKORDERS'
SET FIELDS ON
SET FIELDS TO no, closedate, solddate
COPY ALL FOR CTOD(LEFT(solddate,10)) >= bdate .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) <= edate TO tempwork
COUNT ALL FOR CTOD(LEFT(solddate,10)) >= bdate .and. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(closedate,10)) <= edate TO M.NO_CALLS
STORE EDATE - BDATE TO M.NO_DAYS


******************************************************************
***	SORT ALL INFORMATION IN 'TEMPWORK' TO EXTRACT DATA TO COMPILE ***
******************************************************************

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
	SUM ALL HOURS FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(solddate,7,4) .AND. CTOD(LEFT(solddate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate .AND. ot = 'N' TO m.hours_paid
	SUM ALL HOURS FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. ot = 'Y' TO m.ot_paid
	SUM ALL miles FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(solddate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO	m.miles_paid
	SUM ALL amount FOR TECHNICIAN = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(solddate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO m.wage_paid
*	COUNT ALL FOR TECHNICINA = M.TECHNICIAN .AND. wo = m.no .AND. YEAR = SUBSTR(closedate,7,4) .AND. CTOD(LEFT(solddate,10)) >= bdate .AND. CTOD(LEFT(closedate,10)) <= edate TO M.NO_trips
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

browse


REPORT FORM TIMbonus preview

store 'Y' TO YN
@ 24,5 SAY 'Would you like to print this report to the printer?  Y / N'  GET YN PICTURE 'X'
READ

IF YN = 'Y'
REPORT FORM TIMbonus to print
endif

RETURN
