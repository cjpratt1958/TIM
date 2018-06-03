SELECT 3
USE report4 EXCL
ZAP
USE
USE report4

set talk off
set status on


SELECT 1
IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF



SELECT 2
IF USED("employee")
	SELECT employee
	SET ORDER TO 0
ELSE
	SELECT 2
	USE (LOCFILE("employee.dbf","DBF","Where is employee?"));
		AGAIN ALIAS employee ;
		ORDER 0
ENDIF

SELECT 3
IF USED("report4")
	SELECT report4
	SET ORDER TO 0
ELSE
	SELECT 3
		USE (LOCFILE("report4.dbf","DBF","Where is report4?"));
		AGAIN ALIAS report4 ;
		ORDER 0
ENDIF




CLEAR

STORE RIGHT(DTOC(DATE()),4) TO YEAR
STORE '01/01' TO bdate
STORE '03/31' TO edate
STORE 'Report4' TO FileLocation


DO WHILE YEAR > ' '



	clear
	@ 17,5 SAY 'To exit, leave Year blank'
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ
	IF ALLTRIM(YEAR) > " "
*		DO NOTHING
	ELSE
		EXIT
	ENDIF

	*******************
	* reset paramters *
	*******************

	STORE '       ' TO empno
	STORE SPACE(20) TO mtechnician
	STORE 0 TO est
	STORE 0.00 TO est_rev
	STORE 0 TO OTHER
	STORE 0.00 TO oth_rev
	STORE 0 TO w_parts
	STORE 0 TO open_calls
	STORE 0 TO ALL
	STORE 0.00 TO combine_rev
	STORE 0.00 TO AVERAGE
	STORE 0 TO calls_w_rev
	STORE 0.00 TO avg_w_rev
	STORE 0 TO CANCEL
	STORE 'S' TO ps
	STORE 1 TO x
	STORE 'SERVICE SUMMARY REPORT (by close date)' TO  TITLE
	STORE 66 - (LEN(TITLE)/2) TO y
	STORE 60 TO lines
	STORE 0 TO test
	STORE 0.00 TO test_rev
	STORE 0 TO tother
	STORE 0.00 TO toth_rev
	STORE 0 TO tw_parts
	STORE 0 TO topen_calls
	STORE 0 TO tall
	STORE 0.00 TO tcombine_rev
	STORE 0.00 TO taverage
	STORE 0 TO tcalls_w_rev
	STORE 0.00 TO tavg_w_rev
	STORE 0 TO tcancel

	* set index to employee number *


	CLEAR
	@ 10,20 SAY 'COMPILING DATA'
	SELECT 1
	SET ORDER TO Technician

	SELECT 2
	SET ORDER TO Emp_No

	GOTO TOP
	STORE RECNO() TO jn
	STORE 0 TO pass


	DO WHILE .NOT. EOF()
		GOTO jn
		IF ALLTRIM(emp_no) = ' '
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF

		IF ALLTRIM(emp_no) < '001'
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF
		IF ALLTRIM(empno) = ALLTRIM(emp_no)
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF


		STORE emp_no TO empno
		STORE name TO mtechnician
		SELECT 1
		STORE pass + 1 TO pass
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' TO m.combine_rev

		IF m.combine_rev = 0
			SELECT 2
			GOTO jn
			IF .NOT. EOF()
				SKIP 1
				STORE RECNO() TO jn
				LOOP
			ELSE
				EXIT
			ENDIF
		ENDIF

		STORE tcombine_rev + m.combine_rev TO tcombine_rev

		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' .AND. ALLTRIM(priority) = '02' TO m.est
		STORE test + m.est TO test
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. ALLTRIM(priority) = '02' .AND. trade='S' TO m.est_rev
		STORE test_rev + m.est_rev TO test_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' .AND. priority <> '02' TO m.other
		STORE tother + m.other TO tother
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. priority <> '02' .AND. trade='S' TO m.oth_rev
		STORE toth_rev + m.oth_rev TO toth_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' .AND. material > 0 TO m.w_parts
		STORE tw_parts + m.w_parts TO tw_parts
		COUNT ALL FOR technician = empno .AND. closedate =  ' ' .AND. trade='S' TO m.open_calls
		STORE topen_calls + m.open_calls TO topen_calls
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' TO m.all
		STORE tall + m.all TO tall
		STORE combine_rev / ALL TO m.average
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' .AND. amount > 0 TO m.calls_w_rev
		STORE tcalls_w_rev + m.calls_w_rev TO tcalls_w_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= bdate .AND. LEFT(closedate,5) <= edate .AND. trade='S' .AND. paid = 'CNCL' TO m.cancel
		STORE tcancel + m.cancel TO tcancel
		STORE combine_rev / calls_w_rev TO m.avg_w_rev

		SELECT 3
		GOTO BOTTOM
		APPEND BLANK
		REPLACE daterange WITH bdate+' TO '+edate+ ' ' + YEAR
		REPLACE empno WITH m.empno
		REPLACE technician WITH mtechnician
		REPLACE est WITH m.est
		REPLACE est_rev WITH m.est_rev
		REPLACE OTHER WITH m.other
		REPLACE oth_rev WITH m.oth_rev
		REPLACE w_parts WITH m.w_parts
		REPLACE open_calls WITH m.open_calls
		REPLACE ALL WITH m.all
		REPLACE combine_re WITH m.combine_rev
		REPLACE AVERAGE WITH m.average
		REPLACE call_w_rev WITH m.calls_w_rev
		REPLACE avg_w_rev WITH m.avg_w_rev
		REPLACE CANCEL WITH m.cancel


		SELECT 2
		GOTO jn
		IF .NOT. EOF()
			SKIP 1
			STORE RECNO() TO jn
		ENDIF
	ENDDO while .not. eof()

ENDDO while YEAR > ' '

SELECT 3
REPORT FORM report4 PREVIEW
STORE 'Y' TO PRINT
CLEAR

@ 10,20 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?  Y / N' GET PRINT PICTURE '!'
READ
IF PRINT = 'Y'
	REPORT FORM report4 TO PRINT
ENDIF

@ 10,20 SAY 'WOULD YOU LIKE TO EXPORT THIS REPORT TO XLS FORMAT? (Excel) Y / N' GET PRINT PICTURE '!'
READ
IF PRINT = 'Y'
	@ 12,20 SAY 'ENTER NAME FOR REPORT  Report4 'GET FileLocation
	READ
	IF alltrim(FileLocation) > " "
		EXPORT TYPE XLS TO &FileLocation
	else
		DO NOTHING
	endif
ENDIF
set talk off
set echo off
clear