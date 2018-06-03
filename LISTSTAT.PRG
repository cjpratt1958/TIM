*PRIVATE OLDRECNO, OLDDBF
*STORE " " TO OLDDBF
*STORE 0 TO OLDRECNO

*IF DBF() > ' '
*	STORE RECNO() TO oldrecno
*	STORE DBF() TO olddbf
*ENDIF

*!* SET UP DATABASES

IF USED("EMPLOYEE")
	SELECT employee
	SET ORDER TO 1
ELSE
	SELECT 2
	USE (LOCFILE("employee.dbf","DBF","Where is Employee?"));
		AGAIN ALIAS employee ;
		ORDER 1
ENDIF


IF USED("ListStat")
	SELECT liststat
	SET ORDER TO 0
ELSE
	SELECT 3
	USE (LOCFILE("ListStat.dbf","DBF","Where is ListStat?"));
		AGAIN ALIAS liststat ;
		ORDER 0
ENDIF
REPLACE ALL rev_30day WITH 0.00
REPLACE ALL rev_14day WITH 0.00
REPLACE ALL rev_7day WITH 0.00
REPLACE ALL cnt_30day WITH 0
REPLACE ALL cnt_14day WITH 0
REPLACE ALL cnt_7day WITH 0




IF USED("timwork")
	SELECT timwork
	SET ORDER TO TECHNICIAN
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER TECHNICIAN
ENDIF





*******************
* reset paramters *
*******************
STORE 0 TO trev_30day
STORE 0 TO trev_7day
STORE 0 TO trev_14day
STORE 0 TO tcnt_30day
STORE 0 TO tcnt_7day
STORE 0 TO tcnt_14day
STORE ' ' TO m.emp_no
STORE RIGHT(DTOC(DATE()),4) TO YEAR



SELECT employee
SET ORDER TO emp_no

GOTO TOP
store employee.emp_no to m.emp_no
STORE RECNO() TO jn
STORE 0 TO pass
DO WHILE .NOT. EOF()
	SELECT EMPLOYEE
	GOTO jn
	IF ALLTRIM(employee.emp_no) = ' '
		SKIP 1
		STORE RECNO() TO jn
		LOOP
	ENDIF


	IF ALLTRIM(employee.emp_no) < '001'
		SKIP 1
		STORE RECNO() TO jn
		LOOP
	ENDIF


	IF ALLTRIM(employee.emp_no) = ALLTRIM(m.emp_no)
		SKIP 1
		STORE RECNO() TO jn
		LOOP
	ENDIF


	STORE employee.emp_no TO m.emp_no
	STORE employee.name TO m.technician
	SELECT timwork
	STORE pass + 1 TO pass

	*!*		store count * Sum to m.???_30day

SUM ALL timwork.amount FOR timwork.technician = m.emp_no .AND. RIGHT(CLOSEDATE,4) = YEAR .AND. LEFT(CLOSEDATE,5) => LEFT(DTOC(DATE()-30),5) TO mrev_30day
	IF mrev_30day = 0

		SELECT employee
		GOTO jn
		SKIP 1
		IF .NOT. EOF()
			STORE RECNO() TO jn
			LOOP
		ELSE
			EXIT
		ENDIF
	ENDIF
	STORE trev_30day + mrev_30day TO trev_30day
suspend

	COUNT ALL FOR timwork.technician = m.emp_no .AND. LEFT(CLOSEDATE,5) => LEFT(DTOC(DATE()-30),5) .AND. RIGHT(CLOSEDATE,4) = YEAR .AND. ALLTRIM(priority) = '01' TO mcnt_30day

*	COUNT ALL FOR timwork.technician = m.technician .AND. LEFT(CLOSEDATE,5) => DATE()-30 .AND.  ALLTRIM(priority) = '01' TO mcnt_30day
	STORE tcnt_30day + mcnt_30day TO tcnt_30day
	
	
	
	*!*		CREATE TOTALS FOR 7DAY
	SUM ALL timwork.amount FOR timwork.technician = m.emp_no .AND. LEFT(CLOSEDATE,5) => LEFT(DTOC(DATE()-7),5) TO mrev_7day
	STORE trev_7day + mrev_7day TO trev_7day





	COUNT ALL FOR timwork.technician = m.technician .AND. CTOD(closedate) => DATE()-7 .AND.   ALLTRIM(priority) = '01' TO mcnt_7day
	STORE tcnt_7day + mcnt_7day TO tcnt_7day



	*!*		CREATE TOTALS FOR 14DAY
	SUM ALL timwork.amount FOR timwork.technician = m.technician .AND. CTOD(closedate) => DATE()-7 .AND. trade=m.trade .AND. state = mstate TO mrev_14day
	STORE trev_14day + mrev_14day TO trev_14day


	COUNT ALL FOR timwork.technician = m.technician .AND. CTOD(closedate) => DATE()-7 .AND.  ALLTRIM(priority) = '01' TO m.cnt_14day
	STORE tcnt_14day + mcnt_14day TO tcnt_14day



	*!*		store data to a table
	SELECT liststat
	set order to emp_no
	goto top	
	SEEK m.emp_no

	IF EOF()
		APPEND BLANK
	ENDIF

	REPLACE emp_no WITH m.emp_no
	REPLACE technician WITH m.technician
	REPLACE rev_30day WITH trev_30day
	REPLACE rev_14day WITH trev_14day
	REPLACE rev_7day WITH trev_7day
	REPLACE cnt_30day WITH tcnt_30day
	REPLACE cnt_14day WITH tcnt_14day
	REPLACE cnt_7day WITH tcnt_7day


	IF .NOT. EOF()
		SKIP 1
		STORE RECNO() TO jn
	ENDIF
ENDDO while .not. eof()
SELECT liststat
BROWSE

*IF OLDDBF > ' '
*	USE OLDDBF
*	SET ORDER TO OLDRECNO
*ELSE
*	USE
*ENDIF
