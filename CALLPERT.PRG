
PUBLIC dbfold,RECNO,rn,qty7day,qty14day,qty30day,amount7day,amount14day,amount30day,avg7day,avg14day,avg30day

SET FIELDS OFF

STORE DBF() TO dbfold
STORE RECNO() TO RECNO
STORE DATE() TO DispatchDate

*	SELECT 1

IF USED("timwork")
	SELECT timwork
	SET ORDER TO technician
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER technician
ENDIF


*	SELECT 3

IF USED("employee")
	SELECT employee
	SET ORDER TO emp_no
ELSE
	SELECT 3
	USE (LOCFILE("employee.dbf","DBF","Where is employee?"));
		AGAIN ALIAS employee ;
		ORDER emp_no
ENDIF





*	SELECT 7
IF USED("report31")
	SELECT report31
	SET ORDER TO technician
ELSE
	SELECT 7
	USE (LOCFILE("report31.dbf","DBF","Where is report31?"));
		AGAIN ALIAS report31 EXCLUSIVE;
		ORDER technician
ENDIF
ZAP


*~* convert dispdate to date
*store ctod(left(dispdate,10)) to DispatchDate




SELECT timwork
SET FILTER TO SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-30)
GOTO TOP
DO WHILE .NOT. EOF()
	STORE ALLTRIM(technician) TO m.technician
	IF EMPTY(m.technician)
		SKIP 1
		LOOP
	ENDIF
	*	IF AMOUNT = 0
	*		SKIP 1
	*		LOOP
	*	ENDIF
	SKIP 1
	IF technician = m.technician
		SKIP 1
		LOOP
	ELSE
		SKIP-1
	*~* reset memvar

		STORE 0.00 TO m.amount7day,m.amount14day,m.amount30day,m.avg7day,m.avg14day,m.avg30day
		STORE 0 TO m.qty7day,m.qty14day,m.qty30day
		store year(date()) to year
		

	*~* 	
		
	ENDIF


	STORE RECNO() TO rn

*~* 7 day calculations
	COUNT ALL FOR SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-7) .AND. technician= m.technician TO m.qty7day
	SUM ALL amount FOR SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-7) .AND. technician = m.technician TO m.amount7day

*	COUNT ALL FOR left(dispdate,10) > DTOC(DATE()-7) .AND. YEAR = YEAR(DATE) .AND. technician= m.technician TO m.qty7day
*	SUM ALL amount FOR left(dispdate,10) > DTOC(DATE()-7) .AND. YEAR = YEAR(DATE) .AND. technician = m.technician TO m.amount7day





	IF m.amount7day = 0.00
		STORE 0.00 TO mavg7day
	ELSE
		STORE m.amount7day / qty7day TO m.avg7day
	ENDIF

*~* 14 day calculations

	COUNT ALL FOR SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-14) .AND. technician= m.technician TO m.qty14day
	SUM ALL amount FOR SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-14) .AND. technician = m.technician TO m.amount14day

	IF m.amount14day = 0.00
		STORE 0.00 TO mavg14day
	ELSE

		STORE m.amount14day / qty14day TO m.avg14day
	ENDIF
*~* 30 day calculations

	COUNT ALL FOR SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-30) .AND. technician= m.technician TO m.qty30day
	SUM ALL amount FOR SYS(11, ctod(left(dispdate,10))) > SYS(11, DATE()-30) .AND. technician = m.technician TO m.amount30day

	IF m.amount30day = 0.00
		STORE 0.00 TO mavg30day
	ELSE
		STORE m.amount30day / qty30day TO m.avg30day
	ENDIF




*~* apply technician's name to reports

	*	SELECT EMPLOYEE
	*	SET ORDER TO EMP_NO
	*	SEEK M.TECHNICIAN
	*	STORE NAME TO M.NAME

*~* Save to report 31	

	SELECT report31
	GOTO BOTTOM
	APPEND BLANK
	GATHER MEMVAR
	SELECT timwork
	GOTO rn
	SKIP 1
ENDDO
SELECT report31
*BROWSE
REPORT FORM report31 preview
*SELECT dbfold
*GOTO RECNO
SET EXCLUSIVE OFF
SET FILTER TO
RETURN
