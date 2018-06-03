*********************************************************************
*	This program removes all customers that have only used us       *
*	one time													    *
*																	*
*	Written by Carl on 10/16/2007	Looking for a Customer List		*
*	that we can use to sell HVAC and Plumbing Services				*
*																	*
*********************************************************************
Close Databases

ON ESCAPE cancel


Local lnNameCount, lnBNameCount, lnRecNo As Number
LOCAL lcName, lcBName as Character
SET DEFAULT TO Z:\TIM
* Open Database Z:\Tim\Tim.Dbc


*	SELECT 2
IF USED("maillist")
	SELECT maillist
ELSE
	SELECT 2
	USE (LOCFILE("maillist.dbf","DBF","Where is maillist?"));
		AGAIN ALIAS maillist ;
		ORDER 0
ENDIF
SET ORDER TO NAME
ZAP


*	SELECT 1
IF USED("timwork")
	SELECT timwork
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF




Set Fields To Name, STREET, CITY, STATE, ZIP, BNAME, BSTREET, BCITY, BSTATE, BZIP, AMOUNT, TRADE, Date, Company

Goto Top
STORE RECNO() TO lnRecNo
CLEAR
Do While .Not. Eof()
	STORE ALLTRIM(name) TO lcName
	STORE ALLTRIM(bname) TO lcBName
	Count All For Alltrim(Name) = lcName To lnNameCount
	Count All For Alltrim(BNAME) = lcbName To lnBNameCount
	Goto lnRecNo
	IF lnNameCount = 1 .and. lnBNameCount = 1
		SKIP 1
		STORE RECNO() TO lnRecNo
		loop		
	ENDIF

	If  lnBNameCount > 1
		SELECT 2
		SET ORDER TO BNAME
		SEEK lcBName
		IF EOF()
			APPEND BLANK
		ENDIF
		SELECT 1
		Goto lnRecNo
		SKIP 1
		STORE RECNO() TO lnRecNo
		LOOP
	Endif

	If  lnNameCount > 1
		SELECT 2
		SET ORDER TO NAME
		SEEK lcName
		IF EOF()
			APPEND BLANK
		ENDIF
		SELECT 1
		Goto lnRecNo
		SKIP 1
		STORE RECNO() TO lnRecNo
		LOOP
	ENDIF
	
	IF .NOT. EOF()
		SKIP 1
		Store Recno() To lnRecNo		
	ENDIF
	
ENDDO
SELECT 2
BROWSE LAST

* RECALL ALL



