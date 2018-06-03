*:*****************************************************************************
*:
*: Procedure file: REPORT30.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 10/28/06, CARL PRATT
*:  Last modified: 10/28/06 at 23:02:48
*:
*:         Set by: TIMWREPO.PRG
*:
*:
*:           Uses: TIMWORK.DBF
*:               : EMPLOYEE.DBF
*:
*:
*:      Documented 12:29:33                                FoxDoc version 3.00a
*:*****************************************************************************

SET EXCLUSIVE OFF
ON ESCAPE RETURN


IF USED("employee")
	SELECT employee
	SET ORDER TO 0
ELSE
	SELECT 2
	USE (LOCFILE("employee.dbf","DBF","Where is employee?"));
		AGAIN ALIAS employee ;
		ORDER 0
ENDIF


IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF

@ 0,0 CLEAR TO 1,0
STORE 'TIMSALES.PRG' TO prg_name
STORE '********  SALES INFORMATION  ********' TO TITLE
DO addtitle
SET ECHO OFF
SET TALK OFF
SET INTENSITY ON
SET COLOR TO &screenatr
SET STATUS OFF
SET EXCLUSIVE OFF
SET CENTURY ON
SET readborder ON
