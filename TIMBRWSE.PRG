*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMBRWSE.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 01/17/97 at 11:03:56
*:
*:      Called by: TIM1MENU.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : LOGO               (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF
*:               : TIMSTOCK.DBF
*:               : TIMREORD.DBF
*:               : TIMHOLD.DBF
*:               : TIMTRKST.DBF
*:               : TIMRMBRS.DBF
*:               : TIMHISTR.DBF
*:               : TIMPOOL.DBF
*:               : TIMPREFX.DBF
*:               : TIMCANCL.DBF
*:               : TIMTRUCK.DBF
*:
*:        Indexes: H_JOBS.IDX
*:               : T_JOBS.IDX
*:               : HI_JOBS.IDX
*:               : PREFIX.IDX
*:               : CANCEL_C.IDX
*:
*:      CDX files: TIMRMBRS.CDX
*:
*:      Documented 12:30:14                                FoxDoc version 3.00a
*:*****************************************************************************
SET COLOR TO &screenatr
STORE '*****  B R O W S E  M E N U  *****' TO TITLE
STORE 'TIMBRWSE.PRG' TO prg_name

DIMENSION brws(15)
STORE 'QUIT' TO brws(1)
STORE 'WORK ORDERS' TO brws(2)
STORE 'STOCK LIST' TO brws(3)
STORE 'REORDER LIST' TO brws(4)
STORE 'HOLD FILE' TO brws(5)
STORE "ADT'L TRUCKSTOCK" TO brws(6)
STORE 'REIMBURSEMENTS' TO brws(7)
STORE 'HISTORY FILE' TO brws(8)
STORE 'TIMPOOL     ' TO brws(9)
STORE 'TIMPREFX    ' TO brws(10)
STORE 'CANCEL CODES' TO brws(11)
STORE 'ESTIMATES   ' TO brws(12)
STORE 'TRUCKS      ' TO brws(13)
STORE 'ADVERTISING ' TO brws(14)
STORE 'STATES      ' TO brws(15)

STORE 38 - LEN(brws(6))/2 TO y
CLOSE DATABASES


DO addtitle
DO LOGO
SET DELETED OFF
SET STATUS ON
DO WHILE .T.
	STORE 0 TO ky
	SET COLOR TO &statusatr
	@ 9,y MENU brws,15 TITLE 'Browse'
	READ MENU TO ky
	SET COLOR TO &screenatr
	@ 24,0 CLEAR
	DO CASE
	CASE ky = 1
		SET STATUS OFF
		CLOSE DATABASES
		RETURN
	CASE ky = 2
		USE timwork
		SET ORDER TO NO
	CASE ky = 3
		USE timstock
	CASE ky = 4
		USE timreord
	CASE ky = 5
		USE timhold
	CASE ky = 6
		USE timtrkst
	CASE ky = 7
		USE timrmbrs
	CASE ky = 8
		USE timhistr
	CASE ky = 9
		USE timpool
	CASE ky = 10
		USE timprefx
	CASE ky = 11
		USE timcancl
	*CASE ky = 12
	*	USE timesti
	CASE ky = 13
		USE timtruck
		SET DELETED On

	CASE ky = 14
		USE advertis
	CASE ky = 15
		USE timstate
	ENDCASE

	GOTO TOP
	IF ms_level = '1'
		BROWSE
		SET DELETED ON
	ELSE
		BROWSE NOEDIT
	ENDIF
	DO addtitle
	DO LOGO
ENDDO
*: EOF: TIMBRWSE.PRG
