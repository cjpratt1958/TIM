*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMPEND.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 01/25/95 at  0:02:12
*:
*:      Called by: TIM1MENU.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : WORKMENU           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF
*:
*:      Documented 12:29:55                                FoxDoc version 3.00a
*:*****************************************************************************
SET EXCLUSIVE OFF
CLEAR
STORE '*****  P E N D I N G   F I L E   L O O K U P  *****' TO TITLE
STORE 40 - (LEN(serial)/2) TO SCOL
STORE 40 - (LEN(TITLE)/2) TO COL
STORE 'TIMDMENU.PRG' TO prg_name
DO addtitle
STORE '       ' TO memp_no
@ 1,5 SAY 'Enter Employee number ' GET memp_no
READ
SET COLOR TO &promptatr
@ 2,15 SAY ' PENDING CALLS '
*	SELECT 1 TO TIMWORK


IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF
GOTO BOTTOM
STORE RECNO() TO lastrec
SET ORDER TO Technician
SET COLOR TO &screenatr
LOCATE ALL FOR technician = memp_no .AND. closedate = ' '
DO WHILE .NOT. EOF()
	STORE 3 TO x
	DO WHILE x < 24
		IF technician = memp_no .AND. closedate = ' '
			SET COLOR TO &promptatr
			@ x,1 SAY STR(RECNO())
			SET COLOR TO &screenatr
			@ x,12 SAY no+' '+incident+' '+LEFT(name,13)+' '+LEFT(city,13)+' '+phone+' '+priority
			SKIP 1
			STORE x+1 TO x
		ELSE
			SKIP 1
		ENDIF
		IF EOF()
			EXIT
		ENDIF
	ENDDO
	STORE '     ' TO rn
	@ 24,5 SAY 'Enter record number to display, [Enter] next page    [E]xit' GET rn
	READ

	IF UPPER(rn) = 'E'
		ON KEY
		RETURN
	ENDIF
	IF VAL(rn) < 0
		ON KEY
		RETURN
	ENDIF
	IF VAL(rn) > lastrec
		ON KEY
		RETURN
	ENDIF


	IF rn > '     '
		STORE RECNO() TO rp
		SAVE SCREEN TO SCREEN
		CLEAR
		STORE VAL(rn) TO rn

		GOTO rn
		DO workmenu
		SET COLOR TO &screenatr
		@ 1,20 SAY '[       ]'
		@ 1,62 SAY 'Refferal'
		@ 12,0 CLEAR
		@ 13,44 SAY 'Tax  [ .  %]'
		@ 15,0 TO 15,80
		@ 15,0 SAY 'ENTER JOB DESCRIPTION'
		@ 19,0 TO 19,80
		SET COLOR TO &screenatr
		@ 0,9 SAY rn
		@ 1,72 SAY ref
		@ 2,68 SAY DATE
		@ 4,60 SAY area PICTURE '(999)'
		@ 4,66 SAY phone PICTURE ' 999-9999'
		@ 4,8 SAY warea PICTURE '(999)'
		@ 4,15 SAY wphone PICTURE ' 999-9999'
		@ 4,33 SAY barea PICTURE '(999)'
		@ 4,40 SAY bphone PICTURE ' 999-9999'
		@ 5,53 SAY name
		@ 6,53 SAY street
		@ 7,53 SAY city
		@ 8,53 SAY state
		@ 8,34 SAY bzip
		@ 5,14 SAY bname
		@ 6,14 SAY bstreet
		@ 7,14 SAY bcity
		@ 8,14 SAY bstate
		@ 8,73 SAY zip
		@ 2,57 SAY sector
		@ 2,44 SAY incident
		@ 9,14 SAY contact
		@ 9,53 SAY xstreet
		@ 11,49 SAY rate PICTURE '999.99'
		@ 13,50 SAY tax PICTURE '9.99'
		@ 16,0 SAY jobdesc1
		@ 17,0 SAY jobdesc2
		@ 18,0 SAY jobdesc3
		ON KEY LABEL f9 MODIFY MEMO notes
		IF LEN(notes) > 0
			SET COLOR TO &promptatr
			@ 0,57 SAY ' See Notes'
		ENDIF
		SET COLOR TO &screenatr
		STORE ' ' TO K
		@ 24,10 SAY 'Press any key to continue...' GET K
		READ

		ON KEY
		RETURN
	ENDIF


ENDDO


				*****  CALCULATE PERFORMANCE REPORTS  *************
				SELECT 1
				SUM ALL amount FOR DATE >= DATE()-7 .AND. DATE <= DATE() .AND. technician = memp_no TO weektotal
				COUNT ALL FOR DATE >= DATE()-7 .AND. DATE <= DATE() .AND. technician = memp_no TO weekAVG
				
				SUM ALL amount FOR DATE >= DATE()-30 .AND. DATE <= DATE() .AND. technician = memp_no TO monthtotal
				COUNT ALL FOR DATE >= DATE()-30 .AND. DATE <= DATE() .AND. technician = memp_no TO monthAVG

				STORE (SUBSTR(DTOC(DATE()),4,2)) TO firstofmonth

				SUM ALL amount FOR DATE >= DATE()-VAL(firstofmonth) .AND. DATE <= DATE() .AND. technician = memp_no TO thismonth
				COUNT ALL FOR DATE >= DATE()-VAL(firstofmonth) .AND. DATE <= DATE() .AND. technician = memp_no TO thismoAVG

				@ 22,1 TO 26,80 DOUBLE
				@ 22,65 SAY TIME()
				SET COLOR TO W+/B
				@ 23,3 SAY "Last 7 days $" + ALLTRIM(STR(weektotal)) + ".00    * Last 30 days $" + ALLTRIM(STR(monthtotal)) + ".00    * This Month $" + ALLTRIM(STR(thismonth)) + ".00"
				@ 24,3 SAY "Last 7 days $" + ALLTRIM(STR(weektotal/weekAVG)) + ".00    * Last 30 days $" + ALLTRIM(STR(monthtotal/monthAVG)) + ".00    * This Month $" + ALLTRIM(STR(thismonth/thismoAVG)) + ".00"
SET COLOR TO &promptatr
WAIT '                              Press any key to continue ... '

*: EOF: TIMPEND.PRG
