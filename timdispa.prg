*:*****************************************************************************
*:
*:        Program: C:\TIM\DATA\TIMDISPA.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/05/96 at  9:02:50
*:
*:      Called by: TIM1MENU.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : WORKMENU           (procedure in TIMPROC.PRG)
*:               : WORK_SAY           (procedure in TIMPROC.PRG)
*:               : PPER()             (function in ?)
*:               : TIMINV.PRG
*:
*:           Uses: TIMWORK.DBF		select 1
*:               : TIMPOOL.DBF		SELECT 2
*:               : EMPLOYEE.DBF		select 3
*: 						Name
*:						Emp_no
*:               : TIMCANCL.DBF		select 4
*:      				CANCEL_C
*:               : TIMSTATE.DBF		select 5
*:               : TIMTRACK.DBF		select 6
*:						ORDER TO 1
*:
*:               : SECTOR.IDX
*:
*:      Documented 12:29:40                                FoxDoc version 3.00a
*:*****************************************************************************
DIMENSION state(52,3)
PRIVATE lnhour, lnminute

SET REPROCESS TO AUTOMATIC
SET EXCLUSIVE OFF
SET CENTURY ON
SET ECHO OFF
SET TALK OFF
SET INTENSITY ON
SET COLOR OF SCHEME 1 TO
SET STATUS OFF
SET readborder ON
SET DELETED ON
USE
ON KEY
ON KEY LABEL f8 DO f8screen.spr
ON ESCAPE RETURN
* ON ERROR DO ERRHAND
@ 0,0 CLEAR TO 1,0
UNLOCK ALL
STORE 0 TO callcount
STORE 3 TO CNT
STORE 'V' TO vt
STORE 'TIMDISPA.PRG' TO prg_name
STORE '********  DISPATCH A CALL  ********' TO TITLE
DO addtitle

*  ---DIMENSION HOW PAID POPUP WINDOW
DIMENSION pd(6)
STORE 'NOT DISCUSSED' TO pd(1)
STORE 'CHECK' TO pd(2)
STORE 'CASH' TO pd(3)
STORE 'CREDIT CARD' TO pd(4)
STORE 'BILL' TO pd(5)
STORE 'ESTIMATE' TO pd(6)


*************************
*	Select Databases	*
*************************

*	SELECT 1 TO TIMWORK AND COUNT RECORDS FOR LAST RECORD

IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF


SET DELETED OFF
COUNT TO workrecords
SET DELETED ON


*	SELECT 2 TO TIMPOOL

IF USED("timpool")
	SELECT timpool
	SET ORDER TO priority
ELSE
	SELECT 2
	USE (LOCFILE("timpool.dbf","DBF","Where is timpool?"));
		AGAIN ALIAS timpool ;
		ORDER priority
ENDIF
SET DELETED OFF
COUNT TO poolrecords
SET DELETED ON

*	SELECT 3 TO Employee

IF USED("Employee")
	SELECT employee
	SET ORDER TO 0
ELSE
	SELECT 3
	USE (LOCFILE("Employee.dbf","DBF","Where is Employee?"));
		AGAIN ALIAS employee ;
		ORDER 0
ENDIF



*	SELECT 4 TO TIMCANCL

IF USED("timcancl")
	SELECT timcancl
	SET ORDER TO 1
ELSE
	SELECT 4
	USE (LOCFILE("timcancl.dbf","DBF","Where is timcancl?"));
		AGAIN ALIAS timcancl ;
		ORDER 1
ENDIF


*	SELECT 5 TO TIMSTATE

IF USED("timstate")
	SELECT timstate
	SET ORDER TO state
ELSE
	SELECT 5
	USE (LOCFILE("timstate.dbf","DBF","Where is timstate?"));
		AGAIN ALIAS timstate ;
		ORDER state

ENDIF
GOTO TOP


*	SELECT 6 TO TimTrack

IF USED("TimTrack")
	SELECT timtrack
	SET ORDER TO 1
ELSE
	SELECT 6
	USE (LOCFILE("TimTrack.dbf","DBF","Where is TimTrack?"));
		AGAIN ALIAS timtrack ;
		ORDER 1
ENDIF
*********************************
*	COUNT CALLS TO EACH STATE	*
*********************************

SELECT timpool

COUNT TO callcount	&& GET NUMBER OF CALLS IN TIMPOOL

SELECT timstate		&& SELECT TIMSTATES
COUNT TO nostates	&& GET NUMBER OF STATES WE ARE IN
GOTO TOP
REPLACE calls WITH callcount	&& STORE TOTAL NUMBER OF CALLS TO "ALL" FIELD
STORE 2 TO x
DO WHILE x < nostates +1
	GOTO x
	STORE state TO m.state
	SELECT timpool		&& timpool
	COUNT ALL FOR state = m.state TO callcount
	SELECT timstate		&& timstate
	GOTO x
	REPLACE calls WITH callcount
	STORE x+1 TO x
ENDDO


*************************************
*	CHOOSE STATE TO DISPATCH FROM 	*
*************************************

*	Define Window

STORE .F. TO mexit
DEFINE WINDOW mvalidate FROM 3,5 TO 17,45 TITLE "Choose a State to Dispatch" ;
	SYSTEM GROW CLOSE ZOOM FLOAT FONT "MS Sans Serif",8
MOVE WINDOW mvalidate CENTER

*	ASSIGN STATES TO VARIABLES

SELECT timstate		&& timstate
SET ORDER TO descriptio
GOTO 1
STORE 1 TO stateprompt
STORE 1 TO y
IF .NOT. EOF()
	ACTIVATE WINDOW mvalidate
	DO WHILE .NOT. EOF()
		IF calls <> 0
			STORE state TO state(stateprompt,1)
			STORE descriptio TO state(stateprompt,2)
			STORE calls TO state(stateprompt,3)
			@ y,10 PROMPT STR(state(stateprompt,3))+' Calls in '+ALLTRIM(state(stateprompt,2))
			STORE y + 1 TO y
			@ y,15 SAY '  -------------------------------'
			STORE y+1 TO y
			STORE stateprompt+1 TO stateprompt
		ENDIF
		SKIP 1
	ENDDO
	MENU TO mchoice
	DEACTIVATE WINDOW mvalidate
ENDIF
RELEASE WINDOW mvalidate

STORE state(mchoice,1) TO m.statecode


*****************************************
*	ADD TITLE AND COUNT EACH COMPANY	*
*****************************************

SELECT timpool
STORE '       ' TO mtechnician
STORE '       ' TO mnum
STORE '   ' TO mref
STORE '       ' TO memp_no
STORE SPACE(25) TO mcity
STORE 0 TO CANCEL
* SET COLOR TO &promptatr
* SET COLOR TO &promptatr
@ 24,0 CLEAR
STORE DATE() TO scheddate

@ 44,0 SAY 'PLEASE ENTER SCHEDULE DATE TO DISPATCH FROM ' GET scheddate
* SET COLOR TO &promptatr
READ
SELECT timpool
SET ORDER TO 5
GOTO TOP
STORE trade TO gtrade
STORE '1' TO gline
STORE 'W*' TO backgrnd

DO WHILE .NOT. EOF()
	CLEAR
	@ 0,1 SAY '   #  INC #    SCHDL  NAME          CITY         PRI SCHEDU       REF     TECH DAYS  STATE  TRADE'


	*  set display to vga50

	*****************************************
	*	Set Colors to each line of choice	*
	*****************************************

	STORE 'N' TO foregrnd

	DO WHILE CNT < 42
		ON KEY
		SET HEADING OFF
		STORE RECNO() TO item
		STORE 0 TO days
		*	@ 0,9 SAY RECNO()
		IF priority = '01'
			STORE 'N' TO foregrnd
		ENDIF
		IF priority = '00'
			STORE 'R+' TO foregrnd
		ENDIF
		IF priority = '02'
			STORE 'B+' TO foregrnd
		ENDIF

		IF trade = gtrade
			* Do Nothing
		ELSE
			STORE trade TO gtrade
			IF gline = '1'
				STORE '2' TO gline
				STORE 'GB*' TO backgrnd
			ELSE
				STORE '1' TO gline
				STORE 'W*' TO backgrnd
			ENDIF
		ENDIF



		STORE foregrnd+'/'+'W*' TO linecolor

		SET COLOR TO &linecolor

		STORE DATE() - DATE TO days

		*********************************************
		*	Choose if this line items lists or not	*
		*********************************************

		** IF STATE = ALL **


		IF m.statecode = '**'  && IF STATECODE = ALL
			IF DATE <= scheddate
				@ CNT,1 TO CNT,80 CLEAR
				@ CNT,1 SAY STR(RECNO(),4)+'  '+incident+' '+LEFT(DTOC(DATE),5)+'  '+LEFT(name,13)+' '+LEFT(city,13)+' ';
					+priority+'  '+schedule+'  '+ref+'  '+LEFT(technician,4)+' '+RIGHT(STR(days),3)+'    '+state+'      '+trade
				STORE foregrnd+'/'+backgrnd TO linecolor
				SET COLOR TO &linecolor
				@ CNT,96 SAY trade
			ELSE
				IF .NOT. EOF()
					STORE CNT - 1 TO CNT
				ENDIF
			ENDIF



			IF EOF()
				SET COLOR OF SCHEME 1 TO
				*				@ CNT,0 CLEAR
				EXIT
			ELSE
				IF .NOT. EOF()
					STORE CNT + 1 TO CNT
					SKIP 1
				ENDIF
			ENDIF

			** IF STATE IS SPECIFIC

		ELSE
			IF state = m.statecode
				IF DATE <= scheddate
					@ CNT,1 TO CNT,80 CLEAR
					@ CNT,1 SAY STR(RECNO(),4)+'  '+incident+' '+LEFT(DTOC(DATE),5)+'  '+LEFT(name,13)+' '+LEFT(city,13)+' ';
						+priority+'  '+schedule+'  '+ref+'  '+LEFT(technician,4)+' '+RIGHT(STR(days),3)+'    '+state+'      '+trade
					STORE foregrnd+'/'+backgrnd TO linecolor
					SET COLOR TO &linecolor
					@ CNT,96 SAY trade

				ELSE
					IF .NOT. EOF()
						STORE CNT - 1 TO CNT
					ENDIF
				ENDIF
			ELSE
				STORE CNT - 1 TO CNT
			ENDIF

			STORE CNT + 1 TO CNT

			IF EOF()
				SET COLOR OF SCHEME 1 TO
				*				@ CNT,0 CLEAR
				EXIT
			ELSE
				SKIP 1
			ENDIF
		ENDIF
		** END IF STATE = SPECIFIC



	ENDDO WHILE CNT < 42

	* SET COLOR TO &hiliteatr
	STORE '   ' TO ky
	@ 44,60 SAY TIME()
	@ 45,1 CLEAR
	@ 46,0 SAY ' [E]xit,   [CR] to continue,   [-] previous screen,   # to dispatch  '
	@ 46,70 GET ky PICTURE '!!!'
	READ
	STORE VAL(ky) TO recordpointer

	*************************************************************
	*	Looking for RecordPointer (desired record pointer)		*
	*************************************************************

	*	if exit

	STORE 'W*' TO backgrnd
	IF UPPER(ky) = 'E'
		SET HEADING ON
		@ 43,0 CLEAR
		* SET COLOR TO &promptatr
		@ 43,5 SAY '*****  Please Wait  *****'
		* SET COLOR TO &screenatr
		UNLOCK ALL
		CLOSE DATABASES
		RETURN
	ENDIF

	*	IF LAST PAGE

	IF UPPER(ky) = '-'
		IF STR(RECNO()) < '42'
			GOTO TOP
		ELSE
			SKIP -41
		ENDIF
	ENDIF

	*	FIND RECORD


	IF recordpointer > 0
		ON KEY LABEL f9 DO f9key
		IF recordpointer > poolrecords
			STORE CNT-1 TO CNT
			SKIP -1
			STORE '   ' TO ky
			LOOP
		ENDIF

		GOTO recordpointer
		STORE incident TO mincident
		STORE rec_no TO mrecord
		STORE priority TO mpriority

		* SET COLOR TO &screenatr
		SELECT timwork
		SET ORDER TO technician
		IF mrecord > workrecords

			STORE '   ' TO ky
			@ 4,10 TO 12,60 CLEAR
			@ 4,10 TO 12,60 DOUBLE
			@ 6,13 SAY 'The record pointer is '+ STR(mrecord)
			@ 7,13 SAY 'The last record is    '+ STR(workrecords)
			@ 8,13 SAY 'Please correct in the TIMPOOL file then retry'
			@ 10,20 SAY 'Press any key to continue.....'
			WAIT ''
			SELECT timpool
			GOTO TOP
			STORE 2 TO CNT
			LOOP
		ENDIF
		*  ---CREATE PICTURE ON SCREEN
		DO workmenu
		@ 1,20 SAY '[       ]'
		@ 1,62 SAY 'Refferal'
		@ 12,0 CLEAR
		@ 13,44 SAY 'Tax  [ .  %]'
		@ 15,0 TO 15,80
		@ 15,0 SAY 'ENTER JOB DESCRIPTION'
		@ 19,0 TO 19,80
		@ 23,0 CLEAR
		SET REPROCESS TO 1 SECONDS
		GOTO mrecord
		*	  ? RLOCK()
		*	  IF RLOCK() = .F.
		*        @ 4,10 TO 12,60 CLEAR
		*        @ 4,10 TO 12,60 DOUBLE
		*        @ 6,13 SAY 'This record is in use by another user'
		*        @ 7,13 SAY 'Please try again later'
		*        @ 10,20 SAY 'Press any key to continue.....'
		*        WAIT ''
		*        SELECT TIMPOOL
		*        GOTO TOP
		*        STORE 2 TO CNT
		*        LOOP
		*     ENDIF
		SET REPROCESS TO AUTOMATIC

		IF EOF()
			** do nothing **
		ELSE
			STORE 'Go For It' TO priority1
			STORE RECNO() TO recordnumber
			DO WHILE .T.
				* SET COLOR TO &hiliteatr
				IF priority = '00'
					STORE 'EMERGENCY   ' TO priority1
					SET COLOR TO R+/W*
				ENDIF
				IF priority = '01'
					STORE 'C. O. D.    ' TO priority1
				ENDIF
				IF priority = '02'
					STORE 'ESTIMATE    ' TO priority1
				ENDIF
				IF priority = '03'
					STORE 'CANCEL      ' TO priority1
				ENDIF
				IF priority = '04'
					STORE 'REWORK      ' TO priority1
				ENDIF
				IF priority = '05'
					STORE 'SELF GENERAT' TO priority1
				ENDIF
				IF priority = '06'
					STORE 'OTHER       ' TO priority1
				ENDIF
				IF priority = '07'
					STORE 'BILL TO:    ' TO priority1
				ENDIF
				STORE afterhours TO ah
				STORE area TO mmarea
				STORE phone TO mphone
				STORE priority TO mpriority
				STORE DATE TO mdate
				STORE jobdesc1 TO mjobdesc
				@ 0,9 SAY recordnumber
				@ 21,40 SAY priority1
				@ 1,72 SAY ref
				@ 2,68 SAY DATE
				@ 4,60 SAY area
				@ 4,66 SAY phone
				@ 4,8 SAY warea
				@ 4,15 SAY wphone
				@ 4,33 SAY barea
				@ 4,40 SAY bphone


				*				@ 4,33 SAY barea PICTURE '(999)'
				*				@ 4,40 SAY bphone PICTURE ' 999-9999'
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
				@ 23,0 SAY 'Change  [D]escription, [R]eschedule,  [A]ssign,  [P]hone,  [C]ustomer,  [L]abor  [?]Techs'
				SET COLOR OF SCHEME 1 TO
				@ 22,65 SAY TIME()
				@ 23,0 SAY 'Change  D'
				@ 23,21 SAY 'R'
				@ 23,34 SAY 'A'
				@ 23,43 SAY 'P'
				@ 23,51 SAY 'C'
				@ 23,62 SAY 'L'
				IF LEN(notes) > 0
					SET COLOR TO N/R*
					@ 0,57 SAY ' See Notes'
				ENDIF
				STORE technician TO memp_no
				SET COLOR OF SCHEME 1 TO
				@ 1,1 SAY 'EMPLOYEE NO.'
				@ 1,21 GET memp_no PICTURE '@k !!!!!!!'
				READ
				IF memp_no = '?      '
					SELECT EMPLOYEE
					BROWSE FIELDS emp_no, name, city, state, harea, hphone, carea, cphone LAST
					STORE emp_no to memp_no
					@ 1,21 say memp_no
					SELECT TIMWORK
				ENDIF
				
				
				
				STORE UPPER(memp_no) TO memp_no
				
				
				
				
				@ 23,0 SAY '                                                                  '

				IF EMPTY(memp_no)
					* DO NOTHING
				ELSE
					STORE 'CONFIRM HOW WILL THEY BE PAYING TODAY ?' TO titlebar



					*					do paymenu
				ENDIF


				IF memp_no = '       ' 

					SELECT timpool
					GOTO TOP
					STORE 2 TO CNT
					UNLOCK ALL
					STORE '1' TO gline
					EXIT
				ENDIF

				**************************************************
				** CHECK TO SEE HOW MANY PENDING FILES ARE OPEN	**
				**************************************************
				SELECT timwork
				STORE RECNO() TO recordnumber
				COUNT ALL FOR technician = memp_no .AND. closedate = ' ' .AND. no <> ' ' TO CNT
				GOTO recordnumber
				IF CNT > 5


					? CHR(7)
					DEFINE WINDOW output FROM 2,1 TO 20,90 TITLE 'THERE ARE TOO MANY CALLS IN THIS PENDING FILE PLEASE CLOSE SOME CALLS' ;
						CLOSE FLOAT GROW SHADOW ZOOM
					ACTIVATE WINDOW output
					DISPLAY ALL FOR technician = memp_no .AND. closedate = ' ';
						FIELDS no,incident,LEFT(name,15),LEFT(city,15),phone,dispdate OFF
					WAIT WINDOW 'press any key to continue'
					RELEASE WINDOW output



					STORE '       ' TO mnum
					IF ms_level = '1'
						? CHR(7)
						WAIT WINDOW "This Technician has more than 10 calls" + CHR(13) + "Press any key to continue"
					ELSE

						IF CNT > 10
							IF UPPER(ky) = 'D'
								STORE 'N' TO ky
							ELSE
								STORE '       ' TO memp_no
								GOTO recordnumber
								LOOP
							ENDIF
						ENDIF
					ENDIF
				ENDIF

				IF ALLTRIM(memp_no) = 'D'
					@ 23,0 SAY 'Enter Description'
					STORE jobdesc1 TO mjobdesc1
					STORE jobdesc2 TO mjobdesc2
					STORE jobdesc3 TO mjobdesc3
					@ 16,0 GET mjobdesc1
					@ 17,0 GET mjobdesc2
					@ 18,0 GET mjobdesc3
					READ
					REPLACE notes WITH notes + CHR(13) +'Descriptio changed on '+DTOC(DATE()) +' by '+ALLTRIM(operator)
					REPLACE jobdesc1 WITH mjobdesc1
					REPLACE jobdesc2 WITH mjobdesc2
					REPLACE jobdesc3 WITH mjobdesc3
					UNLOCK ALL
					CLOSE DATABASES
					RETURN
				ENDIF
				IF ALLTRIM(memp_no) = 'R'
					REPLACE notes WITH notes + CHR(13) + ' Rescheduled on '+DTOC(DATE()) +' by '+ALLTRIM(operator)
					DO timsched.spx
					RETURN
				ENDIF
				IF ALLTRIM(memp_no) = 'P'
					@ 23,0 SAY 'Enter new Phone Numbers'
					STORE area TO marea
					STORE phone TO mphone
					STORE warea TO mwarea
					STORE wphone TO mwphone
					STORE barea TO mbarea
					STORE bphone TO mbphone
					@ 4,8 GET mwarea PICTURE '(999)'
					@ 4,15 GET mwphone PICTURE ' 999-9999'
					@ 4,33 GET mbarea PICTURE '(999)'
					@ 4,40 GET mbphone PICTURE ' 999-9999'
					@ 4,60 GET marea PICTURE '(999)'
					@ 4,67 GET mphone PICTURE ' 999-9999'
					READ
					REPLACE area WITH marea
					REPLACE phone WITH mphone
					REPLACE warea WITH mwarea
					REPLACE wphone WITH mwphone
					REPLACE barea WITH mbarea
					REPLACE bphone WITH mbphone
					REPLACE notes WITH notes + CHR(13) +'Phone number changed on '+DTOC(DATE()) +' by '+ALLTRIM(operator)
					UNLOCK ALL
					CLOSE DATABASE
					RETURN
				ENDIF
				IF ALLTRIM(memp_no) = 'A'
					SELECT timpool
					@ 20,0 SAY 'Enter Technician    '
					STORE technician TO mtechnician
					@ 1,21 GET mtechnician PICTURE '@kXXXXXXX'
					READ
					REPLACE technician WITH mtechnician
					UNLOCK ALL
					STORE rec_no TO recordnumber
					SELECT timwork
					GOTO recordnumber
					REPLACE notes WITH notes + CHR(13) +'Reassigned to '+mtechnician+' on '+DTOC(DATE()) +' by '+ALLTRIM(operator)
					CLOSE DATABASES
					RETURN
				ENDIF


				**  test to verify employee's employment status  **
				SELECT employee
				SET ORDER TO emp_no
				SET DELETED OFF

				IF ALLTRIM(memp_no) = ' '
					RETURN
				ELSE
					*              store trim(memp_no) to memp_no
					SEEK memp_no
				ENDIF


				**  get employee data if employee is still with the company  **
				IF .NOT. EOF()
					STORE name TO mname
					STORE skill TO mskill
					STORE commission TO mcommission
					@ 1,32 SAY mname
					** check to see if employee has been terminated  **
					IF DELETED()
						STORE ' ' TO ky
						* SET COLOR TO &windowatr
						*						SAVE SCREEN TO WINDOW
						@ 4,10 TO 8,40 CLEAR
						@ 4,10 TO 8,40 DOUBLE
						* SET COLOR TO &promptatr
						@ 5,11 SAY '  This Employee has been  '
						@ 6,11 SAY '        Terminated        '
						@ 7,11 SAY '  Press any key to continue' GET ky
						READ
						*						RESTORE SCREEN FROM WINDOW
						SELECT timwork
						* SET COLOR TO &hiliteatr
						LOOP
					ENDIF
				ELSE
					SELECT timwork
					? CHR(7)
					WAIT WINDOW "This Employee does not exist" + CHR(13) + CHR(13) +" Press any key to continue"
					LOOP
				ENDIF
				EXIT
			ENDDO
			**  check to see if employee number has been entered
			IF memp_no = '       ' 
				LOOP
			ENDIF
			SELECT timwork
			SET ORDER TO no

			@ 23,5 SAY 'Enter [P] for a List of Pending Calls Needing to be Closed '
			DO WHILE .T.
				IF mskill = 'APPRENTICE' .OR. mskill = 'JOURNEYMAN' .OR. mskill = 'MASTER' .OR. mskill = 'SALES'.OR. mskill = 'TECHNICIAN'
					@ 2,21 GET mnum PICTURE '!!!!!!!'
					READ
					*				ELSE
					*					STORE '       ' TO mnum
				ENDIF
				IF ALLTRIM(mnum) = 'P'
					*					SAVE SCREEN TO WINDOW
					* SET COLOR TO &windowatr
					@ 2,0 TO 24,79 CLEAR
					@ 1,50 TO 1,80 CLEAR
					@ 2,0 TO 2,79
					@ 3,79 TO 24,79
					* SET COLOR TO &promptatr
					@ 4,15 SAY ' PENDING CALLS '
					@ 5,0 SAY ' '
					DISPLAY ALL FOR technician = memp_no .AND. closedate = ' ';
						FIELDS no,incident,LEFT(name,15),LEFT(city,15),phone,dispdate OFF
					WAIT
					* SET COLOR TO &screenatr
					*					RESTORE SCREEN FROM WINDOW
					STORE '       ' TO mnum
					LOOP
				ENDIF
				@ 23,0 CLEAR
				IF mnum = '       '
					STORE '0000000' TO mnum
					STORE '2' TO ky
					* SET COLOR TO &windowatr
					@ 4,10 TO 9,70 CLEAR
					@ 4,10 TO 9,70 DOUBLE
					* SET COLOR TO &promptatr

					@ 6,16 SAY '1 - Cancel this Job and do not dispatch'
					@ 7,16 SAY '2 - Return to Main Menu But do not cancel Job'
					*					@ 8,16 SAY '3 - Return to Dispatch Screen'
					@ 8,16 GET ky
					READ

					*					IF ky = '3'
					*						clear
					*						do timdispa
					*					ENDIF

					IF ky = '2'
						CLOSE DATABASES
						RETURN
					ELSE
						STORE 1 TO CANCEL
					ENDIF
				ENDIF
				
				SELECT timwork
				SEEK mnum


**************************************************************
*   CHECK TO SEE IF THIS IS A RE-DISPATCH ON SAME WO         *
**************************************************************
*
*				IF mnum = no
*					CONTINUE
*				ENDIF
*
**************************************************************
*       CHECK TO SEE IF THIS WORKORDER HAS BEEN USED         * 
**************************************************************

				IF FOUND() 
					IF closedate <> ' '
						STORE ' ' TO ky
						* SET COLOR TO &windowatr
						DO work_say
						* SET COLOR TO &hiliteatr
						? CHR(7)
						WAIT WINDOW "This Workorder already exists" + CHR(13) + CHR(13) +" Press any key to continue"
						LOOP
					ENDIF
				ELSE
*					SELECT timwork
					GOTO recordnumber
					STORE phone TO mphone
					STORE area TO marea

					STORE city TO mcity
					STORE state TO mstate
					STORE zip TO mzip
					STORE DTOC(DATE())+' '+LEFT(TIME(),5) TO mdispdate
					IF CANCEL = 1
						STORE '  ' TO cc
						*						SAVE SCREEN TO SCREEN
						@ 1,20 TO 23,80 CLEAR
						* SET COLOR TO &windowatr
						@ 0,21 TO 23,80 DOUBLE
						CLEAR TYPEAHEAD
						* SET COLOR TO &screenatr
						STORE 3 TO x
						STORE 22 TO y
						STORE DBF() TO DATABASE
						SELECT timcancl
						GOTO TOP
						* SET COLOR TO &popupatr
						@ 1,22 SAY 'CODE    DESCRIPTION          CODE    DESCRIPTION         '
						DO WHILE .NOT. EOF()
							* SET COLOR TO &promptatr
							IF x > 21
								STORE 3 TO x
								STORE 51 TO y
							ENDIF
							IF x > 42
								@ x,24 SAY 'More....  Press any key to continue' GET mref PICTURE '!!!!!!!'
								READ
								STORE 3 TO x
								STORE 22 TO y
							ENDIF
							@ x,y SAY cancel_cod
							* SET COLOR TO &screenatr
							@ x,y+8 SAY descriptio
							STORE x+1 TO x
							SKIP 1
						ENDDO
						STORE '  ' TO cc
						DO WHILE .T.
							@ 20,0 SAY 'ENTER CANCEL CODE' GET cc PICTURE '@k XX'
							READ
							STORE UPPER(cc) TO mcc
							IF mcc = 'X '
								CLOSE DATABASES
								RETURN
							ENDIF
							IF mcc = '  '
								SET COLOR TO R+/N
								@ 22,40 SAY '  A CANCEL CODE MUST BE ENTERED  '
								? CHR(7)
								? CHR(7)
								SET COLOR OF SCHEME 1 TO
								LOOP
							ENDIF
							GOTO TOP
							FIND &mcc
							IF EOF()
								LOOP
							ELSE
								STORE descriptio TO mdescriptio
								EXIT
							ENDIF
						ENDDO
						*						RESTORE SCREEN FROM SCREEN
						SELECT timwork
						GOTO recordnumber
						REPLACE comments WITH mdescriptio + comments
						UNLOCK ALL
						@ 22,0 SAY 'Enter Reason for Cancel'
						@ 23,0 GET comments
						READ
						REPLACE notes WITH notes + CHR(13) +'Cancelled on '+DTOC(DATE()) +' by '+ALLTRIM(operator)+ ' '+ mcc
						REPLACE pddate WITH DATE()
						REPLACE cancel_cod WITH mcc
						REPLACE paid WITH 'CNCL    '
						REPLACE closedate WITH DTOC(DATE())+' '+LEFT(TIME(),5)
						REPLACE cleared WITH 'Y'
						REPLACE technician WITH memp_no

						* REPORT FORM TIMDISPA TO PRINT

						STORE 0 TO CANCEL
						UNLOCK ALL
						SELECT timpool
						GOTO recordpointer
						DELETE
						ON KEY
						USE
						CLOSE DATABASES
						RETURN
					ENDIF


					****************************************************
					*                                                  * 
					*    Send Dispatch information to Nextel Phone     *
					*                                                  * 
					****************************************************
					STORE 'N' TO sendpage
					DEFINE WINDOW sendpage FROM 23,20 TO 27,87 TITLE " SEND TEXT MESSAGE ? " SYSTEM
					ACTIVATE WINDOW sendpage
					@ 1,2 SAY 'Would you like to Send this dispatch to a TEXT MESSAGE? Y/N ' GET sendpage PICTURE '!'
					READ
					STORE 'V' TO vt
					DEACTIVATE WINDOW sendpage
					RELEASE WINDOW sendpage
						STORE ALLTRIM(STR(rate)) TO c_rate
						IF rate = 59.50
							STORE '59.50' TO c_rate
						ENDIF
						IF rate = 69.50
							STORE '69.50' TO c_rate
						ENDIF
						IF rate = 79.50
							STORE '79.50' TO c_rate
						ENDIF
						IF rate = 89.50
							STORE '89.50' TO c_rate
						ENDIF
						IF rate = 99.50
							STORE '99.50' TO c_rate
						ENDIF

						STORE ALLTRIM(mnum) + ' : ' + ALLTRIM(priority1) + CHR(13) + ALLTRIM(incident) + ' : ' + ALLTRIM(ref) + chr(13) + ALLTRIM(area) + '-' + ALLTRIM(phone) + chr(13) + ALLTRIM(name) TO _CLIPTEXT
						IF Empty(Company)
							STORE _CLIPTEXT + CHR(13) + ALLTRIM(street) + CHR(13) + ALLTRIM(city) + ' ' + ALLTRIM(state) + ' ' + ALLTRIM(zip) + CHR(13) + ALLTRIM(xstreet) + chr(13) + ;
							ALLTRIM(c_rate) + ' per half' + chr(13) + ALLTRIM(jobdesc1) TO _CLIPTEXT
						ELSE
							STORE _CLIPTEXT + chr(13) + ALLTRIM(contact) + chr(13) + ALLTRIM(street) + CHR(13) + ALLTRIM(city) + ' ' + ALLTRIM(state) + ' ' + ALLTRIM(zip) + CHR(13) + ALLTRIM(xstreet) + chr(13) + ;
							ALLTRIM(c_rate) + ' per half' + chr(13) + ALLTRIM(jobdesc1) TO _CLIPTEXT						
						ENDIF


					IF UPPER(sendpage) = 'Y'

					******************************************************************************************
					*                                                                                        * 
					*    Popup window lists phone numbers and gives a choice of which text program is used   *
					*                                                                                        * 
					******************************************************************************************


						DEFINE WINDOW TEXTMESSAGE FROM 10,05 TO 50,80 TITLE 'List of Technicians phones';
						CLOSE FLOAT GROW SHADOW ZOOM SYSTEM
						ACTIVATE WINDOW TEXTMESSAGE
						SET DELIMITERS ON
						STORE 'V' TO vt
						DO WHILE .T.
							CLEAR
							@ 5,3 SAY 'EMP #       NAME           CELLULAR'
							SET DELETED ON
							SELECT employee
							SET ORDER TO 2
							set headings off
							DISP ALL FIELDS  emp_no,LEFT(name,15),+'  '+LEFT(carea,5),LEFT(cphone,8) OFF
							@ 1,5 SAY 'Would you like to use Verizon or T-Mobile      [V] or [T] or [E]' get vt picture 'X'
							READ
							IF UPPER(VT) = 'V'
								EXIT
							ENDIF
							IF UPPER(VT) = 'T'
								EXIT
							ENDIF
							IF UPPER(VT) = 'E'
								EXIT
							ENDIF
						ENDDO
						DO CASE
							CASE UPPER(VT) = 'V'
								run "C:\Progra~1\Intern~1\iexplore.exe http://www.vtext.com/"
							CASE UPPER(VT) = 'T'
								RUN "C:\Progra~1\Intern~1\iexplore.exe http://www.textem.net/"
					
						ENDCASE	
						RELEASE WINDOW
						SET DELIMITERS OFF
					ENDIF
					SELECT TIMWORK
					GOTO recordnumber
			
					IF UPPER(VT) = 'V'
						REPLACE notes WITH notes +CHR(13) + "Sent to Verizon " +chr(13)+ _CLIPTEXT
					ENDIF
					IF UPPER(VT) = 'T'
						REPLACE notes WITH notes +CHR(13) + "Sent to T-Mobile " +chr(13)+ _CLIPTEXT
					ENDIF

     				****************************************************
					*                                                  * 
					*** End Send Dispatch ***
					*                                                  * 
					****************************************************




					REPLACE notes WITH notes +CHR(13) + 'Dispatched to '+ALLTRIM(memp_no)+' on '+DTOC(DATE()) +' by '+ALLTRIM(operator) + ' on '+mnum
					REPLACE no WITH mnum
					REPLACE technician WITH memp_no
					REPLACE dispdate WITH mdispdate


					******  this was added to try to link estimates to calls 04/06/06 *******

					GOTO TOP
					LOCATE ALL FOR area = marea .AND. ALLTRIM(phone) = ALLTRIM(mphone) .AND. priority='02' .AND. DATE > DATE()-30  .AND. no=LINK
					IF .NOT. EOF()
						STORE .F. TO mexit
						STORE 'Y' TO yn
						STORE no TO mlink
						? CHR(7)
						DEFINE WINDOW linkest FROM 1,1 TO 9,54 TITLE 'Estimate within 30 days for this Customer';
							CLOSE FLOAT GROW SHADOW ZOOM SYSTEM

						ACTIVATE WINDOW linkest
						@ 1,3 SAY '  WO   INCIDENT    DATE'
						@ 3,3 SAY no+' '+incident+' ' +DTOC(DATE)
						@ 4,3 SAY jobdesc1
						@ 6,5 SAY 'Attach this estimate to this job?  Y/N ' GET yn PICTURE '!'
						READ
						RELEASE WINDOW linkest
						GOTO recordnumber
						IF UPPER(yn) = 'Y'
							REPLACE LINK WITH mlink
							REPLACE priority WITH '02'
							REPLACE notes WITH notes +CHR(13) + 'linked to estimate workorder number '+ALLTRIM(mlink)+' on '+DTOC(DATE()) +' by '+ALLTRIM(operator) + ' on '+mnum
							GOTO TOP
							LOCATE ALL FOR no = mlink
							IF .NOT. EOF()
								REPLACE LINK WITH mnum
								REPLACE notes WITH notes +CHR(13) + 'linked this estimate to workorder number '+ALLTRIM(mnum)+' on '+DTOC(DATE()) +' by '+ALLTRIM(operator)
							ENDIF
							GOTO recordnumber


						ELSE
							REPLACE LINK WITH mnum
						ENDIF
					ENDIF



					******  end add  ******



					* REPORT FORM TIMDISPA TO PRINT


					IF mref = 'RPT'
						STORE mcommission + 05 TO mcommission
					ENDIF
					IF mpriority = '02'
						STORE mcommission + 05 TO mcommission
					ENDIF
					IF mpriority = '05'
						STORE mcommission + 10 TO mcommission
					ENDIF
					REPLACE wage WITH mcommission


					*  LIST memo to print

					*				IF PRIORITY = '02'
					* 					SCATTER MEMVAR MEMO
					*		    		 USE TIMESTI
					*					 APPEND BLANK
					*				     GATHER MEMVAR MEMO
					*				ENDIF

					*  LIST memo to print



					UNLOCK ALL
					*					ON ERROR DO ERRHAND

					****  FIND EXCLUSIVE .T. OR .F. *****

					SELECT timpool
					GOTO recordpointer
					DELETE
				ENDIF



				ON ERROR

				****** END FIND ******

				* SET COLOR TO &promptatr


				*****  CALCULATE PERFORMANCE REPORTS  *************
				SELECT timwork
				SUM ALL amount FOR DATE >= DATE()-7 .AND. DATE <= DATE() .AND. technician = memp_no TO weektotal
				COUNT ALL FOR DATE >= DATE()-7 .AND. DATE <= DATE() .AND. technician = memp_no TO weekavg

				SUM ALL amount FOR DATE >= DATE()-30 .AND. DATE <= DATE() .AND. technician = memp_no TO monthtotal
				COUNT ALL FOR DATE >= DATE()-30 .AND. DATE <= DATE() .AND. technician = memp_no TO monthavg

				STORE (SUBSTR(DTOC(DATE()),4,2)) TO firstofmonth

				SUM ALL amount FOR DATE >= DATE()-VAL(firstofmonth) .AND. DATE <= DATE() .AND. technician = memp_no TO thismonth
				COUNT ALL FOR DATE >= DATE()-VAL(firstofmonth) .AND. DATE <= DATE() .AND. technician = memp_no TO thismoavg

				@ 22,1 TO 25,80 DOUBLE
				@ 22,65 SAY TIME()
				SET COLOR TO W+/B
				@ 23,3 SAY "Last 7 days $" + ALLTRIM(STR(weektotal)) + ".00    * Last 30 days $" + ALLTRIM(STR(monthtotal)) + ".00    * This Month $" + ALLTRIM(STR(thismonth)) + ".00"
				@ 24,3 SAY "Last 7 days $" + ALLTRIM(STR(weektotal/weekavg)) + ".00    * Last 30 days $" + ALLTRIM(STR(monthtotal/monthavg)) + ".00    * This Month $" + ALLTRIM(STR(thismonth/thismoavg)) + ".00"

				SET COLOR OF SCHEME 1 TO
				STORE 'N' TO ah
				@ 26,0 CLEAR
				@ 26,10 SAY 'After Hours Call?                       ' GET ah PICTURE '!'
				READ
				@ 22,1 TO 26,90 CLEAR
				REPLACE afterhours WITH ah




				*				IF UPPER(ky) = 'P'
				*					SELECT timwork
				*					DO timinv
				*					SELECT timpool
				*				ENDIF

				*!*		CONVERT TIME() TO TIME() +1 HOUR

*				IF VAL(LEFT(TIME(),2))+1 > 24
*					STORE "00:"+SUBSTR(TIME(),4,2)) TO meta
*				ELSE
					STORE ALLTRIM(STR(INT(VAL(LEFT(TIME(),2)))+1)+":"+SUBSTR(TIME(),4,2)) TO meta
*				ENDIF

				@ 23,0 CLEAR
				@ 23,5 SAY 'Estimated Time of Arrival  '
				@ 23,33 GET meta PICTURE '@k 99:99'
				READ
				@ 23,0 CLEAR

				**   UPDATE EMPLOYEE TRACKING
				IF memp_no < '1'
					@ 23,5 SAY 'ELECTRICIAN' GET memp_no PICTURE '@k XXXXXXX'
					READ
					@ 23,0 CLEAR
				ENDIF

				SELECT timtrack
				SET ORDER TO 1
				SEEK memp_no
				IF EOF()
					APPEND BLANK
				ENDIF
				REPLACE DATE WITH DATE()
				REPLACE technician WITH memp_no
				REPLACE invoice WITH mnum
				REPLACE name WITH mname
				REPLACE city WITH mcity
				REPLACE timein WITH TIME()
				REPLACE dispatched WITH DATE()
				REPLACE disp_by WITH operator
				REPLACE TIMEOUT WITH ' '
				REPLACE closed WITH CTOD('  /  /    ')
				REPLACE closed_by WITH ' '
				UNLOCK ALL
				**** END TRACKING
				EXIT

			ENDDO
			EXIT
		ENDIF
		ON KEY
	ENDIF
	STORE 2 TO CNT
ENDDO
SET HEADING ON
@ 23,0 CLEAR
* SET COLOR TO &promptatr
@ 23,0 SAY '*****  Please Wait  *****'
* SET COLOR TO &screenatr
CLEAR
ON KEY
CLOSE DATABASES

PROCEDURE f9key

REPLACE notes WITH notes + CHR(13)+CHR(13)+'-- Viewed by '+ALLTRIM(operator)+' on '+DTOC(DATE())+' at '+TIME()+' from dispatch screen --'+CHR(13)
MODIFY MEMO notes

*: EOF: TIMDISPA.PRG
