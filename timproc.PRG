
*:*********************************************************************
*:
*: Procedure file: TIMPROC.PRG
*:
*:         System: TIM
*:         Author: CARL PRATT
*:      Copyright (c) 1993, CARL PRATT
*:  Last modified: 02/07/93     20:33
*:
*:  Procs & Fncts: SECURITY
*:               : LOGO
*:               : ADDTITLE
*:               : DBF_CHK
*:               : NOTCOMPLETE
*:               : WORKMENU
*:               : WORK_STO
*:               : ERRHAND
*:               : WORKSAVE
*:               : DATASTOR
*:               : WORK_GET
*:               : WORK_SAY
*:               : WORKMSAY
*:               : STOC_STO
*:               : STOCMENU
*:               : STOCSCRN
*:               : STOCSTOR
*:               : STOCK_GET
*:               : STOC_GET
*:               : STOC_SAY
*:               : TS_SAVE
*:               : H_SAVE
*:               : S_SAVE
*:               : DISPFORM
*:               : DISPREC
*:               : EDITREC
*:               : BLANKREC
*:               : REPLREC
*:               : ADDREC
*:               : CHKRMBRS
*:
*:         Set by: TIMDMENU.PRG
*:               : TIMADVER.PRG
*:
*:     Documented: 02/21/93 at 04:39               FoxDoc version 1.0
*:*********************************************************************
SET EXCLUSIVE OFF
*!*********************************************************************
*!
*!      Procedure: SECURITY
*!
*!      Called by: TIMDMENU.PRG
*!
*!           Uses: TIMUSER.DBF
*!
*!        Indexes: TIMUSERN.NDX
*!
*!*********************************************************************




PROCEDURE security
*  ---CHECK FOR THE EXISTANCE OF USER AND INDEX
*	SELECT 1
IF USED("timuser")
	SELECT timuser
	SET ORDER TO 1

ELSE
	SELECT 1
	USE (LOCFILE("timuser.dbf","DBF","Where is timuser?"));
		AGAIN ALIAS timuser ;
		ORDER 1
ENDIF


STORE 'T' TO ky
STORE '     ' TO aorder
STORE '           ' TO msecurity
STORE '          ' TO mpassword
STORE 0 TO ms_level
STORE ' ' TO H
STORE 0 TO naccess
COUNT TO CNT
IF CNT = 0
	SET DELIMITERS ON
	SET DELIMITERS TO '[]'
	STORE '                    ' TO mname
	STORE '   ' TO mid
	@ 4,4 TO 9,55 CLEAR
	@ 4,4 TO 9,55
	@ 5,6 SAY "Please enter id code" GET mid PICTURE '!!!'
	@ 7,6 SAY "Please enter user's name" GET mname PICTURE '!!!!!!!!!!!!!!!!!!!'
	@ 8,6 SAY "Please enter password" GET msecurity PICTURE 'XXXXXXXXXXX'
	READ
	SET DELIMITERS OFF
	STORE UPPER(mid) TO id
	STORE UPPER(mname) TO name
	STORE id TO operator
	IF RIGHT(serial,11) <> LEFT(msecurity,11)
		CLEAR
		@ 5,10 SAY '*** A C C E S S   D E N I E D ***'
		@ 7,10 SAY 'for more information call TIM SOFTWARE'
		@ 8,10 SAY '            (801) 561-9594'
		STORE 3000 TO counter
		DO WHILE counter > 0
			STORE counter - 1 TO counter
		ENDDO
		QUIT
	ENDIF
	STORE msecurity + '-045-'+'TIM*' TO msecurity
	STORE msecurity TO security
	= LOCK(1)
	APPEND BLANK
	REPLACE id WITH mid
	REPLACE name WITH mname
	REPLACE security WITH msecurity
	REPLACE r_level WITH mr_level
	UNLOCK IN timuser
	SET TALK OFF
	*	STORE 'Y' TO YN
	*	@ 22,0 TO 24,80 CLEAR
	*	@ 22,0 TO 24,80 DOUBLE
	*	STORE TIME() TO TIME
	*	STORE LEFT(TIME,2) TO HR
	*	STORE SUBSTR(TIME,4,2) TO MN
	*	STORE VAL(HR)+(VAL(MN)/100) TO TIME
	*	@ 23,11 SAY "DO YOU WISH TO CLOCK IN? "+TIME+" Y / N"   GET YN PICTURE '!'
	*	READ
	*	IF YN = 'Y'
	*		USE TIMTIME
	*		GOTO BOTTOM
	*		STORE PAYPERIOD TO M.PAYPERIOD
	*		@ 23,0 TO 23,80 CLEAR
	*		@ 23,11 SAY "ENTER THE NEXT PAYPERIOD? " GET M.PAYPERIOD PICTURE '  /  /    '
	*		READ
	*		APPEND BLANK
	*		REPLACE TECHNICIAN WITH MID
	*		REPLACE DAY WITH CDOW(DATE())
	*		REPLACE DATE WITH DATE()
	*		REPLACE WO WITH '535'
	*		REPLACE TIME_IN WITH TIME
	*		REPLACE MODFIED WITH "COMPUTER"
	*		REPLACE PAYPERIOD WITH M.PAYPERIOD
	*	ENDIF

	RETURN
ENDIF
STORE 'Y' TO ky
DO WHILE ky <> 'Y'
	IF FILE('timuser.dbf')
		SELECT timuser
		STORE SUBSTR(security,13,3) TO ACCESS
		STORE VAL(ACCESS) TO naccess
		IF naccess = 999
			STORE id TO USER
			STORE name TO operator
			STORE s_level TO ms_level
			IF FCOUNT() = 4
				STORE security TO msecurity
				IF RIGHT(serial,11) <> LEFT(msecurity,11)
					CLEAR
					@ 5,10 SAY '*** A C C E S S   D E N I E D ***'
					@ 7,10 SAY 'for more information call TIM SOFTWARE'
					@ 8,10 SAY '            (801) 561-9594'
					STORE 3000 TO counter
					DO WHILE counter > 0
						STORE counter - 1 TO counter
					ENDDO
					CLEAR
					QUIT
				ENDIF
			ENDIF
			EXIT
		ENDIF
		IF naccess < 1
			CLEAR
			@ 5,10 SAY '*** A C C E S S   D E N I E D ***'
			@ 7,10 SAY 'for more information call TIM SOFTWARE'
			@ 8,10 SAY '            (801) 561-9594'
			STORE 3000 TO counter
			DO WHILE counter > 0
				STORE counter - 1 TO counter
			ENDDO
			CLEAR
			QUIT
		ENDIF
	ENDIF
	CLEAR
	@ 1,57 SAY VERSION
	STORE id TO USER
	@ 20,24 SAY 'PLEASE ENTER YOUR ID CODE'
	*	SET COLOR TO N/W
	@ 20,50 GET USER PICTURE '!!!'
	READ
	*	SET COLOR TO G/W
	@ 24,8 SAY "                                        T I M                               "
	IF LEFT(USER,1) = '*'
		CLEAR

		@ 10,5 SAY 'THANK YOU FOR YOUR INTEREST, PLEASE CALL CARL AT (801) 561-9594'
		@ 11,5 SAY ' ******** SIGNING OFF ********* '
		STORE 0 TO ct
		DO WHILE ct < 300
			STORE ct + 1 TO ct
		ENDDO
		CLEAR
		QUIT
	ENDIF
	IF USER = '   '
		LOOP
	ENDIF
	IF FILE('TIMUSER.DBF')
		SELECT timuser
		FIND &user
		IF EOF()
			@ 5,10 SAY '*** A C C E S S   D E N I E D ***'
			? '********** THIS USER DOES NOT HAVE ACCESS AT THIS TIME **********'
			STORE 1 TO ct
			DO WHILE ct < 50
				STORE ct+1 TO ct
			ENDDO
			STORE 'N' TO ky
			LOOP
		ENDIF
		STORE id TO USER
		STORE name TO operator
		STORE s_level TO ms_level
		STORE 'Y' TO ky
	ELSE
		? '     ************  THIS VERSION NEEDS TO BE INSTALLED  *************'
		STORE 1 TO ct
		DO WHILE ct < 50
			STORE ct+1 TO ct
		ENDDO
	ENDIF
ENDDO
*IF FCOUNT() = 5
*	IF naccess <> 999
*		STORE naccess - 1 TO naccess
*		STORE STR(naccess) TO naccess
*		STORE RIGHT(naccess,3) TO ACCESS
*		STORE LEFT(security,12) + ACCESS + RIGHT(security,5) TO msecurity
*		REPLACE security WITH msecurity
*	ENDIF
*ENDIF

*** --- enter security information
SET COLOR TO G/N
SET TALK OFF
SET ECHO OFF
STORE 0 TO trys
DO WHILE .T.
	STORE trys + 1 TO trys
	IF trys > 3
		CLEAR
		QUIT
	ENDIF
	SET DELIMITERS ON
	SET DELIMITERS TO '[]'
	STORE '          ' TO mpassword
	STORE '                    ' TO mname
	STORE '   ' TO mid
	@ 4,4 TO 9,55 CLEAR
	@ 4,4 TO 9,55
	@ 5,6 SAY "Please enter id code" GET mid PICTURE '!!!'
	@ 7,6 SAY "Please enter user's name" GET mname PICTURE '!!!!!!!!!!!!!!!!!!!'
	@ 8,6 SAY "Please enter password"
	READ
	IF mid = 'Q  '
		CLEAR
		CANCEL
	ENDIF
	IF mid = "NEW"
		@ 4,6 SAY "NEW USER"
		@ 5,6 SAY "PLEASE ENTER ID CODE" GET mid PICTURE '!!!'
		@ 7,6 SAY "Please enter user's name" GET mname PICTURE '!!!!!!!!!!!!!!!!!!!'
		@ 8,6 SAY "Please enter password"
		READ
		STORE security TO msecurity
		SET COLOR TO N/N
		@ 8,30 GET mpassword
		READ
		SET COLOR TO N/W*

		APPEND BLANK
		REPLACE id WITH mid
		REPLACE name WITH mname
		REPLACE password WITH mpassword
		REPLACE security WITH msecurity
		REPLACE s_level WITH ms_level
		UNLOCK
		LOOP
	ENDIF

	SET DELIMITERS OFF
	SET BELL OFF
	SET COLOR TO N/N
	@ 8,30 GET mpassword
	READ
	SET COLOR TO G/N
	SET BELL ON

	*---  check id code for existance

	SELECT timuser
	IF mid = '   '
		LOOP
	ENDIF
	FIND &mid
	IF FOUND() = .T.
		IF name = mname
			IF LTRIM(mpassword) = RTRIM(password)
				STORE s_level TO ms_level

				*	SELECT 0
				IF USED("timlog")
					SELECT timlog
					SET ORDER TO 0

				ELSE
					SELECT 0
					USE (LOCFILE("timlog.dbf","DBF","Where is timlog?"));
						AGAIN ALIAS timlog ;
						ORDER 0
				ENDIF

				APPEND BLANK
				REPLACE name WITH mname
				REPLACE DATE WITH DATE()
				REPLACE TIME WITH TIME()
				REPLACE descrip WITH 'Log On'
				REPLACE try WITH trys
				REPLACE dayofweek WITH CDOW(DATE())
				USE
				EXIT
			ENDIF
		ENDIF
	ELSE
		LOOP
	ENDIF
	@ 4,4 TO 9,55 CLEAR
	@ 4,4 TO 9,55
	@ 7,10 SAY ' ****   ACCESS DENIED   ****'

ENDDO
SET TALK OFF
STORE mname TO operator











*!*********************************************************************
*!
*!      Procedure: LOGO
*!
*!      Called by: TIMDMENU.PRG
*!               : DBF_CHK          (procedure in TIMPROC.PRG)
*!               : TIMSRCWK.PRG
*!               : TIMLSTWK.PRG
*!               : TIMSRCHD.PRG
*!               : TIMPRINV.PRG
*!               : TIMBRWSE.PRG
*!               : TIMINQUR.PRG
*!               : TIMRORDR.PRG
*!               : TIMBCKUP.PRG
*!               : TIMADVER.PRG
*!
*!*********************************************************************
PROCEDURE LOGO
CLOSE DATABASES
SET COLOR TO &screenatr
@ 13,0 TO 13,80 CLEAR
@ 13,7 SAY '        IIIII               IIIII        IIIII    IIIII    IIIII '
@ 12,0 TO 12,80 CLEAR
@ 12,7 SAY '        IIIII               IIIII        IIIII    IIIII    IIIII '
@ 14,0 TO 14,80 CLEAR
@ 14,7 SAY '       MMMMMMM           MMMMMMMMMMM    MMMMMMM           MMMMMMM'
@ 11,0 TO 11,80 CLEAR
@ 11,7 SAY '        IIIII               IIIII        IIIII   III III   IIIII '
@ 15,0 TO 15,80 CLEAR
@ 15,7 SAY '       MMMMMMM           MMMMMMMMMMM    MMMMMMM           MMMMMMM'
@ 10,0 TO 10,80 CLEAR
@ 10,7 SAY 'TT      IIIII      TT       IIIII        IIIII  III   III  IIIII '
@ 16,0 TO 16,80 CLEAR
@ 16,7 SAY '       MMMMMMM           MMMMMMMMMMM    MMMMMMM           MMMMMMM'
@ 9,0 TO 9,80 CLEAR
@ 9,7 SAY 'TTT     IIIII     TTT       IIIII        IIIII III     III IIIII '
@ 17,0 TO 17,80 CLEAR
@ 17,7 SAY '                                                                 '
@ 8,0 TO 8,80 CLEAR
@ 8,7 SAY 'TTTTTTTTTTTTTTTTTTTTT    TTTTTTTTTTT    TTTTTTTTT       TTTTTTTTT'
@ 18,0 TO 18,80 CLEAR
@ 18,7 SAY '           Truck Inventory Maintenance Program  Ver           '
@ 7,0 TO 7,80 CLEAR
@ 7,7 SAY 'TTTTTTTTTTTTTTTTTTTTT    TTTTTTTTTTT    TTTTTTTT         TTTTTTTT'
@ 19,0 TO 19,80 CLEAR
@ 19,7 SAY '                      Written by Carl Pratt                   '
@ 6,0 TO 6,80 CLEAR
@ 6,7 SAY 'TTTTTTTTTTTTTTTTTTTTT    TTTTTTTTTTT    TTTTTTT           TTTTTTT TM'
SET COLOR TO &windowatr
SET COLOR TO G+/B
STORE TRIM(dcompany) TO dcompany
STORE 'This Program is Licensed to '+dcompany TO tcompany
MODIFY WINDOW SCREEN TITLE LEFT(dcompany,12)
STORE 1 TO x
STORE (80-LEN(tcompany))/2 TO x
@ 13,x-2 TO 15,x+LEN(tcompany)+1 CLEAR
@ 13,x-2 TO 15,x+LEN(tcompany)+1
@ 14,x SAY tcompany
SET COLOR TO &screenatr
@ 18,59 SAY VERSION
@ 18,65 SAY 'System Date'
@ 19,67 SAY DATE()
@ 18,5 SAY 'Today is'
@ 19,5 SAY CDOW(DATE())
SET COLOR TO &statusatr
@ 20,0 TO 20,80 CLEAR
@ 21,0 TO 21,80 CLEAR
@ 5,0 TO 5,80 CLEAR
@ 5,0 TO 20,79
@ 17,15 TO 20,63
@ 17,0 TO 20,15
@ 17,63 TO 20,79
@ 22,0 TO 22,80 CLEAR
@ 4,0 TO 4,80 CLEAR
@ 23,0 TO 23,80 CLEAR
@ 3,0 TO 3,80 CLEAR
@ 24,0 TO 24,80 CLEAR
@ 2,0 TO 2,80 CLEAR
@ 1,0 TO 1,80 CLEAR
SET COLOR TO &screenatr










*!*********************************************************************
*!
*!      Procedure: ADDTITLE
*!
*!      Called by: TIMDMENU.PRG
*!               : TIMSALES.PRG
*!               : TIMDISPA.PRG
*!               : TIMCLOSE.PRG
*!               : TIMADDWK.PRG
*!               : TIMSRCWK.PRG
*!               : TIMLSTWK.PRG
*!               : TIMCNGWK.PRG
*!               : TIMWREPO.PRG
*!               : TIMADDST.PRG
*!               : TIMSRCHD.PRG
*!               : TIMPRINV.PRG
*!               : TIMCNGST.PRG
*!               : TIMDELST.PRG
*!               : TIMBRWSE.PRG
*!               : TIMINQUR.PRG
*!               : TIMRIMBS.PRG
*!               : TIMRORDR.PRG
*!               : README.PRG
*!               : TIMBCKUP.PRG

*!               : TIMINDEX.PRG
*!               : TIMADVER.PRG
*!               : TIMPAYRO.PRG
*!               : TIMADCHK.PRG
*!               : TIMAPCHK.PRG
*!
*!*********************************************************************
PROCEDURE addtitle
*  ---DISPLAY TITLE ON THE SCREEN
SET COLOR TO &windowatr
STORE 40 - (LEN(TITLE)/2) TO COL
@ 0,0 TO 0,80 CLEAR
@ 0,COL SAY TITLE
@ 0,68 SAY prg_name
@ 0,1 SAY 'Record #'
STORE 'A' TO add_chse
SET COLOR TO &screenatr






*!*********************************************************************
*!
*!      Procedure: NOTCOMPLETE
*!
*!*********************************************************************
PROCEDURE notcomplete
*  ANY RELEASE NOT COMPLETED RUNS THIS SHORT MESSAGE
CLEAR
STORE 5000 TO timer
STORE 1 TO C
@ 5,5 SAY 'This program is not yet completed'
DO WHILE C < timer
	STORE C+1 TO C
ENDDO
RETURN






*!*********************************************************************
*!
*!      Procedure: WORKMENU
*!
*!      Called by: TIMSALES.PRG
*!               : TIMDISPA.PRG
*!               : TIMCLOSE.PRG
*!               : TIMADDWK.PRG
*!               : TIMCNGWK.PRG
*!
*!*********************************************************************
PROCEDURE workmenu
*  ---DRAW MENU PICTURE
CLEAR
*  ---DRAW BORDER
SET COLOR TO &windowatr
@ 3,0 TO 10,80
*  ---DISPLAY TITLE ON THE SCREEN
@ 0,COL SAY TITLE
@ 0,68 SAY prg_name
@ 0,1 SAY 'Record #'
STORE 'A' TO add_chse
SET COLOR TO &screenatr
@ 2,1 SAY 'Work Order Number  [       ]'
@ 2,32 SAY 'Incident # [        ] - [  ]  Date [  /  /  ]'
@ 4,1 SAY '    W [(   )][         ]     B [(   )][         ]       J [(   )][         ]'
@ 5,1 SAY 'BILL:Name   [                         ] JOB:Name   [                         ]'
@ 6,1 SAY '     Street [                         ]     Street [                         ]'
@ 7,1 SAY '     City   [                         ]     City   [                         ]'
@ 8,1 SAY '     State  [  ]            Zip [     ]     State  [  ]            Zip [     ]'
@ 9,1 SAY '    Contact [                         ]   X STREET [                         ]'
@ 11,20 SAY ' Labor ------- [     ]   X  [      ]                '
@ 12,20 SAY ' Labor ------- [     ]   X  [      ]                '
@ 13,0 SAY 'Payroll'
@ 13,20 SAY 'Discount applied ------- [         ]'
@ 14,20 SAY 'Material --------------- [         ]'
@ 14,0 SAY 'Cleared  [ ]'
@ 15,20 SAY 'Service charge --------- [         ]'
@ 16,20 SAY 'Special equipment ------ [         ]'
@ 17,20 SAY 'Tax paid ------------------ [      ]'
@ 18,0 SAY 'Wage %[      ]'
@ 18,20 SAY 'Preffered card sold? [   ]%[       ]'
@ 19,20 SAY 'Down total ------------- [         ]'
@ 20,20 SAY 'Paid by           '
@ 20,39 SAY '[                ]'
@ 21,0 SAY 'More work[                                                                ]'
@ 22,0 SAY 'Comments [                                                                ]'















*!*********************************************************************
*!
*!      Procedure: WORK_STO
*!
*!      Called by: TIMSALES.PRG
*!               : TIMCLOSE.PRG
*!               : TIMADDWK.PRG
*!               : TIMCNGWK.PRG
*!
*!           Uses: TIMWORK.DBF
*!
*!        Indexes: W_NUM.NDX
*!               : W_PHONE.NDX
*!               : W_BPHONE.NDX
*!               : W_WPHONE.NDX
*!               : W_INCIDENT.NDX
*!               : W_TECH.NDX
*!
*!*********************************************************************
PROCEDURE work_sto




* select 1
IF USED("timwork")
	SELECT timwork
ELSE
	SELECT 1
	USE timwork
ENDIF




STORE DTOC(DATE()) TO mdate
STORE '       ' TO mtechnician
STORE '       ' TO mnum
STORE '                         ' TO mCompany
STORE '                         ' TO mFirstName
STORE '                         ' TO mLastName
STORE '                         ' TO mname
STORE '                         ' TO mstreet
STORE '                         ' TO mcity
STORE '                         ' TO mxstreet
STORE '  ' TO mstate
STORE '     ' TO mzip
STORE '                         ' TO mbname
STORE '                         ' TO mbstreet
STORE '                         ' TO mbcity
STORE '  ' TO mbstate
STORE '     ' TO mbzip
STORE '(   )' TO marea
STORE '        ' TO mphone
STORE '(   )' TO mwarea
STORE '        ' TO mwphone
STORE '(   )' TO mbarea
STORE '        ' TO mbphone
STORE 0.00 TO mmaterial
STORE 0.00 TO mamount
STORE '         ' TO mpaid
STORE '         ' TO mpaidno
STORE 0 TO mhours
STORE 0 TO mrate
STORE SPACE(80) TO mjobdesc1
STORE SPACE(80) TO mjobdesc2
STORE SPACE(80) TO mjobdesc3
STORE 'N' TO mcleared
STORE 0.00 TO mtax
STORE SPACE(65) TO mwork
STORE 0.00 TO mpreferred
STORE 0 TO mprefdisc
STORE 0 TO mhours1
STORE 0 TO mrate1
STORE SPACE(250) TO mcomments
STORE 0 TO mdiscount
STORE 0 TO mat_cost
STORE '  ' TO msector
STORE '        ' TO mincident
STORE 0 TO wage
STORE 0 TO mwageextens
STORE 0 TO mservice
STORE 0.00 TO mspecial
STORE '                         ' TO mcontact
STORE SPACE(4) TO mckn
STORE '  /  /  ' TO mpddate
STORE SPACE(13) TO msolddate
STORE SPACE(13) TO mdispdate
STORE SPACE(13) TO mclosedate
STORE SPACE(7) TO mtechnician
STORE SPACE(80) TO mjobdesc1
STORE SPACE(80) TO mjobdesc2
STORE SPACE(80) TO mjobdesc3
STORE SPACE(3) TO mref
STORE SPACE(2) TO mpriority

*  ---RESET DEFAULT VARIABLES
GOTO BOTTOM
STORE rate TO mrate
STORE rate1 TO mrate1
STORE wage TO mwage
IF mwage = 0
	STORE 25 TO mwage
ENDIF
STORE sector TO msector
STORE service TO mservice
*STORE area TO marea
*STORE warea TO mwarea
*STORE barea TO mbarea
*STORE state TO mstate
*STORE bstate TO mbstate






*!*********************************************************************
*!
*!      Procedure: ERRHAND
*!
*!          Calls: TEST.PRG
*!
*!           Uses: TIMERROR.DBF
*!
**********************************************************************
PROCEDURE errhand
Parameters ERRNUM, message
SET TALK OFF
SET ECHO OFF
STORE ERROR() TO errnum
STORE MESSAGE() TO MESSAGE
STORE DBF() TO olddata
merror = .T.
yesno  = 'Y'
* Define and activate error message window
DEFINE WINDOW err_win FROM 21,00 TO 24,79 ;
	COLOR SCHEME 7
IF DBF() > ' '
	STORE DBF() TO olddata
ENDIF
DO CASE
CASE ERROR() = 125                          && printer errors
	line1 = "The printer is either turned off"
	line2 = "or not online, Do you want to try again"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TO yesno
	RELEASE WINDOW err_win
	IF UPPER(yesno) = 'Y'
		RETRY
	ELSE
		SET CONSOLE ON
		SET PRINT OFF
		SET DEVICE TO SCREEN
		RETURN TO MASTER
	ENDIF
CASE ERROR()=5.or.error()=9.or.error()=20.or.error()=114.or.error()=1683.or.error()=1707 && Index errors
	line1 = "An index file is missing or damaged"
	line2 = "Please run the Reindex utility and try again"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW
	RELEASE WINDOW err_win
	RETURN TO MASTER

CASE ERROR() = 110         && file open by another workstation
	line1 = DBF()+" File is in use by another workstation.  "
	line2 = "Press Enter to keep trying to open this file. "
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TO yesno
	RELEASE WINDOW err_win
	IF UPPER(yesno) = 'Y'
		RETRY
	ELSE
		RETURN
	ENDIF

CASE INLIST(ERROR(), 3, 1705, 1708)   && file open in another work area
	line1 = "Database is in use by another workarea"
	line2 = "Please try later, (notify your system programmer)"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TIMEOUT 10
	RELEASE WINDOW err_win

CASE ERROR() = 108       && file in use by another
	line1 = "The file is in use by another user"
	line2 = "Do you want to try again"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TO yesno
	RELEASE WINDOW err_win
	IF UPPER(yesno) = 'Y'
		RETRY
	ELSE
		RETURN
	ENDIF



CASE ERROR() = 109   && Record in use by another
	line1 = "The record is in use by another user"
	line2 = "Do you want to try again"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TO yesno
	RELEASE WINDOW err_win
	IF UPPER(yesno) = 'Y'
		RETRY
	ELSE
		RETURN
	ENDIF

CASE ERROR() = 1001   && Feature not available
	line1 = "This feature in not available on this version"
	line2 = "Call Tim Software Co. (801) 561-9594"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TIMEOUT 10
	RELEASE WINDOW err_win

OTHERWISE
	line1 = "An error has occurred that cannot be defined. Please press (PrtSc)"
	line2 = "to print this screen and call your system administrator for help"
	Line3 = "Let your system administrator know how you get this error"
	ACTIVATE WINDOW err_win
	@ 0,(WCOLS()-LEN(line1))/2 SAY line1
	@ 1,(WCOLS()-LEN(line2))/2 SAY line2
	WAIT WINDOW TIMEOUT 10
	RELEASE WINDOW err_win
** IF AVAIL
*	IGNORE
	RETURN	
ENDCASE
STORE DBF() TO olddata


*	SELECT 0
IF USED("timerror")
	SELECT timerror
ELSE
	SELECT 0
	USE (LOCFILE("timerror.dbf","DBF","Where is timerror?"));
		AGAIN ALIAS timerror ;
		ORDER 0
ENDIF



APPEND BLANK
REPLACE errnum WITH ERROR()
REPLACE errmess WITH MESSAGE()
REPLACE DATE WITH DATE()
REPLACE source WITH olddata
REPLACE PROGRAM WITH SYS(16)
UNLOCK
SELECT &olddata
RETURN






*  ---SAVE WORKSHEET FILE
*!*********************************************************************
*!
*!      Procedure: WORKSAVE
*!
*!      Called by: TIMADDWK.PRG
*!               : TIMCNGWK.PRG
*!
*!*********************************************************************
PROCEDURE worksave
= LOCK()
REPLACE DATE WITH mdate
REPLACE no WITH mnum
REPLACE name WITH mname
REPLACE street WITH mstreet
REPLACE city WITH mcity
REPLACE state WITH mstate
REPLACE zip WITH mzip
REPLACE bname WITH mbname
REPLACE bstreet WITH mbstreet
REPLACE bcity WITH mbcity
REPLACE bstate WITH mbstate
REPLACE bzip WITH mbzip
REPLACE area WITH marea
REPLACE phone WITH mphone
REPLACE warea WITH mwarea
REPLACE wphone WITH mwphone
REPLACE barea WITH mbarea
REPLACE bphone WITH mbphone
REPLACE material WITH mmaterial
REPLACE amount WITH mamount
REPLACE paid WITH mpaid
REPLACE paidno with mpaidno
REPLACE HOURS WITH mhours
REPLACE rate WITH mrate
REPLACE cleared WITH mcleared
REPLACE tax WITH mtax
REPLACE WORK WITH mwork
REPLACE preferred WITH mpreferred
REPLACE prefdisc WITH mprefdisc
REPLACE hours1 WITH mhours1
REPLACE rate1 WITH mrate1
REPLACE comments WITH mcomments
REPLACE discount WITH mdiscount
REPLACE mat_cost WITH tcost
REPLACE sector WITH msector
REPLACE incident WITH mincident
REPLACE wage WITH mwage
REPLACE wageextens WITH (amount-tax)*(wage/100)
REPLACE service WITH mservice
REPLACE special WITH mspecial
REPLACE contact WITH mcontact
REPLACE xstreet WITH mxstreet
REPLACE solddate WITH msolddate
REPLACE dispdate WITH mdispdate
REPLACE closedate WITH mclosedate
REPLACE pddate WITH CTOD(mpddate)
REPLACE technician WITH mtechnician
REPLACE ref WITH mref
REPLACE priority WITH mpriority
REPLACE jobdesc1 WITH mjobdesc1
REPLACE jobdesc2 WITH mjobdesc2
REPLACE jobdesc3 WITH mjobdesc3

IF amount = 0
	REPLACE cleared WITH 'Y'
ELSE
	REPLACE cleared WITH 'N'
ENDIF
UNLOCK
STORE '       ' TO mnum


*  ---RESTORE Mfile FROM file WORKSHEET FILE
*!*********************************************************************
*!
*!      Procedure: DATASTOR
*!
*!      Called by: TIMCLOSE.PRG
*!               : TIMADDWK.PRG
*!               : TIMCNGWK.PRG
*!
*!*********************************************************************
PROCEDURE datastor
STORE DATE TO mdate
STORE no TO mnum
STORE name TO mname
STORE street TO mstreet
STORE city TO mcity
STORE state TO mstate
STORE zip TO mzip
STORE bname TO mbname
STORE bstreet TO mbstreet
STORE bcity TO mbcity
STORE bstate TO mbstate
STORE bzip TO mbzip
STORE area TO marea
STORE phone TO mphone
STORE warea TO mwarea
STORE wphone TO mwphone
STORE barea TO mbarea
STORE bphone TO mbphone
STORE material TO mmaterial
STORE amount TO mamount
STORE paid TO mpaid
STORE paidno TO mpaidno
STORE HOURS TO mhours
STORE rate TO mrate
STORE cleared TO mcleared
STORE tax TO mtax
STORE WORK TO mwork
STORE preferred TO mpreferred
STORE prefdisc TO mprefdisc
STORE hours1 TO mhours1
STORE rate1 TO mrate1
STORE comments TO mcomments
STORE discount TO mdiscount
STORE mat_cost TO mmat_cost
STORE mat_cost TO tcost
STORE sector TO msector
STORE incident TO mincident
STORE wage TO mwage
STORE wageextens TO mwageextens
STORE service TO mservice
STORE special TO mspecial
STORE contact TO mcontact
STORE DTOC(pddate) TO mpddate
STORE solddate TO msolddate
STORE dispdate TO mdispdate
STORE closedate TO mclosedate
STORE technician TO mtechnician
STORE jobdesc1 TO mjobdesc1
STORE jobdesc2 TO mjobdesc2
STORE jobdesc3 TO mjobdesc3
STORE ref TO mref
STORE priority TO mpriority
STORE xstreet TO mxstreet




*  ---DO GET STATEMENTS FOR ADD WORKORDER PRG
*!*********************************************************************
*!
*!      Procedure: WORK_GET
*!
*!      Called by: TIMADDWK.PRG
*!               : TIMCNGWK.PRG
*!
*!*********************************************************************
PROCEDURE   work_get
SET COLOR TO &hiliteatr
@ 23,0 CLEAR
CLEAR GETS
@ 0,9 SAY RECNO()
IF LEN(notes) > 0
	SET COLOR TO &promptatr
	@ 0,57 SAY ' See Notes'
ENDIF
SET COLOR TO &hiliteatr

IF MPHONE <> '        '
	STORE ALLTRIM(mphone) TO mphone
ENDIF
IF MBPHONE <> '        '
	STORE ALLTRIM(mbphone) TO mbphone
ENDIF
IF MWPHONE <> '        '
	STORE ALLTRIM(mwphone) TO mwphone
ENDIF

@ 24,5 SAY '******* FULL SCREEN EDIT *******'
@ 2,21 GET mnum PICTURE '!!!!!!!'
@ 2,44 GET mincident PICTURE '!!!!!!!!'
@ 2,57 GET msector PICTURE '!!'
@ 2,68 GET mdate PICTURE '99/99/99'
@ 4, 8 GET mwarea PICTURE '(999)'
@ 4,15 GET mwphone PICTURE '999-9999'
@ 4,33 GET mbarea PICTURE '(999)'
@ 4,40 GET mbphone PICTURE '999-9999'
@ 4,60 GET marea PICTURE '(999)'
@ 4,67 GET mphone PICTURE '999-9999'
@ 5,14 GET mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 6,14 GET mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 7,14 GET mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 8,14 GET mbstate PICTURE '!!'
@ 8,34 GET mbzip PICTURE '99999'
@ 9,14 GET mcontact PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 5,53 GET mname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 6,53 GET mstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 7,53 GET mcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 8,53 GET mstate PICTURE '!!'
@ 8,73 GET mzip PICTURE '99999'
@ 9,53 GET mxstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 11,36 GET mhours PICTURE '99.99'
@ 11,50 GET mrate PICTURE '99.99'
@ 12,36 GET mhours1 PICTURE '99.99'
@ 12,50 GET mrate1 PICTURE '99.99'
@ 13,46 GET mdiscount PICTURE '999999.99'
@ 14,10 GET mcleared PICTURE '!'
@ 14,46 GET mmaterial PICTURE '999999.99'
@ 15,46 GET mservice PICTURE '999999.99'
@ 16,46 GET mspecial PICTURE '999999.99'
@ 17,49 GET mtax PICTURE '999.99'
@ 18,7 GET mwage PICTURE '999.99'
@ 18,43 GET mprefdisc PICTURE '99'
@ 18,49 GET mpreferred PICTURE '999.99'
@ 19,46 GET mamount PICTURE '999999.99'
@ 20,40 GET mpaid PICTURE '!!!!!!!!!'
@ 20,52 GET mpaidno PICTURE '!!!!!!!!!!'
@ 21,10 GET mwork PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 22,10 GET mcomments
@ 11,0 SAY 'Page 1'
READ

SET COLOR TO &screenatr
@ 11,0 CLEAR
TEXT
     date opened   [             ]     date dispatched   [             ]

     date closed   [             ]     date paid      [        ]

     technician    [       ]               refferal   [   ]     priority [  ]

job description_______________________________________________________________________________



_______________________________________________________________________________
ENDTEXT
SET COLOR TO &hiliteatr

@ 11,0 SAY 'Page 2'
IF ms_level = '1'
	@ 12,20 GET msolddate
	@ 12,58 GET mdispdate
	@ 14,20 GET mclosedate
	@ 14,55 GET mpddate
	@ 16,74 GET mpriority

ELSE
	@ 12,20 SAY msolddate
	@ 12,58 SAY mdispdate
	@ 14,20 SAY mclosedate
	@ 14,55 SAY mpddate
	@ 16,74 SAY mpriority

ENDIF
@ 16,20 GET mtechnician
@ 16,55 GET mref
@ 19,0 GET mjobdesc1
@ 20,0 GET mjobdesc2
@ 21,0 GET mjobdesc3
SET COLOR TO &screenatr


*  ---DO SAY STATEMENTS FOR CHANGE/DELETE WORKORDER PRG
*!*********************************************************************
*!
*!      Procedure: WORK_SAY
*!
*!      Called by: TIMDISPA.PRG
*!               : TIMCLOSE.PRG
*!               : TIMADDWK.PRG
*!
*!*********************************************************************
PROCEDURE   work_say
SET COLOR TO &hiliteatr
@ 2,21 SAY no
@ 2,68 SAY DATE
@ 5,53 SAY name
@ 6,53 SAY street
@ 7,53 SAY city
@ 8,53 SAY state
@ 8,34 SAY bzip
@ 9,53 SAY xstreet
@ 5,14 SAY bname
@ 6,14 SAY bstreet
@ 7,14 SAY bcity
@ 8,14 SAY bstate
@ 8,73 SAY zip
@ 4,60 SAY area PICTURE '(999)'
@ 4,66 SAY ALLTRIM(phone)
@ 4,8 SAY warea PICTURE '(999)'
@ 4,15 SAY ALLTRIM(wphone)
@ 4,33 SAY barea PICTURE '(999)'
@ 4,40 SAY ALLTRIM(bphone)
@ 14,46 SAY material PICTURE '999999.99'
@ 19,46 SAY amount PICTURE '999999.99'
@ 20,40 SAY paid PICTURE '!!!!!!!!!'
@ 20,52 SAY Paidno PICTURE '!!!!!!!!!!'
@ 11,36 SAY HOURS PICTURE '99.99'
@ 11,49 SAY rate PICTURE '999.99'
@ 14,10 SAY cleared
@ 17,49 SAY tax PICTURE '999.99'
@ 21,10 SAY WORK
@ 18,48 SAY preferred PICTURE '9999.99'
@ 18,43 SAY prefdisc PICTURE '99'
@ 12,36 SAY hours1 PICTURE '99.99'
@ 12,49 SAY rate1 PICTURE '999.99'
@ 22,10 SAY comments
@ 13,46 SAY discount PICTURE '999999.99'
@ 2,57 SAY sector
@ 2,44 SAY incident
@ 18,7 SAY wage PICTURE '999.99'
@ 15,46 SAY service PICTURE '999999.99'
@ 16,46 SAY special PICTURE '999999.99'
@ 9,14 SAY contact
SET COLOR TO &screenatr

*  ---DO SAY STATEMENTS FOR ADD WORKORDER PRG
*!*********************************************************************
*!
*!      Procedure: WORKMSAY
*!
*!      Called by: TIMCLOSE.PRG
*!               : TIMADDWK.PRG
*!
*!*********************************************************************
PROCEDURE   workmsay
SET COLOR TO &hiliteatr
@ 2,21 SAY mnum
@ 2,68 SAY mdate
@ 5,53 SAY mname
@ 6,53 SAY mstreet
@ 7,53 SAY mcity
@ 8,53 SAY mstate
@ 8,73 SAY mzip
@ 9,53 SAY mxstreet
@ 5,14 SAY mbname
@ 6,14 SAY mbstreet
@ 7,14 SAY mbcity
@ 8,14 SAY mbstate
@ 8,34 SAY mbzip
@ 4,60 SAY marea PICTURE '(999)'
@ 4,67 SAY mphone
@ 4,8 SAY mwarea PICTURE '(999)'
@ 4,15 SAY mwphone
@ 4,33 SAY mbarea PICTURE '(999)'
@ 4,40 SAY mbphone
@ 14,46 SAY mmaterial PICTURE '999999.99'
@ 19,46 SAY mamount PICTURE '999999.99'
@ 20,40 SAY mpaid PICTURE '!!!!!!!!!'
@ 20,52 SAY Mpaidno PICTURE '!!!!!!!!!!'
@ 11,36 SAY mhours PICTURE '99.99'
@ 11,49 SAY mrate PICTURE '999.99'
@ 14,10 SAY mcleared
@ 17,49 SAY mtax PICTURE '999.99'
@ 21,10 SAY mwork
@ 18,48 SAY mpreferred PICTURE '9999.99'
@ 18,43 SAY mprefdisc PICTURE '99'
@ 12,36 SAY mhours1 PICTURE '99.99'
@ 12,49 SAY mrate1 PICTURE '999.99'
@ 22,10 SAY mcomments
@ 13,46 SAY mdiscount PICTURE '999999.99'
@ 2,57 SAY msector
@ 2,44 SAY mincident
@ 18,7 SAY mwage PICTURE '999.99'
@ 15,46 SAY mservice PICTURE '999999.99'
@ 16,46 SAY mspecial PICTURE '999999.99'
@ 9,14 SAY mcontact
SET COLOR TO &screenatr


*  ---STOCK
*  ---RESET STOCK VARIABLES
*!*********************************************************************
*!
*!      Procedure: STOC_STO
*!
*!      Called by: TIMADDST.PRG
*!               : TIMCNGST.PRG
*!               : TIMDELST.PRG
*!
*!*********************************************************************
PROCEDURE stoc_sto
STORE '               ' TO mnum
STORE '       ' TO mwo
STORE '              ' TO mpacking
STORE '           ' TO msupp
STORE '                    ' TO mname
STORE '               ' TO mpart_no
STORE '                                  ' TO mdesc
STORE 0 TO monhand
STORE 0.00 TO mcost
STORE 0.00 TO mprice
STORE 0 TO mqty
STORE '          ' TO mjobs
STORE 0 TO morder
STORE '         ' TO mnaed_no
STORE '        ' TO mmfer
STORE 0 TO mmrkup
STORE 0 TO msold
STORE '    ' TO mcomodity
STORE '  ' TO munit
STORE 0.00 TO mcol3

*  ---THIS PROGRAM DRAWS SCREEN FOR ADDSTOCK.PRG
*!*********************************************************************
*!
*!      Procedure: STOCMENU
*!
*!      Called by: TIMADDST.PRG
*!
*!*********************************************************************
PROCEDURE stocmenu
SET COLOR TO &windowatr
@ 1,0 TO 6,79 DOUBLE
@ 2,40 TO 5,40
SET COLOR TO &screenatr
@ 2,2 SAY 'JOB #    [       ]'
@ 3,2 SAY 'JOB NAME [                    ]'
@ 4,2 SAY 'TECH     [       ]'
@ 2,45 SAY 'PACKING SLIP # [              ]'
@ 3,45 SAY 'SUPPLIER       [           ]'
@ 7,0 SAY 'PART#      NAED#      DESCRIP             MFER         COST   PRICE  ONHAND  MIN'

*  ---THIS PROGRAM DRAWS MENU FOR EDIT/DELETE STOCK PRG
*!*********************************************************************
*!
*!      Procedure: STOCSCRN
*!
*!      Called by: TIMADDST.PRG
*!               : TIMCNGST.PRG
*!               : TIMDELST.PRG
*!
*!*********************************************************************
PROCEDURE stocscrn
@ 1,0
TEXT


    Supplier [           ]       Commodity [    ]         Unit [  ]

    Part Number [               ]     Naed Number [         ]

    Description [                                   ]  Mfer [        ]

           Qty [     ]         End Col [        ]      Markup [       ]

    Min. Stock [     ]           Price                   Sold

    Qty Onhand
ENDTEXT
SET COLOR TO &windowatr
@ 3,0 TO 15,80
SET COLOR TO &screenatr



*  ---THIS PROGRAM RESTORES TEMP VARIABLES FOR CHANGE/DELETE STOCK PRG
*!*********************************************************************
*!
*!      Procedure: STOCSTOR
*!
*!      Called by: TIMCNGST.PRG
*!               : TIMDELST.PRG
*!
*!*********************************************************************
PROCEDURE stocstor
STORE part_no TO mnum
STORE descrip TO mdesc
STORE price TO mprice
STORE cost TO mcost
STORE supplier TO msupp
STORE onhand TO monhand
STORE ORDER TO morder
STORE naed_no TO mnaed_no
STORE mfer TO mmfer
STORE part_no TO mpart_no
STORE mrkup TO mmrkup
STORE sold TO msold
STORE comodity TO mcomodity
STORE unit TO munit
STORE col3 TO mcol3

*!*********************************************************************
*!
*!      Procedure: STOCK_GET
*!
*!      Called by: TIMADDST.PRG
*!               : TIMCNGST.PRG
*!
*!*********************************************************************
PROCEDURE stock_get
@ 23,0 CLEAR
@ 23,5 SAY '*******  FULL SCREEN EDIT  *******'
@ 4,14 GET msupp PICTURE '!!!!!!!!!!!'
@ 4,44 GET mcomodity PICTURE '9999'
@ 4,64 GET munit PICTURE '!!'
@ 6,17 GET mnum PICTURE '!!!!!!!!!!!!!!!'
@ 6,51 GET mnaed_no PICTURE '999999999'
@ 8,17 GET mdesc PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 8,61 GET mmfer PICTURE '!!!!!!!!'
@ 10,40 GET mcost PICTURE '99999.99'
@ 10,63 GET mmrkup PICTURE '9999.99'
@ 12,16 GET morder PICTURE '99999'
SET COLOR TO &promptatr
@ 12,41 SAY mprice PICTURE '99999.99'
@ 12,65 SAY msold PICTURE '99999'
@ 14,16 SAY monhand PICTURE '99999'
SET COLOR TO &screenatr

*!*********************************************************************
*!
*!      Procedure: STOC_SAY
*!
*!      Called by: TIMCNGST.PRG
*!               : TIMDELST.PRG
*!
*!*********************************************************************
PROCEDURE stoc_say
@ 4,44 SAY mcomodity
@ 4,64 SAY munit
@ 6,17 SAY mnum
@ 8,61 SAY mmfer
@ 6,51 SAY mnaed_no
@ 8,17 SAY mdesc
@ 4,14 SAY msupp
@ 10,40 SAY mcost PICTURE '99999.99'
@ 10,63 SAY mmrkup PICTURE '9999.99'
@ 12,16 SAY morder PICTURE '99999'
SET COLOR TO &promptatr
@ 12,41 SAY mprice PICTURE '99999.99'
@ 12,65 SAY msold PICTURE '99999'
@ 14,16 SAY monhand PICTURE '99999'
SET COLOR TO &screenatr

*  ---THIS PROGRAM GETS FOR ADDSTOCK PROGRAM
*!*********************************************************************
*!
*!      Procedure: STOC_GET
*!
*!      Called by: TIMADDST.PRG
*!               : TIMCNGST.PRG
*!
*!*********************************************************************
PROCEDURE stoc_get
@ 2,12 SAY mjobs
@ 3,12 GET mname PICTURE '!!!!!!!!!!!!!!!!!!!!'
@ 2,61 GET mpacking PICTURE '!!!!!!!!!!!!!!'
@ 3,61 GET msupp PICTURE '!!!!!!!!!!!'

*!*********************************************************************
*!
*!      Procedure: TS_SAVE
*!
*!*********************************************************************
PROCEDURE ts_save
= LOCK()
REPLACE mfer WITH mmfer
REPLACE packing WITH mpacking
REPLACE name WITH mname
REPLACE supplier WITH msupp
REPLACE part_no WITH mnum
REPLACE descrip WITH mdesc
REPLACE cost WITH mcost
IF bestprice = 0
	REPLACE bestprice WITH mcost
	REPLACE bestsuppl WITH msupp
ENDIF
IF mcost < bestprice
	REPLACE bestprice WITH mcost
	REPLACE bestsuppl WITH msupp
ENDIF
REPLACE qty WITH mqty
IF mqty > 0
	CLEAR
	@ 10,10 SAY '******** F A T A L   E R R O R **********'
	@ 12,10 SAY 'THE QUANTITY WILL BE SAVED AS A NEGATIVE #'
	@ 14,10 SAY 'IF YOU CAN DUPLICATE THIS ERROR PLEASE CALL'
	@ 15,10 SAY 'CARL PRATT   (801) 561-9594  AND NOTIFY OF'
	@ 16,10 SAY 'THIS ERROR AND HOW IT OCCURRED!!!!!!      '
	WAIT
ENDIF

REPLACE wo WITH mjobs
REPLACE naed_no WITH mnaed_no
REPLACE mrkup WITH mmrkup
UNLOCK

*!*********************************************************************
*!
*!      Procedure: H_SAVE
*!
*!      Called by: TIMADDST.PRG
*!
*!*********************************************************************
PROCEDURE h_save
= LOCK()

REPLACE mfer WITH mmfer
REPLACE name WITH mname
REPLACE packing WITH mpacking
REPLACE supplier WITH msupp
REPLACE part_no WITH mnum
REPLACE descrip WITH mdesc
REPLACE cost WITH mcost
IF bestprice = 0
	REPLACE bestprice WITH mcost
	REPLACE bestsuppl WITH msupp
ENDIF
IF mcost < bestprice
	REPLACE bestprice WITH mcost
	REPLACE bestsuppl WITH msupp
ENDIF
REPLACE qty WITH mqty
REPLACE wo WITH mjobs
REPLACE naed_no WITH mnaed_no
REPLACE po WITH STR(morder)
REPLACE technician WITH mtech
UNLOCK

*!*********************************************************************
*!
*!      Procedure: S_SAVE
*!
*!      Called by: TIMADDST.PRG
*!               : TIMCNGST.PRG
*!               : TIMDELST.PRG
*!
*!*********************************************************************
PROCEDURE s_save

REPLACE technician WITH mtech
*  REPLACE NO WITH MNO
REPLACE part_no WITH mnum
REPLACE descrip WITH mdesc
REPLACE price WITH mcost*mmrkup
REPLACE cost WITH mcost
IF bestprice = 0
	REPLACE bestprice WITH mcost
	REPLACE bestsuppl WITH msupp
ENDIF
IF mcost < bestprice
	REPLACE bestprice WITH mcost
	REPLACE bestsuppl WITH msupp
ENDIF
REPLACE supplier WITH msupp
REPLACE onhand WITH monhand
REPLACE ORDER WITH morder
REPLACE mrkup WITH mmrkup
REPLACE naed_no WITH mnaed_no
REPLACE mfer WITH mmfer
REPLACE comodity WITH mcomodity
REPLACE unit WITH munit
REPLACE col3 WITH mcost
REPLACE last_ud WITH DATE()
REPLACE pbook WITH 'Y'
UNLOCK

*!*********************************************************************
*!
*!      Procedure: DISPFORM
*!
*!      Called by: ALLIED.PRG
*!               : TIMADVER.PRG
*!               : STATETAX.PRG
*!
*!*********************************************************************
PROCEDURE dispform
CLEAR
SET COLOR TO &screenatr
@ 0, 0 SAY SPACE(80)
@ 0,72 SAY DATE()
SET COLOR TO G+/W,W+/W
@ 2,5 SAY "Employee "
@ 3,5 SAY "S. S. N. "
@ 1,0,4,26 BOX "ÚÄ¿³ÙÄÀ³"
@ 6,2 SAY "Name      "
@ 4,0,21,55 BOX "ÖÄ·º½ÄÓº"
@ 8,2 SAY "Home phone  ("
@ 8,18 SAY ") "
@ 9,2 SAY "Pager phone ("
@ 9,18 SAY ") "
@ 10,2 SAY "Cellular    ("
@ 10,18 SAY ") "
@ 12,2 SAY "Address   "
@ 13,2 SAY "City      "
@ 14,2 SAY "State     "
@ 15,2 SAY "Zip       "
@ 18,2 SAY "Skill     "
@ 18,30 SAY "Hired      "
@ 19,30 SAY "Terminated "
@ 20,2 SAY "Sectors"
@ 20,29 SAY "Commission %"

RETURN


*!*********************************************************************
*!
*!      Procedure: DISPREC
*!
*!      Called by: ALLIED.PRG
*!               : TIMADVER.PRG
*!               : STATETAX.PRG
*!
*!*********************************************************************
PROCEDURE disprec
SET COLOR TO &windowatr
@ 0, 0 SAY "Record: "+SUBSTR(STR(RECNO()+1000000,7),2)
@ 0,18 SAY ne
@ 0,30 SAY 'EMPLOYEES'
SET COLOR TO R+/W,N/W
IF DELETED()
	@ 0,50 SAY "*TERMINATED*"
ELSE
	SET COLOR TO G+/W,N/W
	@ 0,50 SAY " * ACTIVE * "
ENDIF
@ 2,30 SAY "SUPERVISOR'S MENU"
SET COLOR TO W+/W,W+/W
@ 2,14 GET emp_no PICTURE "!!!!!!!"
@ 3,14 GET ssn PICTURE "999-99-9999"
@ 6,12 GET name PICTURE "@!"
@ 8,15 GET harea PICTURE "999"
@ 8,20 GET hphone PICTURE "999-9999"
@ 9,15 GET parea PICTURE "999"
@ 9,20 GET pphone PICTURE "999-9999"
@ 10,15 GET carea PICTURE "999"
@ 10,20 GET cphone PICTURE "999-9999"
@ 12,12 GET address PICTURE "@!"
@ 13,12 GET city PICTURE "@!"
@ 14,12 GET state PICTURE "@!"
@ 15,12 GET zip PICTURE "XXXXX"
@ 18,12 GET skill PICTURE "@!"
@ 18,41 GET hire PICTURE "@D"
@ 19,41 GET terminate PICTURE "@D"
@ 20,12 GET sectors PICTURE "99-99-99-99-99"
@ 20,41 GET commission PICTURE "99.99"
CLEAR GETS
RETURN


*!*********************************************************************
*!
*!      Procedure: EDITREC
*!
*!      Called by: ADDREC           (procedure in TIMPROC.PRG)
*!               : ALLIED.PRG
*!               : TIMADVER.PRG
*!               : STATETAX.PRG
*!
*!*********************************************************************
PROCEDURE editrec
IF DELETED()
	RETURN
ENDIF
@ 2,14 GET emp_no PICTURE "!!!!!!!"
@ 3,14 GET ssn PICTURE "999-99-9999"
@ 6,12 GET name PICTURE "@!"
@ 9,15 GET harea PICTURE "999"
@ 9,20 GET hphone PICTURE "999-9999"
@ 10,15 GET parea PICTURE "999"
@ 10,20 GET pphone PICTURE "999-9999"
@ 12,12 GET address PICTURE "@!"
@ 13,12 GET city PICTURE "@!"
@ 14,12 GET state PICTURE "@!"
@ 15,12 GET zip PICTURE "XXXXX"
@ 18,12 GET skill PICTURE "@!"
@ 18,41 GET hire PICTURE "@D"
@ 19,41 GET terminate PICTURE "@D"
@ 20,12 GET sectors PICTURE "99-99-99-99-99"
@ 20,41 GET commission PICTURE "99.99"
READ
RETURN


*!*********************************************************************
*!
*!      Procedure: BLANKREC
*!
*!      Called by: ADDREC           (procedure in TIMPROC.PRG)
*!               : ALLIED.PRG
*!
*!*********************************************************************
PROCEDURE blankrec
memp_no = SPACE( 7 )
mssn = SPACE (11)
mharea = SPACE( 3 )
mhphone = SPACE( 8 )
mparea = SPACE( 3 )
mpphone = SPACE( 8 )
mname = SPACE( 30 )
maddress = SPACE( 30 )
mcity = SPACE( 20 )
mstate = SPACE( 2 )
mzip = SPACE( 5 )
mskill = SPACE( 10 )
mhire = DATE()
mterminate = DATE()
msectors = SPACE( 14 )
mcommission = 00.00
RETURN


*!*********************************************************************
*!
*!      Procedure: REPLREC
*!
*!      Called by: ADDREC           (procedure in TIMPROC.PRG)
*!
*!*********************************************************************
PROCEDURE replrec
= LOCK()
REPLACE emp_no WITH memp_no
REPLACE ssn WITH mssn
REPLACE harea WITH mharea
REPLACE hphone WITH mhphone
REPLACE parea WITH mparea
REPLACE pphone WITH mpphone
REPLACE name WITH mname
REPLACE address WITH maddress
REPLACE city WITH mcity
REPLACE state WITH mstate
REPLACE zip WITH mzip
REPLACE skill WITH mskill
REPLACE hire WITH mhire
REPLACE terminate WITH mterminate
REPLACE sectors WITH msectors
REPLACE commission WITH mcommission
UNLOCK
RETURN


*!*********************************************************************
*!
*!      Procedure: ADDREC
*!
*!      Called by: ALLIED.PRG
*!               : TIMADVER.PRG
*!               : STATETAX.PRG
*!
*!          Calls: BLANKREC         (procedure in TIMPROC.PRG)
*!               : EDITREC          (procedure in TIMPROC.PRG)
*!               : REPLREC          (procedure in TIMPROC.PRG)
*!
*!*********************************************************************
PROCEDURE addrec
* ---Initialize memvars.
DO blankrec
CLEAR GETS
@ 24,0
@ 24,0 SAY "Press {Ctrl-W} to Exit"
@ 0,50 SAY "            "

* ---Read into memvars.
SET COLOR TO W+/W,W+/W
@ 2,14 GET memp_no PICTURE "!!!!!!!"
READ
LOCATE FOR emp_no = memp_no
IF .NOT. EOF()
	DO editrec
	RETURN
ENDIF
CLEAR GETS
@ 3,14 GET mssn PICTURE "999-99-9999"
@ 6,12 GET mname PICTURE "@!"
@ 9,15 GET mharea PICTURE "999"
@ 9,20 GET mhphone PICTURE "999-9999"
@ 10,15 GET mparea PICTURE "999"
@ 10,20 GET mpphone PICTURE "999-9999"
@ 12,12 GET maddress PICTURE "@!"
@ 13,12 GET mcity PICTURE "@!"
@ 14,12 GET mstate PICTURE "@!"
@ 15,12 GET mzip PICTURE "XXXXX"
@ 18,12 GET mskill PICTURE "@!"
@ 18,41 GET mhire PICTURE "@D"
@ 19,41 GET mterminate PICTURE "@D"
@ 20,12 GET msectors PICTURE "99-99-99-99-99"
@ 20,41 GET mcommission PICTURE "99.99"
READ
* ---Confirm that user wants to append this record.
choice = " "
SET COLOR TO &promptatr
@ 24,0
@ 24,0 SAY "SELECT:  {A}ccept  {I}gnore" GET choice PICTURE "!" VALID( choice $ "AI" )
READ
IF choice = "A"
	* ---Add the new record.
	= LOCK()
	APPEND BLANK
	STORE ne + 1 TO ne
	* ---Replace from memvars
	DO replrec
	UNLOCK
ENDIF
RETURN

*!*********************************************************************
*!
*!      Procedure: CHKRMBRS
*!
*!      Called by: TIMADCHK.PRG
*!               : TIMAPCHK.PRG
*!
*!           Uses: TIMRMBRS.DBF
*!
*!        Indexes: RM_NO.NDX
*!
*!*********************************************************************
PROCEDURE chkrmbrs


*	SELECT 0
IF USED("timrmbrs")
	SELECT timrmbrs
	SET ORDER TO 0
ELSE
	SELECT 0
	USE (LOCFILE("timrmbrs.dbf","DBF","Where is timrmbrs?"));
		AGAIN ALIAS timrmbrs ;
		ORDER 0
ENDIF


LOCATE ALL FOR no = mno
DO WHILE .NOT. EOF()
	IF DTOC(paid) =  '  /  /  '
		SET COLOR TO &screenatr
		@ 1,0 CLEAR
		@ 1,3 SAY "  Electrician's Name                                  DATE"
		@ 3,3 SAY '  w.o. number             Job Name'
		@ 4,3 SAY '                           Address'
		@ 5,3 SAY '                              City'
		@ 6,3 SAY '                             State'
		@ 7,3 SAY '                               Zip'
		@ 9,3 SAY '   List monies expended by electrician,  sales tickets,'
		@ 10,3 SAY '  receipts, cash register slips, copy of work orders etc. '
		@ 11,3 SAY '   must be attached to this form or Electrician will '
		@ 12,3 SAY '   not be reimbursed.'
		@ 14,3 SAY '      LINE #       ITEM                   VENDOR'
		@ 16,3 SAY '      LINE #       ITEM                   VENDOR'
		@ 18,3 SAY '      LINE #       ITEM                   VENDOR'
		@ 20,3 SAY '      LINE #       ITEM                   VENDOR'
		@ 22,3 SAY '                                         TOTAL '
		SET COLOR TO W+/W,N/W
		@ 1,24 SAY e_n PICTURE 'XXXXXXXXXXXXXXXXXXXX'
		@ 1,62 SAY e_date PICTURE '99/99/99'
		@ 3,17 SAY no PICTURE 'XXXXXXX'
		@ 3,39 SAY j_name PICTURE 'XXXXXXXXXXXXXXXXXXXX'
		@ 4,39 SAY j_addr PICTURE 'XXXXXXXXXXXXXXXXXXXX'
		@ 5,39 SAY j_city PICTURE 'XXXXXXXXXXXXXXXXXXXX'
		@ 6,39 SAY j_state PICTURE 'XX'
		@ 7,39 SAY j_zip PICTURE 'XXXXX'
		@ 13,5 SAY v_1 PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		@ 13,62 SAY a_1 PICTURE '$9999.99'
		@ 15,5 SAY v_2 PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		@ 15,62 SAY a_2 PICTURE '$9999.99'
		@ 17,5 SAY v_3 PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		@ 17,62 SAY a_3 PICTURE '$9999.99'
		@ 19,5 SAY v_4 PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		@ 19,62 SAY a_4 PICTURE '$9999.99'
		@ 22,60 SAY TOTAL PICTURE '$999999.99'
	ELSE
		CONTINUE
		LOOP
	ENDIF
	SET COLOR TO &hiliteatr
	@ 24,5 SAY 'Has this Cash Reimburstment been paid?    Y / N' GET yn PICTURE '!'
	SET COLOR TO &screenatr
	READ
	IF yn = 'Y'
		= LOCK()
		REPLACE paid WITH ppe
		UNLOCK
	ENDIF
	CONTINUE
ENDDO

*!*********************************************************************
*!
*!      Procedure: TEMPF9
*!
*!      Called by: TIMSALES.PRG
*!
*!*********************************************************************
PROCEDURE tempf9
DEFINE WINDOW f9screen FROM 1,1 TO 8,52 TITLE 'F9 Notes';
	CLOSE FLOAT GROW SHADOW ZOOM SYSTEM

ACTIVATE WINDOW f9screen
@ 0,0 EDIT m.notes SIZE 6, 50
READ
RELEASE WINDOW f9screen



PROCEDURE paymenu
*  ---DIMENSION HOW PAID POPUP WINDOW
DIMENSION pd(6)
STORE 'NOT DISCUSSED' TO pd(1)
STORE 'CHECK' TO pd(2)
STORE 'CASH' TO pd(3)
STORE 'CREDIT CARD' TO pd(4)
STORE 'BILL' TO pd(5)
STORE 'ESTIMATE' TO pd(6)

STORE 0 TO ky
@ 23,0 TO 23,80 CLEAR
@ 23,0 SAY 'Enter Method of Payment'
@ 8,80 MENU pd,6 TITLE 'HOW WILL YOU BE PAYING TODAY ?'
READ MENU TO ky
STORE pd(ky) TO mpaid


PROCEDURE trademenu
*  ---DIMENSION CHOOSE TRADE POPUP WINDOW
DIMENSION pd(6)
STORE 'Electrical' TO pd(1)
STORE 'Plumbing' TO pd(2)
STORE 'HVAC' TO pd(3)
STORE 'Energy Management' TO pd(4)
STORE 'Security' TO pd(5)
STORE 'Tele / Data' TO pd(6)

STORE 0 TO ky
@ 23,0 TO 23,80 CLEAR
@ 23,0 SAY 'Enter a Trade Code'
@ 8,80 MENU pd,6 TITLE 'Please enter a trade'
READ MENU TO ky
STORE pd(ky) TO mpaid



*: EOF: TIMPROC.PRG



PROCEDURE choosetrade
STORE '  ' TO mtrade
STORE .F. TO mexit
DEFINE WINDOW choosetrade FROM 13,30 TO 23,55 SYSTEM
DO WHILE NOT mexit
	ACTIVATE WINDOW choosetrade
	@ 1,2 PROMPT 'ALL  -  All Trades    '
	@ 2,2 PROMPT 'ELC  -  Electrical    '
	@ 3,2 PROMPT 'PLM  -  Plumbing      '
	@ 4,2 PROMPT 'HVC  -  Heating / Air '
	@ 5,2 PROMPT 'AEM  -  Energy Mngmt  '
	@ 6,2 PROMPT 'SEC  -  Security      '
	@ 7,2 PROMPT 'TEL  -  Tele / Data   '

	MENU TO mchoice
	DEACTIVATE WINDOW choosetrade
	DO CASE

	CASE mchoice = 1
		STORE 'ALL' TO mtrade
		STORE .T. TO mexit
		LOOP


	CASE mchoice = 2
		STORE 'ELC' TO mtrade
		STORE .T. TO mexit
		LOOP

	CASE mchoice = 3
		STORE 'PLM' TO mtrade
		STORE .T. TO mexit
		LOOP

	CASE mchoice = 4
		STORE 'HVC' TO mtrade
		STORE .T. TO mexit
		LOOP

	CASE mchoice = 5
		STORE 'AEM' TO mtrade
		STORE .T. TO mexit
		LOOP

	CASE mchoice = 6
		STORE 'SEC' TO mtrade
		STORE .T. TO mexit
		LOOP

	CASE mchoice = 7
		STORE 'TEL' TO mtrade
		STORE .T. TO mexit
		LOOP

	ENDCASE
ENDDO
RELEASE WINDOW choosetrade

*** ~< End Trade code >~

PROCEDURE FirstLastName
SET CURSOR ON
SET COLOR OF SCHEME 1 TO
DEFINE WINDOW FirstLastName TITLE 'Enter First Name and Last Name ' FROM 1,1 TO 9,50 SYSTEM
ACTIVATE WINDOW FirstLastName
@ 1,2 SAY 'Enter First Name   'GET mFirstName Picture'@K!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 3,2 SAY 'Enter Last Name    'GET mLastName Picture'@K!!!!!!!!!!!!!!!!!!!!!!!!!'
@ 5,2 SAY 'Enter Company Name 'GET mcompany Picture'@K!!!!!!!!!!!!!!!!!!!!!!!!!'

READ

DEACTIVATE WINDOW FirstLastName
RELEASE WINDOW FirstLastName

IF LEFT(mCompany,2) = '  '
	STORE ALLTRIM(mFirstName) +' '+ ALLTRIM(mLastName) to mName
ELSE
	STORE ALLTRIM(mCompany) to mName
	STORE ALLTRIM(mFirstName) +' '+ mLastName to mContact
ENDIF

