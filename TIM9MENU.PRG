*:*****************************************************************************
*:
*: Procedure file: C:\TIM\PRG\TIM1MENU.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 08/21/96 at 17:02:24
*:
*:          Calls: TIMTRANS.PRG
*:               : DBF_CHK            (procedure in TIMPROC.PRG)
*:               : LOGO               (procedure in TIMPROC.PRG)
*:               : TIMSALES.PRG
*:               : TIMDISPA.PRG
*:               : TIMCLOSE.PRG
*:               : TIMADDWK.PRG
*:               : TIMSRCWK.PRG
*:               : TIMLSTWK.PRG
*:               : TIMCNGWK.PRG
*:               : TIMPEND.PRG
*:               : TIMWREPO.PRG
*:               : WORK_STO           (procedure in TIMPROC.PRG)
*:               : WORKMENU           (procedure in TIMPROC.PRG)
*:               : DATASTOR           (procedure in TIMPROC.PRG)
*:               : WORKSAVE           (procedure in TIMPROC.PRG)
*:               : PPER()             (function in ?)
*:               : TIMADPTS.PRG
*:               : TIMTRACK.PRG
*:               : TIMADDST.PRG
*:               : TIMSRCHD.PRG
*:               : TIMPRINV.PRG
*:               : TIMCNGST.PRG
*:               : TIMDELST.PRG
*:               : TIMCOMPA.PRG
*:               : TIMVFY.PRG
*:               : PBADD.PRG
*:               : TIMBRWSE.PRG
*:               : TIMINQUR.PRG
*:               : TIMRIMBS.PRG
*:               : TIMRORDR.PRG
*:               : README.PRG
*:               : STOCKBID.PRG
*:               : TIM.PRG
*:               : TIMBCKUP.PRG
*:               : TIMINDEX.PRG
*:               : ALLIED.PRG
*:               : TIMADVER.PRG
*:               : TIMPAYRO.PRG
*:               : TIMBILL.PRG
*:               : COMPANY.SPR
*:
*:           Uses: TIMWORK.DBF
*:               : SUPPLIERS.DBF
*:               : TIMLOG.DBF
*:               : TIMWORK.BAK
*:               : TIMPOOL.DBF
*:               : TIMPOOL.BAK
*:               : EMPLOYEE.DBF
*:               : EMPLOYEE.BAK
*:               : TIMPREFX.DBF
*:               : TIMPREFX.BAK
*:
*:        Indexes: SUP_NAME.IDX
*:               : SUP_CITY.IDX
*:
*:      Documented 12:29:28                                FoxDoc version 3.00a
*:*****************************************************************************
SET EXCLUSIVE OFF
* ---Initialize an array for pulldown menu bar
DIMENSION TOP (5,2)
STORE '  WORKORDERS  ' TO TOP(1,1)
STORE '  INVENTORY   ' TO TOP(2,1)
STORE '  UTILITIES   ' TO TOP(3,1)
STORE '   OPTIONS    ' TO TOP(4,1)
STORE '     QUIT     ' TO TOP(5,1)
STORE 'WORKORDER SELECTIONS  ' TO TOP(1,2)
STORE 'TRUCKSTOCK SELECTIONS ' TO TOP(2,2)
STORE 'OPTIONS SELECTIONS    ' TO TOP(3,2)
STORE 'PARAMETERS SELECTIONS ' TO TOP(4,2)
STORE 'QUIT                  ' TO TOP(5,2)

* ---Initialize an array for the workorder menu
DIMENSION WORK(14)

STORE 'CUST SERVICE ' TO WORK(1)
STORE 'DISPATCH     ' TO WORK(2)
STORE 'CLOSE A CALL ' TO WORK(3)
STORE 'ADD PARTS /WO' TO WORK(4)
STORE 'SRCH EXIST WO' TO WORK(5)
STORE 'LIST EXIST WO' TO WORK(6)
STORE 'VIEW EXIST WO' TO WORK(7)
STORE 'PENDING FILE ' TO WORK(8)
STORE 'REPORTS      ' TO WORK(9)
STORE 'PAGE TECH    ' TO WORK(10)
* store 'ADD WORKORDER' to work(11)
STORE 'TRACKING     ' TO WORK(11)
STORE 'E-MAIL       ' TO WORK(12)
STORE 'DAY PLANNER  ' TO WORK(13)
STORE 'NOTE PAD     ' TO WORK(14)

* ---Initialize an array for the truckstock menu
DIMENSION stock(9)
STORE 'ADD INVOICES ' TO stock(1)
STORE 'SEARCH PARTS ' TO stock(2)
STORE 'LIST PARTS   ' TO stock(3)
STORE 'CHANGE / ADD ' TO stock(4)
STORE 'DELETE PARTS ' TO stock(5)
STORE 'COMPARE PARTS' TO stock(6)
STORE 'VERIFY MARKUP' TO stock(7)
STORE 'PHONE BOOK   ' TO stock(8)
STORE 'PRICE BOOK   ' TO stock(9)

* ---Initialize an array for the utilitys menu
DIMENSION option(9)
STORE 'BROWSE             ' TO option(1)
STORE 'INQUIRY            ' TO option(2)
STORE 'CASH REIMBURSTMENTS' TO option(3)
STORE 'REORDER TRUCKSTOCK ' TO option(4)
STORE 'EDIT CANCEL CODES  ' TO option(5)
STORE 'ESTIMATE JOB       ' TO option(6)
STORE 'CC DEPOSIT LOG     ' TO option(7)
STORE 'EXPENSE CARD LOG   ' TO option(8)
STORE 'FLAT RATE BOOK     ' TO option(9)

*  ---Initialize an array for the options menu
DIMENSION util(11)
STORE 'BACKUP         ' TO util(1)
STORE 'REINDEX        ' TO util(2)
STORE 'PERSONELL      ' TO util(3)
STORE 'ADVERTISING    ' TO util(4)
STORE 'PAYROLL        ' TO util(5)
STORE 'BELL ON        ' TO util(6)
STORE 'BELL OFF       ' TO util(7)
STORE 'BILLING        ' TO util(8)
STORE 'COMPANY INFO   ' TO util(9)
STORE 'ESTIMATOR LOG  ' TO util(10)
STORE 'CHANGE PASSWORD' TO util(11)
* ---Initialize an array for the quit menu
DIMENSION QUIT(2)
STORE 'END OF THE DAY ' TO QUIT(1)
STORE 'LOGOFF         ' TO QUIT(2)
*STORE 'CD MUTE ON     ' TO QUIT(3)
*STORE 'CD MUTE OFF    ' TO QUIT(4)



STORE 'T' TO sk
STORE 'N' TO ky

* do security

STORE '     ' TO aorder
STORE ' ' TO H

*               IF FILE('TIMTRANS.FOX') .OR. FILE('TIMTRANS.PRG') .OR. FILE('timtrans.fxp')
*                  DO timtrans
*               ELSE
*                  CLEAR
*                  @ 5,5 SAY '******  Access Denied  ******'
*                  @ 7,5 SAY 'For more information contact:       '
*                  @ 9,5 SAY 'Tim Software c/o Carl Pratt         '
*                  @ 10,5 SAY '(801) 561-9594                     '
*                  @ 11,5 SAY 'or write to:'
*                  @ 12,5 SAY '1103 E. Buchnell Dr.'
*                  @ 13,5 SAY 'Sandy, Utah 84094'
*                  WAIT
*                  CLEAR
*                  QUIT
*               ENDIF


USE
*  ---CHECK FOR MISSING DBF AND IDX FILES
DO dbf_chk

DO WHILE sk = 'T'
	DO LOGO
	* ---Initialize Global memory variables and OPEN file(s).
	STORE 'TIMMMENU.PRG' TO prg_name
	STORE '*****  M A I N   M E N U  *****' TO TITLE
	STORE 40 - (LEN(TITLE)/2) TO COL
	USE timwork share

	GOTO BOTTOM
	STORE incident TO lastinc
	USE
	SET COLOR TO &screenatr
	@ 4,SCOL SAY serial
	@ 3,COL SAY TITLE
	@ 3,68 SAY prg_name
	@ 22,55 SAY 'Last incident # was '
	@ 23,60 SAY lastinc
	STORE TIME() TO TIME
	IF TIME < '24:00:00'
		@ 22,5 SAY '  Good Evening '
	ENDIF
	IF TIME < '17:00:00'
		@ 22,6 SAY 'Good Afternoon '
	ENDIF
	IF TIME < '12:00:00'
		@ 22,5 SAY '  Good Morning '
	ENDIF
	@ 22,21 SAY operator
	SET COLOR TO &screenatr
	USE

	*  ---Install pull down menu system
	MENU BAR TOP,5
	MENU 1,WORK,14,14
	MENU 2,stock,9,9
	MENU 3,option,9,9
	MENU 4,util,11,11
	MENU 5,QUIT,2,2

	*  ---Initialize placement of pull down menu
	men_row = 1
	men_col = 1

	*  ---Activate the pull down menu
	SET COLOR TO &statusatr

	@ 24,0 CLEAR
	READ MENU BAR TO men_row,men_col SAVE
	SET COLOR TO &screenatr

	@ 21,0 CLEAR
	SET COLOR TO &promptatr
	@ 22,5 SAY '*****  Please wait  *****'
	SET COLOR TO &screenatr
	@ 24,4 SAY '  THANK-YOU      '
	@ 24,17 SAY operator

	SET COLOR TO &screenatr
	DO CASE
		*  ---WORKORDER MENU
	CASE men_row = 1
		SELECT 1
		IF men_col = 1
			DO timsales
		ENDIF
		IF men_col = 2
			DO timdispa
		ENDIF
		IF men_col = 3
			DO timclose.prg
		ENDIF
		IF men_col = 4
			STORE 0 TO C
*			DO timaddwk
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 5
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
*			DO timsrcwk
		ENDIF
		IF men_col = 6
*			DO timlstwk
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 7
*			DO timcngwk
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 8
			DO timpend
		ENDIF

		IF men_col = 9
*			DO timwrepo
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 10
*			RUN "C:\Progra~1\InfoRa~1\WINPAGE.EXE"
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 15
			DO work_sto
			CLEAR
			DO workmenu
			STORE 0 TO tcost
			@ 23,0 SAY 'Tech no. [       ]'
			SELECT 1
			USE timwork share
			SET ORDER TO 1
			DO WHILE .T.
				SET COLOR TO &hiliteatr
				@ 24,0 SAY '     ******* FULL SCREEN EDIT *******            '
				@ 2,21 GET mnum PICTURE '!!!!!!!'
				READ
				IF SUBSTR(mnum,1,1) = ' '
					USE
					EXIT
				ENDIF
				SEEK mnum
				IF .NOT. EOF()
					DO datastor
				ENDIF
				@ 2,44 GET mincident PICTURE '!!!!!!!!'
				@ 2,57 GET msector PICTURE '!!'
				@ 2,68 GET mdate PICTURE '99/99/99'
				@ 4,8 GET mwarea PICTURE '(999)'
				@ 4,15 GET mwphone PICTURE ' 999-9999'
				@ 4,33 GET mbarea PICTURE '(999)'
				@ 4,40 GET mbphone PICTURE ' 999-9999'
				@ 4,60 GET marea PICTURE '(999)'
				@ 4,67 GET mphone PICTURE ' 999-9999'
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
				@ 20,40  GET mpaid PICTURE '!!!!!!!!!'
				@ 21,10 GET mwork PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
				@ 22,10 GET mcomments PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
				@ 23,10 GET mtechnician PICTURE '!!!!!!!'
				READ
				STORE SUBSTR(mdate,1,2) TO MONTH
				STORE SUBSTR(mdate,4,2) TO DAY
				STORE SUBSTR(mdate,8,1) TO YEAR
				STORE TIME() TO TIME
				STORE SUBSTR(TIME,1,5) TO TIME
				STORE MONTH+'/'+DAY+' '+TIME TO msolddate
				SET COLOR TO &screenatr
				STORE 'Y' TO yn
				@ 24,0 SAY 'Is this correct?   [Y]es,   [N]o,   [E]xit' GET yn PICTURE '!'
				READ
				IF UPPER(yn) = 'N'
					LOOP
				ENDIF
				IF UPPER(yn) = 'E'
					USE
					EXIT
				ELSE
					GOTO BOTTOM
					IF .NOT. FOUND()
						APPEND BLANK
					ENDIF
					DO worksave
					REPLACE solddate WITH msolddate
					REPLACE dispdate WITH msolddate
					REPLACE closedate WITH msolddate
					REPLACE pddate WITH DATE()
					REPLACE payroll WITH DATE()
					REPLACE rcvdate WITH DATE()
					STORE 'N' TO yn
					@ 24,0 SAY 'WOULD YOU LIKE TO ENTER PARTS? [Y]es,;
						[N]o   ' get yn picture '!'
					READ
					IF UPPER(yn) = 'Y'
						STORE mamount/4 TO MAX
						STORE 0 TO prcnt
						DO timadpts
					ENDIF
					DO work_sto
					CLEAR
					DO workmenu
					STORE 0 TO tcost
					@ 23,0 SAY 'Tech no. [       ]'
				ENDIF
			ENDDO
		ENDIF
		IF men_col = 11
			DO timtrack
		ENDIF
		IF men_col = 12
*			RUN C:\Progra~1\Outloo~1\msimn.exe
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 13
*			RUN "C:\Program Files\Microsoft Office\Office\OUTLOOK.EXE"
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 14
*			RUN "C:\Progra~1\ACT\SideACT.exe"
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF

		*  ---INVENTORY MENU
	CASE men_row = 2
		SELECT 2
		IF men_col = 1
			STORE 0 TO C
*			DO timaddst
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 2
			DO timsrchd
		ENDIF
		IF men_col = 3
*			DO timprinv
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 4
*			DO timcngst
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 5
*			DO timdelst
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 6
*			DO timcompa
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 7
*			DO timvfy
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 8
			DO supplier.spr
		ENDIF
		IF men_col = 9
*			DO pbadd
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF

		*  ---OPTION MENU
	CASE men_row = 3
		IF men_col = 1
			DO timbrwse
		ENDIF
		IF men_col = 2
*			DO timinqur
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 3
*			DO timrimbs
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 4
*			DO timrordr
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 5
*			DO timcancl.spr
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 6
*			DO stockbid
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 7
			DO TIMCCLOG.SPR
		ENDIF
		IF men_col = 8
			DO TIMAELOG
		ENDIF
		IF men_col = 9
			DO FLATRATE.SPR
		ENDIF


		*  ---QUIT MENU
	CASE men_row = 5
		IF men_col = 2
			USE timlog
			APPEND BLANK
			REPLACE name WITH operator
			REPLACE DATE WITH DATE()
			REPLACE TIME WITH TIME()
			REPLACE descrip WITH 'Log Off'
			REPLACE dayofweek WITH CDOW(DATE())
			SET TALK OFF
			SET ECHO OFF
			CLEAR
			STORE 'N' TO yn
*			@ 5,10 SAY 'Would you like to undispatch all pending calls?   Y / N ' GET yn PICTURE 'X'
*			READ

			CLEAR

			SET TALK OFF
			SET STATUS OFF
			CLOSE ALL
			CLEAR ALL
			ON ERROR
			DO tim
		ENDIF
		IF men_col = 1
			@ 13,20 SAY 'HAVE A NICE DAY  '
			@ 13,37 SAY operator
			STORE 0 TO tim
			DO WHILE tim < 40
				STORE tim +1 TO tim
			ENDDO
			USE timtrack
			REPLACE ALL invoice with '*AVAIL*'
			
			USE timlog
			APPEND BLANK
			REPLACE name WITH operator
			REPLACE DATE WITH DATE()
			REPLACE TIME WITH TIME()
			REPLACE descrip WITH 'Log Off'
			REPLACE dayofweek WITH CDOW(DATE())
			USE
			SET TALK OFF
			SET ECHO OFF
			CLEAR
			STORE 'N' TO yn
*			@ 5,10 SAY 'Would you like to undispatch all pending calls?   Y / N ' GET yn PICTURE 'X'
*			READ

			CLEAR
			@ 5,10 SAY '******** SAVING BACKUP FILES ********'

			IF UPPER(yn) = 'Y'
				DO timundis
			ENDIF

			STORE CDOW(DATE()) TO backupname
			USE
			STORE 'W_'+LEFT(backupname,5)+'.DBF' TO backupname
			@ 6,10 SAY 'COPYING TIMWORK FILES'
			COPY FILE timwork.dbf TO &backupname
			STORE LEFT(backupname,7)+'.FPT' TO backupname
			@ 6,10 SAY 'COPYING TIMWORK F9 SCREENS'

			COPY FILE timwork.fpt TO &backupname
			? backupname


			USE timtime
			STORE 'T_'+RIGHT(backupname,9) TO backupname
			@ 6,10 SAY backupname
			COPY TO &backupname
			USE timpool 
			IF FLOCK( )
				USE TIMPOOL EXCL
				PACK
			ELSE
				*** File is locked, warn user ***
				WAIT WINDOW 'Unable to compile timpool, try again later!' NOWAIT
			ENDIF
			STORE 'p_'+RIGHT(backupname,9) TO backupname
			@ 6,10 SAY backupname
			COPY TO &backupname

			USE
			STORE 'E_'+RIGHT(backupname,9) TO backupname
			@ 6,10 SAY backupname
			COPY FILE employee.dbf TO &backupname

			STORE LEFT(backupname,7)+'.FPT' TO backupname
			@ 6,10 SAY 'COPYING EMPLOYEE F9 SCREENS'
			COPY FILE employee.fpt TO &backupname


			USE timbill
			STORE 'B_'+RIGHT(backupname,9) TO backupname
			@ 6,10 SAY backupname
			COPY TO &backupname


			USE timprefx
			STORE 'PR_'+RIGHT(backupname,8) TO backupname
			@ 6,10 SAY backupname
			COPY TO &backupname




			SET TALK OFF
			SET STATUS OFF

			CLOSE ALL
			CLEAR ALL
			CLEAR
			QUIT
		ENDIF

		*  ---UTILITY MENU
	CASE men_row = 4
		IF men_col = 1
*			DO timbckup
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"

		ENDIF
		IF men_col = 2
*			DO timindex
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF

		IF men_col = 3
			IF FILE('allied.fxp') .OR. FILE('allied.prg') .OR. FILE('allied.fox')
*				DO allied
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
			ELSE
				CLEAR
				@ 5,10 SAY 'This option is not installed !!!'
				@ 6,10 SAY 'If you would like this option   '
				@ 7,10 SAY 'Please contact TIM SOFTWARE CO. '
				@ 8,10 SAY '       (801) 561-9594           '
				WAIT
			ENDIF
		ENDIF
		IF men_col = 4
*			DO timadver
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 5
*			DO timpayro
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 6
			SET BELL ON
		ENDIF
		IF men_col = 7
			SET BELL OFF
		ENDIF
		IF men_col = 8
*			DO timbill
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF
		IF men_col = 9
*			DO company.spr
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF

		IF men_col = 10
*			DO timbid_1.spr
			WAIT WINDOW "SEE YOUR SUPERVISOR FOR ACCESS TO THIS PROGRAM"
		ENDIF

		IF men_col = 11
*			USE employee
*			SET ORDER TO 2			
WAIT WINDOW 'This function is not completed, try again later!' NOWAIT			
		ENDIF


	ENDCASE
ENDDO


PROCEDURE listempl
		DEFINE WINDOW ListEmpl FROM 20,05 TO 40,80 DOUBLE
		ACTIVATE WINDOW ListEmpl

CLEAR

@ 1,0 SAY 'EMP #       NAME           HOME           PAGER          CELLULAR'
@ 2,0
SET DELETED ON
USE employee
SET ORDER TO 2
SET DELETED On
set title off
DISP ALL FIELDS  emp_no,LEFT(name,15),+'  '+harea,hphone,+'  '+carea,cphone OFF

wait
RELEASE WINDOW

* EOF: TIMMMENU.PRG
*: EOF: TIM1MENU.PRG
