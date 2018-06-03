*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMBILL.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 01/25/95 at  0:22:08
*:
*:      Called by: TIM1MENU.PRG
*:
*:          Calls: BILLMENU           (procedure in ACCTPROC.PRG)
*:
*:           uses: TIMPAYRO.DBF		3
*:               : TIMWORK.DBF		1
*:               : TIMBILL.DBF		2
*:               : TIMBILLH.DBF		5
*:               : EMPLOYEE.DBF     4
*:
*:
*:      Documented 12:30:40                                FoxDoc version 3.00a
*:*****************************************************************************


*  ---Set Global Variables
SET TALK OFF
SET ECHO OFF
SET PRINT OFF
SET CONSOLE ON
SET STATUS OFF
SET SCOREBOARD OFF
SET PROCEDURE TO acctproc
SET EXCLUSIVE OFF
STORE 0.00 TO mcommission
STORE ' ' TO choice
STORE 1 TO PAGE
STORE '9 ' TO nchoice
STORE 0 TO rec_count
STORE '***** B I L L I N G   M E N U *****' TO TITLE
STORE 40 - (LEN(serial)/2) TO SCOL
STORE SPACE(250) TO mcomments
STORE 40 - (LEN(TITLE)/2) TO COL
STORE 'TIMBILL.PRG' TO prg_name
ON KEY LABEL f9 MODIFY MEMO notes
CLEAR
*  ---DISPLAY TITLE ON THE SCREEN
SET COLOR TO &windowatr
STORE 40 - (LEN(TITLE)/2) TO COL
@ 0,0 TO 0,80 CLEAR
@ 0,COL SAY TITLE
@ 0,68 SAY prg_name
@ 0,1 SAY 'Record #'
STORE 'A' TO add_chse
SET COLOR TO &screenatr




*	SELECT 1  TIMWORK
IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF


*	SELECT 2  TIMBILL
IF USED("timbill")
	SELECT timbill
	SET ORDER TO 0
ELSE
	SELECT 2
	USE (LOCFILE("timbill.dbf","DBF","Where is timbill?"));
		AGAIN ALIAS timbill ;
		ORDER 0
ENDIF

*	SELECT 3 TIMPAYRO
IF USED("timpayro")
	SELECT timpayro
	SET ORDER TO emp_no
ELSE
	SELECT 3
	USE (LOCFILE("timpayro.dbf","DBF","Where is timpayro?"));
		AGAIN ALIAS timpayro ;
		ORDER emp_no
ENDIF



*	SELECT 5  TIMBILLh
IF USED("timbillh")
	SELECT timbillh
	SET ORDER TO 0
ELSE
	SELECT 5
	USE (LOCFILE("timbillh.dbf","DBF","Where is timbillh?"));
		AGAIN ALIAS timbillh ;
		ORDER 0
ENDIF





DO WHILE .T.
	SET COLOR TO &screenatr
	@ 2,0 CLEAR
   TEXT



      1 - enter payment on account   ie.. check received
      2 - enter debit on account     ie.. bounced check
      3 - do billing cycle           ie.. print all billing
      4 - send specific billing      ie.. print bill for a job
      5 - view specific bill status  ie.. customer inquiry
      6 - list open accounts to screen
      7 - list open accounts to printer
      8 - browse
      9 - return to main menu
      C - Do Collections



   ENDTEXT
	@ 23,5 SAY 'Please Enter Choice      ' GET nchoice PICTURE '@k !!'
	READ
	DO CASE
	CASE nchoice = '1'
		* --- Enter payment
		DO billmenu
		DO WHILE .T.
			IF mnum = ' '
				EXIT
			ENDIF
			SET COLOR TO W+/N
			STORE SPACE(25) TO mdescrip
			STORE SPACE(250) TO mcomments
			STORE 0.00 TO mamount
			STORE 0.00 TO mbalance
			STORE DATE() TO mdate
			@ x,1 GET mdate
			@ x,19 GET mdescrip
			@ x,59 GET mamount PICTURE '999,999.99'
			READ
			STORE mamount-(mamount*2) TO mamount
			IF mdescrip = ' '
				EXIT
			ENDIF
			IF mamount = 0
				EXIT
			ENDIF
			SUM ALL amount FOR no = mnum TO mbalance
			STORE mbalance + mamount TO mbalance
			@ x,69 SAY mbalance PICTURE '999,999.99'
			@ 23,69 SAY mbalance PICTURE '999,999.99'
			@ 22,0 SAY mcomments
			READ
			APPEND BLANK
			= LOCK()
			REPLACE no WITH mnum
			REPLACE DATE WITH mdate
			REPLACE descrip WITH mdescrip
			REPLACE amount WITH mamount
			REPLACE balance WITH mbalance
			REPLACE comments WITH mcomments
			UNLOCK

			STORE 0 TO mwage
			SELECT TIMWORK
			SET ORDER TO NO
			seek mnum
			IF .NOT. EOF()
				STORE wage TO mwage
				STORE name TO mname
				STORE technician TO mtechnician
				REPLACE notes WITH notes  + CHR(13) + 'PAYMENT MADE '+STR(mamount)+' '+ DTOC(DATE())

			ENDIF



			IF mwage > 0
				SELECT TIMPAYRO

				GOTO TOP
				**  Find existing job and make adjustments to the payroll info
				IF mbalance <> 0.00
					LOCATE ALL FOR wo = mnum
					DO WHILE .NOT. EOF()
						IF mnum = wo .AND. LEFT(descrip,11) = 'COMMISSIONS'
							REPLACE gross_pay WITH gross_pay+(mamount*(mwage/100))
							REPLACE descrip WITH 'COMMISSIONS ADJUSTED '+DTOC(DATE())
							IF gross_pay <= 0.00
								REPLACE gross_pay WITH 0.00
								REPLACE ALL date_apprv WITH DATE() FOR DTOC(date_apprv) = ' ' .AND. wo = mnum
								REPLACE descrip WITH 'COMMISSIONS FINAL BILL'+DTOC(DATE())
							ENDIF
							EXIT
						ELSE
							CONTINUE
						ENDIF
					ENDDO
				ENDIF
				= LOCK()
				GOTO BOTTOM
				APPEND BLANK
				REPLACE amount WITH 0.00
				REPLACE percent WITH mwage
				REPLACE wo WITH mnum
				REPLACE emp_no WITH mtechnician
				REPLACE gross_pay WITH (mamount-(mamount*2))*(mwage/100)
				REPLACE DATE WITH DATE()
				REPLACE jobname WITH mname
				REPLACE date_rcved WITH DATE()
				REPLACE date_apprv WITH DATE()
				REPLACE descrip WITH ALLTRIM(mdescrip) + '  $'+STR(mamount-(mamount*2))
				UNLOCK
				SELECT TIMBILL
			ENDIF
			STORE x+1 TO x
			IF x > 22
				STORE 12 TO x
				@ 12,0 TO 23,80 CLEAR
				STORE PAGE + 1 TO PAGE
			ENDIF
			SET COLOR TO &screenatr
			IF mbalance <= 0
				SELECT TIMWORK

				GOTO mrec_no
				STORE DATE() TO mpddate
				= LOCK()
				REPLACE notes WITH notes  + CHR(13) + 'FINAL PAYMENT MADE '+STR(mamount)+' '+ DTOC(DATE())
				REPLACE pddate WITH mpddate
				IF mbalance = 0.00
					REPLACE paid WITH 'PAID BILL'
				ENDIF
				UNLOCK
				select timbill
			ENDIF

		ENDDO


	CASE nchoice = '2'
		* --- Enter Debit
		DO billmenu
		DO WHILE .T.
			IF mnum = ' '
				EXIT
			ENDIF
			SET COLOR TO W+/N
			STORE SPACE(25) TO mdescrip
			STORE 0.00 TO mamount
			STORE 0.00 TO mbalance
			STORE DATE() TO mdate
			STORE SPACE(250) TO mcomments
			@ x,1 GET mdate
			@ x,19 GET mdescrip
			@ x,49 GET mamount PICTURE '999,999.99'
			READ
			IF mdescrip = ' '
				EXIT
			ENDIF
			IF mamount = 0
				EXIT
			ENDIF
			SUM ALL amount FOR no = mnum TO mbalance
			STORE mbalance + mamount TO mbalance
			@ x,69 SAY mbalance PICTURE '999,999.99'
			@ 23,69 SAY mbalance PICTURE '999,999.99'
			@ 22,0 SAY mcomments
			READ
			= LOCK()
			APPEND BLANK
			= LOCK()
			REPLACE comments WITH mcomments
			REPLACE no WITH mnum
			REPLACE DATE WITH mdate
			REPLACE descrip WITH mdescrip
			REPLACE amount WITH mamount
			REPLACE balance WITH mbalance
			UNLOCK
			select timpayro
			= LOCK()
			APPEND BLANK
			REPLACE wo WITH mnum
			REPLACE emp_no WITH mtechnician
			REPLACE gross_pay WITH mamount*(mcommission/100)
			REPLACE DATE WITH DATE()
			REPLACE jobname WITH mname
			REPLACE date_rcved WITH DATE()
			REPLACE descrip WITH 'Payment on a collection account'
			UNLOCK
			select timbill
			STORE x+1 TO x
			IF x > 22
				STORE 12 TO x
				@ 12,0 TO 23,80 CLEAR
				STORE PAGE + 1 TO PAGE
			ENDIF
			SET COLOR TO &screenatr
		ENDDO








	CASE nchoice = '3'
		* --- Send General billing
		DO timbill.spx



	CASE nchoice = '4'
		* --- Send Specific billing
		DO billmenu
*		select timwork
		*		STORE comments TO mcomments
*		GOTO mrec_no


*		select timbill
		SET COLOR TO W/B+
		@ 24,0 CLEAR
		@ 24,5 SAY 'Enter Comments to print on invoice'
		@ 22,0 GET mcomments
		READ
		REPLACE comments WITH mcomments
		SET COLOR TO &screenatr
		STORE 'Y' TO choice
		@ 24,0 CLEAR
		@ 24,0 SAY 'Insert blank page in the printer and press [Enter] or [N] to not print'
		@ 24,78 GET choice PICTURE '!'
		READ
		@ 24,0 CLEAR
		IF choice <> 'N'
			IF invstate = 'I'
				IF left(mnum,1)='5'
					REPORT FORM timbilli FOR no = mnum TO PRINT
				ELSE
					REPORT FORM timbilli FOR no = mnum TO PRINT
				ENDIF
			ELSE
				IF left(mnum,1)='5'
					REPORT FORM timbill FOR no = mnum TO PRINT
				ELSE
					REPORT FORM timbill FOR no = mnum TO PRINT
				ENDIF
			ENDIF
			REPLACE lastbilled WITH DATE()
			REPLACE followup WITH DATE() + 15

			CLEAR
			select timwork
			SET ORDER TO NO
			GOTO TOP
			SEEK mnum
			STORE RECNO() TO mrec_no
			IF invstate = 'I'
				STORE notes + CHR(13) + DTOC(DATE()) + ' INVOICE PRINTED BY ' + operator TO m.notes
			ELSE
				STORE notes + CHR(13) + DTOC(DATE()) + ' STATEMENT PRINTED BY ' + operator TO m.notes
			ENDIF
			REPLACE notes WITH m.notes
			CLEAR
			*			STORE 'N' TO yn
			*			@ 23,0 SAY 'Print Envelope?     (Y - N)           ' GET yn PICTURE '!'
			*			READ
			*			IF yn = 'Y'
			*				@ 23,0 SAY 'Insert Envelope in the printer and press online on'
			*				WAIT
			*				SET DEVICE TO PRINT
			*				@ 1,2 SAY dcompany
			*				@ 2,2 SAY dstreet
			*				@ 3,2 SAY TRIM(dcity)+', '+LTRIM(dstate)+' '+dzip
			*				@ 10,40 SAY mbname
			*				@ 11,40 SAY mbstreet
			*				STORE TRIM(mbcity) TO mbcity
			*				@ 12,40 SAY mbcity+', '
			*				@ 12,LEN(mbcity)+42 SAY mbstate
			*				@ 12,LEN(mbcity)+43+LEN(state) SAY mbzip
			*
			*				EJECT
			*				SET DEVICE TO SCREEN
			*			ENDIF
		ENDIF


	CASE nchoice = '5'
		* --- View billing
		DO billmenu

	CASE nchoice = '6'
		CLEAR

		SET DELETED ON
		STORE '***** L I S T   B I L L I N G *****' TO TITLE
		STORE 40 - (LEN(serial)/2) TO SCOL
		STORE 40 - (LEN(TITLE)/2) TO COL
		STORE 'TIMBILL.PRG' TO prg_name
		CLEAR
		*  ---DISPLAY TITLE ON THE SCREEN
		SET COLOR TO &windowatr
		STORE 40 - (LEN(TITLE)/2) TO COL
		@ 0,0 TO 0,80 CLEAR
		@ 0,COL SAY TITLE
		@ 0,68 SAY prg_name
		SET COLOR TO &screenatr
		select timbill
		SET ORDER TO NO
		GO TOP
		STORE 4 TO x
		STORE 1 TO PAGE
		STORE 0 TO tbalance
		SET COLOR TO &promptatr
		@ 3,0 SAY 'Billed    W.O. #   Name                           Status     Balance  Contact'
		SET COLOR TO &screenatr
		DO WHILE .NOT. EOF()
			IF x > 22
				WAIT
				STORE 4 TO x
				STORE PAGE + 1 TO PAGE
				@ 4,0 CLEAR
			ENDIF
			STORE no TO mno
			STORE balance TO mbalance
			STORE balance TO bbalance
			STORE contact TO mcontact
			STORE DATE TO mdate
			STORE DATE() - DATE TO mstatus
			IF LEFT(STATUS,1) = ' '
				REPLACE STATUS WITH STR(mstatus)
			ENDIF
			IF VAL(STATUS) > 30
				SET COLOR TO GR/W*
			ENDIF
			IF VAL(STATUS) > 60
				SET COLOR TO R+/W*
			ENDIF
			IF STATUS > ' '
				SET COLOR TO G+/W*
			ENDIF
			IF STATUS = 'COLLECTION'
				SET COLOR TO R+/W*
			ENDIF
			IF STATUS = 'LIENED'
				SET COLOR TO B/W*
			ENDIF
			@ x,0 SAY DTOC(mdate)+'  '+no+'  '+name+'  '+STR(DATE() - DATE)+'  '+STR(mbalance,10,2)+'  '+LEFT(mcontact,10)
			DO WHILE .T.
				SKIP 1
				IF no = mno
					STORE balance TO mbalance
					STORE DATE TO mdate
					IF balance = 0
						DO WHILE .NOT. EOF()
							STORE RECNO() TO trecno
							STORE DATE TO mdate
							STORE descrip TO mdescrip
							STORE no TO mnum
							STORE amount TO mamount
							STORE balance TO mamount
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
							STORE incident TO mincident
							STORE technician TO mtechnician
							STORE contact TO mcontact
							STORE rec_no TO mrec_no
							DELETE
							select timbillh
							APPEND BLANK
							REPLACE DATE WITH mdate
							REPLACE descrip WITH mdescrip
							REPLACE no WITH mnum
							REPLACE amount WITH mamount
							REPLACE balance WITH mamount
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
							REPLACE incident WITH mincident
							REPLACE technician WITH mtechnician
							REPLACE contact WITH mcontact
							REPLACE rec_no WITH mrec_no
							select timbill
							SKIP -1
							IF no = mnum
								LOOP
							ELSE
								GOTO trecno
								EXIT
							ENDIF
						ENDDO
					ENDIF
					LOOP
				ELSE
					IF mbalance > 0
						@ x,58 SAY STR(mbalance,10,2)
						@ x,0 SAY DTOC(mdate)
					ELSE
						STORE x-1 TO x
					ENDIF
					STORE tbalance + mbalance TO tbalance
					EXIT
				ENDIF
			ENDDO
			SET COLOR TO &screenatr
			STORE x+1 TO x
		ENDDO
		@ x,0 TO x,80 CLEAR
		@  x,52 SAY 'total '+STR(tbalance,10,2)
		SET DELETED OFF
		WAIT

	CASE nchoice = '7'
		CLEAR
		STORE '***** L I S T   B I L L I N G *****' TO TITLE
		STORE 40 - (LEN(serial)/2) TO SCOL
		STORE 40 - (LEN(TITLE)/2) TO COL
		STORE 'TIMBILL.PRG' TO prg_name
		CLEAR
		*  ---DISPLAY TITLE ON THE SCREEN
		SET COLOR TO &windowatr
		STORE 40 - (LEN(TITLE)/2) TO COL
		@ 0,0 TO 0,80 CLEAR
		@ 0,COL SAY TITLE
		@ 0,68 SAY prg_name
		SET COLOR TO &screenatr
		select timbill
		SET ORDER TO 1
		SET DELETED ON
		STORE 'Y' TO PRINTER
		REPORT FORM listbill PREVIEW
		@ 23,5 SAY 'Would you like to list to printer?   [y / n]' GET PRINTER PICTURE 'X'
		READ
		IF PRINTER = 'Y'
			REPORT FORM listbill TO PRINT
		ENDIF
		SET DELETED OFF


	CASE nchoice = '8'
		SELECT TIMBILL
		SET ORDER TO NO
		
		CLEAR
		STORE 'Y' TO yn
		@ 5,10 SAY 'VIEW DELETED ITEMS?   Y / N ' GET yn PICTURE 'X'
		READ
		IF UPPER(yn) = 'Y'
			SET DELETED OFF
		ELSE
			SET DELETED ON
		ENDIF
		Browse last
		SET DELETED OFF

	CASE nchoice = '9'
		CLOSE DATABASES
		USE
		SET PROCEDURE TO timproc
		RETURN



	CASE nchoice = 'C'
		DO timbill.spr



	CASE nchoice = '10'
		STORE '***** L I S T   A L L    A C C O U N T S *****' TO TITLE
		STORE 40 - (LEN(serial)/2) TO SCOL
		STORE 40 - (LEN(TITLE)/2) TO COL
		STORE 'TIMBILL.PRG' TO prg_name
		CLEAR
		*  ---DISPLAY TITLE ON THE SCREEN
		SET COLOR TO &windowatr
		STORE 40 - (LEN(TITLE)/2) TO COL
		@ 0,0 TO 0,80 CLEAR
		@ 0,COL SAY TITLE
		@ 0,68 SAY prg_name
		@ 0,1 SAY 'Record #'
		select timbill
		GO TOP
		STORE 4 TO x
		STORE 1 TO PAGE
		STORE 0 TO tbalance
		SET COLOR TO &promptatr
		@ 3,0 SAY 'Date      W.O. #   Name                           Amount     Balance  Contact'
		SET COLOR TO &screenatr
		DO WHILE .NOT. EOF()
			IF x > 22
				WAIT
				STORE 4 TO x
				STORE PAGE + 1 TO PAGE
				@ 4,0 CLEAR
			ENDIF
			STORE no TO mno
			STORE balance TO mbalance
			STORE balance TO bbalance
			STORE contact TO mcontact
			STORE DATE TO mdate
			@ x,0 SAY DTOC(mdate)+'  '+no+'  '+name+'  '+STR(bbalance,10,2)+'  '+STR(mbalance,10,2)+'  '+LEFT(mcontact,10)
			DO WHILE .T.
				SKIP 1
				IF no = mno
					STORE balance TO mbalance
					LOOP
				ELSE
					@ x,58 SAY STR(mbalance,10,2)
					STORE tbalance + mbalance TO tbalance
					EXIT
				ENDIF
			ENDDO
			STORE x+1 TO x
		ENDDO
		@  x,52 SAY 'total '+STR(tbalance,10,2)
		WAIT

	ENDCASE
ENDDO
SET PROCEDURE TO timproc

*: EOF: TIMBILL.PRG
