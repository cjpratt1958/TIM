*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMPAYRO.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/24/98 at 21:35:24
*:
*:      Called by: TIM1MENU.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : TIMADCHK.PRG
*:               : TIMAPCHK.PRG
*:               : TIMTAXES.PRG
*:
*:           Uses: TIMWORK.DBF
*:               : JOBS_PAY.DBF
*:               : TIMPAYRO.DBF
*:
*:        Indexes: JOBS_TEC.IDX
*:               : TPR_TEC.IDX
*:               : TPR_WO.IDX
*:
*:   Report Forms: COMMISSI.FRX
*:
*:      Documented 12:30:38                                FoxDoc version 3.00a
*:*****************************************************************************
SET EXCLUSIVE OFF
SET DELETED ON
CLEAR
STORE 'P' TO PRINTER
STORE 'Y' TO A
STORE 'N' TO ok
STORE 'N' TO DELE
STORE '1' TO choice
STORE 'A      ' TO mtechnician
STORE DATE() TO mdate_paid
STORE 1 TO X
DO WHILE DOW(MDATE_PAID) <> 6
	STORE DATE()+X TO mdate_paid
	STORE X+1 TO X
ENDDO	

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


*	SELECT 2
IF USED("timpayro")
	SELECT timpayro
	SET ORDER TO emp_no
ELSE
	SELECT 2
	USE (LOCFILE("timpayro.dbf","DBF","Where is timpayro?"));
		AGAIN ALIAS timpayro ;
		ORDER emp_no
ENDIF


*	SELECT 3
IF USED("employee")
	SELECT employee
	SET ORDER TO 0
ELSE
	SELECT 3
	USE (LOCFILE("employee.dbf","DBF","Where is employee?"));
		AGAIN ALIAS employee ;
		ORDER 0
ENDIF



IF .NOT. FILE('jobs_pay.dbf')
	SELECT timwork
	COPY STRUC TO jobs_pay
	USE
ENDIF

SELECT 5
IF USED("jobs_pay")
	SELECT jobs_pay
	SET ORDER TO 0
ELSE
	SELECT 5
	USE (LOCFILE("jobs_pay","DBF","Where is jobs_pay?"));
		AGAIN ALIAS jobs_pay ;
		ORDER 0
ENDIF


SELECT jobs_pay 
IF .NOT. FILE('jobs_pay.cdx')
	SELECT jobs_pay 
	INDEX ON technician TAG Technician ADDITIVE
ENDIF
USE


STORE 'TIMPAYRO.PRG' TO prg_name
STORE '***** P A Y R O L L   F I L E *****' TO TITLE
DO addtitle

DO WHILE .T.
	@ 2,0 CLEAR
	*--- Draw menu items ---*
	@  4,12 SAY '1 - List all Workorders Approved for Payroll        '
	@  5,12 SAY '2 - List all Workorders Pending Approval            '
	@  6,12 SAY '3 - Enter Workorders received in the Office         '
	@  7,12 SAY '4 - Approve Workorders for payroll                  '
	@  8,12 SAY '5 - Enter Cash advances or other Debits             '
	@  9,12 SAY '6 - Enter Bonuses or other Credits                  '
	@ 10,12 SAY '7 - Issue Workorders                                '
	@ 11,12 SAY '8 - Browse payroll file                             '
	@ 12,12 SAY 'T - Enter Time Cards                                '
	@ 13,12 SAY 'V - View past payrolls                              '
	@ 14,12 SAY 'R - List all Approved Jobs that Redflag             '
	@ 15,12 SAY 'B - Tool Benefit Expense Claim Forms                '
	@ 16,12 SAY 'D - Database Management                             '
	@ 17,12 SAY '9 - Return to Main Menu                             '

	@ 19,12 SAY 'Choose item ' GET choice
	READ
	STORE UPPER(CHOICE) TO choice
	DO CASE
	STORE UPPER(CASE) TO CASE

	CASE choice = '1'
		CLEAR
		SELECT 2
		set order to emp_no
		STORE 'S' TO PRINTER
		STORE 'A ' TO M.STATE
		STORE DATE()-7 TO edate

		@ 16,12 SAY 'Enter the End Date of this Pay Period ' GET edate PICTURE '99/99/9999' 	
		@ 17,12 SAY 'Enter State or Enter [A] for all' GET M.STATE PICTURE '@k!!'
		@ 18,12 SAY 'Enter Technician number or Enter [A] for all' GET mtechnician PICTURE '@k!!!!!!!'
		@ 19,12 SAY 'Printer or Screen    Choose P or S ' GET PRINTER PICTURE '@k!'
		READ


		IF ALLTRIM(m.STATE) = 'A'
		SET FILTER TO
		ELSE	
			SET FILTER TO STATE = M.STATE
		ENDIF

		IF LEFT(mtechnician,1) = 'A'
			IF PRINTER = 'P'
				REPORT FORM commissi  FOR DTOC(Date_Apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. DATE_APPRV =< edate TO PRINT
			ELSE
				REPORT FORM commissi FOR DTOC(Date_Apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. DATE_APPRV =< edate PREVIEW
			ENDIF
			STORE edate+14 to mdate_paid
			STORE 'N' TO ok
			@ 23,12 SAY 'Mark Jobs Paid ? ' GET ok PICTURE '!'
*			@ 24,12 SAY 'Please enter Payroll date ' GET mdate_paid PICTURE '  /  /    '
			READ
			IF UPPER(ok) = 'Y'
*				@ 23, 0 TO 23, 80 CLEAR
				@ 24, 12 SAY 'Please enter Payroll date ' GET mdate_paid PICTURE '  /  /    '
				READ
				= LOCK()
				REPLACE ALL date_paid WITH mdate_paid FOR DTOC(Date_Apprv) <> '  /  /    ' .AND.;
				DTOC(date_paid) = '  /  /    ' .AND. DATE_RCVED =< edate
				REPLACE ALL paidby WITH operator FOR DTOC(Date_Apprv) <> '  /  /    ' .AND.;
				DTOC(date_paid) = '  /  /    ' .AND. DATE_RCVED =< edate

*				REPORT FORM commissi FOR DTOC(Date_Apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. DATE_APPRV =< edate
				UNLOCK
				SELECT 1
				= LOCK()
				REPLACE ALL payroll WITH mdate_paid FOR DTOC(payroll) = '  /  /    ' .AND. cleared = 'Y' .AND. amount > 0
				UNLOCK
			ENDIF
		ELSE
			IF UPPER(PRINTER) = 'P'
				REPORT FORM commissi  FOR mtechnician=emp_no .AND. DTOC(Date_Apprv) <> '  /  /    ' .AND.;
				DTOC(date_paid) = '  /  /    ' .AND. DATE_APPRV =< edate TO PRINT
			ELSE
				REPORT FORM commissi  FOR mtechnician=emp_no .AND. DTOC(Date_Apprv) <> '  /  /    ' .AND.;
				DTOC(date_paid) = '  /  /    ' .AND. DATE_APPRV =< edate PREVIEW
			ENDIF
			STORE 'N' TO ok
			@ 23,12 SAY 'Mark Jobs Paid ? ' GET ok PICTURE '!'
			READ
			IF UPPER(ok) = 'Y'
				@ 23, 0 TO 23, 80 CLEAR
				@ 24, 12 SAY 'Please enter Payroll date ' GET mdate_paid PICTURE '  /  /    '
				READ

				= LOCK()
				REPLACE ALL date_paid WITH mdate_paid FOR mtechnician=emp_no .AND.;
				DTOC(Date_Apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. DATE_APPRV =< edate
				UNLOCK
				SELECT 1
				= LOCK()
				REPLACE ALL payroll WITH mdate_paid FOR DTOC(payroll) = '  /  /    ' .AND. cleared = 'Y' .AND. amount > 0
				
				UNLOCK
			ENDIF
		ENDIF
		STORE 'N' to yn
		@ 23, 0 TO 23, 80 CLEAR
*		@ 23, 11 SAY 'Would you like to print the Tool Benifits Forms? ' GET yn PICTURE '!'
*		READ
		IF yn = 'Y'
			REPORT FORM TOOLBENIFIT FOR Job_Title = 'ELECTRICIA' PREVIEW
			@ 23, 11 SAY 'Would you like to print these forms to the printer?  ' GET yn PICTURE '!'			
			READ
			IF yn = 'Y'
				REPORT FORM TOOLBENIFIT FOR Job_Title = 'ELECTRICIA' TO PRINT
			ENDIF
		ENDIF
		SET FILTER TO
		






	CASE choice = '2'
		SELECT 2
		set order to emp_no
		STORE 'S' TO PRINTER
		STORE 'A ' TO M.STATE

		@ 17,12 SAY 'Enter State or Enter [A] for all' GET M.STATE PICTURE '@k!!'
		@ 18,12 SAY 'Enter Technician number or Enter [A] for all' GET mtechnician PICTURE '@k!!!!!!!'
		@ 19,12 SAY 'Printer or Screen    Choose P or S ' GET PRINTER PICTURE '@k!'
		READ
		IF ALLTRIM(m.STATE) = 'A'
			SET FILTER TO
		ELSE	
			SET FILTER TO STATE = M.STATE
		ENDIF
		CLEAR
		IF LEFT(mtechnician,1) = 'A'
			*--- USE PRINTER ---*
			IF PRINTER = 'P'
				REPORT FORM commissi FOR DTOC(date_paid) = '  /  /    '  TO PRINT
			ELSE
				REPORT FORM commissi FOR DTOC(date_paid) = '  /  /    ' PREVIEW
			ENDIF
		ELSE
			*--- USE PRINTER ---*
			IF PRINTER = 'P'
				REPORT FORM commissi FOR mtechnician=emp_no .AND. DTOC(date_paid) = '  /  /    '  TO PRINT
			ELSE
				REPORT FORM commissi FOR mtechnician=emp_no .AND. DTOC(date_paid) = '  /  /    ' PREVIEW
			ENDIF
		ENDIF
		SET FILTER TO



	CASE choice = '3'
		DO timadchk

	CASE choice = '4'
*		@ 23, 0 TO 23, 80 CLEAR
*		@ 23, 11 SAY 'Please enter Payroll date ' GET mdate_paid PICTURE '  /  /    '
*		READ

		DO timapchk

	CASE choice = '5'
		*** --- CASH ADVANCES  / DEDUCT
		STORE 0.00 TO damount
		STORE SPACE (60) TO ddescrip
		STORE SPACE(7) TO dtechnician
		STORE '       ' TO dno
		STORE 'UT' TO m.state
		STORE DATE()-7 TO m.date_rcved
		SET COLOR TO W/B,N/W
		@ 14,5 TO 19,70 CLEAR
		@ 13,4 TO 20,71
		@ 14,20 SAY 'CASH ADVANCES OR OTHER DEDUCTIONS'
		@ 16,7 SAY 'EMP # '
		@ 16,22 SAY 'WORKORDER # '
		@ 16,43 SAY 'AMOUNT DEBIT'
		@ 17, 7 SAY 'ENTER DESCRIPTION'
		@ 19, 7 SAY 'ENTER DATE TO BE DEDUCTED '
		SET COLOR TO N/W
		@ 16,13 GET dtechnician
		@ 16,34 GET dno
		@ 16,57 GET damount
		@ 18, 8 GET ddescrip
		@ 19,34 GET m.date_rcved
		READ
		SET COLOR TO &screenatr
		CLEAR
		IF EMPTY(dno)
			@ 20,8 SAY 'Work Order Entered, Please Enter the State ' GET m.state picture '!!'
			READ
		ELSE
			SELECT 1
			GOTO TOP
			SEEK ALLTRIM(dno)
			IF FOUND()
				SCATTER MEMVAR
				REPLACE notes WITH notes + CHR(13) +  'Deduction Entered on '+DTOC(DATE()) +' by '+ operator
				
			ELSE
				@ 20,8 SAY 'Work Order not found Please Enter the State ' GET m.state picture '!!'
				READ
			ENDIF
		ENDIF 
		IF damount > 0
			SELECT 2
			
			? LOCK(2)
			APPEND BLANK
			UNLOCK
			GATHER MEMVAR
			REPLACE deduct WITH damount
			REPLACE descrip WITH ddescrip
			REPLACE DATE WITH DATE()
			REPLACE date_rcved WITH m.date_rcved
			REPLACE date_apprv WITH DATE()
			REPLACE wo WITH dno
			REPLACE emp_no WITH dtechnician
		ENDIF
		SET COLOR TO &screenatr
		STORE '9' TO choice



	CASE choice = '6'
		*** --- BONUSES  / CREDITS
		STORE 0.00 TO damount
		STORE SPACE (60) TO ddescrip
		STORE SPACE(7) TO dtechnician
		STORE '       ' TO dno
		STORE 'UT' TO m.state
		STORE DATE()-7 TO m.date_rcved
		SET COLOR TO W/B,N/W
		@ 14,5 TO 19,70 CLEAR
		@ 13,4 TO 20,71
		@ 14,20 SAY 'BONUSES OR OTHER CREDITS'
		@ 16,7 SAY 'EMP # '
		@ 16,22 SAY 'WORKORDER # '
		@ 16,43 SAY 'AMOUNT CREDIT'
		@ 17, 7 SAY 'ENTER DESCRIPTION'
		@ 19, 7 SAY 'ENTER DATE TO BE PAID '

		SET COLOR TO N/W
		@ 16,13 GET dtechnician
		@ 16,34 GET dno
		@ 16,57 GET damount
		@ 18, 8 GET ddescrip
		@ 19,30 GET m.date_rcved
		READ
		SET COLOR TO &screenatr
		CLEAR
		IF EMPTY(dno)
			@ 20,8 SAY 'Work Order not entered Please Enter the State ' GET m.state picture '!!'
				READ
		ELSE
			SELECT 1
			GOTO TOP
			SEEK ALLTRIM(dno)
			IF FOUND()
				SCATTER MEMVAR
				REPLACE notes WITH notes + CHR(13) +  'Credit entered on '+DTOC(DATE()) +' by '+ operator
				
			ELSE
				@ 20,8 SAY 'Work Order not found Please Enter the State ' GET m.state picture '!!'
				READ
			ENDIF
		ENDIF 


		IF damount > 0
			SELECT 2
			? LOCK(2)
			APPEND BLANK
			UNLOCK
			GATHER MEMVAR
			REPLACE other_pay WITH damount
			REPLACE descrip WITH ddescrip
			REPLACE DATE WITH DATE()
			REPLACE date_rcved WITH m.date_rcved
			REPLACE date_apprv WITH DATE()
			REPLACE wo WITH dno
			REPLACE emp_no WITH dtechnician
		ENDIF
		SET COLOR TO &screenatr
		STORE '9' TO choice

	CASE choice = '7'
		DO timwolog

	CASE choice = '8'
		SELECT timpayro
		set order to 1
		IF ms_level = '1'
			SET DELETED On
			BROWSE LAST
		ELSE
			BROWSE LAST && NOEDIT NODELETE NOAPPEND
		ENDIF

	CASE choice = '9'
		CLOSE DATABASES
		RETURN

	CASE choice = 'T'
		DO timtime.spx

	CASE choice = 'V'
*		SELECT 2
		SELECT timpayro
		SET ORDER TO emp_no
		STORE 'Y' TO PRINTER
		@ 15,0 CLEAR
		@ 15,12 SAY 'Enter Technician number or Enter [A] for all' GET mtechnician PICTURE '!!!!!!!'
		@ 16,12 SAY "Please enter Payroll date " GET mdate_paid PICTURE '  /  /    '
		READ
		CLEAR
		IF LEFT(mtechnician,1) = 'A'
			REPORT FORM commissi FOR date_paid = mdate_paid PREVIEW
			@ 17,12 SAY 'Would you like to print this report?   Y / N' GET PRINTER PICTURE '!'
			READ
			IF PRINTER = 'Y'
				REPORT FORM commissi FOR date_paid = mdate_paid  TO PRINT
			ENDIF
		ELSE
			REPORT FORM commissi FOR mtechnician=emp_no .AND. date_paid = mdate_paid PREVIEW
			@ 17,12 SAY 'Would you like to print this report?   Y / N' GET PRINTER PICTURE '!'
			READ
			IF PRINTER = 'Y'
				REPORT FORM commissi FOR mtechnician=emp_no .AND. date_paid = mdate_paid TO PRINT
			ENDIF
		ENDIF

	CASE choice = 'R'
		CLEAR
		SELECT 2
		set order to emp_no
		STORE 'S' TO PRINTER
		@ 15,12 SAY 'Enter Technician number or Enter [A] for all' GET mtechnician PICTURE '!!!!!!!'
		@ 16,12 SAY 'Printer or Screen    Choose P or S ' GET PRINTER PICTURE '!'
		READ
		STORE UPPER(printer) TO printer
		IF LEFT(mtechnician,1) = 'A'
			IF PRINTER = 'P'
				REPORT FORM commissi  FOR DTOC(date_apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. ALLTRIM(RED_FLAG) = 'Y'  TO PRINT
			ELSE
				REPORT FORM commissi FOR DTOC(date_apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. ALLTRIM(RED_FLAG) = 'Y'  PREVIEW
			ENDIF
		ELSE
			IF UPPER(PRINTER) = 'P'
				REPORT FORM commissi  FOR mtechnician=emp_no .AND. DTOC(date_apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    '  .AND. ALLTRIM(RED_FLAG) = 'Y' TO PRINT
			ELSE
				REPORT FORM commissi FOR mtechnician=emp_no .AND. DTOC(date_apprv) <> '  /  /    ' .AND. DTOC(date_paid) = '  /  /    ' .AND. ALLTRIM(RED_FLAG) = 'Y'  PREVIEW
			ENDIF
		ENDIF
	
	
	CASE choice = 'B'
		SELECT 3
		STORE 'Y' to yn
		@ 1, 0 TO 23, 80 CLEAR
		REPORT FORM TOOLBENIFIT FOR Job_Title = 'ELECTRICIA' PREVIEW
		@ 23, 11 SAY 'Would you like to print these forms to the printer?  ' GET yn PICTURE '!'			
		READ
		IF yn = 'Y'
			REPORT FORM TOOLBENIFIT FOR Job_Title = 'ELECTRICIA' TO PRINT
		ENDIF

	CASE choice = 'D'
		DO TIMPAYRO.SPR
		
	
	ENDCASE

ENDDO
*: EOF: TIMPAYRO.PRG
