*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMWREPO.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 02/07/96 at 17:13:20
*:
*:      Called by: TIM1MENU.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF
*:               : EMPLOYEE.DBF
*:           	 : TIMTIME.DBF
*:
*:
*:   Report Forms: CUSTOMER.FRX
*:               : PRIORITY.FRX
*:               : TECH.FRX
*:
*:      Documented 12:29:57                                FoxDoc version 3.00a
*:*****************************************************************************


CLEAR
*  ---Set Global Variables
CLEAR GETS
SET COLOR ON
SET COLOR TO G/N,W/N
STORE '*****  W O R K O R D E R   R E P O R T S *****' TO TITLE
STORE 40 - (LEN(serial)/2) TO SCOL
STORE 40 - (LEN(TITLE)/2) TO COL
STORE 'TIMWREPO.PRG' TO prg_name
DO addtitle

***********************************
***		Select Databases		***
***********************************



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



*	SELECT 2 TO EMPLOYEE

IF USED("EMPLOYEE")
	SELECT EMPLOYEE
	SET ORDER TO 1
ELSE
	SELECT 2
	USE (LOCFILE("employee.dbf","DBF","Where is Employee?"));
	AGAIN ALIAS employee ;
	order 1
ENDIF


SELECT 3 
USE report4 EXCL
ZAP
USE
USE report4

SELECT 4
USE report23 EXCL
ZAP
USE
USE report23

SELECT 5
USE report24 EXCL
ZAP
USE
USE report24



STORE 9 TO choice
STORE 'S' TO PRINTER
STORE DATE()-14 TO bdate
STORE DATE() TO edate

   TEXT

	1 -- Advertising by Referral
	11 - Advertising by Referral (sorted by closedate)
	2 -- Report by Ref for Self Generated Commissions
	3 -- jobs done sorted BY technician
	4 -- performance reports BY technician (CLOSE DATE)
	41 - Performance reports By Tech (CLOSE DATE) accumulative
	5 -- Performance & Bonus reports
	6 -- Workorders dispatched and not closed
	7 -- Missing Workorder Report
	8 -- Weekly New Dispatch report
	9 -- RETURN TO main MENU
	10 - Time Card vs Workorder efficiency
	111- Calls to dispatch
	12 - Performance reports By technician (CLOSE DATE) Detailed
	13 - Time Card Reports
   	14 - Service Charge & Preferred Cards sold
	15 - Price Service Book
	16 - Print mailing labels for customers
	17 - Time per workorder report
	171- Time per workorder report SUMMARY
	172- Time per Customer Name
	18 - Letter of Hours
	19 - Employee License list (Drivers & Professional)
	20 - Job time report per day
	21 - Payroll Reports (by tech+wo)
	22 - Workorders turned in
	23 - rpt #4 with high & Low removed
	24 - rpt #23 with increment options
	25 - On call or after hours call & Bonus
	26 - Call Records by Technician
	27 - Contest Reports
	28 - Calls per day summary report
	29 - Override Report per year
	30 - Average per day
	31 - Calls taken per Tech by Dispatch Date
	32 - Summary Payroll Report Detail per Tech	
	33 - Customer Mail List Generator
	34 - Cancelled Calls Report
	
   ENDTEXT
@ 42,5 SAY 'Please enter choice   ' GET choice PICTURE '999'
READ
IF choice = 9
	RETURN
ENDIF

IF choice = 13
	DO timecard
	RETURN
ENDIF

IF choice = 10
	DO timeffic
	RETURN
ENDIF

DO CASE

CASE choice = 1

	CLEAR

	***--- ADVERTISING REPORT BY REFERRAL
	STORE 'A      ' TO refno
	STORE 'C' TO match

	*	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99/9999'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99/9999'
	@ 22,5 SAY 'Enter Ref No to search [A] FOR ALL  ' GET refno PICTURE '@k !!!!!!!'
	@ 23,5 SAY 'Search for exact match or Content   E or C' GET match PICTURE '!'
	READ

	DO ChooseTrade
	
	STORE ALLTRIM(refno) TO refno
	STORE RIGHT(DTOC(bdate),4) TO YEAR
	SELECT 1 && TIMWORK
	SET order TO ref
	IF ALLTRIM(mtrade) = 'ALL'
		* DO NOTHING
	ELSE
		SET FILTER TO ALLTRIM(TRADE) = ALLTRIM(mtrade)
	ENDIF
	SET FIELDS TO DATE,name,amount,ref,trade,city
	IF ALLTRIM(refno) = 'A'
		COPY ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(DTOC(DATE),5) >= LEFT(DTOC(bdate),5) .AND. LEFT(DTOC(DATE),5) <= LEFT(DTOC(edate),5) TO report1
	ELSE	
		COPY ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(DTOC(DATE),5) >= LEFT(DTOC(bdate),5) .AND. LEFT(DTOC(DATE),5) <= LEFT(DTOC(edate),5) .and. alltrim(refno)$ref TO report1
	ENDIF
	SELECT 0
	USE REPORT1
	SORT ON REF TO REPORT1A
	USE report1A
	IF ALLTRIM(refno) = 'A'
		REPORT FORM refersum PREVIEW
	ELSE
		IF match = 'E'
			REPORT FORM refsumde FOR ref = refno PREVIEW
		ELSE
			REPORT FORM refsumde FOR refno$ref PREVIEW
		ENDIF
	ENDIF
	STORE 'N' TO yn
	@ 23,10 SAY 'Would you like to Export this report to Excel?   Y / N   ' GET yn PICTURE '!'
	READ

** to printer
*	IF UPPER(yn) = 'Y'
*		IF ALLTRIM(refno) = 'A'
*			REPORT FORM refersum TO PRINT
*		ELSE
*			IF match = 'E'
*				REPORT FORM refsumde FOR ref = refno TO PRINT
*			ELSE
*				REPORT FORM refsumde FOR refno$ref TO PRINT
*			ENDIF
*		ENDIF
*	ENDIF


* export to excel
	IF UPPER(yn) = 'Y'
		IF ALLTRIM(refno) = 'A'
			EXPORT TO REPORT1 TYPE XLS
		ELSE
			IF match = 'E'
				EXPORT TO REPORT1 FOR ref = refno TYPE XLS
			ELSE
				EXPORT TO REPORT1 FOR refno$ref TYPE XLS
			ENDIF
		ENDIF
	ENDIF
	SET FILTER TO
	








CASE choice = 2

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	STORE '       ' TO qryref
	*	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	*	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	*	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	@ 22,5 SAY 'Enter Referral number to search     ' GET qryref PICTURE '!!!!!!!'
	READ



	***--- all customers sorted by priority
	CLEAR
	@ 10,20 SAY 'COMPILING DATA'

	SELECT 1
	SET ORDER TO priority
	REPORT FORM refdet FOR ref = qryref  PREVIEW
	STORE 'N' TO yn
	@ 23,5 SAY 'PRINT REPORT?   Y / N' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM refdet FOR ref = qryref TO PRINT
	ENDIF


CASE choice = 3

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ


	CLEAR
	@ 10,20 SAY 'COMPILING DATA'
	SET CONSOLE OFF

	***--- all customers sorted by technician
	SELECT 1
	SET ORDER TO technician
	REPORT FORM tech FOR CTOD(DATE) >= bdate .AND. CTOD(DATE) <= edate .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO PRINT


CASE choice = 4	
	STORE '1' TO mstate

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	@ 22,5 SAY 'Enter State   1=UT  2=TX			' GET mstate PICTURE '9'
	READ
	DO ChooseTrade
	STORE MTRADE TO M.TRADE


	IF M.TRADE = 'ALL'
	    SET FILTER TO
	ELSE
		SET FILTER TO ALLTRIM(trade)= ALLTRIM(m.trade)
	ENDIF


	*******************
	* reset paramters *
	*******************

	STORE '       ' TO empno
	STORE SPACE(20) TO mtechnician
	STORE 0 TO est
	STORE 0.00 TO est_rev
	STORE 0 TO OTHER
	STORE 0.00 TO oth_rev
	STORE 0 TO w_parts
	STORE 0 TO open_calls
	STORE 0 TO ALL
	STORE 0.00 TO combine_rev
	STORE 0.00 TO AVERAGE
	STORE 0 TO calls_w_rev
	STORE 0.00 TO avg_w_rev
	STORE 0 TO CANCEL
	STORE 'S' TO ps
	STORE 1 TO x
	STORE 'SERVICE SUMMARY REPORT (by close date)' TO  TITLE
	STORE 66 - (LEN(TITLE)/2) TO y
	STORE 60 TO lines
	STORE 0 TO test
	STORE 0.00 TO test_rev
	STORE 0 TO tother
	STORE 0.00 TO toth_rev
	STORE 0 TO tw_parts
	STORE 0 TO topen_calls
	STORE 0 TO tall
	STORE 0.00 TO tcombine_rev
	STORE 0.00 TO taverage
	STORE 0 TO tcalls_w_rev
	STORE 0.00 TO tavg_w_rev
	STORE 0 TO tcancel
	STORE RIGHT(DTOC(bdate),4) TO YEAR
	IF MSTATE = '1'
		STORE 'UT' TO M.STATE
	ENDIF
	IF MSTATE = '2'
		STORE 'TX' TO M.STATE
	ENDIF
	
	* set index to employee number *


	CLEAR
	@ 10,20 SAY 'COMPILING DATA'
	SELECT 1  && TIMWORK
	SET ORDER TO technician

	SELECT 2	&& EMPLOYEE
	SET ORDER TO EMP_NO

	GOTO TOP
	STORE RECNO() TO jn
	STORE 0 TO pass


	DO WHILE .NOT. EOF()
		GOTO jn
		IF ALLTRIM(emp_no) = ' '
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF

		IF ALLTRIM(emp_no) < '001'
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF
		IF ALLTRIM(empno) = ALLTRIM(emp_no)
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF


		STORE emp_no TO empno
		STORE name TO mtechnician
		SELECT 1	&& TIMWORK



	IF M.TRADE = 'ALL'
	    SET FILTER TO
	ELSE
		SET FILTER TO ALLTRIM(trade)= ALLTRIM(m.trade)
	ENDIF




		STORE pass + 1 TO pass
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state TO m.combine_rev

		IF m.combine_rev = 0
			SELECT 2	&& EMPLOYEE
			GOTO jn
			IF .NOT. EOF()
				SKIP 1
				STORE RECNO() TO jn
				LOOP
			ELSE
				EXIT
			ENDIF
		ENDIF

		STORE tcombine_rev + m.combine_rev TO tcombine_rev

		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state .AND. ALLTRIM(priority) = '02' TO m.est
		STORE test + m.est TO test
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(priority) = '02' .AND. state = m.state TO m.est_rev
		STORE test_rev + m.est_rev TO test_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state .AND. priority <> '02' TO m.other
		STORE tother + m.other TO tother
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. priority <> '02' .AND. state = m.state TO m.oth_rev
		STORE toth_rev + m.oth_rev TO toth_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state .AND. material > 0 TO m.w_parts
		STORE tw_parts + m.w_parts TO tw_parts
		COUNT ALL FOR technician = empno .AND. closedate =  ' ' .AND. state = m.state TO m.open_calls
		STORE topen_calls + m.open_calls TO topen_calls
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5)  .AND. state = m.state TO m.all
		STORE tall + m.all TO tall
		STORE combine_rev / ALL TO m.average
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state .AND. amount > 0 TO m.calls_w_rev
		STORE tcalls_w_rev + m.calls_w_rev TO tcalls_w_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state .AND. paid = 'CNCL' TO m.cancel
		STORE tcancel + m.cancel TO tcancel
		STORE combine_rev / calls_w_rev TO m.avg_w_rev

		SELECT 3	&& REPORT4
		GOTO BOTTOM
		APPEND BLANK
		REPLACE empno WITH m.empno
		REPLACE technician WITH mtechnician
		REPLACE est WITH m.est
		REPLACE est_rev WITH m.est_rev
		REPLACE OTHER WITH m.other
		REPLACE oth_rev WITH m.oth_rev
		REPLACE w_parts WITH m.w_parts
		REPLACE open_calls WITH m.open_calls
		REPLACE ALL WITH m.all
		REPLACE combine_re WITH m.combine_rev
		REPLACE AVERAGE WITH m.average
		REPLACE call_w_rev WITH m.calls_w_rev
		REPLACE avg_w_rev WITH m.avg_w_rev
		REPLACE CANCEL WITH m.cancel


		SELECT 2	&& EMPLOYEE
		GOTO jn
		IF .NOT. EOF()
			SKIP 1
			STORE RECNO() TO jn
		ENDIF
	ENDDO while .not. eof()

	*               STORE x+1 TO x
	*STORE tcombine_rev / tall TO taverage
	*STORE tcombine_rev / tcalls_w_rev TO tavg_w_rev
	*COUNT ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. state = m.state TO totcalls
	*@ x,0 SAY 'TOTAL FOR ALL   '
	*@ x,35 SAY test PICTURE '999'
	*@ x,41 SAY test_rev PICTURE '99999.99'
	*@ x,52 SAY tother PICTURE '999'
	*@ x,59 SAY toth_rev PICTURE '99999.99'
	*@ x,70 SAY tw_parts PICTURE '999'
	*@ x,75 SAY topen_calls PICTURE '999'
	*@ x,80 SAY tall PICTURE '999'
	*@ x,86 SAY tcombine_rev PICTURE '99999.99'
	*@ x,98 SAY taverage PICTURE '99999.99'
	*@ x,108 SAY tcalls_w_rev PICTURE '999'
	*@ x,113 SAY tavg_w_rev PICTURE '999.99'
	*@ x,125 SAY tcancel PICTURE '999'
	*SET DEVICE TO SCREEN
	*SET CONSOLE ON
	SELECT 3
	REPORT FORM report4 PREVIEW
	STORE 'Y' TO PRINT
	CLEAR
	@ 10,20 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?  Y / N   (E for Exel) ' GET PRINT PICTURE '!'
	READ
	IF PRINT = 'Y'
		REPORT FORM report4 TO PRINT
	ENDIF
	IF PRINT = 'E'
		EXPORT TO REPORT4 TYPE XLS
	ENDIF

CASE choice = 41
	do data
	
CASE choice = 5

	CLEAR

	*******************
	* reset paramters *
	*******************

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ




	SELECT 1	&& TIMWORK
	SET FIELDS TO DATE,name,no,phone,amount,technician,material,paid,closedate,cancel_cod,solddate,trade
	COPY ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(solddate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)='S' TO REPORT5
	SELECT 0
	USE REPORT5
	REPORT FORM REPORT5 PREVIEW
	STORE 'N' TO yn
	@ 23,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM REPORT5 TO PRINT
	ENDIF








CASE choice = 6
	CLEAR
	STORE 'N' TO YN


*	STORE RIGHT(DTOC(DATE()),4) TO YEAR
*	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
*	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
*	READ
	SELECT timwork
	set filter to empty(closedate)
	set fields to date, no, incident, name, technician, closedate
	REPORT FORM NOTCLOSE PREVIEW
	@ 23,5 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?   Y/N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM NOTCLOSE TO PRINT
	ENDIF


CASE choice = 7

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	READ

	**** Workorders turned in to the office
	SELECT 1
	SET ORDER TO technician
	STORE 'N' TO yn
	REPORT FORM missingw FOR SUBSTR(closedate,7,4) = YEAR .AND. DTOC(rcvdate) = '  /  /  ' .AND. dispdate > '             ' .AND. no > '       ' PREVIEW
	@ 20,5 SAY 'Print this report   Y / N ' GET yn PICTURE '!'
	READ
	@ 20,5 CLEAR
	IF UPPER(yn) = 'Y'
		REPORT FORM missingw FOR DTOC(rcvdate) = '  /  /  ' .AND. dispdate > '             ' .AND. no > '       ' TO PRINT
	ENDIF



CASE choice = 8

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ

	SELECT 1
	SET TALK OFF
	REPORT FORM newdispa FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate PREVIEW
	STORE 'N' TO yn
	@ 7,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM newdispa FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate TO PRINT
	ENDIF


CASE choice = 9

	CLEAR

	RETURN



CASE choice = 111
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ


	CLEAR
	SELECT 1
	SET TALK OFF
	REPORT FORM newdispa FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >=bdate .AND. closedate = '             ' .AND. dispdate = '             ' .AND. paid = '         ' PREVIEW
	STORE 'N' TO yn
	@ 7,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM newdispa FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >=bdate .AND. closedate = '             ' .AND. dispdate = '             ' .AND. paid = '         ' TO PRINT
	ENDIF

CASE choice = 12
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ
	*******************
	* reset paramters *
	*******************
IF USED("timwork")
	SELECT timwork
	SET ORDER TO technician
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER technician
ENDIF

	SET FIELDS TO DATE,name,no,phone,amount,technician,material,paid,closedate,cancel_cod,trade,aux_work,service,hours,hours1,special
	STORE 'N' TO yn
	STORE 'Y' TO est
	STORE 'Y' TO rwk
	@ 22,0 TO 24,80 CLEAR
	@ 22,5 SAY 'DO YOU WANT TO INCLUDE CANCELS? Y/N' GET yn PICTURE '!'
	@ 23,5 SAY 'DO YOU WANT TO INCLUDE ESTIMATES? Y/N' GET est PICTURE '!'
	@ 24,5 SAY 'DO YOU WANT TO INCLUDE REWORKS? Y/N' GET rwk PICTURE '!'
	READ

	DO ChooseTrade

	STORE ' ' TO filters

	IF yn = 'N'
		STORE ' .AND. CANCEL_COD = " " .AND. AMOUNT <> 0 ' TO filters
	ENDIF
	IF est = 'N'
		STORE filters + ' .AND. LEFT(ALLTRIM(PAID),4) <> "ESTI" .AND. AMOUNT <> 0' TO filters
	ENDIF
	IF rwk = 'N'
		STORE filters + ' .AND. LEFT(ALLTRIM(PAID),4) <> "RWRK" .AND. AMOUNT <> 0 ' TO filters
	ENDIF

	IF ALLTRIM(mtrade) = 'ALL'
		COPY ALL FOR SUBSTR(closedate,7,4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) &filters TO report
	ELSE
		COPY ALL FOR SUBSTR(closedate,7,4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(mtrade) &filters TO report
	ENDIF


	SELECT 0


IF USED("report")
	SELECT report
	SET ORDER TO 0
ELSE
	SELECT 0
	USE (LOCFILE("report.dbf","DBF","Where is report?"));
		AGAIN ALIAS report ;
		ORDER 0
ENDIF

SUM ALL amount FOR TRADE = 'ELC' TO ELCAmt
SUM ALL amount FOR TRADE = 'PLM' TO PLMAmt
SUM ALL amount FOR TRADE = 'HVC' TO HVCAmt
SUM ALL amount FOR TRADE = 'AEM' TO AEMAmt
SUM ALL amount FOR TRADE = 'SEC' TO SECAmt
SUM ALL amount FOR TRADE = 'TEL' TO TELAmt
COUNT ALL FOR TRADE = 'ELC' TO ELCCnt
COUNT ALL FOR TRADE = 'PLM' TO PLMCnt
COUNT ALL FOR TRADE = 'HVC' TO HVCCnt
COUNT ALL FOR TRADE = 'AEM' TO AEMCnt
COUNT ALL FOR TRADE = 'SEC' TO SECCnt
COUNT ALL FOR TRADE = 'TEL' TO TELCnt



	REPORT FORM report1 PREVIEW
	STORE 'N' TO yn
	@ 22,0 TO 24,80 CLEAR
	@ 23,10 SAY 'Would you like to print this report?   [Y]es / [N]o / [E]xcel  ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM report1 TO PRINT
	ENDIF
	IF UPPER(yn) = 'E'
		export to report12 type xls
	ENDIF





CASE choice = 14
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ

	CLEAR
	SELECT 1
	SET ORDER TO technician
	SET TALK OFF
	REPORT FORM service FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate .AND. service > 0 PREVIEW
	REPORT FORM prefcard FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate .AND. preferred > 0 PREVIEW
	STORE 'N' TO yn
	@ 23,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM service FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate .AND. service > 0 TO PRINT
		REPORT FORM prefcard FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate .AND. preferred > 0 TO PRINT
	ENDIF

CASE choice = 15
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ

	SELECT 3
	USE timstock share
	SET ORDER TO descrip
	REPORT FORM pricebook FOR pbook = 'Y' PREVIEW
	STORE 'N' TO yn
	@ 23,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM pricebook FOR pbook = 'Y' TO PRINT
	ENDIF

CASE choice = 16
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ


CASE choice = 17

	CLEAR


	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ

	SELECT 0
	USE timtime share
	SET TALK OFF
	SORT ON wo FOR DATE <= edate .AND. DATE >=bdate TO REPORT17

	USE REPORT17
	SET order TO wo, tech

	STORE 'N' TO yn
	REPORT FORM jobtime PREVIEW
	@ 23,5 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?   Y/N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM jobtime TO PRINT
	ENDIF

CASE choice = 171

	CLEAR

	STORE '       ' TO mwo
	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 18,5 SAY 'Enter Job Number to use in report   ' GET mwo PICTURE '9999999'
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ

	SELECT 0
	USE timtime share
	SET TALK OFF
	SORT ON wo FOR DATE <= edate .AND. DATE >=bdate .AND. wo=mwo TO REPORT171

	USE REPORT171
	SET ORDER TO tech, DAY

	STORE 'N' TO yn
	REPORT FORM hrsjob PREVIEW
	@ 23,5 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?   Y/N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM hrsjob TO PRINT
	ENDIF

CASE choice = 172


CASE choice = 18 && Print letter of hours

	SELECT 3
	USE timtime
	CLEAR
	@ 20,5 SAY 'Enter Employee number to report     ' GET emp_no PICTURE '999'
	READ
	REPORT FORM ltr_hrs TO PRINT

CASE choice = 19 && Print License list, choose from Drivers or Electricians license

	STORE 'N' TO yn
	STORE ' ' TO DE
	SELECT 2 	&& employee
	SET ORDER TO emp_no
	CLEAR TYPEAHEAD
	DEFINE WINDOW mlicmenu FROM 13,20 TO 23,75 DOUBLE
	ACTIVATE WINDOW mlicmenu

	@ 2,2 SAY 'Enter [D] for Drivers Lic      '
	@ 3,2 say 'Enter [E] for Electricians Lic '
	@ 5,2 SAY 'Please Choose which list to print: ' GET DE
	READ
	DEACTIVATE WINDOW mlicmenu
	RELEASE WINDOW mlicmenu

	IF UPPER(DE) = 'E'
 		REPORT FORM lic_list FOR elect_lic < 'A' .AND. elect_lic > ' ' PREVIEW
	ENDIF
	IF UPPER(DE) = 'D'
		REPORT FORM dl_list for DRIVER_lic > ' ' PREVIEW
	ENDIF
	
	
	WAIT WINDOW 'Would you like to print this report?   Y / N ' TO yn


	IF UPPER(yn) = 'Y' .AND. UPPER(DE) = 'E'
		REPORT FORM lic_list FOR elect_lic < 'A' .AND. elect_lic > ' ' TO PRINT
	ENDIF

	IF UPPER(yn) = 'Y' .AND. UPPER(DE) = 'D'
		REPORT FORM DL_list FOR  DRIVER_lic > ' ' TO PRINT
	ENDIF


CASE choice = 20

	SELECT 0
	USE timtime
	CLEAR
	@ 20,5 SAY 'Enter Job number to report     ' GET jobno PICTURE '9999999'
	READ
	REPORT FORM slcctime FOR wo = jobno PREVIEW
	STORE 'N' TO yn
	@ 23,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM lic_list FOR elect_lic < 'A' .AND. elect_lic > ' ' TO PRINT
	ENDIF
	REPORT FORM slcctime FOR wo = jobno PREVIEW


CASE choice = 21
	**  Report  for payroll by employee number and wo number **

	CLEAR


	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ

	SELECT 0
	USE timtime share
	SET TALK OFF
	SORT ON wo FOR DATE <= edate .AND. DATE >=bdate .AND. RIGHT(DTOC(DATE),4) = YEAR TO REPORT21
	

	USE REPORT21
	SET ORDER TO tech, wo

	STORE 'N' TO yn
	REPORT FORM payroll PREVIEW
	@ 23,5 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?   Y/N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM payroll TO PRINT
	ENDIF

CASE choice = 22
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ


	STORE 'N' TO yn
	REPORT FORM rcvdate FOR CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate .AND. closedate = '             ' .AND. dispdate > '             ' .AND. technician > '       ' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) PREVIEW
	@ 23,5 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?   Y/N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM rcvdate FOR closedate = '             ' .AND. CTOD(DATE) <= edate .AND. CTOD(DATE) >= bdate .AND. dispdate > '             ' .AND. technician > '       ' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO PRINT
	ENDIF



CASE choice = 23
	**************************************************************
	*      report Number 4 with High and Low removed             *
	**************************************************************
	STORE '1' TO mstate
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	@ 22,5 SAY 'Enter State 	1=UT	2=TX        ' GET mstate PICTURE '9'
	READ
	DO ChooseTrade
	STORE MTRADE TO M.TRADE

	*******************
	* reset paramters *
	*******************

	STORE '       ' TO empno
	STORE SPACE(20) TO mtechnician
	STORE 0 TO est
	STORE 0.00 TO est_rev
	STORE 0 TO OTHER
	STORE 0.00 TO oth_rev
	STORE 0 TO w_parts
	STORE 0 TO open_calls
	STORE 0 TO ALL
	STORE 0.00 TO combine_rev
	STORE 0.00 TO AVERAGE
	STORE 0 TO calls_w_rev
	STORE 0.00 TO avg_w_rev
	STORE 0 TO CANCEL
	STORE 'S' TO ps
	STORE 1 TO x
	STORE 'SERVICE SUMMARY REPORT (by close date)' TO  TITLE
	STORE 66 - (LEN(TITLE)/2) TO y
	STORE 60 TO lines
	STORE 0 TO test
	STORE 0.00 TO test_rev
	STORE 0 TO tother
	STORE 0.00 TO toth_rev
	STORE 0 TO tw_parts
	STORE 0 TO topen_calls
	STORE 0 TO tall
	STORE 0.00 TO tcombine_rev
	STORE 0.00 TO taverage
	STORE 0 TO tcalls_w_rev
	STORE 0.00 TO tavg_w_rev
	STORE 0 TO tcancel
	STORE RIGHT(DTOC(bdate),4) TO YEAR
	STORE 0.00 TO HIGH
	STORE 0.00 TO low
	IF mstate = '1'
		STORE 'UT' TO m.state
	ENDIF
	IF mstate = '2'
		STORE 'TX' TO m.state
	ENDIF
	

	* set index to employee number *


	CLEAR
	@ 10,20 SAY 'COMPILING DATA'
	SELECT 1  && TIMWORK
	SET ORDER TO technician

	SELECT 2	&& EMPLOYEE
	SET ORDER TO EMP_NO

	GOTO TOP
	STORE RECNO() TO jn
	STORE 0 TO pass


	DO WHILE .NOT. EOF()
		GOTO jn
		IF ALLTRIM(emp_no) = ' '
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF

		IF ALLTRIM(emp_no) < '001'
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF
		IF ALLTRIM(empno) = ALLTRIM(emp_no)
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF


		STORE emp_no TO empno
		STORE name TO mtechnician
		SELECT 1	&& TIMWORK
		STORE pass + 1 TO pass

		** TOTAL ALL REVENUE FOR TECHNICIAN TO M.COMBINE_REV **
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade).AND. state = m.state TO m.combine_rev
		
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade).AND. state = m.state .AND. amount > 0 TO m.all


		** GOTO NEXT TECH IF NO REVENUE **
		IF m.combine_rev = 0
			SELECT 2	&& EMPLOYEE
			GOTO jn
			IF .NOT. EOF()
				SKIP 1
				STORE RECNO() TO jn
				LOOP
			ELSE
				EXIT
			ENDIF
		ENDIF


		** DETERMINE HIGHEST AND LOWEST FOR THIS TECHNICIAN **
		GOTO TOP
		STORE 0.00 TO HIGH
		STORE 999999.00 TO low
		store 0 to LowFound
		STORE 0 TO HighFound
		LOCATE FOR empno = technician .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade).AND. state = m.state .AND. amount > 0
		DO WHILE .NOT. EOF()
			IF amount > HIGH
				STORE amount TO HIGH
				STORE 1 TO HighFound
			ENDIF
			IF amount < low
				STORE amount TO low
				STORE 1 TO LowFound
			ENDIF
			CONTINUE
		ENDDO




		** REMOVE THE HIGHEST AND LOWEST WORKORDER

			** REMOVE THE HIGHEST & LOWEST COUNT
			
		STORE m.all - LowFound - HighFound to m.all
		IF m.all =< 0
			STORE 0 to m.all
		ENDIF

		STORE m.all TO tall

		
			** REMOVE THE HIGHEST AND LOWEST AMOUNT
			
		STORE m.combine_rev - HIGH - low TO m.combine_rev
		IF m.combine_rev <= 0
			STORE 0.00 TO m.combine_rev
		ENDIF


		IF m.all <= 0
			STORE 0.00 TO m.average
		ELSE
			STORE m.combine_rev / m.all TO m.average
		ENDIF


		STORE m.combine_rev TO tcombine_rev



		** CALCULATE ESTIMATE REVENUE **
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. ALLTRIM(priority) = '02';
		.AND. state = m.state .AND. amount > 0 TO m.est
		STORE test + m.est TO test

		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(priority) = '02' .AND. ALLTRIM(trade)=ALLTRIM(m.trade);
		.AND. state = m.state .AND. amount > 0 TO m.est_rev
		STORE m.est_rev TO test_rev



		** CALCULATE OTHER REVENUE **
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. priority <> '02';
		.AND. state = m.state .AND. amount > 0  TO m.other
		
		IF m.other <= 2
			STORE 0 TO m.other
		ELSE
			STORE m.other - 2 TO m.other
		ENDIF
		STORE tother + m.other TO tother

		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. priority <> '02' .AND. ALLTRIM(trade)=ALLTRIM(m.trade);
		.AND. state = m.state .AND. amount > 0  TO m.oth_rev
		STORE m.oth_rev - HIGH - low TO m.oth_rev
		IF m.oth_rev <= 0.00
			STORE 0.00 TO m.oth_rev
		ENDIF
		STORE m.oth_rev TO toth_rev




		** CALCULATE ALL CALLS WITH REVENUE
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. state = m.state ;
		.AND. amount > 0 TO m.calls_w_rev
		IF m.calls_w_rev <= 2
			STORE 0 TO m.calls_w_rev
		ELSE
			STORE m.calls_w_rev - 2 TO m.calls_w_rev
		ENDIF
		STORE m.calls_w_rev TO tcalls_w_rev
		IF m.calls_w_rev <= 0
			STORE 0.00 TO m.avg_w_rev
		ELSE
			STORE m.combine_rev / m.calls_w_rev TO m.avg_w_rev
		ENDIF



		** CALCULATE PARTS SOLD, OPEN CALLS, PENDING FILE, CANCELS
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. state = m.state .AND. material > 0 TO m.w_parts
		STORE tw_parts + m.w_parts TO tw_parts
		COUNT ALL FOR technician = empno .AND. closedate =  ' ' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.open_calls
		STORE topen_calls + m.open_calls TO topen_calls
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5);
		.AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. state = m.state ;
		.AND. paid = 'CNCL' TO m.cancel
		STORE tcancel + m.cancel TO tcancel



		SELECT 4
		GOTO BOTTOM
		APPEND BLANK
		REPLACE empno WITH m.empno
		REPLACE technician WITH mtechnician
		REPLACE est WITH m.est
		REPLACE est_rev WITH m.est_rev
		REPLACE OTHER WITH m.other
		REPLACE oth_rev WITH m.oth_rev
		REPLACE w_parts WITH m.w_parts
		REPLACE open_calls WITH m.open_calls
		REPLACE ALL WITH m.all
		REPLACE combine_re WITH m.combine_rev
		REPLACE AVERAGE WITH m.average
		REPLACE call_w_rev WITH m.calls_w_rev
		REPLACE avg_w_rev WITH m.avg_w_rev
		REPLACE CANCEL WITH m.cancel


		SELECT 2	&& EMPLOYEES
		GOTO jn
		IF .NOT. EOF()
			SKIP 1
			STORE RECNO() TO jn
		ENDIF
	ENDDO while .not. eof()

	*               STORE x+1 TO x
	*STORE tcombine_rev / tall TO taverage
	*STORE tcombine_rev / tcalls_w_rev TO tavg_w_rev
	*COUNT ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO totcalls
	*@ x,0 SAY 'TOTAL FOR ALL   '
	*@ x,35 SAY test PICTURE '999'
	*@ x,41 SAY test_rev PICTURE '99999.99'
	*@ x,52 SAY tother PICTURE '999'
	*@ x,59 SAY toth_rev PICTURE '99999.99'
	*@ x,70 SAY tw_parts PICTURE '999'
	*@ x,75 SAY topen_calls PICTURE '999'
	*@ x,80 SAY tall PICTURE '999'
	*@ x,86 SAY tcombine_rev PICTURE '99999.99'
	*@ x,98 SAY taverage PICTURE '99999.99'
	*@ x,108 SAY tcalls_w_rev PICTURE '999'
	*@ x,113 SAY tavg_w_rev PICTURE '999.99'
	*@ x,125 SAY tcancel PICTURE '999'
	*SET DEVICE TO SCREEN
	*SET CONSOLE ON
	SELECT 4
	REPORT FORM report23 PREVIEW
	STORE 'Y' TO PRINT
	CLEAR
	@ 10,20 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?  Y / N / E for Excel' GET PRINT PICTURE '!'
	READ
	IF PRINT = 'Y'
		REPORT FORM report23 TO PRINT
	ENDIF
	IF PRINT = 'E'
		EXPORT TO report23 TYPE XLS
	ENDIF



CASE choice = 24

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	READ


	*******************
	* reset paramters *
	*******************

	STORE '       ' TO empno
	STORE SPACE(20) TO mtechnician
	STORE 0 TO est
	STORE 0.00 TO est_rev
	STORE 0 TO OTHER
	STORE 0.00 TO oth_rev
	STORE 0 TO w_parts
	STORE 0 TO open_calls
	STORE 0 TO ALL
	STORE 0.00 TO combine_rev
	STORE 0.00 TO AVERAGE
	STORE 0 TO calls_w_rev
	STORE 0.00 TO avg_w_rev
	STORE 0 TO CANCEL
	STORE 'S' TO ps
	STORE 1 TO x
	STORE 'SERVICE SUMMARY REPORT (by close date)' TO  TITLE
	STORE 66 - (LEN(TITLE)/2) TO y
	STORE 60 TO lines
	STORE 0 TO test
	STORE 0.00 TO test_rev
	STORE 0 TO tother
	STORE 0.00 TO toth_rev
	STORE 0 TO tw_parts
	STORE 0 TO topen_calls
	STORE 0 TO tall
	STORE 0.00 TO tcombine_rev
	STORE 0.00 TO taverage
	STORE 0 TO tcalls_w_rev
	STORE 0.00 TO tavg_w_rev
	STORE 0 TO tcancel
	STORE RIGHT(DTOC(bdate),4) TO YEAR
	STORE 0.00 TO HIGH
	STORE 0.00 TO low

	* set index to employee number *



	CLEAR
	@ 10,20 SAY 'COMPILING DATA'
	SELECT 1
	SET ORDER TO technician

	SELECT 2
	SET ORDER TO 1

	GOTO TOP
	STORE RECNO() TO jn
	STORE 0 TO pass


	DO WHILE .NOT. EOF()
		GOTO jn
		IF ALLTRIM(emp_no) = ' '
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF

		IF ALLTRIM(emp_no) < '001'
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF
		IF ALLTRIM(empno) = ALLTRIM(emp_no)
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF


		STORE emp_no TO empno
		STORE name TO mtechnician
		SELECT 1
		STORE pass + 1 TO pass

		** TOTAL ALL REVENUE FOR TECHNICIAN TO M.COMBINE_REV **
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0 TO m.combine_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0 TO m.all
		IF m.all =< 2
			STORE 0 TO m.all
		ELSE
			STORE m.all - 2 TO m.all
		ENDIF
		STORE m.all TO tall


		** GOTO NEXT TECH IF NO REVENUE **
		IF m.combine_rev = 0
			SELECT 2
			GOTO jn
			IF .NOT. EOF()
				SKIP 1
				STORE RECNO() TO jn
				LOOP
			ELSE
				EXIT
			ENDIF
		ENDIF


		** DETERMINE HIGHEST AND LOWEST FOR THIS TECHNICIAN **
		GOTO TOP
		STORE 0.00 TO HIGH
		STORE 999999.00 TO low
		LOCATE FOR empno = technician .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0
		DO WHILE .NOT. EOF()
			IF amount > HIGH
				STORE amount TO HIGH
			ENDIF
			IF amount < low
				STORE amount TO low
			ENDIF
			CONTINUE
		ENDDO


		** REMOVE THE HIGHEST AND LOWEST WORKORDER


		STORE m.combine_rev - HIGH - low TO m.combine_rev
		IF m.combine_rev <= 0
			STORE 0.00 TO m.combine_rev
		ENDIF

		IF m.all <= 0
			STORE 0.00 TO m.average
		ELSE
			STORE m.combine_rev / m.all TO m.average
		ENDIF


		STORE m.combine_rev TO tcombine_rev



		** CALCULATE ESTIMATE REVENUE **
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. ALLTRIM(priority) = '02' .AND. amount > 0 TO m.est
		STORE test + m.est TO test

		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(priority) = '02' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0 TO m.est_rev
		STORE m.est_rev TO test_rev



		** CALCULATE OTHER REVENUE **
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. priority <> '02' .AND. amount > 0  TO m.other
		IF m.other <= 2
			STORE 0 TO m.other
		ELSE
			STORE m.other - 2 TO m.other
		ENDIF
		STORE tother + m.other TO tother

		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. priority <> '02' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0  TO m.oth_rev
		STORE m.oth_rev - HIGH - low TO m.oth_rev
		IF m.oth_rev <= 0.00
			STORE 0.00 TO m.oth_rev
		ENDIF
		STORE m.oth_rev TO toth_rev




		** CALCULATE ALL CALLS WITH REVENUE
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0 TO m.calls_w_rev
		IF m.calls_w_rev <= 2
			STORE 0 TO m.calls_w_rev
		ELSE
			STORE m.calls_w_rev - 2 TO m.calls_w_rev
		ENDIF
		STORE m.calls_w_rev TO tcalls_w_rev
		IF m.calls_w_rev <= 0
			STORE 0.00 TO m.avg_w_rev
		ELSE
			STORE m.combine_rev / m.calls_w_rev TO m.avg_w_rev
		ENDIF



		** CALCULATE PARTS SOLD, OPEN CALLS, PENDING FILE, CANCELS
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. material > 0 TO m.w_parts
		STORE tw_parts + m.w_parts TO tw_parts
		COUNT ALL FOR technician = empno .AND. closedate =  ' ' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.open_calls
		STORE topen_calls + m.open_calls TO topen_calls
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. paid = 'CNCL' TO m.cancel
		STORE tcancel + m.cancel TO tcancel



		SELECT 5
		GOTO BOTTOM
		APPEND BLANK
		REPLACE empno WITH m.empno
		REPLACE technician WITH mtechnician
		REPLACE est WITH m.est
		REPLACE est_rev WITH m.est_rev
		REPLACE OTHER WITH m.other
		REPLACE oth_rev WITH m.oth_rev
		REPLACE w_parts WITH m.w_parts
		REPLACE open_calls WITH m.open_calls
		REPLACE ALL WITH m.all
		REPLACE combine_re WITH m.combine_rev
		REPLACE AVERAGE WITH m.average
		REPLACE call_w_rev WITH m.calls_w_rev
		REPLACE avg_w_rev WITH m.avg_w_rev
		REPLACE CANCEL WITH m.cancel


		SELECT 2
		GOTO jn
		IF .NOT. EOF()
			SKIP 1
			STORE RECNO() TO jn
		ENDIF
	ENDDO while .not. eof()

	*               STORE x+1 TO x
	*STORE tcombine_rev / tall TO taverage
	*STORE tcombine_rev / tcalls_w_rev TO tavg_w_rev
	*COUNT ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO totcalls
	*@ x,0 SAY 'TOTAL FOR ALL   '
	*@ x,35 SAY test PICTURE '999'
	*@ x,41 SAY test_rev PICTURE '99999.99'
	*@ x,52 SAY tother PICTURE '999'
	*@ x,59 SAY toth_rev PICTURE '99999.99'
	*@ x,70 SAY tw_parts PICTURE '999'
	*@ x,75 SAY topen_calls PICTURE '999'
	*@ x,80 SAY tall PICTURE '999'
	*@ x,86 SAY tcombine_rev PICTURE '99999.99'
	*@ x,98 SAY taverage PICTURE '99999.99'
	*@ x,108 SAY tcalls_w_rev PICTURE '999'
	*@ x,113 SAY tavg_w_rev PICTURE '999.99'
	*@ x,125 SAY tcancel PICTURE '999'
	*SET DEVICE TO SCREEN
	*SET CONSOLE ON
	SELECT 5
	REPORT FORM report24 PREVIEW
	STORE 'Y' TO PRINT
	CLEAR
	@ 10,20 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?  Y / N' GET PRINT PICTURE '!'
	READ
	IF PRINT = 'Y'
		REPORT FORM report24 TO PRINT
	ENDIF

CASE choice = 25
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report      ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report       ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report         ' GET edate PICTURE '99/99'

	READ


	*******************
	* reset paramters *
	*******************
	*	STORE 'report25.RPT' TO reportname
	use

IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
	AGAIN ALIAS timwork ;
	order 0
ENDIF
	

	SET FIELDS TO DATE,name,no,phone,amount,technician,material,paid,closedate,cancel_cod,trade,aux_work,afterhours,wage


	COPY ALL FOR SUBSTR(closedate,7,4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. afterhours = 'Y'  TO REPORT25

IF USED("REPORT25")
	SELECT REPORT25
	SET ORDER TO 0
ELSE
	SELECT 6
	USE (LOCFILE("REPORT25.dbf","DBF","Where is REPORT25?"));
	AGAIN ALIAS REPORT25 ;
	order 0
ENDIF




	REPLACE ALL wage WITH amount * .05 FOR amount => 500.00
	REPLACE ALL wage WITH 0.00 FOR amount < 500.00
	REPORT FORM report25 PREVIEW
	STORE 'N' TO yn
	@ 22,0 TO 24,80 CLEAR
	@ 23,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM report25 TO PRINT
	ENDIF


CASE choice = 26
	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	@ 19,5 SAY 'Enter Year to use in report      ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report       ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report         ' GET edate PICTURE '99/99'

	READ

	SELECT 7
	USE report26 EXCL
	ZAP
	USE
	USE report26



	*******************
	* reset paramters *
	*******************
	*	STORE ALLTRIM(STR(MONTH(DATE())))+'_'+ALLTRIM(STR(DAY(DATE())))+'_25.RPT' TO reportname
	SELECT 1
	SET FIELDS TO DATE,no,amount,technician,solddate,dispdate,closedate


	*	IF FILTERS > ' '
	*		COPY ALL FOR SUBSTR(closedate,7,4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO REPORT
	*	ELSE
	COPY ALL FOR SUBSTR(closedate,7,4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5)  TO report26
	*	ENDIF



	SELECT 0
	USE REPORT26
	SET INDEX TO technician 
	REPORT FORM report26 PREVIEW
	STORE 'N' TO yn
	@ 22,0 TO 24,80 CLEAR
	@ 23,10 SAY 'Would you like to print this report?   Y / N ' GET yn PICTURE '!'
	READ
	IF UPPER(yn) = 'Y'
		REPORT FORM report26 TO PRINT
	ENDIF

CASE choice = 27
	CLEAR
	DO rpt27

CASE choice = 28
	CLEAR
	DO jobcount
CASE choice = 29
	CLEAR
	DO overides
	
CASE choice = 30

	CLEAR

	STORE RIGHT(DTOC(DATE()),4) TO YEAR
	STORE 00 TO DAYSOFF
	STORE 'ALL  ' TO EMPLOYEE
	
	@ 18,5 SAY 'Enter Employee number [All] for all ' GET employee PICTURE '@K XXXXX'
	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99'
	@ 22,5 SAY 'Enter number of days off            ' GET daysoff picture '9999'
	
	READ


	*******************
	* reset paramters *
	*******************

	STORE '       ' TO empno
	STORE SPACE(20) TO mtechnician
	STORE 0 TO est
	STORE 0.00 TO est_rev
	STORE 0 TO OTHER
	STORE 0.00 TO oth_rev
	STORE 0 TO w_parts
	STORE 0 TO open_calls
	STORE 0 TO ALL
	STORE 0.00 TO combine_rev
	STORE 0.00 TO AVERAGE
	STORE 0 TO calls_w_rev
	STORE 0.00 TO avg_w_rev
	STORE 0 TO CANCEL
	STORE 'S' TO ps
	STORE 1 TO x
	STORE 'SERVICE SUMMARY REPORT (by close date)' TO  TITLE
	STORE 66 - (LEN(TITLE)/2) TO y
	STORE 60 TO lines
	STORE 0 TO test
	STORE 0.00 TO test_rev
	STORE 0 TO tother
	STORE 0.00 TO toth_rev
	STORE 0 TO tw_parts
	STORE 0 TO topen_calls
	STORE 0 TO tall
	STORE 0.00 TO tcombine_rev
	STORE 0.00 TO taverage
	STORE 0 TO tcalls_w_rev
	STORE 0.00 TO tavg_w_rev
	STORE 0 TO tcancel
	STORE RIGHT(DTOC(bdate),4) TO YEAR

* Determine number of days *
	store edate - bdate to nodays
	STORE NODAYS - DAYSOFF TO NODAYS
	


* set index to employee number *


	CLEAR
	@ 10,20 SAY 'COMPILING DATA'
	SELECT 1
	SET ORDER TO technician

	SELECT 2
	SET ORDER TO 1
	set deleted off

	GOTO TOP
	STORE RECNO() TO jn
	STORE 0 TO pass


	DO WHILE .NOT. EOF()
		GOTO jn
*		IF ALLTRIM(emp_no) = ' '
*			SKIP 1
*			STORE RECNO() TO jn
*			LOOP
*		ENDIF

*		IF ALLTRIM(emp_no) < '001'
*			SKIP 1
*			STORE RECNO() TO jn
*			LOOP
*		ENDIF

		IF ALLTRIM(empno) = ALLTRIM(emp_no)
			SKIP 1
			STORE RECNO() TO jn
			LOOP
		ENDIF


		STORE emp_no TO empno
		STORE name TO mtechnician
		SELECT 1
		STORE pass + 1 TO pass
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.combine_rev

		IF m.combine_rev = 0
			SELECT 2
			GOTO jn
			IF .NOT. EOF()
				SKIP 1
				STORE RECNO() TO jn
				LOOP
			ELSE
				EXIT
			ENDIF
		ENDIF

		STORE tcombine_rev + m.combine_rev TO tcombine_rev

		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. ALLTRIM(priority) = '02' TO m.est
		STORE test + m.est TO test
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(priority) = '02' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.est_rev
		STORE test_rev + m.est_rev TO test_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. priority <> '02' TO m.other
		STORE tother + m.other TO tother
		SUM ALL amount FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. priority <> '02' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.oth_rev
		STORE toth_rev + m.oth_rev TO toth_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. material > 0 TO m.w_parts
		STORE tw_parts + m.w_parts TO tw_parts
		COUNT ALL FOR technician = empno .AND. closedate =  ' ' .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.open_calls
		STORE topen_calls + m.open_calls TO topen_calls
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO m.all
		STORE tall + m.all TO tall
		STORE combine_rev / ALL TO m.average
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. amount > 0 TO m.calls_w_rev
		STORE tcalls_w_rev + m.calls_w_rev TO tcalls_w_rev
		COUNT ALL FOR technician = empno .AND. RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) .AND. paid = 'CNCL' TO m.cancel
		STORE tcancel + m.cancel TO tcancel
		STORE combine_rev / calls_w_rev TO m.avg_w_rev

		SELECT 3
		GOTO BOTTOM
		APPEND BLANK
		REPLACE empno WITH m.empno
		REPLACE technician WITH mtechnician
		REPLACE est WITH m.est
		REPLACE est_rev WITH m.est_rev
		REPLACE OTHER WITH m.other
		REPLACE oth_rev WITH m.oth_rev
		REPLACE w_parts WITH m.w_parts
		REPLACE open_calls WITH m.open_calls
		REPLACE ALL WITH m.all
		REPLACE combine_re WITH m.combine_rev
		REPLACE AVERAGE WITH m.average
		REPLACE call_w_rev WITH m.calls_w_rev
		REPLACE avg_w_rev WITH m.avg_w_rev
		REPLACE CANCEL WITH m.cancel


		SELECT 2
		GOTO jn
		IF .NOT. EOF()
			SKIP 1
			STORE RECNO() TO jn
		ENDIF
	ENDDO while .not. eof()

	*               STORE x+1 TO x
	*STORE tcombine_rev / tall TO taverage
	*STORE tcombine_rev / tcalls_w_rev TO tavg_w_rev
	*COUNT ALL FOR RIGHT(DTOC(DATE),4) = YEAR .AND. LEFT(closedate,5) >= LEFT(DTOC(bdate),5) .AND. LEFT(closedate,5) <= LEFT(DTOC(edate),5) .AND. ALLTRIM(trade)=ALLTRIM(m.trade) TO totcalls
	*@ x,0 SAY 'TOTAL FOR ALL   '
	*@ x,35 SAY test PICTURE '999'
	*@ x,41 SAY test_rev PICTURE '99999.99'
	*@ x,52 SAY tother PICTURE '999'
	*@ x,59 SAY toth_rev PICTURE '99999.99'
	*@ x,70 SAY tw_parts PICTURE '999'
	*@ x,75 SAY topen_calls PICTURE '999'
	*@ x,80 SAY tall PICTURE '999'
	*@ x,86 SAY tcombine_rev PICTURE '99999.99'
	*@ x,98 SAY taverage PICTURE '99999.99'
	*@ x,108 SAY tcalls_w_rev PICTURE '999'
	*@ x,113 SAY tavg_w_rev PICTURE '999.99'
	*@ x,125 SAY tcancel PICTURE '999'
	*SET DEVICE TO SCREEN
	*SET CONSOLE ON
	
	SELECT 0
	USE REPORT30

	IF ALLTRIM(EMPLOYEE) <> 'ALL'
		SET FILTER TO EMPNO = EMPLOYEE
	ENDIF

	REPORT FORM report30 PREVIEW
	STORE 'N' TO PRINT
	CLEAR
	@ 10,20 SAY 'WOULD YOU LIKE TO PRINT THIS REPORT?  Y / N' GET PRINT PICTURE '!'
	READ
	IF PRINT = 'Y'
		REPORT FORM report30 TO PRINT
	ENDIF
	SET FILTER TO
	select 2
	set deleted on
	
	
	* CASE CHOICE = 30
	*	DO REPORT30
		
	CASE CHOICE = 31
		DO CALLPERT.SCX
		



CASE choice = 11

	CLEAR

	***--- ADVERTISING REPORT BY REFERRAL
	STORE 'A      ' TO refno
	STORE 'C' TO match

	*	@ 19,5 SAY 'Enter Year to use in report         ' GET YEAR PICTURE '9999'
	@ 20,5 SAY 'Enter date to begin report          ' GET bdate PICTURE '99/99/9999'
	@ 21,5 SAY 'Enter date to end report            ' GET edate PICTURE '99/99/9999'
	@ 22,5 SAY 'Enter Ref No to search [A] FOR ALL  ' GET refno PICTURE '@k !!!!!!!'
	@ 23,5 SAY 'Search for exact match or Content   E or C' GET match PICTURE '!'
	READ
	
	DO ChooseTrade
	
	STORE ALLTRIM(refno) TO refno
	STORE RIGHT(DTOC(bdate),4) TO YEAR
	SELECT 1 && TIMWORK
	SET order TO ref
	
	IF ALLTRIM(mtrade) = 'ALL'
		* DO NOTHING
	ELSE
		SET FILTER TO ALLTRIM(TRADE) = ALLTRIM(mtrade)
	ENDIF
	
	
	
	SET FIELDS TO DATE,name,amount,ref,trade,city,CLOSEDATE
	IF ALLTRIM(refno) = 'A'
		COPY ALL FOR SUBSTR(CLOSEDATE,7,4) = YEAR .AND. LEFT(CLOSEDATE,5) >= LEFT(dtoc(bdate),5) .AND. LEFT(CLOSEDATE,5) <= LEFT(dtoc(edate),5) TO report11
	ELSE	
		COPY ALL FOR SUBSTR(CLOSEDATE,7,4) = YEAR .AND. LEFT(CLOSEDATE,5) >= LEFT(dtoc(bdate),5) .AND. LEFT(CLOSEDATE,5) <= LEFT(dtoc(edate),5) .and. alltrim(refno)$ref TO report11
	ENDIF
	SELECT 0
	USE report11
	SORT ON REF TO REPORT1A
	USE report1A

	IF ALLTRIM(refno) = 'A'
		REPORT FORM refersum PREVIEW
	ELSE
		IF match = 'E'
			REPORT FORM refsumde FOR ref = refno PREVIEW
		ELSE
			REPORT FORM refsumde FOR refno$ref PREVIEW
		ENDIF
	ENDIF

	STORE 'N' TO yn
	@ 23,10 SAY 'Would you like to print this report?   Y / N   (E for Excel) ' GET yn PICTURE '!'
	READ

* to printer
	IF UPPER(yn) = 'Y'
		IF ALLTRIM(refno) = 'A'
			REPORT FORM refersum TO PRINT
		ELSE
			IF match = 'E'
				REPORT FORM refsumde FOR ref = refno TO PRINT
			ELSE
				REPORT FORM refsumde FOR refno$ref TO PRINT
			ENDIF
		ENDIF
	ENDIF


* export to excel
	IF UPPER(yn) = 'E'
		IF ALLTRIM(refno) = 'A'
			EXPORT TO REPORT11 TYPE XLS
		ELSE
			IF match = 'E'
				EXPORT TO REPORT11 FOR ref = refno TYPE XLS
			ELSE
				EXPORT TO REPORT11 FOR refno$ref TYPE XLS
			ENDIF
		ENDIF
	ENDIF

SET FILTER TO

CASE CHOICE = 33
	DO MailList.spr
	DO DropDownMenuCnfg

CASE CHOICE = 34

*	SELECT 1
	IF USED("timwork")
		SELECT timwork
		SET ORDER TO 0
	ELSE
		SELECT 1
		USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
			AGAIN ALIAS timwork ;
			ORDER 0
	ENDIF

	STORE DATE()-14 TO bdate
	STORE DATE() TO edate
	STORE RIGHT(DTOC(bdate),4) TO YEAR
	STORE 'Y' TO YN
	
	clear	

	@ 20,5 SAY 'Enter date to begin report          ' GET bdate 
	@ 21,5 SAY 'Enter date to end report            ' GET edate 
	READ


	REPORT FORM TIMCNCL for .not. empty(cancel_cod) .and. left(closedate,10)  > DTOC(bdate) .AND. left(closedate,10) < DTOC(edate) .AND. YEAR(DATE) = YEAR PREVIEW
	
	@ 22,5 SAY 'Would you like to print this Report?  Y / N ' GET YN PICTURE '!'
	READ
	IF YN = 'Y'
		REPORT FORM TIMCNCL for .not. empty(cancel_cod) .and. left(closedate,10) > dtoc(bdate).AND. left(closedate,10) < DTOC(edate) .AND. YEAR(DATE) = YEAR TO PRINT
	ENDIF			



ENDCASE
*: EOF: TIMWREPO.PRG

