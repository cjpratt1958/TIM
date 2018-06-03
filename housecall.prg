*******************************************************************************
*
*	THIS PROGRAM CONVERTS FROM A FILE DOWNLOADED FROM HOUSECALL NAMED HOUSECALLDOWNLOAD.CSV
*	TO A CSV FILE AND THEN NEEDS TO BE CONVERTED TO AN XLS FILE NAMED HOUSECAL.XLS USING EXCEL
*	THEN THIS PROGRAM CONVERTS THE XLS FILE TO A DBF FILE AND APPENDS IT TO TIMWORK
*	AND LOCATES FOR DUPLICATES AS IT RUNS.
*
*	This program uses:
*	timwork.dbf
*	housecal.dbf
*
*
*	this program is called by:
*	tim1menu.fxp
*
*
*	Writen by Carl Pratt on 05/12/18
*	Last modified on 05/13/18
*******************************************************************************
ON ESCAPE RETURN	
SET TALK Off	
SET STATUS ON
STORE '' TO M.NO
STORE 'Y' TO YN

CLEAR

DO WHILE UPPER(yn) ='Y'
	WAIT WINDOW '1) Download complete report from housecall as a CSV file, housecall will email you a copy'+CHR(13)+CHR(13)+;
	'Press any key when done' TO yn
	
	WAIT WINDOW '2) Open the download that was emailed to you with EXCEL and save as housecal.xls'+CHR(13)+CHR(13)+;
	'Press any key when done' TO yn
	
	WAIT WINDOW '3) Copy the downloaded file housecal.xls to the working TIM directory'+CHR(13)+CHR(13)+;
	'Press any key when done' TO yn
	
	WAIT WINDOW '4) Press any key to continue and tim will append it to the timwork file or Press "N" to abort' TO yn
	IF UPPER(YN) = 'N'
		RETURN
	ENDIF
enddo

*	SELECT 1
IF USED("timwork")
	SELECT timwork
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF


*	SELECT 2
IF USED("housecal")
	SELECT housecal
ELSE
	SELECT 2
	USE (LOCFILE("housecal.dbf","DBF","Where is housecal?"));
		AGAIN ALIAS housecal ;
		ORDER 0
ENDIF


*	USE A SPECIAL TABLE THAT MATCHES THE DOWNLOAD FROM HOUSECALL
SELECT housecal
USE HOUSECAL EXCLUSIVE
*	CLEAR OLD DATA IN HOUSECAL.DBF
ZAP

*	COPY ALL DATA FROM THE SPREADSHEET NAMED HOUSECAL.XLS TO THE TEMP TABLE HOUSECAL.DBF
SELECT housecal 
IMPORT from housecal TYPE XLS

* delete top record which is the field titles

GOTO TOP
DELETE


*	BROWSE
*	OPEN WORK ORDER TABLE
*   SELECT timwork
*   GOTO BOTTOM
*   STORE '80501' TO NO


*	OPEN TEMP FILE
*	STORE FIRST WORKORDER NUMBER TO TEMP FILE
SELECT 2
GOTO TOP

DO WHILE .not. EOF()
		STORE '  ' TO M.NOTES
		STORE A TO M.NO
		STORE B TO M.INCIDENT
		STORE LEFT(C,10) TO M.DATE
		STORE LEFT(D,16) TO M.DISPDATE
		STORE LEFT(E,16) TO M.CLOSEDATE
		STORE UPPER(G) TO M.NAME
		STORE UPPER(H) TO M.FIRSTNAME
		STORE UPPER(I) TO M.LASTNAME
		STORE J TO M.EMAIL
		STORE UPPER(K) TO M.COMPANY
		STORE LEFT(L,5) TO M.CAREA
		STORE RIGHT(L,8) TO M.CPHONE
		STORE LEFT(M,5) TO M.AREA
		STORE RIGHT(M,8) TO M.PHONE
		STORE N TO M.REF
		STORE UPPER(O) TO M.ADDRESS
		STORE UPPER(P) TO M.STREET
		STORE UPPER(R) TO M.CITY
		STORE UPPER(S) TO M.STATE
		STORE T TO M.ZIP
		STORE UPPER(U) TO M.JOBDESC1
		STORE UPPER(V) TO M.JOBDESC2
		STORE M.NOTES + CHR(13) + V TO M.NOTES
		STORE W TO M.AMOUNT
		STORE Y TO M.MATERIALS
		STORE M.NOTES + CHR(13) + AA TO M.NOTES
		STORE AE TO M.PREFDISC
		STORE AF TO M.TRADE
		STORE M.NOTES + ' ' + AG TO M.NOTES
		STORE AH TO M.TECHNICIAN
		STORE LEFT(AJ,16) TO M.CLOSEDATE
		STORE AK TO M.PAID
		STORE M.NOTES + CHR(13) + ' INVOICE SENT ' + AL TO M.NOTES
		

	* CHCECK TO SEE IF RECORD IS A DUPLICATE
	SELECT 1
	GOTO TOP

	LOCATE FOR  ALLTRIM(NO) = M.NO
	IF FOUND()= .T.
		*		SELECT 2
		*	? 'RECORD FOUND' + '  ' + M.NO + '  ' + NO
		* DO NOTHING
		@ 5,35 say STR(RECNO()) + ' RECORD FOUND '+ M.NO
		* TEST
		*SUSPEND
		* ENDTEST
	
	ELSE

		GOTO BOTTOM

		*	? 'RECORD NOT FOUND' + '  ' + M.NO + '  ' + NO
		@ 5,15 SAY STR(RECNO()) +  ' RECORD NOT FOUND ' + M.NO
		* 	TEST
		*SUSPEND
		* 	ENDTEST


		APPEND BLANK
		GATHER MEMVAR memo
		
		
		
		*AND SO ON

	ENDIF
	SELECT 2
	SKIP 1
	 
ENDDO
RETURN
