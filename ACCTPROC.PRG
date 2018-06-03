*:*****************************************************************************
*:
*: Procedure file: C:\TIM\PRG\ACCTPROC.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 09/14/94 at 17:31:24
*:
*:  Procs & Fncts: BILLMENU
*:
*:      Documented 12:31:02                                FoxDoc version 3.00a
*:*****************************************************************************

*!*********************************************************************
*!*****************************************************************************
*!
*!      Procedure: BILLMENU
*!
*!      Called by: TIMBILL.PRG
*!
*!*****************************************************************************
PROCEDURE billmenu
STORE '       ' TO mnum
STORE 'S' TO invstate
SET readborder ON


* SELECT 2
IF USED("TIMBILL")
	SELECT timbill
	SET ORDER TO 0
ELSE
	SELECT 2
	USE(loc("TIMBILL","DBF","Where is Timbill?")) ;
		AGAIN ALIAS timbill ;
		ORDER 0
ENDIF

DO WHILE .T.
	@ 1,0 CLEAR
	@ 1,50 SAY 'Page'
	SET COLOR TO &screenatr
	@ 2,1 SAY 'Work Order Number  [       ]'
	*   @ 2,32 SAY 'Incident # [        ] - [  ]  Date [  /  /    ]'
	*   @ 3,0 TO 10,79
	*   @ 4,1 SAY '    W [(   )][    -    ]     B [(   )][    -    ]       J [(   )][    -    ]'
	*   @ 5,1 SAY 'BILL:Name   [                         ] JOB:Name   [                         ]'
	*   @ 6,1 SAY '     Street [                         ]     Street [                         ]'
	*   @ 7,1 SAY '     City   [                         ]     City   [                         ]'
	*   @ 8,1 SAY '     State  [  ]            Zip [     ]     State  [  ]            Zip [     ]'
	*   @ 9,1 SAY '    Contact [                         ]   '
	@ 2,21 GET mnum
	@ 2,31 SAY 'INVOICE OR STATEMENT  I/S' GET invstate PICTURE '!'

	READ
	IF mnum = ' '
		POP KEY
		STORE 0 TO mrec_no
		RETURN
	ENDIF
	SELECT TIMBILL
	SET ORDER TO no
	SEEK mnum
	IF EOF()
		@ 23,0 CLEAR
		STORE '       ' TO mnum
		@ 23,5 SAY 'File does not exist in Billing'
		WAIT
		@ 23,0 CLEAR
		LOOP
	ELSE

		**  CALCULATE INTEREST
		*		IF LEFT(descrip,8) = 'Interest'
		*			STORE DATE TO int_date
		*		ENDIF
		*
		*		STORE (.024/30.42) TO day_int
		*		STORE DATE() - int_date TO int_days
		*		STORE int_days * (mbalance*day_int) TO interest
		*		IF interest > .01
		*			STORE 'Y' TO yn
		*			? CHR(7)
		*			? CHR(7)
		*			WAIT ' DO YOU WANT TO ADD INTEREST?   N / Y ' TO yn WINDOW
		*			IF yn = 'Y'
		*				APPEND BLANK
		*				REPLACE DATE WITH DATE()
		*				REPLACE descrip WITH 'Interest from '+DTOC(int_date)
		*				REPLACE amount WITH interest
		*				REPLACE no WITH mnum
		*				STORE RECNO() TO MREC_NO
		*				SUM ALL amount TO mbalance FOR no = mnum
		*				GOTO MREC_NO
		*				REPLACE balance WITH mbalance
		*				REPLACE comments WITH mcomments
		*			ENDIF
		*		ELSE
		*			SUM ALL amount TO mbalance FOR no = mnum
		*		ENDIF

		STORE RECNO() TO mrec_no
		GOTO mrec_no
		SUM ALL amount TO mbalance FOR no = mnum



		PUSH KEY
		ON KEY LABEL f9 DO noteedit
		IF invstate = 'I'
				IF left(mnum,1)='5'
					REPORT FORM timbili2 FOR no = mnum PREVIEW
				ELSE
					REPORT FORM timbilli FOR no = mnum PREVIEW
				ENDIF
		ELSE


				IF left(mnum,1)='5'
					REPORT FORM timbill2 FOR no = mnum PREVIEW
				ELSE
					REPORT FORM timbill FOR no = mnum PREVIEW
				ENDIF

		ENDIF

	ENDIF



	POP KEY
	EXIT
	SET COLOR TO &screenatr
ENDDO
*: EOF: ACCTPROC.PRG


*!*****************************************************************************
*!
*!      Procedure: NOTEEDIT
*!
*!      Called by: BILLMENU.PRG
*!
*!*****************************************************************************
PROCEDURE noteedit
SELECT 1
GOTO mrec_no
MODIFY MEMO notes
SELECT 2
