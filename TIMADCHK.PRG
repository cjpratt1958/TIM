
*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMADCHK.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 02/20/96 at 17:10:58
*:
*:      Called by: TIMPAYRO.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : CHKRMBRS           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF
*:               : TIMPAYRO.DBF
*:
*:        Indexes: TPR_TEC.IDX
*:               : TPR_WO.IDX
*:
*:      Documented 12:30:59                                FoxDoc version 3.00a
*:*****************************************************************************
SET ECHO OFF
SET DELIMITERS ON
SET DELIMITERS TO '[]'
SET TALK OFF
STORE 'TIMADCHK.PRG' TO prg_name
STORE '***** E N T E R  I N V O I C E S   R E C I E V E D  *****' TO TITLE
STORE 'N' TO cl
STORE 0 TO dt
STORE 0 TO wge
STORE 2 TO C
STORE 'N' TO yn
STORE 'N' TO aux
STORE 'Y' TO ok
STORE DATE() TO ppe
STORE 0 TO tw
STORE '       ' TO m.no
STORE '       ' TO m.link
CLEAR
SET COLOR TO &windowatr
@ 0,1 TO 19,80 DOUBLE
SET COLOR TO W/N
@ 1,5 SAY 'W O #    HOURS   TOTAL-TAX    WAGE%       WAGES PD   R/F?    RCVED   '
@ 18,30 SAY 'Total'
SET COLOR TO &screenatr
DO addtitle
@ 24,0 CLEAR



*	SELECT 1
IF USED("timwork")
	SELECT timwork
	SET ORDER TO 1
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 1
ENDIF

*	SELECT 2
IF USED("timpayro")
	SELECT timpayro
	SET ORDER TO 8
ELSE
	SELECT 2
	USE (LOCFILE("timpayro.dbf","DBF","Where is timpayro?"));
		AGAIN ALIAS timpayro ;
		ORDER 8
ENDIF

DO WHILE .T.

	STORE '       ' TO m.no
	SET COLOR TO &hiliteatr
	IF C = 17
		STORE 2 TO C
		CLEAR
		SET COLOR TO &windowatr
		@ 0,1 TO 19,80 DOUBLE
		SET COLOR TO W/N
		@ 1,5 SAY 'W O #    HOURS   TOTAL-TAX    WAGE%       WAGES PD   R/F?     RCVED   '
		@ 18,30 SAY 'Total'
		SET COLOR TO &screenatr
		DO addtitle
	ENDIF
	SELECT 1
	SET ORDER TO 1
	@ 22,0 CLEAR
	@ 23,5 SAY 'ENTER WORK ORDER 'GET m.no PICTURE '!!!!!!!'
	SET COLOR TO &screenatr
	@ 24,5 SAY 'ENTER [RETURN] TO RETURN TO MAIN MENU'
	READ
*	IF ALLTRIM(m.no) = ' '
	IF EMPTY(m.no)
		SET DELIMITERS OFF
		RETURN
*		EXIT
	ENDIF
	SEEK ALLTRIM(m.no)
	IF EOF()
		@ 22,0 CLEAR
		*      SET COLOR TO &promptatr
		? CHR(7)

		STORE .F. TO mexit
		DEFINE WINDOW mlink FROM 5,10 TO 20,70 DOUBLE
		DO WHILE NOT mexit
			ACTIVATE WINDOW mlink
			@ 1,5 SAY '****  This Workorder does not exist  ****'
			@ 3,5 SAY 'Do you want to add this workorder as a   '
			@ 4,5 SAY 'page 2 of 2 etc.. '
			@ 6,5 PROMPT 'YES'
			@ 7,5 PROMPT 'NO'

			MENU TO mchoice
			DEACTIVATE WINDOW mlink
			DO CASE
			CASE mchoice = 1
				@ 22,5 SAY 'Please enter the workorder number to link this to' GET m.link PICTURE '!!!!!!!'
				READ
				STORE .T. TO mexit

			CASE mchoice = 2
				STORE .T. TO mexit

			ENDCASE
		ENDDO
		RELEASE WINDOW mlink


		*      @ 22,5 SAY '****** THIS WORKORDER DOES NOT EXIST ******'
		*      SET COLOR TO &screenatr
		*      IF SYS(2001,"bell") = 'on'
		*        ? CHR(7)
		*      ENDIF
		*      WAIT
		*      SAVE SCREEN TO SCREEN

*		IF mchoice = 2
*			DO chkrmbrs
*		ENDIF
		*      RESTORE SCREEN FROM SCREEN
		LOOP
	ENDIF
	@ C,9 SAY RECNO()
	STORE (amount-tax)*(wage/100) TO wge
	STORE (amount-tax) TO dt


	STORE amount TO mamount
	IF cleared = 'N' .OR. amount = 0
		@ C,5 SAY no
		@ C,12 SAY HOURS PICTURE '999.99'
		@ C,20 SAY amount-tax
		@ C,33 SAY wage
		@ C,45 SAY wageextens PICTURE '99999.99'
		STORE 'NO' TO rf

* Check for Redflags

		IF amount > 0
			IF (mat_cost*4) > (amount-tax)
				STORE 'YES' TO rf
			ENDIF
		ENDIF


		@ C,59 SAY rf
		SET COLOR TO &hiliteatr
		@ 22,5 SAY 'HAS THIS WORKORDER BEEN RECIEVED  (Y/N)????        ' GET ok PICTURE '!'
		@ 23,5 SAY 'HAS A DIAGNOSTIC FORM BEEN TURNED IN?   (Y/N)????  ' GET aux PICTURE '!'
		READ
		SET COLOR TO &screenatr

		IF aux = 'Y'
			REPLACE aux_work WITH 'Y'
		ELSE
			REPLACE aux_work WITH 'N'
		ENDIF
		IF ok = 'Y'
			@ C,63 SAY '****  RCVD  ****'
			STORE tw + wageextens TO tw
			@ 18,40 SAY tw
			
			IF EMPTY(rcvdate)
				REPLACE rcvdate WITH DATE()
				REPLACE notes WITH notes + CHR(13) +  'Recieved on '+DTOC(DATE()) +' by '+ operator
			ENDIF
			
			SELECT 2
			SET ORDER TO 8
			GOTO TOP
			SEEK m.no
			DO WHILE .NOT. EOF()
				IF wo = m.no
					IF 	EMPTY(date_rcved)
						REPLACE date_rcved WITH DATE()
						@ C,63 SAY '**   LOGGED   **'
					
					ENDIF
				ELSE
					EXIT
				ENDIF
				SKIP 1
			ENDDO
			SELECT 1
		ELSE
			@ C,63 SAY '**  NOT RCVD  **'
		ENDIF
		STORE C+1 TO C
		*      SAVE SCREEN TO SCREEN
*		DO chkrmbrs
		*      RESTORE SCREEN FROM SCREEN
	ELSE
		@ C,5 SAY no
		@ C,12 SAY HOURS PICTURE '999.99'
		@ C,20 SAY amount-tax
		@ C,33 SAY wage
		@ C,45 SAY (amount-tax)*(wage/100) PICTURE '99999.99'
		STORE 'NO' TO rf
		IF (mat_cost*4) > (amount-tax)
			STORE 'YES' TO rf
		ENDIF
		@ C,59 SAY rf
		@ 22,0 CLEAR
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		SET COLOR TO &promptatr
		@ 23,5 SAY '********* THIS JOB HAS ALREADY BEEN RECIEVED *********'
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		SET COLOR TO &screenatr
		*      SAVE SCREEN TO SCREEN



		SELECT 2
		SET ORDER TO 8
		SEEK m.no
		DO WHILE .NOT. EOF()
		
			IF EMPTY(rcvdate)
				REPLACE date_rcved WITH DATE()
			ELSE
				EXIT
			ENDIF
			SKIP 1
		ENDDO
		SELECT 1

*		DO chkrmbrs
		*      RESTORE SCREEN FROM SCREEN
		STORE C+1 TO C
		LOOP
	ENDIF
ENDDO
SET DELIMITERS OFF
RETURN



*: EOF: TIMADCHK.PRG
