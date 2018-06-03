
*:*****************************************************************************
*:
*:        Program: C:\TIM\TIMCNGWK.PRG
*:         System: tim2000
*:         Author: Carl Pratt
*:      Copyright (c) 1987, Carl Pratt
*:  Last modified: 01/28/1997 at 12:29:26
*:
*:      Called by: TIMDMENU.PRG                      
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : WORK_STO           (procedure in TIMPROC.PRG)
*:               : WORKMENU           (procedure in TIMPROC.PRG)
*:               : DATASTOR           (procedure in TIMPROC.PRG)
*:               : WORK_GET           (procedure in TIMPROC.PRG)
*:               : WORKSAVE           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF        
*:               : TIMBILL.DBF        
*:
*:        Indexes: B_NUM.IDX          
*:
*:      CDX files: TIMWORK.CDX
*:               : TIMBILL.CDX
*:
*:      Documented 02:54:47                                FoxDoc version 3.00a
*:*****************************************************************************

ON KEY LABEL f9 DO F9KEY

STORE 'TIMCNGWK.PRG' TO prg_name
STORE ' ***** C H A N G E   W O R K ***** ' TO TITLE
DO addtitle

SET DELIMITERS OFF
SET STATUS OFF


*	SELECT 1 TO TIMWORK
IF USED("timwork")
	SELECT timwork
	SET ORDER TO 1
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 1
ENDIF

*	SELECT 2 TO timbill

IF USED("timbill")
	SELECT timbill
	SET ORDER TO 1
ELSE
	SELECT 2
	USE (LOCFILE("timbill.dbf","DBF","Where is timbill?"));
		AGAIN ALIAS timbill ;
		ORDER 1
ENDIF

DO work_sto
@ 1,0 CLEAR
DO workmenu

STORE 1 TO cs
DIMENSION sw(3)
STORE 'RETURN TO MAIN MENU       ' TO sw(1)
STORE 'SEARCH BY WORKORDER NUMBER' TO sw(2)
STORE 'SEARCH BY INCIDENT NUMBER ' TO sw(3)

STORE 40 - LEN(sw(1))/2 TO C
STORE 2 TO cs
DO WHILE .T.
   SET COLOR TO &statusatr
   @ 3,C MENU sw,3 TITLE 'Search by ?'
   READ MENU TO cs
   SET COLOR TO &screenatr
   DO work_sto
   IF cs = 1
      CLOSE DATABASES
      RETURN
   ENDIF
   IF cs = 2
      STORE SPACE(7) TO mnum
      STORE 1 TO cs
      SET ORDER TO 1
      @ 2,21 GET mnum PICTURE 'XXXXXXX'
      READ
   ELSE
      STORE SPACE(7) TO mnum
      STORE 1 TO cs
      SET ORDER TO incident
      @ 2,44 GET mnum PICTURE 'XXXXXXX'
      READ
   ENDIF
   
   IF mnum = '       '
      @ 23,0 CLEAR
      SET COLOR TO &promptatr
      @ 23,0 SAY '*****  Please Wait  *****'
      SET COLOR TO &screenatr
      CLOSE DATABASES
      USE
      SET INTENSITY OFF
      CLOSE DATABASES
      RETURN
   ENDIF
   FIND &mnum
   IF .NOT. EOF()
      STORE 'N' TO ok
      DO datastor
      STORE ' ' TO ok
      STORE 'Y' TO A
      DO WHILE A <> 'N'
         DO workmenu
         DO work_get
         IF LEN(notes) > 0
            SET COLOR TO &promptatr
            @ 0,57 SAY ' See Notes'
         ENDIF
         SET COLOR TO &screenatr
         DO WHILE A = 'Y'
            @ 24,0 CLEAR
            @ 24,17 SAY 'Press [-] Previous page   SAVE CHANGES? (Y/N / [-]Back / [B]ill / [P]rint)' GET ok PICTURE '!'
            READ
            
            IF ok = '-'
               STORE ' ' TO ok
               STORE 'Y' TO A
               EXIT
            ENDIF
            
            IF ok = 'P'
               STORE ' ' TO ok
               STORE 'Y' TO A
				report form jobprint preview
				store 'N' to Printyn 
				@ 25,17 say 'Print this page ? ' get printjob picture '!'
				if upper(printyn) = 'Y'
					report form printjob to print 
				endif
               EXIT
            ENDIF



            IF ok = 'Y'
               STORE 'N' TO A
            ENDIF
            
            IF ok = 'B'
               IF mbname = ' '
                  STORE PAGE+1 TO PAGE
                  STORE mname TO mbname
                  STORE mstreet TO mbstreet
                  STORE mcity TO mbcity
                  STORE mstate TO mbstate
                  STORE mzip TO mbzip
                  @ 5,14 GET mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
                  @ 6,14 GET mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
                  @ 7,14 GET mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
                  @ 8,14 GET mbstate PICTURE '!!'
                  @ 8,34 GET mbzip PICTURE '99999'
                  READ
               ENDIF
               STORE RECNO() TO mrec_no
               SELECT 2

               GOTO BOTTOM
               APPEND BLANK
               REPLACE DATE WITH DATE()
               STORE 'Invoice number '+mnum TO mdescrip
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
               STORE 'Y' TO choice
               @ 24,0 SAY 'Insert blank letterhead in printer and press [Enter] or [N] to not print'
               @ 24,78 GET choice
               READ
               @ 24,0 CLEAR
               IF choice <> 'N'
                  SET DEVICE TO PRINT
                  STORE 'I N V O I C E' TO HEADING
                  STORE (40 - (LEN(HEADING)/2)) TO x
                  @ 10,x SAY HEADING
                  @ 12,53 SAY 'Invoice date'
                  @ 12,68 SAY DATE()
                  @ 12,14 SAY 'Acct #'
                  STORE TRIM(mnum) TO mnum
                  @ 12,21 SAY mnum
                  @ 12,21+LEN(mnum) SAY '-'
                  @ 12,22+LEN(mnum) SAY incident
                  @ 15,5 SAY 'Bill to:'
                  @ 15,14 SAY bname
                  @ 15,47 SAY 'Job:'
                  @ 15,53 SAY name
                  @ 16,14 SAY bstreet
                  @ 16,53 SAY street
                  @ 17,14 SAY bcity
                  @ 17,53 SAY city
                  @ 18,14 SAY bstate
                  @ 18,34 SAY bzip
                  @ 18,53 SAY state
                  @ 18,73 SAY zip
                  @ 19,2 SAY '_____________________________________________________________________________'
                  @ 20,5 SAY 'Date'
                  @ 20,15 SAY 'Description'
                  @ 20,52 SAY 'Credit/Debit'
                  @ 20,67 SAY 'Balance'
                  @ 22,5 SAY DATE
                  @ 22,15 SAY descrip
                  @ 22,55 SAY amount
                  @ 22,65 SAY balance
                  @ 40,40 SAY 'Please Pay This amount'
                  @ 40,65 SAY balance
                  @ 41,2 SAY '_____________________________________________________________________________'
                  @ 43,1 SAY 'If payment is not made when due.  Customer agrees to pay a charge on the amount'
                  @ 44,1 SAY 'past due at the rate of   2%  (24% per annum).  If it is necessary for '
                  @ 45,1 SAY dcompany+' to institute legal proceedings against Customer to '
                  @ 46,1 SAY 'enforce terms of this contract sale, '+dcompany+' shall be '
                  @ 47,1 SAY "entitled to recover from Customer it's  reasonable  attorney's fees  and all"
                  @ 48,1 SAY 'other costs pertaining thereto.  Payment due upon completion of work.'
                  @ 51,1 SAY 'Please make check payable to : '+dcompany
                  @ 53,1 SAY 'Please send payment to       : '+dcompany
                  @ 54,1 SAY '                               '+dstreet
                  @ 55,1 SAY '                               '+TRIM(dcity)+', '+LTRIM(dstate)+' '+dzip
                  @ 57,1 SAY 'Please put acct # on check.    For Inquiries call '+dphone
                  SET DEVICE TO SCREEN
                  @ 23,0 SAY 'Print Envelope?     (Y - N)                                     ' GET yn PICTURE '!'
                  READ
                  IF yn = 'Y'
                     @ 23,0 SAY 'Insert Envelope in the printer and press online on'
                     WAIT
                     SET DEVICE TO PRINT
                     @ 1,2 SAY dcompany
                     @ 2,2 SAY dstreet
                     @ 3,2 SAY TRIM(dcity)+', '+LTRIM(dstate)+' '+dzip
                     @ 10,40 SAY mbname
                     @ 11,40 SAY mbstreet
                     STORE TRIM(mbcity) TO mbcity
                     @ 12,40 SAY mbcity+', '
                     @ 12,LEN(mbcity)+42 SAY mbstate
                     @ 12,LEN(mbcity)+43+LEN(state) SAY mbzip
                     EJECT
                     SET DEVICE TO SCREEN
                  ENDIF
                  USE
               ENDIF
            ENDIF
            
            IF ok = 'N'
               STORE 'N' TO A
            ENDIF
         ENDDO
      ENDDO
      IF ok = 'Y'
         DO worksave
      ENDIF
      
   ELSE
      @ 23,0 CLEAR
      @ 23,5 SAY '*********** JOB NUMBER NOT IN FILE ***********'
      WAIT '  Press any key to continue ...'
      @ 23,0 CLEAR
   ENDIF NOT EOF()
ENDDO WHILE T
@ 23,0 CLEAR
SET COLOR TO &promptatr
@ 23,0 SAY '*****  Please Wait  *****'
SET COLOR TO &screenatr
USE
CLEAR
SET STATUS OFF
SET INTENSITY OFF
CLOSE DATABASES
ON KEY
RETURN


PROCEDURE F9KEY

REPLACE NOTES WITH NOTES + chr(13)+chr(13)+'-- Viewed by '+alltrim(operator)+' on '+DTOC(DATE())+' at '+time()+' from view screen --'+chr(13)
MODIFY MEMO NOTES


*: EOF: TIMCNGWK.PRG
