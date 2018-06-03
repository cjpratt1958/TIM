*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMADDWK.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/25/94 at 18:57:48
*:
*:      Called by: TIM1MENU.PRG                      
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : WORK_STO           (procedure in TIMPROC.PRG)
*:               : WORKMENU           (procedure in TIMPROC.PRG)
*:               : WORKMSAY           (procedure in TIMPROC.PRG)
*:               : WORK_SAY           (procedure in TIMPROC.PRG)
*:               : TIMADPTS.PRG
*:               : WORKSAVE           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF        
*:
*:      Documented 12:29:52                                FoxDoc version 3.00a
*:*****************************************************************************
*:*********************************************************************
ON ESCAPE RETURN
*	SELECT 1
IF USED("timwork")
	SELECT timwork
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF

@ 0,0 CLEAR TO 1,0
STORE 'TIMADDWK.PRG' TO prg_name
STORE '********  ADD A WORKORDER FILE  ********' TO TITLE
DO addtitle
SET ECHO OFF
SET TALK OFF
SET INTENSITY ON
SET COLOR TO &screenatr
SET STATUS OFF
STORE 2 TO sr

   *  ---RESET ALL VARIABLES
   DO work_sto
   
      *  ---CREATE PICTURE ON SCREEN
      DO workmenu
      
         STORE 2 TO add_chse
      *  ---CHOOSE EDIT,CHANGE,ADD OPTION
      @ 23,0 CLEAR
      SET COLOR TO &promptatr
      @ 23,0 SAY '*****  Please Wait  *****'
      SET COLOR TO &hiliteatr
      STORE '       ' TO mnum
      DO WHILE .T.
         SET COLOR TO &hiliteatr
         DO work_sto
         STORE 0 TO tcost
         DO workmsay
         @ 23,0 CLEAR
         SET COLOR TO &hiliteatr
         @ 23,0 SAY 'Enter Workorder Number,    Press [Enter] to exit'
         @ 11,60 TO 11,80 CLEAR
         @ 12,60 TO 12,80 CLEAR
         @ 16,60 TO 16,80 CLEAR
         @ 24,0 CLEAR
         @ 2,21 GET mnum PICTURE '!!!!!!!'
         READ
         STORE LEN(TRIM(mnum)) TO N
         SET COLOR TO &hiliteatr
         IF mnum = '       '
            CLOSE DATABASES
            RETURN
         ENDIF
         SELECT timwork
		 SET ORDER TO NO
         SEEK mnum
         IF EOF()
            @ 23,0 CLEAR
            @ 23,5 SAY '********  RECORD DOES NOT EXIST  ******** '
            @ 0,9 SAY RECNO()
            IF SYS(2001,"BELL") = 'ON'
               ? CHR(7)
            ENDIF
            LOOP
         ELSE
            SET COLOR TO &promptatr
            @ 23,0 CLEAR
            @ 0,9 SAY RECNO()
            DO work_say
            STORE 'N' TO ok
            @ 24,5 SAY 'DO YOU WANT TO ADD PARTS TO THIS JOB?  (Y/N)' GET ok PICTURE 'A'
            READ
            SET COLOR TO &screenatr
            STORE RECNO() TO rec
            STORE UPPER(ok) TO ok
            IF ok = 'Y'
               @ 23,0 CLEAR
               @ 24,5 SAY 'Please Wait ***'
               STORE technician TO mtechnician
               STORE amount TO mamount
               STORE tax TO mtax
               STORE mat_cost TO tcost
               DO timadpts
               @ 1,0 CLEAR
               DO workmenu
               SELECT timwork
               = LOCK()
               GOTO rec
               REPLACE mat_cost WITH tcost
               UNLOCK
            ENDIF
            STORE '       ' TO mnum
            @ 23,0 CLEAR
            LOOP
         ENDIF
         select timwork
         @ 23,0 CLEAR
         @ 24,5 SAY 'Please Wait ***'
         IF mnum <> '       '
            = LOCK()
            STORE lastrec+1 TO lastrec
            APPEND BLANK
            DO worksave
            DO work_sto
            UNLOCK
         ENDIF
      ENDDO
      STORE 2 TO add_chse
      CLOSE DATABASES
      RETURN
*: EOF: TIMADDWK.PRG
