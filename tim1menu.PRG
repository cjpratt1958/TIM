*:*****************************************************************************
*:
*: Procedure file: T:\CORNER~1\TIM\TIM1MENU.PRG
*:         System: TIM
*:         Author: Carl Pratt
*:      Copyright (c) 2007, Carl Pratt
*:  Last modified: 01/07/2007 at  1:17:58
*:
*:  Procs & Fncts: LISTEMPL
*:
*:          Calls: DBF_CHK            (procedure in TIMPROC.PRG)
*:               : LOGO               (procedure in TIMPROC.PRG)
*:               : TIMSALES.PRG
*:               : TIMDISPA.PRG
*:               : TIMCLOSE.PRG
*:               : TIMTRACK.PRG
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
*:               : TIMADPTS.PRG
*:               : TIMADDST.PRG
*:               : TIMSRCHD.PRG
*:               : TIMPRINV.PRG
*:               : TIMCNGST.PRG
*:               : TIMDELST.PRG
*:               : TIMCOMPA.PRG
*:               : TIMVFY.PRG
*:               : SUPPLIER.SPR
*:               : PBADD.PRG
*:               : TIMBRWSE.PRG
*:               : TIMINQUR.PRG
*:               : TIMRIMBS.PRG
*:               : TIMRORDR.PRG
*:               : TIMCANCL.SPR
*:               : STOCKBID.PRG
*:               : TIMCCLOG.SPR
*:               : TIMAELOG.PRG
*:               : FLATRATE.SPR
*:               : TIMCASH.SPR
*:               : TIM.PRG
*:               : TIMUNDIS.PRG
*:               : TIMBCKUP.PRG
*:               : TIMINDEX.PRG
*:               : ALLIED.PRG
*:               : ADMAIN.SPR
*:               : TIMPAYRO.PRG
*:               : TIMBILL.PRG
*:               : COMPANY.SPR
*:               : TIMBID_1.SPR
*:
*:           Uses: TIMWORK.DBF        
*:               : TIMLOG.DBF         
*:               : TIMTRACK.DBF       
*:               : TIMTIME.DBF        
*:               : &BACKUPNAME        
*:               : TIMPOOL.DBF        
*:               : EMPLOYEE.DBF       
*:               : TIMBILL.DBF        
*:               : TIMPREFX.DBF       
*:
*:      CDX files: TIMWORK.CDX
*:               : TIMLOG.CDX
*:               : TIMTRACK.CDX
*:               : TIMTIME.CDX
*:               : TIMPOOL.CDX
*:               : EMPLOYEE.CDX
*:               : TIMBILL.CDX
*:               : TIMPREFX.CDX
*:
*:    Other Files: &BACKUPNAME
*:
*:      Documented 20:54:00                                FoxDoc version 3.00a
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
STORE 'TRACKING     ' TO WORK(4)
STORE 'ADD PARTS /WO' TO WORK(5)
STORE 'SRCH EXIST WO' TO WORK(6)
STORE 'LIST EXIST WO' TO WORK(7)
STORE 'VIEW EXIST WO' TO WORK(8)
STORE 'PENDING FILE ' TO WORK(9)
STORE 'REPORTS      ' TO WORK(10)
STORE 'SEND A TEXT  ' TO WORK(11)
* store 'ADD WORKORDER' to work(11)
STORE 'CC AUTHORIZE   ' TO WORK(12)
STORE 'PHONE MEMO     ' TO WORK(13)
STORE 'TIM CALC       ' TO WORK(14)

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
DIMENSION option(10)
STORE 'BROWSE             ' TO option(1)
STORE 'INQUIRY            ' TO option(2)
STORE 'CASH REIMBURSTMENTS' TO option(3)
STORE 'REORDER TRUCKSTOCK ' TO option(4)
STORE 'EDIT CANCEL CODES  ' TO option(5)
STORE 'ESTIMATE JOB       ' TO option(6)
STORE 'CC DEPOSIT LOG     ' TO option(7)
STORE 'EXPENSE CARD LOG   ' TO option(8)
STORE 'FLAT RATE BOOK     ' TO option(9)
STORE 'CASH LOG           ' TO option(10)

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
*DO dbf_chk

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
   MENU 3,option,10,10
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
         DO timtrack
      ENDIF
      IF men_col = 5
         STORE 0 TO C
         DO timaddwk
      ENDIF
      IF men_col = 6
         DO timsrcwk
      ENDIF
      IF men_col = 7
         DO timlstwk
      ENDIF
      IF men_col = 8
         DO timcngwk
      ENDIF
      IF men_col = 9
         DO timpend
      ENDIF
      
      IF men_col = 10
         DO timwrepo
      ENDIF
		IF men_col = 11
			DEFINE WINDOW TEXTMESSAGE FROM 10,05 TO 50,80 TITLE 'List of Technicians phones';
		CLOSE FLOAT GROW SHADOW ZOOM SYSTEM
			ACTIVATE WINDOW TEXTMESSAGE
			SET DELIMITERS ON
			STORE 'V' TO VT
			DO WHILE .T.
				CLEAR
				@ 5,3 SAY 'EMP #       NAME           CELLULAR'
				SET DELETED ON
				USE employee
				SET ORDER TO 2
				set headings off
				DISP ALL FIELDS  emp_no,LEFT(name,15),+'  '+LEFT(carea,5),LEFT(cphone,8) OFF
				@ 1,5 SAY 'Would you like to use Verizon or T-Mobile      [V] or [T]' get vt picture 'X'
				READ
				IF UPPER(VT) = 'V'
					EXIT
				ENDIF
				IF UPPER(VT) = 'T'
					EXIT
				ENDIF
			ENDDO
	*		DO CASE
	*			CASE UPPER(VT) = 'V'
	*				run "C:\Progra~1\Intern~1\iexplore.exe http://www.vtext.com/"
	*			CASE UPPER(VT) = 'T'
					RUN "C:\Progra~1\Intern~1\iexplore.exe http://www.textem.net/"
	*		ENDCASE	
			RELEASE WINDOW
			SET DELIMITERS OFF
		ENDIF

		IF men_col = 12
			RUN "C:\Progra~1\Intern~1\iexplore.exe https://squareup.com/signup"
			DO PHONEMEM.SPR
		ENDIF
		IF men_col = 14
			DO tst_ti_1.spr
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
      





      *  ---INVENTORY MENU
   CASE men_row = 2
      SELECT 2
      IF men_col = 1
         STORE 0 TO C
         DO timaddst
      ENDIF
      IF men_col = 2
         DO timsrchd
      ENDIF
      IF men_col = 3
         DO timprinv
      ENDIF
      IF men_col = 4
         DO timcngst
      ENDIF
      IF men_col = 5
         DO timdelst
      ENDIF
      IF men_col = 6
         DO timcompa
      ENDIF
      IF men_col = 7
         DO timvfy
      ENDIF
      IF men_col = 8
         DO timphone.spr
      ENDIF
      IF men_col = 9
         DO pbadd
      ENDIF
      
      *  ---OPTION MENU
   CASE men_row = 3
      IF men_col = 1
         DO timbrwse
      ENDIF
      IF men_col = 2
         DO timinqur
      ENDIF
      IF men_col = 3
         DO timrmbrs.prg
      ENDIF
      IF men_col = 4
         DO timrordr
      ENDIF
      IF men_col = 5
         DO timcancl.spr
      ENDIF
      IF men_col = 6
         DO stockbid
      ENDIF
      IF men_col = 7
         DO timcclog.spr
      ENDIF
      IF men_col = 8
         DO timaelog
      ENDIF
      IF men_col = 9
         DO flatrate.spr
      ENDIF
      IF men_col = 10
         DO timcash.spr
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
         * ON ERROR
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
         REPLACE ALL invoice WITH '*AVAIL*'
         
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
         IF FLOCK()
            USE timpool EXCL
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
         DO timbckup
         
      ENDIF
      IF men_col = 2
         DO timindex
      ENDIF
      
      IF men_col = 3
         IF FILE('allied.fxp') .OR. FILE('allied.prg') .OR. FILE('allied.fox')
            DO allied
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
         DO admain.spr
      ENDIF
      IF men_col = 5
         DO timpayro
      ENDIF
      IF men_col = 6
         SET BELL ON
      ENDIF
      IF men_col = 7
         SET BELL OFF
      ENDIF
      IF men_col = 8
         DO timbill
      ENDIF
      IF men_col = 9
         DO company.spr
      ENDIF
      
      IF men_col = 10
         DO timbid_1.spr
      ENDIF
      
      IF men_col = 11
         *			USE employee
         *			SET ORDER TO 2
         WAIT WINDOW 'This function is not completed, try again later!' NOWAIT
      ENDIF
      
      
   ENDCASE
ENDDO


*!*****************************************************************************
*!
*!      Procedure: LISTEMPL
*!
*!           Uses: EMPLOYEE.DBF       
*!
*!      CDX files: EMPLOYEE.CDX
*!
*!*****************************************************************************
PROCEDURE listempl
DEFINE WINDOW listempl FROM 20,05 TO 40,80 DOUBLE
ACTIVATE WINDOW listempl

CLEAR

@ 1,0 SAY 'EMP #       NAME           HOME           PAGER          CELLULAR'
@ 2,0
SET DELETED ON
USE employee
SET ORDER TO 2
SET DELETED ON
SET TITLE OFF
DISP ALL FIELDS  emp_no,LEFT(name,15),+'  '+harea,hphone,+'  '+carea,cphone OFF

WAIT
RELEASE WINDOW

* EOF: TIMMMENU.PRG
*: EOF: TIM1MENU.PRG
