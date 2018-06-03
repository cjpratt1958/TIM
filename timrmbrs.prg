*       *********************************************************
*       *                                                         
*       * 04/27/2010           TIMRMBRS.PRG             20:25:52 
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Author's Name                                           
*       *                                                         
*       * Copyright (c) 2010 Company Name                         
*       * Address                                                 
*       * City,     Zip                                           
*       *                                                         
*       * Description:                                            
*       * This program was automatically generated by GENSCRN.    
*       *                                                         
*       *********************************************************


*       *********************************************************
*       *                                                         
*       *         TIMRMBRS/Windows Setup Code - SECTION 1         
*       *                                                         
*       *********************************************************
*

#REGION 1
PRIVATE wzfields,wztalk
IF SET("TALK") = "ON"
	SET TALK OFF
	m.wztalk = "ON"
ELSE
	m.wztalk = "OFF"
ENDIF
m.wzfields=SET('FIELDS')
SET FIELDS OFF
IF m.wztalk = "ON"
	SET TALK ON
ENDIF


#REGION 0
REGIONAL m.currarea, m.talkstat, m.compstat

IF SET("TALK") = "ON"
	SET TALK OFF
	m.talkstat = "ON"
ELSE
	m.talkstat = "OFF"
ENDIF
m.compstat = SET("COMPATIBLE")
SET COMPATIBLE FOXPLUS

m.rborder = SET("READBORDER")
SET READBORDER ON

m.currarea = SELECT()


*       *********************************************************
*       *                                                         
*       *     TIMRMBRS/Windows Databases, Indexes, Relations      
*       *                                                         
*       *********************************************************
*

IF USED("timrmbrs")
	SELECT timrmbrs
	SET ORDER TO TAG "_2x70n7bu3"
ELSE
	SELECT 0
	USE (LOCFILE("timrmbrs.dbf","DBF","Where is timrmbrs?"));
		AGAIN ALIAS timrmbrs ;
		ORDER TAG "_2x70n7bu3"
ENDIF


*       *********************************************************
*       *                                                         
*       *               Windows Window definitions                
*       *                                                         
*       *********************************************************
*

IF NOT WEXIST("_2x717shi1")
	DEFINE WINDOW _2x717shi1 ;
		AT  0.000, 0.000  ;
		SIZE 41.385,84.333 ;
		TITLE "Timrmbrs" ;
		FONT "MS Sans Serif", 8 ;
		STYLE "B" ;
		FLOAT ;
		CLOSE ;
		MINIMIZE ;
		COLOR RGB(,,,255,255,255)
	MOVE WINDOW _2x717shi1 CENTER
ENDIF


*       *********************************************************
*       *                                                         
*       *         TIMRMBRS/Windows Setup Code - SECTION 2         
*       *                                                         
*       *********************************************************
*

#REGION 1

#DEFINE C_DBFEMPTY		'Database is empty, add a record?'
#DEFINE C_EDITS			'Please finish your edits.'
#DEFINE C_TOPFILE		'Top of file.'
#DEFINE C_ENDFILE		'End of file.'
#DEFINE C_BRTITLE		'Locate Record'
#DEFINE C_NOLOCK		'Sorry, could not lock record -- try again later.'
#DEFINE C_ECANCEL		'Edits Canceled.'
#DEFINE C_DELREC		'Delete selected record?'
#DEFINE C_NOFEAT		'Feature not available yet.'
#DEFINE C_NOWIZ			'Wizard application is not available.'
#DEFINE C_MAKEREPO		'Creating report with Report Wizard.'
#DEFINE C_NOREPO		'Could not create report.'
#DEFINE C_DELNOTE 		'Deleting records...'
#DEFINE C_READONLY 		'Table is read-only. No editing allowed.'
#DEFINE C_NOTABLE 		'No table selected. Open table or run query.'
#DEFINE C_BADEXPR		'Invalid expression.'
#DEFINE C_LOCWIZ		'Locate WIZARD.APP:'
#DEFINE C_MULTITABLE	'You have multiple related tables. Adding records in not allowed.'

MOVE WINDOW '_2x717shi1' CENTER
PRIVATE isediting,isadding,wztblarr
PRIVATE wzolddelete,wzolderror,wzoldesc
PRIVATE wzalias, tempcurs,wzlastrec
PRIVATE isreadonly,find_drop,is2table

IF EMPTY(ALIAS())
	WAIT WINDOW C_NOTABLE
	RETURN
ENDIF

m.wztblarr= ''
m.wzalias=SELECT()
m.isediting=.F.
m.isadding=.F.
m.is2table = .F.
m.wzolddelete=SET('DELETE')
SET DELETED ON
m.tempcurs=SYS(2015)  &&used if General field
m.wzlastrec = 1
m.wzolderror=ON('error')
ON ERROR DO wizerrorhandler
wzoldesc=ON('KEY','ESCAPE')
ON KEY LABEL ESCAPE
m.find_drop = IIF(_DOS,0,2)

m.isreadonly=IIF(ISREAD(),.T.,.F.)
IF m.isreadonly
	WAIT WINDOW C_READONLY TIMEOUT 1
ENDIF


IF RECCOUNT()=0 AND !m.isreadonly AND fox_alert(C_DBFEMPTY)
    APPEND BLANK
ENDIF

GOTO TOP
SCATTER MEMVAR MEMO

*       *********************************************************
*       *                                                         
*       *             TIMRMBRS/Windows Screen Layout              
*       *                                                         
*       *********************************************************
*

#REGION 1
IF WVISIBLE("_2x717shi1")
	ACTIVATE WINDOW _2x717shi1 SAME
ELSE
	ACTIVATE WINDOW _2x717shi1 NOSHOW
ENDIF
@ 0.000,30.000 SAY (LOCFILE("logo.bmp","BMP|ICO|PCT|ICN", "Where is logo?" )) BITMAP ;
	SIZE 7.462,22.000 ;
	ISOMETRIC ;
	STYLE "T"
@ 7.308,0.000 TO 7.308,84.000 ;
	PEN 2, 8 ;
	STYLE "1" ;
	COLOR RGB(255,0,0,255,0,0)
@ 37.615,0.000 TO 37.615,84.000 ;
	PEN 2, 8 ;
	STYLE "1" ;
	COLOR RGB(0,0,0,0,0,0)
@ 21.308,2.000 SAY "List monies expended by the Technician with all sales tickets, reciepts,  cash register" + CHR(13) + ;
		"slips, copy; of workorders etc... must be attached to this form or Technician will not" + CHR(13) + ;
		"be reimbursed." ;
	SIZE 3.000,81.000, 0.000 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BT" ;
	COLOR RGB(0,0,0,,,,)
@ 8.385,20.000 SAY "TECHNICIAN'S CASH REIMBURSMENT FORM" ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BT" ;
	COLOR RGB(0,0,0,,,,)
@ 10.077,0.167 TO 20.385,84.000 ;
	PEN 1, 8 ;
	COLOR RGB(0,0,0,,,,)
@ 33.308,25.167 SAY "TOTAL MONIES EXPENDED BY TECHNICIAN:" ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BT" ;
	COLOR RGB(0,0,0,,,,)
@ 35.154,2.000 SAY "Check NO: " ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BT" ;
	COLOR RGB(0,0,0,,,,)
@ 11.154,4.333 SAY "Name to be Reimbursed:" ;
	SIZE 1.000,23.333 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 11.154,58.333 SAY "Date:" ;
	SIZE 1.000,7.833 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 13.000,4.333 SAY "W.O. Number:" ;
	SIZE 1.000,17.333 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 14.846,4.333 SAY "Job Name:" ;
	SIZE 1.000,17.333 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 16.692,4.333 SAY "Address:" ;
	SIZE 1.000,17.333 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 18.538,4.333 SAY "City,State,Zip:" ;
	SIZE 1.000,17.333 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 25.000,6.333 SAY "DESCRIPTION:" ;
	SIZE 1.000,25.333 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 25.000,70.333 SAY "AMT:" ;
	SIZE 1.000,7.167 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BIT" ;
	PICTURE "@J" ;
	COLOR RGB(0,0,0,255,255,255)
@ 11.154,30.333 GET m.e_n ;
	SIZE 1.000,28.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 11.154,68.333 GET m.e_date ;
	SIZE 1.000,13.600 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 13.000,24.333 GET m.no ;
	SIZE 1.000,11.200 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 14.846,24.333 GET m.j_name ;
	SIZE 1.000,44.800 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 16.692,24.333 GET m.j_addr ;
	SIZE 1.000,44.800 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 18.538,24.333 GET m.j_city ;
	SIZE 1.000,28.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 18.538,49.167 GET m.j_state ;
	SIZE 1.000,4.400 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 18.538,54.333 GET m.j_zip ;
	SIZE 1.000,11.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 26.769,4.333 GET m.v_1 ;
	SIZE 1.000,77.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 28.308,4.333 GET m.v_2 ;
	SIZE 1.000,77.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 29.846,4.333 GET m.v_3 ;
	SIZE 1.000,77.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 31.385,4.333 GET m.v_4 ;
	SIZE 1.000,77.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 26.769,70.333 GET m.a_1 ;
	SIZE 1.000,9.600 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K 9,999.99" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 29.846,70.333 GET m.a_3 ;
	SIZE 1.000,9.600 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K 9,999.99" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 38.769,1.667 GET m.top_btn ;
	PICTURE "@*HN \<Top" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('TOP') ;
	MESSAGE 'Go to first record.'
@ 38.769,9.667 GET m.prev_btn ;
	PICTURE "@*HN \<Prev" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('PREV') ;
	MESSAGE 'Go to previous record.'
@ 38.769,17.667 GET m.next_btn ;
	PICTURE "@*HN \<Next" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('NEXT') ;
	MESSAGE 'Go to next record.'
@ 38.769,25.667 GET m.end_btn ;
	PICTURE "@*HN \<End" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('END') ;
	MESSAGE 'Go to last record.'
@ 38.769,33.667 GET m.loc_btn ;
	PICTURE "@*HN \<Locate" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('LOCATE') ;
	MESSAGE 'Locate a record.'
@ 38.769,41.667 GET m.add_btn ;
	PICTURE "@*HN \<Add" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('ADD') ;
	MESSAGE 'Add a new record.'
@ 38.769,49.667 GET m.edit_btn ;
	PICTURE "@*HN Ed\<it" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('EDIT') ;
	MESSAGE 'Edit current record.'
@ 38.769,57.667 GET m.del_btn ;
	PICTURE "@*HN \<Delete" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('DELETE') ;
	MESSAGE 'Delete current record.'
@ 38.769,65.667 GET m.prnt_btn ;
	PICTURE "@*HN P\<rint" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('PRINT') ;
	MESSAGE 'Print report.'
@ 38.769,73.667 GET m.exit_btn ;
	PICTURE "@*HN \<Close" ;
	SIZE 1.769,7.833,0.667 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	STYLE "B" ;
	VALID btn_val('EXIT') ;
	MESSAGE 'Close screen.'
@ 36.385,28.333 GET m.paid ;
	SIZE 1.000,13.600 ;
	DEFAULT {  /  /  } ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 33.308,70.333 GET m.total ;
	SIZE 1.000,11.800 ;
	DEFAULT 0 ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K 9,999.99" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 35.000,0.000 TO 35.000,84.000 ;
	PEN 2, 8 ;
	STYLE "1" ;
	COLOR RGB(0,0,0,0,0,0)
@ 35.077,28.000 SAY "Date: " ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BT" ;
	COLOR RGB(0,0,0,,,,)
@ 35.154,46.000 SAY "Pd by: " ;
	FONT "MS Sans Serif", 8 ;
	STYLE "BT" ;
	COLOR RGB(0,0,0,,,,)
@ 36.385,2.333 GET m.pdno ;
	SIZE 1.000,28.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 36.385,45.333 GET m.pdby ;
	SIZE 1.000,28.000 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K XXXXXXXXXXXXXXXXXXXX" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 28.308,70.333 GET m.a_2 ;
	SIZE 1.000,9.600 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K 9,999.99" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)
@ 31.385,70.333 GET m.a_4 ;
	SIZE 1.000,9.600 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	PICTURE "@K 9,999.99" ;
	WHEN isediting ;
	COLOR ,RGB(0,0,128,255,255,255)

IF NOT WVISIBLE("_2x717shi1")
	ACTIVATE WINDOW _2x717shi1
ENDIF


*       *********************************************************
*       *                                                         
*       *    WindowsREAD contains clauses from SCREEN timrmbrs    
*       *                                                         
*       *********************************************************
*

READ CYCLE ;
	ACTIVATE READACT() ;
	DEACTIVATE READDEAC() ;
	NOLOCK

RELEASE WINDOW _2x717shi1

*       *********************************************************
*       *                                                         
*       *                Windows Closing Databases                
*       *                                                         
*       *********************************************************
*

IF USED("timrmbrs")
	SELECT timrmbrs
	USE
ENDIF

SELECT (m.currarea)


#REGION 0

SET READBORDER &rborder

IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF


*       *********************************************************
*       *                                                         
*       *              TIMRMBRS/Windows Cleanup Code              
*       *                                                         
*       *********************************************************
*

#REGION 1
SET DELETED &wzolddelete
SET FIELDS &wzfields
ON ERROR &wzolderror
ON KEY LABEL ESCAPE &wzoldesc
DO CASE
CASE _DOS AND SET('DISPLAY')='VGA25'
	@24,0 CLEAR TO 24,79
CASE _DOS AND SET('DISPLAY')='VGA50'
	@49,0 CLEAR TO 49,79
CASE _DOS
	@24,0 CLEAR TO 24,79
ENDCASE

****Procedures****


*       *********************************************************
*       *                                                         
*       *  TIMRMBRS/Windows Supporting Procedures and Functions   
*       *                                                         
*       *********************************************************
*

#REGION 1
PROCEDURE readdeac
  IF isediting
    ACTIVATE WINDOW '_2x717shi1'
    WAIT WINDOW C_EDITS NOWAIT
  ENDIF
  IF !WVISIBLE(WOUTPUT())
    CLEAR READ
    RETURN .T.
  ENDIF
RETURN .F.

PROCEDURE readact
  IF !isediting
  	SELECT (m.wzalias)
  	SHOW GETS
  ENDIF
  DO REFRESH
RETURN

PROCEDURE wizerrorhandler
	* This very simple error handler is primarily intended
	* to trap for General field OLE errors which may occur
	* during editing from the MODIFY GENERAL window.
	WAIT WINDOW message()
RETURN


PROCEDURE printrec
	  PRIVATE sOldError,wizfname,saverec,savearea,tmpcurs,tmpstr
	  PRIVATE prnt_btn,p_recs,p_output,pr_out,pr_record
	  STORE 1 TO p_recs,p_output
	  STORE 0 TO prnt_btn
	  STORE RECNO() TO saverec
	  m.sOldError=ON('error')
	  DO pdialog
	  IF m.prnt_btn = 2
	    RETURN
	  ENDIF
	  IF !FILE(ALIAS()+'.FRX')
	  	m.wizfname=SYS(2004)+'WIZARDS\'+'WIZARD.APP'
	  	IF !FILE(m.wizfname)
			ON ERROR *
			m.wizfname=LOCFILE('WIZARD.APP','APP',C_LOCWIZ)
			ON ERROR &sOldError
			IF !'WIZARD.APP'$UPPER(m.wizfname)
      			WAIT WINDOW C_NOWIZ
      			RETURN
			ENDIF
	  	ENDIF
     	WAIT WINDOW C_MAKEREPO NOWAIT
		m.savearea=SELECT()
		m.tmpcurs='_'+LEFT(SYS(3),7)
		CREATE CURSOR (m.tmpcurs) (comment m)
		m.tmpstr = '* LAYOUT = COLUMNAR'+CHR(13)+CHR(10)
		INSERT INTO (m.tmpcurs) VALUES(m.tmpstr)
		SELECT (m.savearea)
	  	DO (m.wizfname) WITH '','WZ_QREPO','NOSCRN/CREATE',ALIAS(),m.tmpcurs
		USE IN (m.tmpcurs)
     	WAIT CLEAR
	  	IF !FILE(ALIAS()+'.FRX')  &&wizard could not create report
     		WAIT WINDOW C_NOREPO
     		RETURN
	  	ENDIF
	  ENDIF
	
  	  m.pr_out=IIF(m.p_output=1,'TO PRINT NOCONSOLE','PREVIEW')
	  m.pr_record=IIF(m.p_recs=1,'NEXT 1','ALL')
  	  REPORT FORM (ALIAS()) &pr_out &pr_record
	  GO m.saverec
RETURN


PROCEDURE BTN_VAL
	PARAMETER m.btnname
	DO CASE
	CASE  m.btnname='TOP'
		GO TOP
		WAIT WINDOW C_TOPFILE NOWAIT
	CASE  m.btnname='PREV'
		IF !BOF()
			SKIP -1
	 	ENDIF
	 	IF BOF()
			WAIT WINDOW C_TOPFILE NOWAIT
			GO TOP
		ENDIF
	CASE  m.btnname='NEXT'
		IF !EOF()
			SKIP 1
		ENDIF
		IF EOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			GO BOTTOM
		ENDIF
	CASE  m.btnname='END'
		GO BOTTOM
		WAIT WINDOW C_ENDFILE NOWAIT
	CASE  m.btnname='LOCATE'
		DO loc_dlog
	CASE  m.btnname='ADD'  AND !isediting &&add record
		isediting=.T.
		isadding=.T.
		=edithand('ADD')
		_curobj=1
		store date to m.e_date
		DO refresh
		SHOW GETS
		RETURN
	CASE  m.btnname='EDIT'  AND !isediting &&edit record
		IF EOF() OR BOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			RETURN
		ENDIF
		IF RLOCK()
			isediting=.T.
			_curobj=1
			DO refresh
			RETURN
		ELSE
			WAIT WINDOW C_NOLOCK
		ENDIF
	CASE m.btnname='EDIT'  AND isediting &&save record
		IF isadding
			=edithand('SAVE')
		ELSE
			GATHER MEMVAR MEMO
		ENDIF
		UNLOCK
		isediting=.F.
		isadding=.F.
		DO refresh
	CASE m.btnname='DELETE'  AND isediting 	&&cancel record
		IF isadding
			=edithand('CANCEL')
		ENDIF
		isediting=.F.
		isadding=.F.
		UNLOCK
		WAIT WINDOW C_ECANCEL NOWAIT
		DO refresh
	CASE m.btnname='DELETE'
		IF EOF() OR BOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			RETURN
		ENDIF
		IF fox_alert(C_DELREC)
			DELETE
			IF !EOF() AND DELETED()
				SKIP 1
			ENDIF
			IF EOF()
				WAIT WINDOW C_ENDFILE NOWAIT
				GO BOTTOM
			ENDIF
		ENDIF
	CASE m.btnname='PRINT'
		DO printrec
		RETURN
	CASE m.btnname='EXIT'
		m.bailout=.T.	&&this is needed if used with FoxApp
		CLEAR READ
		RETURN
	ENDCASE
	SCATTER MEMVAR MEMO
	SHOW GETS
RETURN


PROCEDURE REFRESH
  DO CASE
  CASE m.isreadonly AND RECCOUNT()=0
	SHOW GETS DISABLE
	SHOW GET exit_btn ENABLE
  CASE m.isreadonly
	SHOW GET add_btn DISABLE
	SHOW GET del_btn DISABLE
	SHOW GET edit_btn DISABLE
  CASE (RECCOUNT()=0 OR EOF()) AND !m.isediting
	SHOW GETS DISABLE
	SHOW GET add_btn ENABLE
	SHOW GET exit_btn ENABLE
  CASE m.isediting
    SHOW GET find_drop DISABLE
	SHOW GET top_btn DISABLE
	SHOW GET prev_btn DISABLE
	SHOW GET loc_btn DISABLE
	SHOW GET next_btn DISABLE
	SHOW GET end_btn DISABLE
	SHOW GET add_btn DISABLE
	SHOW GET prnt_btn DISABLE
	SHOW GET exit_btn DISABLE
	SHOW GET edit_btn,1 PROMPT "\<Save"
	SHOW GET del_btn,1 PROMPT "\<Cancel"
	ON KEY LABEL ESCAPE DO BTN_VAL WITH 'DELETE'
	RETURN
  OTHERWISE
	SHOW GET edit_btn,1 PROMPT "Ed\<it"
	SHOW GET del_btn,1 PROMPT "\<Delete"
	SHOW GETS ENABLE
  ENDCASE
  IF m.is2table
  	SHOW GET add_btn DISABLE
  ENDIF
  ON KEY LABEL ESCAPE
RETURN


PROCEDURE edithand
	PARAMETER m.paction
	* procedure handles edits
	DO CASE
	CASE m.paction = 'ADD'
		SCATTER MEMVAR MEMO BLANK
	CASE m.paction = 'SAVE'
		INSERT INTO (ALIAS()) FROM MEMVAR
	CASE m.paction = 'CANCEL'
		* nothing here
	ENDCASE
RETURN

PROCEDURE fox_alert
    PARAMETER wzalrtmess
    PRIVATE alrtbtn
    m.alrtbtn=2
	DEFINE WINDOW _qec1ij2t7 AT 0,0 SIZE 8,50 ;
	  FONT "MS Sans Serif",10 STYLE 'B' ;
	  FLOAT NOCLOSE NOMINIMIZE DOUBLE TITLE WTITLE()
	MOVE WINDOW _qec1ij2t7 CENTER
	ACTIVATE WINDOW _qec1ij2t7 NOSHOW
	@ 2,(50-txtwidth(wzalrtmess))/2 SAY wzalrtmess;
	  FONT "MS Sans Serif", 10 STYLE "B"
	@ 6,18 GET m.alrtbtn ;
	  PICTURE "@*HT \<OK;\?\!\<Cancel" ;
	  SIZE 1.769,8.667,1.333 ;
	  FONT "MS Sans Serif", 8 STYLE "B"
	ACTIVATE WINDOW _qec1ij2t7
	READ CYCLE MODAL
	RELEASE WINDOW _qec1ij2t7
RETURN m.alrtbtn=1


PROCEDURE pdialog
	DEFINE WINDOW _qjn12zbvh ;
		AT  0.000, 0.000  ;
		SIZE 13.231,54.800 ;
		TITLE "Microsoft FoxPro" ;
		FONT "MS Sans Serif", 8 ;
		FLOAT NOCLOSE MINIMIZE SYSTEM
	MOVE WINDOW _qjn12zbvh CENTER
	ACTIVATE WINDOW _qjn12zbvh NOSHOW
	@ 2.846,33.600 SAY "Output:"  ;
		FONT "MS Sans Serif", 8 ;
		STYLE "BT"
	@ 2.846,4.800 SAY "Print:"  ;
		FONT "MS Sans Serif", 8 ;
		STYLE "BT"
	@ 4.692,7.200 GET m.p_recs ;
		PICTURE "@*RVN \<Current Record;\<All Records" ;
		SIZE 1.308,18.500,0.308 ;
		DEFAULT 1 ;
		FONT "MS Sans Serif", 8 ;
		STYLE "BT"
	@ 4.692,36.000 GET m.p_output ;
		PICTURE "@*RVN \<Printer;Pre\<view" ;
		SIZE 1.308,12.000,0.308 ;
		DEFAULT 1 ;
		FONT "MS Sans Serif", 8 ;
		STYLE "BT"
	@ 10.154,16.600 GET m.prnt_btn ;
		PICTURE "@*HT P\<rint;Ca\<ncel" ;
		SIZE 1.769,8.667,0.667 ;
		DEFAULT 1 ;
		FONT "MS Sans Serif", 8 ;
		STYLE "B"
	ACTIVATE WINDOW _qjn12zbvh
	READ CYCLE MODAL
	RELEASE WINDOW _qjn12zbvh
RETURN


PROCEDURE loc_dlog
	PRIVATE gfields,i
	DEFINE WINDOW wzlocate FROM 1,1 TO 20,40;
		SYSTEM GROW CLOSE ZOOM FLOAT FONT "MS Sans Serif",8
	MOVE WINDOW wzlocate CENTER
	m.gfields=SET('FIELDS',2)
	IF !EMPTY(RELATION(1))
		SET FIELDS ON
		IF m.gfields # 'GLOBAL'
			SET FIELDS GLOBAL
		ENDIF
		IF EMPTY(FLDLIST())
			m.i=1
			DO WHILE !EMPTY(OBJVAR(m.i))
				IF ATC('M.',OBJVAR(m.i))=0
					SET FIELDS TO (OBJVAR(m.i))
				ENDIF
				m.i = m.i + 1
			ENDDO
		ENDIF
	ENDIF
	BROWSE WINDOW wzlocate NOEDIT NODELETE ;
		NOMENU TITLE C_BRTITLE
	SET FIELDS &gfields
	SET FIELDS OFF
	RELEASE WINDOW wzlocate
RETURN

