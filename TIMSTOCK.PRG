*       *********************************************************
*       *                                                         
*       * 11/30/06             TimStock.prg              16:40:16 
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Author: Carl Pratt
*       *                                                         
*       * Copyright (c) 2006 Cornerstones Electric                         
*       * Address            P O Box 2138                                     
*       * City,     Zip      Sandy, Utah 84091                                     
*       *                                                         
*       * Description:                                            
*       * This program manages TimInventory Items
*       *                                                         
*       *********************************************************


*       *********************************************************
*       *                                                         
*       *         TimStock/Windows Setup Code - SECTION 1          
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
SET century ON
SET delimited off
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
*       *               Windows Window definitions                
*       *                                                         
*       *********************************************************
*

IF NOT WEXIST("_0uz0zqdf1")
	DEFINE WINDOW _0uz0zqdf1 ;
		AT  0.000, 0.000  ;
		SIZE 32.938,128.000 ;
		TITLE "Time Card" ;
		FONT "MS Sans Serif", 8 ;
		STYLE "B" ;
		FLOAT ;
		CLOSE ;
		MINIMIZE ;
		COLOR RGB(,,,192,192,192)
	MOVE WINDOW _0uz0zqdf1 CENTER
ENDIF


*       *********************************************************
*       *                                                         
*       *         TIMTIME/Windows Setup Code - SECTION 2          
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
#DEFINE C_MULTITABLE	'You have multiple related tables. Adding records is not allowed.'
#DEFINE C_NOEMPLOYEE	'This Employee does not exist.'




MOVE WINDOW '_0uz0zqdf1' CENTER
PRIVATE isediting,isadding,wztblarr
PRIVATE wzolddelete,wzolderror,wzoldesc
PRIVATE wzalias, tempcurs,wzlastrec
PRIVATE isreadonly,find_drop,is2table



*       *********************************************************
*       *                                                         
*       *     S0016949/Windows Databases, Indexes, Relations      
*       *                                                         
*       *********************************************************
*


IF USED("timwork")
	SELECT timwork
	SET ORDER TO TAG "no"
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER TAG "no"
ENDIF


IF USED("employee")
	SELECT employee
	SET ORDER TO TAG "emp_no"
ELSE
	SELECT 2
	USE (LOCFILE("employee.dbf","DBF","Where is employee?"));
		AGAIN ALIAS employee ;
		ORDER TAG "emp_no"
ENDIF



IF USED("TimStock")
	SELECT TimStock
	SET ORDER TO 1
ELSE
	SELECT 3
	USE (LOCFILE("TimStock.dbf","DBF","Where is TimStock?"));
		AGAIN ALIAS TimStock ;
		ORDER TAG 0
ENDIF


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
*ON ERROR DO wizerrorhandler
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

GOTO bottom
SCATTER MEMVAR MEMO
STORE 'N' TO m.ot
STORE 0.00 TO CommPcnt

