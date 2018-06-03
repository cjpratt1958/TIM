*       *********************************************************
*       *                                                         
*       * 04/18/06             rpt27.prg               18:36:37 
*       *                                                         
*       *********************************************************
*       *                                                         
*       * Carl Pratt                                           
*       *                                                         
*       * Copyright (c) 2006 Cornerstones Electrical Services, Inc.                         
*       * PO BOX 2138                                                
*       * Sandy, UT. 84091                                           
*       *                                                         
*       * Description:                                            
*       * This program allows the user to choose from 5 Reports
*       * associated with Continuous Contests Running.    
*       *                                                         
*       *********************************************************


*       *********************************************************
*       *                                                         
*       *         rpt27/Windows Setup Code - SECTION 1          
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
*       *     S2623605/Windows Databases, Indexes, Relations      
*       *                                                         
*       *********************************************************
*

IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 0
	USE (LOCFILE("data\timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF


*       *********************************************************
*       *                                                         
*       *               Windows Window definitions                
*       *                                                         
*       *********************************************************
*

IF NOT WEXIST("rpt27")
	DEFINE WINDOW rpt27 ;
		AT  0.000, 0.000  ;
		SIZE 29.769,96.667 ;
		TITLE "Report 27" ;
		FONT "MS Sans Serif", 8 ;
		STYLE "B" ;
		FLOAT ;
		CLOSE ;
		MINIMIZE ;
		COLOR RGB(,,,192,192,192)
	MOVE WINDOW rpt27 CENTER
ENDIF


*       *********************************************************
*       *                                                         
*       *         COMPANY/Windows Setup Code - SECTION 2          
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

MOVE WINDOW 'rpt27' CENTER
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

ACTIVATE WINDOW rpt27
WAIT WINDOW 'press any key to hide window rpt27'

this is an intentional error


HIDE WINDOW rpt27
WAIT WINDOW 'press any key to show window rpt27'
SHOW WINDOW rpt27
WAIT WINDOW 'press any key to release window rpt27'
RELEASE WINDOW rpt27













** PROCEDURES **

PROCEDURE wizerrorhandler
	* This very simple error handler is primarily intended
	* to trap for General field OLE errors which may occur
	* during editing from the MODIFY GENERAL window.
	WAIT WINDOW message()
RETURN