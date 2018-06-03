*:*****************************************************************************
*:
*:        Program: C:\CORNER~1\TIM\PROGRAMS\TIMDMENU.PRG
*:         System: TIM2006
*:         Author: Carl Pratt
*:      Copyright (c) 2006, Cornerstones Electrical Services, Inc.
*:  Last modified: 03/19/2003 at  9:30:26
*:
*:      Called by: TIM.PRG                           
*:
*:          Calls: LISTEMPL.PRG
*:               : TIMCNGWK.PRG
*:               : TIMTRACK.PRG
*:               : TIM_OPEN.PRG
*:               : TIMPROC.PRG
*:               : LOGO               (procedure in TIMPROC.PRG)
*:               : ADDTITLE           (procedure in TIMPROC.PRG)
*:               : SECURITY           (procedure in TIMPROC.PRG)
*:               : &MENU.PRG
*:
*:      Documented 23:07:44                                FoxDoc version 3.00a
*:*****************************************************************************



*  ---Set Global Variables
UNLOCK ALL
CLOSE ALL
CLEAR GETS
SET CENTURY ON
SET CLOCK OFF
SET EXCLUSIVE ON
SET REPROCESS TO AUTOMATIC
SET COLOR ON
SET COLOR TO G/N,W/N
SET INTENSITY OFF
SET DELETED ON
SET SAFETY OFF
SET DELIMITERS OFF
SET FIELDS ON
SET TALK OFF
SET ECHO OFF
SET PRINT OFF
SET CONSOLE ON
SET STATUS OFF
SET SCOREBOARD OFF
SET PROCEDURE TO timproc
SET MESSAGE TO 24
SET BELL Off
UNLOCK ALL
ON KEY LABEL f2 DO listempl
ON KEY LABEL f3 DO timcngwk
ON KEY LABEL f1 DO timtrack

* Temporarily disabled to troubleshoot 12/06/2015
ON ERROR
* On error do errhand
* PARAMETERS ERRNUM, MESSAGE
* End Disabled
DIMENSION split_pay (5)
DIMENSION split_pcnt (5)
DIMENSION stechnician (5)

*  ---Set all default variables and public variables
DO tim_open




*  ---Load Procedure files
DO timproc
STORE '*****  M A I N   M E N U  *****' TO TITLE
STORE 40 - (LEN(serial)/2) TO SCOL
STORE 40 - (LEN(TITLE)/2) TO COL
STORE 'TIMMMENU.PRG' TO prg_name
DO LOGO
DO addtitle
STORE '9' TO ms_level



DO security



IF ms_level < '1'
   STORE '9' TO ms_level
ENDIF
STORE 'tim'+ms_level+'menu' TO MENU



DO &menu
*  EOF TIMDMENU.PRG

