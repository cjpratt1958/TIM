
*----------------------------------------------
*- Code from z:\tim\addinvce.scx
*----------------------------------------------

*----- _26T1ERRJZ
PROCEDURE Activate
SELECT Timstock
ENDPROC

*----- cmgBtn_specific3
PROCEDURE When
Do AddSpcfc
ENDPROC

*----- cmgBtn_replenish4
PROCEDURE When
STORE 'STOCK  ' TO m.jobs
STORE 'RESTOCK TRUCK' TO m.name
ENDPROC

*----- cmgBtn_close6
PROCEDURE When
CLEAR READ
RELEASE WINDOW
RETURN
ENDPROC

*----- frsAddinvce1
PROCEDURE Load
*- [CONVERTER] Open tables so that fields are availableTHIS.DataEnvironment.OpenTables
*- [CONVERTER] Reset record pointersLOCAL aTbl, iLen, i, iRec, cVar, iPreviPrev = SELECT()DIMENSION aTbl[1,2]iLen = AUSED(aTbl)FOR i = 1 TO iLen	cVar = '_iconv' + PROPER(aTbl[i,1]) + 'GoToPlaceHolder'	IF TYPE(cVar) # 'N'		iRec = -2	ELSE		iRec = EVAL(cVar)	ENDIF	IF USED(aTbl[i,1])		SELECT (aTbl[i,1])		DO CASE			CASE BETWEEN(iRec, 1, RECCOUNT())				GOTO iRec			CASE iRec = 0				GO TOP				SKIP IIF(!BOF(),-1,0)			CASE iRec = -1				GO BOTTOM				SKIP IIF(!EOF(),1,0)			OTHERWISE				GO TOP		ENDCASE	ENDIFNEXTIF iPrev > 0	IF USED(iPrev)		SELECT (iPrev)	ENDIFENDIFRELEASE aTbl, iLen, i, iRec, cVar, iPrev
ENDPROC

PROCEDURE Unload
*- [CONVERTER] Remember record pointersIF USED("_26q1hcxr4")	SELECT _26q1hcxr4	_iconv_26q1hcxr4GoToPlaceHolder = IIF(BOF(), 0, IIF(EOF(), -1, RECNO()))ENDIFIF USED("Timstock")	SELECT Timstock	_iconvTimstockGoToPlaceHolder = IIF(BOF(), 0, IIF(EOF(), -1, RECNO()))ENDIFENDPROC

*----- Miscellaneous code
*- [CONVERTER] Declare variables for record pointersPUBLIC _iconv_26q1hcxr4GoToPlaceHolder
PUBLIC _iconvTimstockGoToPlaceHolder

EXTERNAL PROC addinvce.scx

DO FORM "addinvce.scx" NAME _26T1ERRLN LINKED 
