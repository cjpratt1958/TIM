
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
*- [CONVERTER] Open tables so that fields are available

ENDPROC

PROCEDURE Unload
*- [CONVERTER] Remember record pointers

*----- Miscellaneous code
*- [CONVERTER] Declare variables for record pointers
PUBLIC _iconvTimstockGoToPlaceHolder

EXTERNAL PROC addinvce.scx

DO FORM "addinvce.scx" NAME _26T1ERRLN LINKED 