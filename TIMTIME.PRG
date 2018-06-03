*:		Program Name:	TimTime.prg
*:		



*	SELECT 2
IF USED("timtime")
	SELECT timtime
	SET ORDER TO 4
ELSE
	SELECT 2
	USE (LOCFILE("timtime.dbf","DBF","Where is timtime?"));
		AGAIN ALIAS timtime ;
		ORDER 4
ENDIF


