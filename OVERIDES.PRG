SET COLOR TO

private memp_no,myear,YN

STORE 'Y' TO YN
STORE RIGHT(DTOC(DATE()),4) TO MYEAR
STORE '   ' TO MEMP_NO

IF USED("timpayro")
	SELECT timpayro
	SET ORDER TO 0
ELSE
	SELECT 6
	USE (LOCFILE("timpayro.dbf","DBF","Where is timpayro?"));
		AGAIN ALIAS timpayro ;
		ORDER 0
ENDIF

CLEAR
@ 2,5 SAY 'Enter Employee Number     ' get memp_no
@ 3,5 SAY 'Enter Year                ' get myear
READ
REPORT FORM OVERIDES FOR 'OVERRIDES'$DESCRIP .AND. EMP_NO=memp_no .AND. DTOC(DATE_APPRV) <> '  /  /  ' .AND. RIGHT(DTOC(DATE_APPRV),4)=myear PREVIEW

@ 6,5 SAY 'would you like to print this report?   ' GET YN
READ
IF UPPER(YN) = 'Y'
	REPORT FORM OVERIDES FOR 'OVERRIDES'$DESCRIP .AND. EMP_NO=memp_no .AND. DTOC(DATE_APPRV) <> '  /  /  ' .AND. RIGHT(DTOC(DATE_APPRV),4)=myear TO PRINT
ENDIF
RETURN