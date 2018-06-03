ON ESCAPE RETURN


GOTO TOP
DO WHILE .NOT. EOF()

	STORE '' TO mfirstname
	STORE 1 TO CNT
	DO WHILE SUBSTR(name,CNT,1) <> ' '
		STORE mfirstname + SUBSTR(name,CNT,1) TO mfirstname
		STORE CNT + 1 TO CNT
	ENDDO
	REPLACE firstname WITH mfirstname
	*	REPLACE lastname WITH SUBSTR(NAME,CNT+1,LEN(ALLTRIM(NAME)))
	STORE '' TO mlastname
	STORE LEN(ALLTRIM(name)) TO CNT
	DO WHILE ALLTRIM(SUBSTR(name,CNT,1)) > ' '
		STORE SUBSTR(name,CNT,1) + mlastname TO mlastname
		STORE CNT - 1 TO CNT
	ENDDO
	REPLACE lastname WITH mlastname
	SKIP 1
ENDDO
REPLACE ALL lastname WITH name FOR LEN(ALLTRIM(name)) > LEN(ALLTRIM(firstname)) + LEN(ALLTRIM(lastname)) +1
REPLACE ALL firstname WITH ' ' for name = lastname
