STORE '01' to mPriority
STORE .F. TO mexit
DEFINE WINDOW mPriMenu FROM 13,30 TO 23,55 DOUBLE
DO WHILE NOT mexit
	ACTIVATE WINDOW mPriMenu
	@ 1,2 PROMPT '00  -  Emergency '
	@ 2,2 PROMPT '01  -  Go For It '
	@ 3,2 PROMPT '02  -  Estimate  '
	@ 4,2 PROMPT '03  -  Cancel    '
	@ 5,2 PROMPT '04  -  Rework    '
	@ 6,2 PROMPT '05  -  Self Gen. '
	@ 7,2 PROMPT '06  -  Other     '
	MENU TO mchoice
	DEACTIVATE WINDOW mpriMenu
	DO CASE
		CASE mchoice = 1
			STORE '00' TO mpriority
			STORE .T. TO mexit
			LOOP
		CASE mchoice = 2
			STORE '01' TO mpriority
			STORE .T. TO mexit
			LOOP

		CASE MCHOICE = 3
			STORE '02' TO mpriority
			STORE .T. TO mexit
			LOOP

		CASE MCHOICE = 4
			STORE '03' TO mpriority
			STORE .T. TO mexit
			LOOP

		CASE MCHOICE = 5
			STORE '04' TO mpriority
			STORE .T. TO mexit
			LOOP

		CASE MCHOICE = 6
			STORE '05' TO mpriority
			STORE .T. TO mexit
			LOOP

		CASE MCHOICE = 7
			STORE '06' TO mpriority
			STORE .T. TO mexit
			LOOP

	ENDCASE
ENDDO
? mPriority
