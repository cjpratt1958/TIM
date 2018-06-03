*:*****************************************************************************
*:
*:        Program: C:\TIM\PRG\TIMADDST.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 11/13/95 at 23:22:02
*:
*:      Called by: TIM1MENU.PRG
*:               : TIMCOMPA.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : STOC_STO           (procedure in TIMPROC.PRG)
*:               : STOCMENU           (procedure in TIMPROC.PRG)
*:               : STOCSCRN           (procedure in TIMPROC.PRG)
*:               : STOCK_GET          (procedure in TIMPROC.PRG)
*:               : TIMCHOOSE.PRG
*:               : S_SAVE             (procedure in TIMPROC.PRG)
*:               : H_SAVE             (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMSTOCK.DBF
*:               : TIMWORK.DBF
*:               : COMPANY.DBF
*:               : TIM_PO.DBF
*:               : TIMHISTR.DBF
*:               : TIMHOLD.DBF
*:               : TIMREORD.DBF
*:               : TIMTRKST.DBF
*:
*:      Documented 12:30:02                                FoxDoc version 3.00a
*:*****************************************************************************
*:*********************************************************************

CLEAR
SET HEADING OFF
SET DELIMITERS OFF
SET EXACT ON

*  ---STORE DIMENSION FOR TRUCKSTOCK POPUP MENU
DIMENSION chse(4)
STORE 'RETURN TO MAIN MENU' TO chse(1)
STORE 'REORDER FROM PREVIOUS JOBS' TO chse(2)
STORE 'PURCHASED FOR A SPECIFIC JOB' TO chse(3)
STORE 'ENTER PHONE PO' TO chse(4)
STORE 38 - LEN(chse(4))/2 TO y
STORE '       ' TO mtech
STORE '       ' TO mjobs
STORE 'TIMADDST.PRG' TO prg_name
STORE '***** A D D   I N V O I C E S *****' TO TITLE

*	SELECT 1
IF USED("timwork")
	SELECT timwork
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF

*	SELECT 2
IF USED("timstock")
	SELECT timstock
ELSE
	SELECT 2
	USE (LOCFILE("timstock.dbf","DBF","Where is timstock?"));
		AGAIN ALIAS timstock ;
		ORDER 0
ENDIF



*	SELECT 3
IF USED("company")
	SELECT company
ELSE
	SELECT 3
	USE (LOCFILE("company.dbf","DBF","Where is company?"));
		AGAIN ALIAS company ;
		ORDER 0
ENDIF

*	SELECT 4
IF USED("tim_po")
	SELECT tim_po
ELSE
	SELECT 4
	USE (LOCFILE("tim_po.dbf","DBF","Where is tim_po?"));
		AGAIN ALIAS tim_po;
		ORDER 0
ENDIF

*	SELECT 5
IF USED("timstock")
	SELECT timstock
ELSE
	SELECT 5
	USE (LOCFILE("timstock.dbf","DBF","Where is timstock?"));
		AGAIN ALIAS timstock;
		ORDER 0
ENDIF


DO addtitle
SELECT timstock
SET ORDER TO 1

DO WHILE mjobs <> 'Q'
	STORE 1 TO G
	GOTO BOTTOM
	DO stoc_sto
	DO stocmenu

	SET COLOR TO &statusatr
	@ 10,y MENU chse,4 TITLE  '  packing slip is  '
	READ MENU TO G
	SET COLOR TO &screenatr
	STORE '                  ' TO mname
	STORE '              ' TO mpacking

	SET COLOR TO &hiliteatr
	IF G = 1
		EXIT
	ENDIF
	IF G = 2
		STORE 'STOCK  ' TO mjobs
		STORE 'RESTOCK TRUCK' TO mname
		@ 3,12 SAY mname
		@ 2,12 SAY mjobs
	ENDIF
	IF G = 3
		STORE '       ' TO mjobs
		@ 2,12 GET mjobs PICTURE '!!!!!!!'
		READ
		STORE mjobs TO mno
		SELECT timwork
		SET ORDER TO no
		IF mjobs > ' '
			FIND &mjobs
		ELSE
			RETURN
		ENDIF
		IF .NOT. EOF()
			STORE name TO mname
			STORE technician TO mtech
			STORE  mtech TO mtechnician
		ENDIF
		@ 3,12 GET mname PICTURE '!!!!!!!!!!!!!!!!!!!!'
		READ
		IF EOF()
			@ 4,12 GET mtech PICTURE '!!!!!!!'
			READ
		ELSE
			@ 4,12 SAY mtech PICTURE '!!!!!!!'
		ENDIF
	ENDIF
	@ 2,61 GET mpacking PICTURE '!!!!!!!!!!!!!!'
	@ 3,61 GET msupp PICTURE '!!!!!!!!!!!'
	READ
	SET COLOR TO &screenatr

	*--- check to make sure a job# and jobname was entered ---*
	IF mjobs = '       '
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		SET COLOR TO &hiliteatr
		@ 12,5 SAY '*****  E R R O R  *****'
		@ 13,5 SAY 'You must enter a JOB NUMBER'
		?
		WAIT
		@ 12,0 TO 16,80 CLEAR
		LOOP
		SET COLOR TO &screenatr
	ENDIF
	IF mname = ' '
		IF SYS(2001,"bell") = 'on'
			? CHR(7)
		ENDIF
		SET COLOR TO &hiliteatr
		@ 12,5 SAY '*****  E R R O R  *****'
		@ 13,5 SAY 'You must enter a JOB NAME'
		?
		WAIT
		@ 12,0 TO 16,80 CLEAR
		LOOP
		SET COLOR TO &screenatr
	ENDIF
	*--- end error check ---*
	*--- set to top of form ---*
	STORE 'N' TO ok
	STORE 'A' TO mnum
	STORE 7 TO x

	*--- search for part number ---*
	DO WHILE mnum <> '               '
		STORE 1 TO tst
		SELECT TIMSTOCK
		SET ORDER TO 1
		IF x > 19
			STORE 7 TO x
			@ x+1,0 CLEAR
		ENDIF
		STORE x + 1 TO x
		@ x,0 CLEAR
		STORE '               'TO mnum
		@ 23,5 SAY 'PRESS ENTER TO SEARCH FOR NAED NO.'
		@ x,0 GET mnum PICTURE '!!!!!!!!!!!!!!!'
		READ
		SET ORDER TO 1

		*--- if no part number search for naedno ---*
		IF mnum = '               '
			STORE 2 TO tst
			SET ORDER TO 3
			STORE '         ' TO mnaed_no
			@ 23,5 SAY 'PRESS ENTER FOR NEW PACKING SLIP   '
			@ x,11 GET mnaed_no PICTURE '999999999'
			READ
			IF mnaed_no = '         '
				EXIT
			ENDIF
			STORE mnaed_no TO mnum
		ENDIF
		*--- end searches ---*

		STORE 'N' TO ok
		STORE ' ' TO a1

		*--- locate part that is to be ordered ---*
		STORE mnum TO ano
		SET EXACT OFF
		SET COLOR TO &hiliteatr
		SET ORDER TO 1
		STORE TRIM(ano) TO ano
		STORE LTRIM(ano) TO ano
		STORE LEN(ano) TO reclen
		STORE 0 TO PAGE

		FIND &ano

		*--- if the part number is not in inventory ---*
		IF EOF()
			SET COLOR TO &windowatr
			STORE 'NOTFOUND' TO aa
			DO WHILE ok <> 'Y'

				*--- not found- reset zero values ---*
				STORE '                                   'TO mdesc
				STORE 0.00 TO mprice
				STORE 0.00 TO mcost
				STORE 0 TO monhand
				STORE 0 TO morder
				@ 24,0 CLEAR
				@ 24,0 SAY '***** Record not found *****'
				SET COLOR TO &hiliteatr

				*--- not found-add values to add to inventory ---*
				@ x,6 CLEAR
				SAVE SCREEN TO scrn
				@ 3,0 CLEAR
				DO stocscrn
				DO stock_get
				READ
				STORE mcost * mmrkup TO mprice
				RESTORE SCREEN FROM scrn
				@ x,0 SAY mnum PICTURE '9999999999'
				@ x,11 SAY mnaed_no PICTURE '999999999'
				@ x,21 SAY mdesc PICTURE '!!!!!!!!!!!!!!!!!!!!'
				@ x,42 SAY mmfer PICTURE '!!!!!!!!'
				@ x,52 SAY mcost PICTURE '9999.99'
				@ x,60 SAY mprice PICTURE '9999.99'
				@ x,69 SAY monhand PICTURE '99999'
				@ x,75 SAY morder PICTURE '99999'
				SET COLOR TO &screenatr
				IF mdesc = '                    '
					STORE 'SKIP' TO a1
					STORE x-1 TO x
					EXIT
				ENDIF
				SET COLOR TO &hiliteatr
				@ x,52 SAY mcost PICTURE '9999.99'

				*--- not found check markup value ---*
				STORE mcost * mmrkup TO mprice
				@ x,60 SAY mprice PICTURE '9999.99'
				@ x,69 GET monhand PICTURE '99999'
				@ x,75 SAY morder PICTURE '99999'
				STORE 'N' TO ok
				@ 23,5 SAY 'IS THIS HOW YOU WANT IT TO READ  (Y/N)' GET ok PICTURE '!'
				READ
				SET COLOR TO &screenatr
				STORE 0 TO mqty
				STORE monhand TO mqty
			ENDDO

			*--- item found in inventory ---*
		ELSE
			*--- check for duplicate part numbers ---*
			STORE x TO xx
			DO timchoose
			STORE xx TO x
			STORE ano TO mnum
			SET EXACT ON
			*--- end duplicate check ---*

			STORE descrip TO mdesc
			STORE 'FOUND' TO aa
			STORE 'N' TO ok
			DO WHILE ok <> 'Y'

				*--- found-store found fields to temp value ---*
				STORE part_no TO mnum
				STORE mfer TO mmfer
				STORE cost TO mcost
				STORE price TO mprice
				STORE onhand TO monhand
				STORE ORDER TO morder
				STORE 0 TO mqty
				STORE naed_no TO mnaed_no
				SET COLOR TO &hiliteatr
				@ x,0 CLEAR

				*--- found-allow chance to change inventory item ---*
				@ x,0 SAY mnum
				@ x,11 GET mnaed_no PICTURE '999999999'
				@ x,21 GET mdesc PICTURE '!!!!!!!!!!!!!!!!!!!!'
				@ x,42 GET mmfer PICTURE '!!!!!!!!'
				@ x,52 GET mcost PICTURE '9999.99'
				@ x,60 SAY mprice PICTURE '9999.99'
				@ x,69 SAY monhand PICTURE '99999'
				@ x,75 GET morder PICTURE '99999'
				@ 23,5 SAY 'NUMBER OF ITEMS PURCHASED' GET mqty PICTURE '999999'
				READ
				STORE monhand + mqty TO monhand
				@ x,69 SAY monhand PICTURE '99999'
				@ 23,0 CLEAR
				STORE 'N' TO ok
				@ 23,5 SAY 'IS THIS HOW YOU WANT IT TO READ  (Y/N)' GET ok PICTURE '!'
				READ
				STORE UPPER(ok) TO ok
				SET COLOR TO &screenatr
			ENDDO

		ENDIF
		*--- item is defined whether found or not ---*
		SET COLOR TO &hiliteatr
		STORE 'N' TO ky
		STORE 0.00 TO markup
		STORE 'N' TO ky
		DO WHILE ky <> 'Y'
			*--- check markup value ---*
			IF a1 = 'SKIP'
				STORE ' ' TO a1
				EXIT
			ENDIF
			STORE 0 TO markup
			DO WHILE markup < 2.10
				IF mrkup < 2.10
					STORE mmrkup TO markup
				ELSE
					STORE mrkup TO markup
				ENDIF
				@ x,60 SAY mprice PICTURE '9999.99'
				@ 23,0 CLEAR
				@ 23,5 SAY 'MARK UP PERCENTAGE (0 for no change)' GET markup PICTURE '9999.99%'
				READ
				IF markup = mrkup
					EXIT
				ENDIF
			ENDDO
			IF markup <> 0
				STORE mcost*markup TO mprice
			ENDIF
			@ x,60 SAY STR(mprice,7,2)
			@ 23,0 CLEAR
			STORE 'N' TO ky
			@ 23,5 SAY 'IS THIS THE PRICE YOU WANT?     (Y/N)' GET ky PICTURE '!'
			READ
			STORE UPPER(ky) TO ky
		ENDDO


		*--- markup value defined ---*
		*--- if descripton field exists - save to inventory and history ---*
		IF mdesc > '                    '
			IF aa = 'NOTFOUND'
				= LOCK()
				APPEND BLANK
			ENDIF
			= LOCK()
			DO s_save
			UNLOCK

			*--- create purchase order
			SELECT company
			STORE last_po TO po
			STORE VAL(po)+1 TO vpo
			STORE ALLTRIM(STR(vpo)) TO morder
			@ 5,45 SAY 'Purchase Order ' GET morder
			READ
			IF morder > po
				REPLACE last_po WITH morder
			ENDIF
			STORE VAL(morder) TO morder

			*--- updating purchase order files

			SELECT tim_po
			GOTO BOTTOM
			IF morder > 0
				= LOCK()
				APPEND BLANK
				REPLACE DATE WITH DATE()
				REPLACE num WITH mjobs
				REPLACE name WITH mname
				REPLACE technician WITH mtech
				REPLACE supplier WITH msupp
				REPLACE po WITH morder
				UNLOCK
			ENDIF



			*--- updating history files ---*
			IF mqty <> 0
				USE timhistr share
				SET ORDER TO 1
				= LOCK()
				APPEND BLANK
				DO h_save
				UNLOCK
			ENDIF
		ENDIF

		*--- if purchased qty = 0 goto next item ---*
		IF mqty = 0
			LOOP
		ENDIF

		*--- if ordered for a specific job ---*
		IF mjobs <> 'STOCK  '
			IF mdesc > '                    '

				*--- specific job- save update hold file ---*
				@ 24,3 SAY 'UPDATING HOLDING FILES'
				USE timhold share
				SET ORDER to h_jobs
				LOCATE FOR  wo = TRIM(mjobs) .AND. part_no = TRIM(mnum)
				IF FOUND()
					@ 24,3 SAY 'REPLACE '
					REPLACE qty WITH qty+mqty
				ELSE
					@ 24,3 SAY 'ADD  TO '
					= LOCK()
					APPEND BLANK
					DO h_save
					UNLOCK
				ENDIF
				USE timstock share
				SET ORDER TO 1
				@ 24,3 SAY '                           '
			ENDIF
			*--- end specific job ---*
		ELSE

			*--- if reorder on old jobs ---*
			*--- SUBTRACT FROM REORDER LIST ---*
			USE timreord
			SET ORDER TO 1
			DO WHILE mqty > 0
				GOTO BOTTOM
				STORE RECNO() TO t
				IF part_no = mnum
					STORE RECNO() +1 TO t
				ENDIF
				LOCATE ALL FOR part_no = mnum

				*--- if record is found on reorder list ---*
				IF FOUND()
					IF qty = mqty
						STORE 0 TO qty
						STORE 0 TO mqty
						REPLACE part_no WITH '               '
						DELETE
					ENDIF
					IF qty > mqty
						REPLACE qty WITH qty - mqty
						STORE 0 TO mqty
					ENDIF
					IF qty < mqty
						STORE mqty - qty TO mqty
						REPLACE part_no WITH '               '
						STORE 0 TO qty
						DELETE
						IF EOF()
							IF mdesc > '                    '
								@ 24,3 SAY 'UPDATING TRUCKSTOCK FILES'
								USE timtrkst EXCLUSIVE
								SET ORDER TO t_jobs
								APPEND BLANK
								DO h_save
								USE
								USE timstock share
								SET ORDER TO s_partno,s_descri,s_naed,s_comodi
								@ 24,3 SAY ' '
							ENDIF
							STORE 0 TO mqty
						ENDIF
					ENDIF

					*--- if record does not exist on reorder list ---*
				ELSE
					IF SYS(2001,"bell") = 'on'
						? CHR(7)
					ENDIF
					@ 23,0 CLEAR
					@ 23,5 SAY 'This item was not found on the reorder list'
					IF qty < 0
						STORE (qty * -1) TO qty
					ENDIF
					@ 24,3 SAY 'ADDING TO TRUCKSTOCK FILES'
					USE timtrkst EXCLUSIVE
					SET ORDER TO t_jobs
					APPEND BLANK
					DO h_save
					USE
					*--- endof record does not exist on reorder ---*
				ENDIF
				*--- endof do reorder existing jobs ---*
			ENDDO
			*--- endof if reorder existing jobs ---*
		ENDIF
		*--- search for next record ---*
	ENDDO
	SET COLOR TO &screenatr
	LOOP
ENDDO
RETURN
*: EOF: TIMADDST.PRG
