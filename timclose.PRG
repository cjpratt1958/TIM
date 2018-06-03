*:*********************************************************************
*:
*:        Program: TIMCLOSE.PRG
*:
*:         System: TIM
*:         Author: CARL PRATT
*:      Copyright (c) 1993, CARL PRATT
*:  Last modified: 02/10/93     11:27
*:
*:      Called by: TIMDMENU.PRG
*:
*:          Calls: ADDTITLE         (procedure in TIMPROC.PRG)
*:               : WORK_STO         (procedure in TIMPROC.PRG)
*:               : WORKMENU         (procedure in TIMPROC.PRG)
*:               : WORKMSAY         (procedure in TIMPROC.PRG)
*:               : WORK_SAY         (procedure in TIMPROC.PRG)
*:               : DATASTOR         (procedure in TIMPROC.PRG)
*:               : TIMADPTS.PRG
*:
*:           Uses: TIMWORK.DBF		select 1
*:               : TIMPOOL.DBF		select 2
*:               : EMPLOYEE.DBF		select 3
*:               : TIMBILL.DBF		select 5
*:               : TIMPAYRO.DBF		select 4
*:               : TIMCANCL.DBF		select 6
*:               : TIMTRUCK.DBF		select 7
*:
*:        Indexes: W_NUM.NDX
*:               : W_PHONE.NDX
*:               : W_BPHONE.NDX
*:               : W_WPHONE.NDX
*:               : W_INCIDENT.NDX
*:               : W_TECH.NDX
*:               : W_REF.NDX
*:               : NAME.NDX
*:               : EMP_NO.NDX
*:               : B_NUM.NDX
*:
*:     Documented: 02/21/93 at 04:42               FoxDoc version 1.0
*:*********************************************************************
SET CENTURY ON
ON ESCAPE RETURN





*	SELECT 1 TO TIMWORK


IF USED("timwork")
	SELECT timwork
	SET ORDER TO 0
ELSE
	SELECT 1
	USE (LOCFILE("timwork.dbf","DBF","Where is timwork?"));
		AGAIN ALIAS timwork ;
		ORDER 0
ENDIF





*	SELECT 2 TO TIMPOOL

IF USED("timpool")
	SELECT timpool
	SET ORDER TO priority
ELSE
	SELECT 2
	USE (LOCFILE("timpool.dbf","DBF","Where is timpool?"));
		AGAIN ALIAS timpool ;
		ORDER priority
ENDIF


*	SELECT 3 TO Employee

IF USED("Employee")
	SELECT employee
	SET ORDER TO 2
ELSE
	SELECT 3
	USE (LOCFILE("Employee.dbf","DBF","Where is Employee?"));
		AGAIN ALIAS employee ;
		ORDER 2
ENDIF



*	SELECT 4 TO timpayroll

IF USED("timpayro")
	SELECT timpayro
	SET ORDER TO 2
ELSE
	SELECT 4
	USE (LOCFILE("timpayro.dbf","DBF","Where is timpayro?"));
		AGAIN ALIAS timpayro ;
		ORDER 2
ENDIF



*	SELECT 5 TO timbill

IF USED("timbill")
	SELECT timbill
	SET ORDER TO 0
ELSE
	SELECT 5
	USE (LOCFILE("timbill.dbf","DBF","Where is timbill?"));
		AGAIN ALIAS timbill ;
		ORDER 0
ENDIF





*	SELECT 6 TO TIMCANCL

IF USED("timcancl")
	SELECT timcancl
	SET ORDER TO 1
ELSE
	SELECT 6
	USE (LOCFILE("timcancl.dbf","DBF","Where is timcancl?"));
		AGAIN ALIAS timcancl ;
		ORDER 1
ENDIF






*	SELECT 7 TO TIMTRUCK

IF USED("TimTruck")
	SELECT timtruck
	SET ORDER TO 1
ELSE
	SELECT 7
	USE (LOCFILE("TimTruck.dbf","DBF","Where is TimTruck?"));
		AGAIN ALIAS timtruck ;
		ORDER 1
ENDIF

*	SELECT 8 TO Company

IF USED("Company")
	SELECT company
	SET ORDER TO 0
ELSE
	SELECT 8
	USE (LOCFILE("Company.dbf","DBF","Where is Company?"));
		AGAIN ALIAS company ;
		ORDER 0
ENDIF

*	SELECT 9 TO TimTrack

IF USED("TimTrack")
	SELECT timtrack
	SET ORDER TO 0
ELSE
	SELECT 9
	USE (LOCFILE("TimTrack.dbf","DBF","Where is TimTrack?"));
		AGAIN ALIAS timtrack ;
		ORDER 0
ENDIF


@ 0,0 CLEAR TO 1,0
STORE 'TIMCLOSE.PRG' TO prg_name
STORE '********  CLOSE A WORKORDER  ********' TO TITLE
STORE 2 TO rfprofit
DO addtitle
SET ECHO OFF
SET TALK OFF
SET INTENSITY ON
SET COLOR TO &screenatr
SET STATUS OFF
SET readborder ON
STORE 2 TO sr
STORE 'N' TO yn
STORE 0.00  TO mileage
STORE 1 TO PAGE
STORE '  ' TO cc
STORE '  ' TO mcc

*  ---DIMENSION HOW PAID POPUP WINDOW
DIMENSION pd(15)
STORE 'CHCK' TO pd(1)
STORE 'VISA' TO pd(2)
STORE 'M.C.' TO pd(3)
STORE 'A.EX' TO pd(4)
STORE 'DISC' TO pd(5)
STORE 'CASH' TO pd(6)
STORE 'BILL' TO pd(7)
STORE 'T.S.' TO pd(8)
STORE 'DOWN' TO pd(9)
STORE 'CONT' TO pd(10)
STORE 'COLL' TO pd(11)
STORE 'RWRK' TO pd(12)
STORE 'CNCL' TO pd(13)
STORE 'ESTI' TO pd(14)
STORE 'PCA ' TO pd(15)







*  ---DIMENSION IS THIS CORRECT POPUP WINDOW
DIMENSION wg(2)
STORE 'YES, THIS IS CORRECT' TO wg(1)
STORE 'NO,  NEED TO REWRITE' TO wg(2)

*  ---RESET ALL VARIABLES
DO work_sto

*  ---CREATE PICTURE ON SCREEN
DO workmenu

STORE 2 TO add_chse
*  --- ADD OPTION
DO WHILE .T.
	SET COLOR TO &statusatr
	@ 0,1 SAY 'RECORD # '
	@ 0,9 SAY RECNO()
	SET COLOR TO &promptatr
	@ 23,0 CLEAR
	@ 23,0 SAY '*****  Please Wait  *****'

	SET COLOR TO &screenatr

	*  --- [A] dd
	@ 23,0 CLEAR
	SET COLOR TO &promptatr
	@ 23,0 SAY '*****  Please Wait  *****'
	SET COLOR TO &hiliteatr
	STORE '       ' TO mnum
	DO WHILE .T.
		SET COLOR TO &hiliteatr
		DO work_sto
		STORE 0 TO tcost
		DO workmsay
		@ 20,58 SAY 'Mileage [        ]'
		@ 23,0 CLEAR
		SET COLOR TO &hiliteatr
		@ 23,0 SAY 'Enter Workorder Number,  Press [Enter] to exit,  [U] to undispatch  [?] for list  [R] to Reschedule'
		@ 11,60 TO 11,80 CLEAR
		@ 12,60 TO 12,80 CLEAR
		@ 16,60 TO 16,80 CLEAR
		@ 24,0 CLEAR
		@ 2,21 GET mnum PICTURE 'XXXXXXX'
		READ

		ON KEY LABEL f9 MODIFY MEMO notes
		ON KEY LABEL F8 DO F8SCREEN.SPR
		IF mnum = '?      '
			SET ORDER TO Technician
			BROWSE ALL FOR EMPTY(closedate) .AND. dispdate > ' ' fields no,name,incident,technician,jobdesc1 LAST
			STORE no to mnum
			@ 2,21 say mnum
		ENDIF

********************
**   Reschedule   **
********************

IF mnum = 'R     '
	DO Reschedule  && Procedure at the end of this script
ENDIF



		**************************
		**  UNDISPATCH A CALL   **
		**************************
		
		IF LEFT(UPPER(mnum),1) = 'U'

			@ 2,44 GET mincident PICTURE '!!!!!!!!'
			READ
			IF mincident = '        '
				ON KEY
				CLOSE DATABASES
				RETURN
			ENDIF

			SELECT 1
			SET ORDER TO incident
			SEEK mincident
			IF EOF()
				STORE ' ' TO ky
				SET COLOR TO &windowatr
				? CHR(7)
				WAIT WINDOW "This Incident does not currently exist"+CHR(13)+"press any key to continue"


				*				SAVE SCREEN TO WINDOW
				*				@ 4,10 TO 8,40 CLEAR
				*				@ 4,10 TO 8,40 DOUBLE
				*				SET COLOR TO &promptatr
				*				@ 5,11 SAY '   This incident does not '
				*				@ 6,11 SAY '      currently exist     '
				*				@ 7,11 SAY '  Press any key to continue' GET ky
				*				READ
				*				RESTORE SCREEN FROM WINDOW
				SELECT 1
				SET COLOR TO &hiliteatr
				LOOP
			ELSE
				IF LEN(notes) > 0
					SET COLOR TO &promptatr
					@ 0,57 SAY ' See Notes'
				ENDIF
				SET COLOR TO &screenatr
				DO work_say
				STORE 'Y' TO yn
				STORE 'S' TO companynum


				*				SAVE SCREEN TO mscreen
				@ 8,0 TO 13,80 CLEAR
				SET COLOR TO GR+
				@ 10,5 SAY 'Please enter company number ie.. [A]A Advanced, [P]lumbing or [S]ervice ' GET companynum
				@ 11,5 SAY 'is this correct?   (Y/N) ' GET yn PICTURE 'X'
				READ
				SET COLOR TO &promptatr
				IF UPPER(yn) <> 'Y'
					*					RESTORE SCREEN FROM mscreen
					LOOP
				ENDIF
				STORE DATE TO mdate
				STORE solddate TO msolddate
				STORE ref TO mref
				STORE priority TO mpriority
				STORE name TO mname
				STORE city TO mcity
				STORE state TO mstate
				STORE incident TO mincident
				STORE sector TO msector
				STORE technician TO mtechnician
				STORE RECNO() TO mrecord
				REPLACE technician WITH '       '
				REPLACE dispdate WITH '             '
				REPLACE notes WITH notes + CHR(13) +'Undispatched on '+DTOC(DATE()) +' by '+ALLTRIM(operator)+ ' '+ mcc + CHR(13)

				SELECT 2
				APPEND BLANK
				REPLACE DATE WITH mdate
				REPLACE solddate WITH msolddate
				REPLACE ref WITH mref
				REPLACE priority WITH mpriority
				REPLACE name WITH mname
				REPLACE city WITH mcity
				REPLACE state WITH mstate
				REPLACE incident WITH mincident
				REPLACE sector WITH msector
				REPLACE technician WITH mtechnician
				REPLACE rec_no WITH mrecord
				REPLACE trade WITH companynum
			ENDIF
			ON KEY
			CLOSE DATABASES
			RETURN
		ENDIF

		STORE LEN(TRIM(mnum)) TO N
		SET COLOR TO &hiliteatr
		IF mnum = '       '
			@ 23,0 CLEAR
			SET COLOR TO &promptatr
			@ 23,0 SAY '*****  Please Wait  *****'
			SET COLOR TO &hiliteatr
			ON KEY
			CLOSE DATABASES
			RETURN
		ENDIF
		SELECT  1
		SET ORDER TO no
		GOTO TOP
		SEEK mnum
		IF EOF()
			STORE ' ' TO ky
			SET COLOR TO &windowatr

			? CHR(7)
			WAIT WINDOW "This Workorder Number does not currently exist"+CHR(13)+"press any key to continue"



			*			SAVE SCREEN TO WINDOW
			*			@ 4,10 TO 8,40 CLEAR
			*			@ 4,10 TO 8,40 DOUBLE
			*			SET COLOR TO &promptatr
			*			@ 5,11 SAY '  This workorder does not '
			*			@ 6,11 SAY '      currently exist     '
			*			@ 7,11 SAY '  Press any key to continue' GET ky
			*			READ
			*			RESTORE SCREEN FROM WINDOW
			SELECT 1
			SET COLOR TO &hiliteatr
			LOOP
		ELSE
			DO work_say
			IF closedate <> '                '
				STORE ' ' TO ky
				SET COLOR TO &windowatr

				? CHR(7)
				WAIT WINDOW "This Workorder has already been closed "+CHR(13)+"press any key to continue"


				*				SAVE SCREEN TO WINDOW
				*				SET COLOR TO &promptatr
				*				@ 4,46 TO 6,70 CLEAR
				*				@ 4,46 TO 6,70 DOUBLE
				*				@ 5,49 SAY closedate
				*				@ 4,10 TO 8,40 CLEAR
				*				@ 4,10 TO 8,40 DOUBLE
				*				@ 5,11 SAY '  This workorder has      '
				*				@ 6,11 SAY '  already been closed     '
				*				@ 7,11 SAY '  Press any key to continue' GET ky
				*				READ
				*				RESTORE SCREEN FROM WINDOW
				SELECT 1
				SET COLOR TO &hiliteatr
				LOOP
			ENDIF
			STORE RECNO() TO rn
			STORE technician TO mtechnician
			SELECT 3
			SET ORDER TO 1
			SET DELETED OFF
			SEEK mtechnician
			IF DELETED()
				SET COLOR TO R
				@ 1,50 SAY " ****  TERMINATED  **** "
				SET COLOR TO &windowatr
				STORE name TO mtechname
				@ 1,21 SAY mtechnician
				@ 1,35 SAY mtechname
				SELECT 1
				GO rn
				SET COLOR TO &promptatr
			ENDIF
			SELECT 1
			GO rn
			DO datastor
			STORE 1 TO numberloops
			DO WHILE yn = 'N'
				****  installed to be able to enter new job after close ****
				****   by cp 05/09/2003

				STORE mwarea TO newincwarea
				STORE mwphone TO newincwphone
				STORE mbarea TO newincbarea
				STORE mbphone TO newincbphone
				STORE marea TO newincarea
				STORE mphone TO newincphone
				STORE ref TO newincref

				***    end installed to be able to enter new job after close ****

				@ 23,0 TO 23,80 CLEAR
				@ 23,0 SAY 'Enter Number of Half hour periods Charged for'
				@ 11,36 GET mhours PICTURE '999.99'
				READ
				DO WHILE .T.
					@ 23,0 TO 23,80 CLEAR
					@ 23,0 SAY 'Enter Labor Rate'
					@ 11,50 GET mrate PICTURE '999.99'
					READ
					STORE mhours*mrate TO l1
					@ 11,69 SAY l1 PICTURE '9999.99'
					STORE l1 TO mamount
					@ 16,68 SAY mamount PICTURE '99999.99'
					@ 19,46 SAY mamount PICTURE '999999.99'
					DO WHILE .T.
						@ 23,0 TO 23,80 CLEAR
						@ 23,0 SAY 'Enter Number of Half Hour Periods Charged for'
						@ 12,36 GET mhours1 PICTURE '999.99'
						READ
						DO WHILE .T.
							@ 23,0 TO 23,80 CLEAR
							@ 23,0 SAY 'Enter Labor Rate'
							@ 12,50 GET mrate1 PICTURE '999.99'
							READ
							STORE mhours1*mrate1 TO l2
							@ 12,69 SAY l2 PICTURE '9999.99'
							STORE l1 + l2 TO mamount
							@ 16,68 SAY mamount PICTURE '99999.99'
							@ 19,46 SAY mamount PICTURE '999999.99'
							DO WHILE .T.
								@ 23,0 TO 23,80 CLEAR
								@ 23,0 SAY 'Enter Discount given'
								@ 13,46 GET mdiscount PICTURE '999999.99'
								READ
								STORE l1+l2-mdiscount TO mamount
								@ 16,68 SAY mamount PICTURE '99999.99'
								@ 19,46 SAY mamount PICTURE '999999.99'
								DO WHILE .T.
									@ 23,0 TO 23,80 CLEAR
									@ 23,0 SAY 'Enter Amount Charged for Materials'
									@ 14,46 GET mmaterial PICTURE '999999.99'
									READ
									IF numberloops = 1
										STORE (mtax/100)*mmaterial TO mtax
									ENDIF
									STORE l1+l2-mdiscount+mmaterial TO mamount
									@ 16,68 SAY mamount PICTURE '99999.99'
									@ 19,46 SAY mamount PICTURE '999999.99'
									DO WHILE .T.
										@ 23,0 TO 23,80 CLEAR
										@ 23,0 SAY 'Enter Amount Charged for Service Charge'
										@ 15,46 GET mservice PICTURE '999999.99'
										READ
										STORE l1+l2-mdiscount+mmaterial+mservice TO mamount
										@ 16,68 SAY mamount PICTURE '99999.99'
										@ 19,46 SAY mamount PICTURE '999999.99'
										DO WHILE .T.
											@ 23,0 TO 23,80 CLEAR
											@ 23,0 SAY 'Enter Amount Charged for Special Equipment used'
											@ 16,46 GET mspecial PICTURE '999999.99'
											READ
											STORE l1+l2-mdiscount+mmaterial+mservice+mspecial TO mamount
											@ 16,68 SAY mamount PICTURE '99999.99'
											@ 19,46 SAY mamount PICTURE '999999.99'
											DO WHILE .T.
												@ 23,0 TO 23,80 CLEAR
												@ 23,0 SAY 'Enter Tax Charged for Material'
												@ 17,49 GET mtax PICTURE '999.99'
												READ
												STORE l1+l2-mdiscount+mmaterial+mservice+mspecial+mtax TO mamount
												@ 19,46 SAY mamount PICTURE '999999.99'
												DO WHILE .T.
													@ 23,0 TO 23,80 CLEAR
													@ 23,0 SAY 'Enter Amount charged for Extended Warantee / Preferred Customer'
													@ 18,49 GET mpreferred PICTURE '999.99'
													READ
													IF mpreferred > 0
														IF mprefdisc = 0
															STORE 10 TO mprefdisc
														ENDIF
													ENDIF
													STORE l1+l2-mdiscount+mmaterial+mservice+mspecial+mtax+mpreferred TO mamount
													@ 19,46 SAY mamount PICTURE '999999.99'
													STORE mpreferred TO G
													DO WHILE .T.
														IF G <> 0
															@ 23,0 TO 23,80 CLEAR
															@ 23,0 SAY 'Enter Discount Percentage given with Preferred Customer Card'
															@ 18,43 GET mprefdisc PICTURE '99'
															READ
														ENDIF
														STORE 1 TO G
														CLEAR TYPEAHEAD
														DO WHILE .T.
															@ 23,0 TO 23,80 CLEAR
															@ 23,0 SAY 'Enter Total Amount of Workorder'
															@ 19,46 GET mamount PICTURE '999999.99'
															READ
															STORE 0 TO ky
															SET COLOR TO &statusatr
															@ 23,0 TO 23,80 CLEAR
															@ 23,0 SAY 'Enter Method of Payment'
															@ 6,60 MENU pd,15 TITLE 'how paid'
															READ MENU TO ky
															SET COLOR TO &hiliteatr

															STORE pd(ky) TO mpaid
															SET INTENSITY OFF
															DO WHILE .T.
																*	@ 23,0 TO 23,80 CLEAR
																@ 20,40 SAY '         '	
																*	@ 23,0 SAY 'Re-enter Method of Payment if not on the list'
																@ 20,40 SAY mpaid 
																*	READ
																IF ALLTRIM(mpaid) = 'CNCL'
																	STORE 0.00 TO mamount
																ENDIF
																DO WHILE .T.
																	STORE '          ' TO m.paidno
																	****  Input Check Number
																	IF ALLTRIM(mpaid) = 'CHCK'
																		SET COLOR TO &promptatr
																		CLEAR TYPEAHEAD
																		DEFINE WINDOW mprimenu FROM 13,20 TO 17,75 DOUBLE
																		ACTIVATE WINDOW mprimenu
																		@ 1,2 SAY 'Please Enter Check Number: ' GET m.paidno
																		READ
																		STORE 'N' TO m.exit
																		IF m.paidno > ' '
																			STORE 'Y' TO m.exit
																		ENDIF
																		DO WHILE m.exit = 'N'
																			? CHR(7)
																			@ 1,2 SAY 'A Check Number MUST be Entered: ' GET m.paidno
																			READ
																			IF m.paidno > ' '
																				STORE 'Y' TO m.exit
																			ENDIF
																		ENDDO
																		DEACTIVATE WINDOW mprimenu
																		RELEASE WINDOW mprimenu
																	ENDIF



																	****  end check Number
*  Need to update timwork, timpayro, and return to menu																	
																	
*																	****  Check for Down Payment
*																	IF ALLTRIM(mpaid) = 'DOWN'
*																		STORE DATE() TO m.dnpmntdate
*																		STORE 0.00 TO m.dnpmntamt
*																		STORE 'Enter how paid & payment number here' to m.dnpmntdesc
*																		DEFINE WINDOW mDnPmnt FROM 13,20 TO 19,80 DOUBLE
*																		ACTIVATE WINDOW mDnPmnt
*																		@ 1,2 SAY 'Please Enter Date on Check ' GET m.dnpmntdate picture '@k'
*																		@ 2,2 SAY 'Please Enter Payment Amount ' GET m.dnpmntamnt
*																		@ 3,2 SAY 'Please Enter How Paid and description ' GET m.dnpmntdesc
*																		read
*																		STORE 'Y' TO m.exit
*																		SELECT 1
*																		REPLACE dnpmntamnt with m.dnpmntamnt
*																		REPLACE dnpmntdate with m.dnpmntdate
*																		REPLACE dnpmntdesc with m.dnpmntdesc
*																		SELECT 4
*																		REPLACE DATE WITH m.dnpmntdate
*																		REPLACE  
*														
*																	ENDIF
*																	**** End Check for Down Payment






																	****  check for input of credit card approval code
																	STORE '               ' TO m.appcode
																	IF ALLTRIM(mpaid) = 'M.C.' .OR. ALLTRIM(mpaid) = 'VISA' .OR. ALLTRIM(mpaid) = 'DISC' .OR. ALLTRIM(mpaid) = 'A.EX'
																		SET COLOR TO &promptatr
																		CLEAR TYPEAHEAD
																		DEFINE WINDOW mprimenu FROM 13,20 TO 17,80 DOUBLE
																		ACTIVATE WINDOW mprimenu
																		@ 1,2 SAY 'Please Enter a Credit Card Approval Code: ' GET m.appcode
																		READ
																		STORE 'N' TO m.exit
																		IF m.appcode > ' '
																			STORE 'Y' TO m.exit
																		ENDIF
																		DO WHILE m.exit = 'N'
																			? CHR(7)
																			@ 1,2 SAY 'A Credit Card Approval Code MUST be Entered: ' GET m.appcode
																			READ
																			IF m.appcode > ' '
																				STORE 'Y' TO m.exit
																			ENDIF
																		ENDDO
																		DEACTIVATE WINDOW mprimenu
																		RELEASE WINDOW mprimenu
																	ENDIF
																	****  end check











																	****  Input Continued Workorder number
																	STORE '     ' TO m.link
																	IF ALLTRIM(mpaid) = 'CONT'
																		SET COLOR TO &promptatr
																		CLEAR TYPEAHEAD
																		DEFINE WINDOW mprimenu FROM 13,20 TO 17,75 DOUBLE
																		ACTIVATE WINDOW mprimenu
																		@ 1,2 SAY 'Please Enter Continued WO Number: ' GET m.link
																		READ
																		STORE 'N' TO m.exit
																		IF m.link > ' '
																			STORE 'Y' TO m.exit
																		ENDIF
																		DO WHILE m.exit = 'N'
																			? CHR(7)
																			@ 1,2 SAY 'A Work Order Number MUST be Entered: ' GET m.link
																			READ
																			IF m.link > ' '
																				STORE 'Y' TO m.exit
																			ENDIF
																		ENDDO
																		DEACTIVATE WINDOW mprimenu
																		RELEASE WINDOW mprimenu
																	ENDIF
																	****  end Continue Workorder Number








																	@ 23,0 TO 23,80 CLEAR






																	@ 20,67 GET mileage PICTURE '999999.9'
																	READ
																	DO WHILE .T.
																		@ 23,0 TO 23,80 CLEAR
																		@ 23,0 SAY 'Enter if Customer Needs More Work Done in the Future'
																		@ 21,10 GET mwork PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
																		READ
																		DO WHILE .T.
																			@ 23,0 TO 23,80 CLEAR
																			@ 23,0 SAY 'Enter Comments about Job or Customer'
																			@ 22,10 GET mcomments
																			READ
																			STORE 1 TO sr
																			IF sr = 1
																				EXIT
																			ENDIF
																		ENDDO
																		SELECT 1
																		*																		STORE mpaid + ' ' + mckn TO mpaid
																		SET COLOR TO &hiliteatr
																		STORE 4 TO ky
																		@ 23,0 CLEAR
																		STORE 1 TO sr
																		IF sr = 1
																			EXIT
																		ENDIF
																	ENDDO
																	IF sr = 1
																		EXIT
																	ENDIF
																ENDDO
																IF sr = 1
																	EXIT
																ENDIF
															ENDDO
															IF sr = 1
																EXIT
															ENDIF
														ENDDO
														IF sr = 1
															EXIT
														ENDIF
													ENDDO
													IF sr = 1
														EXIT
													ENDIF
												ENDDO
												IF sr = 1
													EXIT
												ENDIF
											ENDDO
											IF sr = 1
												EXIT
											ENDIF
										ENDDO
										IF sr = 1
											EXIT
										ENDIF
									ENDDO
									IF sr = 1
										EXIT
									ENDIF
								ENDDO
								IF sr = 1
									EXIT
								ENDIF
							ENDDO
							IF sr = 1
								EXIT
							ENDIF
						ENDDO
						IF sr = 1
							EXIT
						ENDIF
					ENDDO
					IF sr = 1
						EXIT
					ENDIF
				ENDDO
				*        if sr = 1
				*          exit
				*        endif
				CLEAR TYPEAHEAD
				STORE 'N' TO newincident
				@ 23,0 CLEAR
				@ 23,0 SAY 'Is This Correct ( [Y]es  [N]o [E]xit )' GET yn PICTURE '!'
				READ
				STORE technician TO stechnician
				STORE wage TO msplit_pcnt
				IF yn = 'E'
					CLOSE DATABASES
					USE
					RETURN
				ENDIF
				IF yn = "Y"
					EXIT
				ENDIF
			ENDDO
		ENDIF

**************  TEMPORARILY DELETED BECAUSE timadpts.prg IS MISSING  *****************


*  Add parts if sold

*		IF mmaterial <> 0
*			STORE 1 TO sr
*			STORE RECNO() TO recnumber
*			DO timadpts
*			STORE recnumber TO rn
*			CLEAR
*			DO addtitle
*			SELECT 1
*			GO rn
*			@ 0,9 SAY RECNO()
*			DO workmenu
*			DO workmsay
*		ENDIF
*  End add parts

		IF sr = 1
			EXIT
		ENDIF
	ENDDO

	IF mmaterial > 0
		IF tcost = 0
			STORE mmaterial/2 TO m.cost
			STORE 'Y' TO yn
			@ 20,0 CLEAR
			@ 1,20 TO 23,80 CLEAR
			SET COLOR TO &windowatr
			@ 20,1 TO 22,80 DOUBLE
			CLEAR TYPEAHEAD
			@ 21,5 SAY 'Would you like to enter '+STR(m.cost,10,2)+' to Material W/S cost? [N]o [Y]es ' GET yn PICTURE '!'
			READ
			IF yn = 'Y'
				STORE m.cost TO tcost
			ENDIF
		ENDIF
	ENDIF

	*** LINE 545 EDITED 04/16/2003  ADDED THE FOLLOWING
	**** This program looks for estimates from the same customer and gives the chance to attach to a workorder *****

	STORE RECNO() TO recordno

	COUNT ALL FOR name = mname .AND. priority = '02' TO noestimates

	IF noestimates > 0
		@ 20,0 CLEAR
		@ 1,20 TO 23,80 CLEAR
		SET COLOR TO &windowatr
		@ 20,1 TO 22,80 DOUBLE
		STORE 'N' TO yesno
		@ 21,5 SAY 'Would you like to link this call to an estimate?   Y / N' GET yesno PICTURE '!'
		READ
		IF yesno = "Y"
			IF noestimates = 1
				LOCATE FOR name = mname .AND. priority = '02'
				STORE no TO m.no
				GOTO recordno
				REPLACE LINK WITH m.no
				REPLACE priority WITH '02'
				STORE '02' TO mpriority
			ELSE
				SET FIELDS TO name,no,amount,DATE,comments,jobdesc1,priority
				BROWSE ALL FOR name = mname .AND. priority = '02'
				STORE no TO m.no
				SET FIELDS TO ALL
				GOTO recordno
				REPLACE priority WITH '02'
				REPLACE LINK WITH m.no
				STORE '02' TO mpriority
			ENDIF
		ENDIF
	ENDIF



	**** end attach workorder to estimate ****

	*** END EDITED 04/16/2003

	STORE 'N' TO rf
	IF tcost > mamount*.25
		STORE 'Y' TO rf
	ENDIF


	CLEAR TYPEAHEAD
	STORE 'N' TO yn
	STORE 'N' TO auxwo
	STORE '      ' TO m.aux_workno

	**************************************************
	
	** need to check this customer and if he / she already has a panel sticker **
	** need to enter a default panel sticker number                            **
	* STORE '????' TO m.panel_stic
	**************************************************

	@ 21,2 TO 22,79 CLEAR
	@ 21,5 SAY 'Has a Diagnostic Report been filled out?   Y / N ' GET auxwo PICTURE '!'
	@ 22,5 SAY 'Has a Panel Sticker been installed?        Y / N ' GET YN PICTURE '!'
	READ


	****  Input Diagnostic Report Number
	IF auxwo = 'Y'
		SET COLOR TO &promptatr
		CLEAR TYPEAHEAD
		STORE '      ' TO m.aux_workno
		DEFINE WINDOW mprimenu FROM 13,20 TO 17,75 DOUBLE
		ACTIVATE WINDOW mprimenu
		@ 1,2 SAY 'Please Enter Diagnostic Report Number: ' GET m.aux_workno
		READ
		STORE 'N' TO m.exit
		IF m.aux_workno > ' '
			STORE 'Y' TO m.exit
		ENDIF
		DO WHILE m.exit = 'N'
			? CHR(7)
			@ 1,2 SAY 'A Diagnostic Report Number MUST be Entered: ' GET m.aux_workno
			READ
			IF m.aux_workno > ' '
				STORE 'Y' TO m.exit
			ENDIF
		ENDDO
		DEACTIVATE WINDOW mprimenu
		RELEASE WINDOW mprimenu
	ENDIF
	****  end Diagnostic Report Number

	****  Input Panel Sticker Number
	IF YN = 'Y'
		SET COLOR TO &promptatr
		CLEAR TYPEAHEAD
		STORE '      ' TO m.panel_stic
		DEFINE WINDOW mprimenu FROM 13,20 TO 17,75 DOUBLE
		ACTIVATE WINDOW mprimenu
		@ 1,2 SAY 'Please Enter Panel Sticker Number: ' GET m.panel_stic
		READ
		STORE 'N' TO m.exit
		IF m.panel_stic > ' '
			STORE 'Y' TO m.exit
		ENDIF
		DO WHILE m.exit = 'N'
			? CHR(7)
			@ 1,2 SAY 'A Panel Sticker Number MUST be Entered: ' GET m.panel_stic
			READ
			IF m.panel_stic > ' '
				STORE 'Y' TO m.exit
			ENDIF
		ENDDO
		DEACTIVATE WINDOW mprimenu
		RELEASE WINDOW mprimenu
	ENDIF
	****  end Panel Sticker Number

	***08/25/04   print letters if a job is closed as an estimate..

	*	IF mpaid = 'ESTI' .and. amount = 0.00
	*		STORE 0.00 TO estamt
	*		STORE 'Y' TO estyn
	*		STORE 'Y' TO yn
	*		@ 23,0 CLEAR
	*		@ 23,0 SAY	'Please enter estimate amount ' GET estamt
	*		READ
	*		@ 23,0 CLEAR
	*		@ 23,0 SAY 'Has this estimate been accepted?  Y/N ' GET estyn PICTURE '!'
	*		READ
	*		IF UPPER(estyn) <> 'Y'
	*			@ 23,0 CLEAR
	*			@ 23,0 SAY 'Would you like to print an estimate follow up letter?' GET yn PICTURE '!'
	*			READ
	*			if upper(yn) = 'Y'
	*				store technician to m.techname
	*				store '
	*				@ 23,0 clear
	*				@ 23,0 say "Please enter Technician's Name " get m.techname picture 'XXXXXXXXXXXXXXXXXXXXXXXXXX'
	*				@ 24,0 say "Please enter Technician's title " get m.techtitle picture 'XXXXXXXXXXXXXXXXXXXXXXXXXX'
	*				REPORT FORM TIMESTLT TO PRINT
	*			ENDIF yn
	*		ELSE
	**** we need to also generate a letter of acceptance if this estimate has been accepted
	*		ENDIF estyn
	**** save amount to memo
	*	REPLACE notes WITH notes +CHR(13)+'Estimate letter printed on '+DTOC(DATE())+' by '+operator+' for '+estamt+' '+techname+' '+techtitle+CHR(13)

	*	ENDIF mpaid
	**** update closed by information





	IF mpaid = 'BILL' .OR. mpaid = 'COLL'
		IF mbname = ' '
			STORE PAGE+1 TO PAGE
			STORE mname TO mbname
			STORE mstreet TO mbstreet
			STORE mcity TO mbcity
			STORE mstate TO mbstate
			STORE mzip TO mbzip
			@ 5,14 GET mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
			@ 6,14 GET mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
			@ 7,14 GET mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
			@ 8,14 GET mbstate PICTURE '!!'
			@ 8,34 GET mbzip PICTURE '99999'
			READ
		ENDIF
		STORE RECNO() TO mrec_no
		REPLACE notes WITH notes + CHR(13) +'Bill Printed on '+DTOC(DATE()) +' by '+ operator+CHR(13)
		SELECT 5
		SET ORDER TO 1
		GOTO BOTTOM
		APPEND BLANK
		REPLACE DATE WITH DATE()
		STORE 'Invoice number '+mnum TO mdescrip
		REPLACE descrip WITH mdescrip

		REPLACE no WITH mnum
		REPLACE amount WITH mamount
		REPLACE balance WITH mamount
		REPLACE name WITH mname
		REPLACE street WITH mstreet
		REPLACE city WITH mcity
		REPLACE state WITH mstate
		REPLACE zip WITH mzip
		REPLACE bname WITH mbname
		REPLACE bstreet WITH mbstreet
		REPLACE bcity WITH mbcity
		REPLACE bstate WITH mbstate
		REPLACE bzip WITH mbzip
		REPLACE area WITH marea
		REPLACE phone WITH mphone
		REPLACE warea WITH mwarea
		REPLACE wphone WITH mwphone
		REPLACE barea WITH mbarea
		REPLACE bphone WITH mbphone
		REPLACE incident WITH mincident
		REPLACE technician WITH mtechnician
		REPLACE contact WITH mcontact
		REPLACE rec_no WITH mrec_no
		REPLACE billcycle WITH .T.
		REPLACE lastbilled WITH DATE()
		REPLACE terms WITH 15
		STORE 'Y' TO choice
		@ 24,0 SAY 'Insert blank letterhead in printer and press [Enter] or [N] to not print'
		@ 24,78 GET choice
		READ
		@ 24,0 CLEAR
		*****  print invoice  01/07/99

		IF UPPER(choice) <> 'N'


				IF left(mnum,1)='5'
					REPORT FORM timbil2i FOR no = mnum TO PRINT
				ELSE
					REPORT FORM timbilli FOR no = mnum TO PRINT
				ENDIF


*			REPORT FORM timbilli FOR no = mnum TO PRINT
			*			Store 'Y' TO PrintEnvelope
			*			@ 24,0 SAY 'Insert blank envelope in printer and press 'Y' or 'N' to skip'
			*			if PrintEnvelope = 'Y'
			*				REPORT FORM TIMENVEL FOR no = mnum to print
			*			endif
		ELSE
			SUM ALL amount TO mbalance FOR no = mnum
		ENDIF
		USE
	ELSE
		REPLACE pddate WITH DATE()
	ENDIF

	* ---  calculate wages
	** --- get technician employee information
	SELECT company  && Company
	STORE co_rate1 TO mco_rate1
	STORE co_rate2 TO mco_rate2
	STORE co_rate3 TO mco_rate3
	STORE co_rate4 TO mco_rate4
	STORE co_rate5 TO mco_rate5
	STORE co_rate6 TO mco_rate6

	SELECT employee 	&& Employee
	SEEK mtechnician
	STORE mtechnician TO stechnician
	STORE 0.00 TO gross_wage
	STORE 0.00 TO wagetotal
	STORE salary TO msalary
	IF msalary > 0
		STORE 4 TO rfprofit
	ENDIF
	STORE commission TO mcommission
	STORE mat_percnt TO mmat_percnt
	STORE hour_rate1/2 TO mhr_rate1
	STORE hour_rate2/2 TO mhr_rate2
	STORE hour_rate3/2 TO mhr_rate3
	STORE hour_rate4/2 TO mhr_rate4
	STORE hour_rate5/2 TO mhr_rate5
	STORE hour_rate6/2 TO mhr_rate6
	STORE supervisor TO m.supervisor
	STORE override TO m.override
	STORE gas_p_job TO mgas_p_job
	IF mcommission > 0
		STORE gross_wage+((mcommission/100) * (mamount-mtax)) TO gross_wage
	ENDIF
	IF mrate<mco_rate1
		STORE gross_wage+((mrate*mhours)*.25) TO gross_wage
	ENDIF
	IF mrate=mco_rate1
		STORE gross_wage+(mhr_rate1*mhours) TO gross_wage
	ENDIF
	IF mrate=mco_rate2
		STORE gross_wage+(mhr_rate2*mhours) TO gross_wage
	ENDIF
	IF mrate=mco_rate3
		STORE gross_wage+(mhr_rate3*mhours) TO gross_wage
	ENDIF
	IF mrate=mco_rate4
		STORE gross_wage+(mhr_rate4*mhours) TO gross_wage
	ENDIF
	IF mrate=mco_rate5
		STORE gross_wage+(mhr_rate5*mhours) TO gross_wage
	ENDIF
	IF mrate=mco_rate6
		STORE gross_wage+(mhr_rate6*mhours) TO gross_wage
	ENDIF
	IF mrate<mco_rate1
		STORE gross_wage+((mrate*mhours)*.3) TO gross_wage
	ENDIF

	IF mrate1=mco_rate1
		STORE gross_wage+(mhr_rate1*mhours1) TO gross_wage
	ENDIF
	IF mrate1=mco_rate2
		STORE gross_wage+(mhr_rate2*mhours1) TO gross_wage
	ENDIF
	IF mrate1=mco_rate3
		STORE gross_wage+(mhr_rate3*mhours1) TO gross_wage
	ENDIF
	IF mrate1=mco_rate4
		STORE gross_wage+(mhr_rate4*mhours1) TO gross_wage
	ENDIF
	IF mrate1=mco_rate5
		STORE gross_wage+(mhr_rate5*mhours1) TO gross_wage
	ENDIF
	IF mrate1=mco_rate6
		STORE gross_wage+(mhr_rate6*mhours1) TO gross_wage
	ENDIF


	*	IF rf = 'Y'
	*		STORE (((mamount-mtax)-((mamount-mtax)*.33))-tcost)/rfprofit TO gross_wage
	*		STORE 2 TO rfprofit
	*	ENDIF


      SELECT 4
      *** LINE 858 EDITED 04/16/2003  REPLACED 'APPEND BLANK' WITH THE FOLLOWING
      
      LOCATE FOR wo = mnum .AND. descrip = 'COMMISSIONS'
      IF EOF()
         APPEND BLANK
      ENDIF
      *** END EDITED 04/16/2003
      REPLACE emp_no WITH stechnician
      REPLACE DATE WITH DATE()
      REPLACE wo WITH mnum
      REPLACE jobname WITH mname
      REPLACE gross_pay WITH gross_wage
      STORE wagetotal + gross_pay TO wagetotal
      REPLACE red_flag WITH rf
      REPLACE mat_cost WITH tcost
      REPLACE tax WITH mtax
      REPLACE descrip WITH 'COMMISSIONS'
      IF mpriority = '02'
         REPLACE descrip WITH 'ESTIMATE '+descrip
      ENDIF
      REPLACE amount WITH mamount - mtax
   


	***  IF ESTIMATE ADD 5% *******

	IF mpriority = '02' .AND. mamount > 0.00

		SELECT 4
		*** LINE 886 EDITED 04/16/2003  REPLACED 'APPEND BLANK' WITH THE FOLLOWING

		LOCATE FOR wo = mnum .AND. descrip = 'ESTIMATE BONUS'
		IF EOF()
			APPEND BLANK
		ENDIF
		*** END EDITED 04/16/2003
		REPLACE emp_no WITH stechnician
		REPLACE DATE WITH DATE()
		REPLACE wo WITH mnum
		REPLACE jobname WITH mname
		REPLACE gross_pay WITH (mamount-mtax) * .05
		STORE wagetotal + gross_pay TO wagetotal
		REPLACE red_flag WITH rf
		REPLACE mat_cost WITH tcost
		REPLACE tax WITH mtax
		REPLACE descrip WITH 'ESTIMATE BONUS'

		** DELETED THIS ADD TO CORRECT TOTAL AMOUNT AND AVERAGE ON REPORT **
		REPLACE amount WITH 0.00
		** END DELETED

	ENDIF
	*** END EDITED 04/16/2003




	*** 04/16/20043  ADDED THE FOLLOWING

	***  IF AUX WO = 'Y' AND PANEL STICKER = 'Y' ADD 5% *******
	
	

	SELECT 4

	LOCATE FOR wo = mnum .AND. descrip = 'PANEL EVALUATION BONUS'
	IF LEFT(mref,1) = 'R' .OR. auxwo = 'Y' .AND. YN = 'Y'

		IF EOF()
			APPEND BLANK
		ENDIF
		*** END EDITED 11/13/2003
		REPLACE emp_no WITH stechnician
		REPLACE DATE WITH DATE()
		REPLACE wo WITH mnum
		REPLACE jobname WITH mname

*  MODIFIED ON 06/01/09 TO REMOVE ANY ADDITIONAL COMMISSIONS PAID ON PANEL EVAL
		REPLACE gross_pay WITH 0.00
*		REPLACE gross_pay WITH (mamount-mtax) * .05
*	END MODIFY 

		STORE wagetotal + gross_pay TO wagetotal
		REPLACE red_flag WITH rf
		REPLACE mat_cost WITH tcost
		REPLACE tax WITH mtax
		REPLACE descrip WITH 'PANEL EVALUATION BONUS'

		** DELETED THIS ADD TO CORRECT TOTAL AMOUNT AND AVERAGE ON REPORT **
		REPLACE amount WITH 0.00
		** END DELETED

	ENDIF
	*** END EDITED 11/13/2003


	*** 03/26/2004  ADDED THE FOLLOWING

	***  IF M.OVERRIDE > 0 ADD % *******

	SELECT 4

	LOCATE FOR wo = mnum .AND. descrip = 'OVERRIDES'
	IF m.override > 0

		IF EOF()
			APPEND BLANK
		ENDIF
		*** END EDITED 11/13/2003
		REPLACE emp_no WITH m.supervisor
		REPLACE DATE WITH DATE()
		REPLACE wo WITH mnum
		REPLACE jobname WITH mname
		REPLACE other_pay WITH (mamount-mtax) * (m.override/100)
		STORE wagetotal + other_pay TO wagetotal
		REPLACE red_flag WITH rf
		REPLACE mat_cost WITH tcost
		REPLACE tax WITH mtax
		REPLACE descrip WITH 'OVERRIDES FROM '+ stechnician


		** DELETED THIS ADD TO CORRECT TOTAL AMOUNT AND AVERAGE ON REPORT **
		REPLACE amount WITH 0.00
		** END DELETED

	ENDIF
	*** END EDITED 03/26/2004



	IF rf = 'N'

		IF mgas_p_job > 0
			@ 23,0 CLEAR
			STORE 'Y' TO ga
			@ 23,5 SAY 'Pay gas allowance on this job?     Y / N ' GET ga PICTURE '!'
			READ
			IF ga = 'Y'
				SELECT 4
				APPEND BLANK
				REPLACE emp_no WITH stechnician
				REPLACE DATE WITH DATE()
				REPLACE wo WITH mnum
				REPLACE jobname WITH mname
				REPLACE descrip WITH 'GAS ALLOWANCE'
				REPLACE other_pay WITH mgas_p_job
				STORE wagetotal + other_pay TO wagetotal
			ENDIF
		ENDIF

		IF mmat_percnt > 0
			SELECT 4
			APPEND BLANK
			REPLACE other_pay WITH (mmaterial*(mmat_percnt/100))
			STORE wagetotal + other_pay TO wagetotal
			REPLACE emp_no WITH stechnician
			REPLACE DATE WITH DATE()
			REPLACE wo WITH mnum
			REPLACE jobname WITH mname
			REPLACE descrip WITH 'MATERIAL BONUS'
		ENDIF
	ENDIF


	IF LEFT(mpaid,4) = 'CNCL'
		STORE '  ' TO cc
		STORE '  ' TO mcc
		@ 20,0 CLEAR
		@ 1,20 TO 23,80 CLEAR
		SET COLOR TO &windowatr
		@ 0,21 TO 23,80 DOUBLE
		CLEAR TYPEAHEAD
		SET COLOR TO &screenatr
		STORE 3 TO x
		STORE 22 TO y
		SELECT 6		&&	TimCancl
		SET ORDER TO 1
		GOTO TOP
		SET COLOR TO &popupatr
		@ 1,22 SAY 'CODE    DESCRIPTION          CODE    DESCRIPTION         '
		DO WHILE .NOT. EOF()
			SET COLOR TO &promptatr
			IF x > 21
				STORE 3 TO x
				STORE 51 TO y
			ENDIF
			IF x > 42
				@ x,24 SAY 'More....  Press any key to continue' GET mref PICTURE '!!!!!!!'
				READ
				STORE 3 TO x
				STORE 22 TO y
			ENDIF
			@ x,y SAY cancel_cod
			SET COLOR TO &screenatr
			@ x,y+8 SAY descriptio
			STORE x+1 TO x
			SKIP 1
		ENDDO
		STORE '  ' TO cc
		DO WHILE .T.
			@ 20,0 SAY 'ENTER CANCEL CODE' GET cc PICTURE 'XX'
			READ
			STORE UPPER(cc) TO mcc
			IF mcc = 'X '
				CLOSE DATABASES
				RETURN
			ENDIF
			IF mcc = '  '
				SET COLOR TO R+/N
				@ 22,40 SAY '  A CANCEL CODE MUST BE ENTERED  '
				? CHR(7)
				? CHR(7)
				SET COLOR TO &promptatr
				LOOP
			ENDIF
			GOTO TOP
			SEEK mcc
			IF EOF()
				LOOP
			ELSE
				STORE descriptio TO mdescriptio
				EXIT
			ENDIF
		ENDDO
		STORE mdescriptio + mcomments TO mcomments
		@ 23,0 SAY 'Enter Reason for Cancel'
		@ 24,0 GET mcomments
		READ

	ENDIF
	SELECT 1
	GO rn
	STORE DTOC(DATE())+' '+LEFT(TIME(),5) TO mclosedate
	STORE afterhours TO ah
	REPLACE material WITH mmaterial
	REPLACE amount WITH mamount
	REPLACE cancel_cod WITH mcc
	REPLACE paid WITH mpaid
	REPLACE HOURS WITH mhours
	REPLACE rate WITH mrate
	REPLACE cleared WITH mcleared
	REPLACE tax WITH mtax
	REPLACE WORK WITH mwork
	REPLACE preferred WITH mpreferred
	REPLACE prefdisc WITH mprefdisc
	REPLACE hours1 WITH mhours1
	REPLACE rate1 WITH mrate1
	REPLACE comments WITH mcomments
	REPLACE discount WITH mdiscount
	REPLACE mat_cost WITH tcost
	REPLACE wage WITH mwage
	REPLACE wageextens WITH wagetotal
	REPLACE service WITH mservice
	REPLACE special WITH mspecial
	REPLACE contact WITH mcontact
	REPLACE closedate WITH mclosedate
	REPLACE miles WITH mileage
	REPLACE pddate WITH DATE()
	IF m.link > ' '
		REPLACE LINK WITH m.link
	ENDIF

	**  UPDATE NOTES

	REPLACE notes WITH notes + CHR(13) +'Closed on '+DTOC(DATE()) +' by '+ operator+CHR(13)
	STORE 'N' TO howpaid
	IF m.appcode > ' '
		REPLACE appcode WITH m.appcode
		REPLACE notes WITH notes + CHR(13) +'Paid by '+mpaid+' Approval Code '+ ALLTRIM(m.appcode) +CHR(13)
		STORE 'Y' TO howpaid
	ENDIF
	IF m.paidno > ' '
		REPLACE paidno WITH m.paidno
		REPLACE notes WITH notes + CHR(13) +'Paid by '+mpaid+' Check Number '+ ALLTRIM(m.paidno) +CHR(13)
		STORE 'Y' TO howpaid
	ENDIF

	IF LEFT(mpaid,4) = 'CONT'
		REPLACE LINK WITH m.link
		REPLACE notes WITH notes + CHR(13) +'Continued On Workorder Number '+ ALLTRIM(m.link) +CHR(13)
		STORE 'Y' TO howpaid
	ENDIF


	IF howpaid = 'N'
		REPLACE notes WITH notes + CHR(13) +'Paid by '+mpaid +CHR(13)
	ENDIF
	IF mcc > '  '
		REPLACE notes WITH notes + CHR(13) +'Cancelled on '+DTOC(DATE()) +' by '+ALLTRIM(operator)+ ' '+ mcc+CHR(13)
	ENDIF
	IF LEFT(paid,4) = "BILL" .OR. LEFT(paid,4) = "COLL"
		REPLACE notes WITH notes + CHR(13) +"********************* BILLING JOURNAL ********************"+CHR(13)
		REPLACE notes WITH notes + CHR(13) + mcity + ' ' + mstate + ' '+ mwarea + ' ' + mwphone + '  ' + mbarea + ' ' + mbphone + '  ' + marea + ' ' + mphone
		REPLACE notes WITH notes + CHR(13) + mjobdesc1
		REPLACE notes WITH notes + CHR(13) + mjobdesc2
		REPLACE notes WITH notes + CHR(13) + mjobdesc3
		REPLACE notes WITH notes + CHR(13)+ CHR(13) + 'Please log all communications as follows :'
		REPLACE notes WITH notes + CHR(13) + 'DATE, TIME, CONVERSATION DETAILS, INITIALS, FOLLOW UP DATE'
		REPLACE notes WITH notes + CHR(13) +"**********************************************************"+CHR(13)+CHR(13)

	ENDIF

	** END UPDATE NOTES



	IF refpaid = 0
		REPLACE jobcost WITH jobcost+mat_cost+tax+wageextens
	ELSE
		REPLACE jobcost WITH jobcost+mat_cost+tax+wageextens+((amount-tax)*(refpaid/100))
	ENDIF
	REPLACE profit WITH amount - jobcost
	IF amount = 0
		REPLACE cleared WITH 'Y'
	ELSE
		REPLACE cleared WITH 'N'
	ENDIF

	@ 20,0 CLEAR
	@ 1,20 TO 24,80 CLEAR
	SET COLOR TO &windowatr
	@ 20,1 TO 24,80 DOUBLE
	CLEAR TYPEAHEAD
	SET COLOR TO &screenatr
	STORE 'N' TO turnedin
	@ 21,5 SAY 'HAS THIS WORKORDER BEEN TURNED INTO THE OFFICE YET?  Y / N' GET turnedin PICTURE '!'
	@ 22,5 SAY 'Generate a new Incident for this Customer?  ( [Y]es  [N]o )' GET newincident PICTURE '!'
	@ 23,5 SAY 'Was this an after hours call?    Y/N                      ' GET afterhours PICTURE '!'
	READ
	IF turnedin = 'Y'
		REPLACE rcvdate WITH DATE()
	ENDIF
	IF auxwo = 'Y'
		REPLACE aux_work WITH 'Y'
		REPLACE aux_workno WITH m.aux_workno
	ENDIF
	IF YN = 'Y'
		REPLACE panel_stic with m.panel_stic
	ENDIF

	* REPORT FORM TIMCLOSE TO PRINT

	*	**   UPDATE EMPLOYEE TRACKING
	*	STORE TIME() TO meta
	*	IF ALLTRIM(memp_no) < '1'
	*		@ 22,5 SAY '  *****  TRACKING UPDATE   ******'
	*		@ 23,5 SAY 'PLEASE ENTER ELECTRICIAN' GET memp_no PICTURE 'XXXXXXX'
	*		READ
	*		@ 23,0 CLEAR
	*	ENDIF

	SELECT 9		&&	TIMTRACK
	SET ORDER TO 1

	SEEK mtechnician
	IF EOF()
		APPEND BLANK
	ENDIF
	IF invoice = mnum
		REPLACE DATE WITH DATE()
		REPLACE technician WITH mtechnician
		REPLACE invoice WITH '*AVAIL*'
		REPLACE name WITH mname
		REPLACE city WITH mcity
		REPLACE TIMEOUT WITH TIME()
		REPLACE closed WITH DATE()
		REPLACE closed_by WITH operator
	ELSE
		*			REPLACE DATE WITH DATE()
		*			REPLACE technician WITH mtechnician
		*			REPLACE invoice WITH '*AVAIL*'
		*			REPLACE name WITH mname
		*			REPLACE city WITH mcity
		*			REPLACE TIMEOUT WITH TIME()
		*			REPLACE closed WITH DATE()
		*			REPLACE closed_by WITH operator
	ENDIF
	UNLOCK ALL
	**** END TRACKING

	SELECT 7
	SEEK mtechnician

	IF FOUND()
		IF mileage > miles
			REPLACE miles WITH mileage
			REPLACE milesupdat WITH DATE()
		ENDIF
	ENDIF


	IF newincident = 'Y'
		DO timsales
	ENDIF

	CLOSE DATABASES
	RETURN
ENDDO   1

PROCEDURE reschedule




*: EOF: TIMCLOSE.PRG


