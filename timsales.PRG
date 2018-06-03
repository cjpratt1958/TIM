
*:*****************************************************************************
*:
*: Procedure file: C:\TIM\PRG\TIMSALES.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 02/04/06 at 23:02:48
*:
*:         Set by: TIM1MENU.PRG
*:
*:          Calls: ADDTITLE           (procedure in TIMPROC.PRG)
*:               : WORK_STO           (procedure in TIMPROC.PRG)
*:               : WORKMENU           (procedure in TIMPROC.PRG)
*:
*:           Uses: TIMWORK.DBF
*:               : TIMPOOL.DBF
*:               : ADVERTIS.DBF
*:               : TIMPREFX.DBF
*:               : COMPANY.DBF
*:               : TIMTASK.DBF
*:
*:
*:      Documented 12:29:33                                FoxDoc version 3.00a
*:*****************************************************************************

SET EXCLUSIVE OFF
ON ESCAPE RETURN
ON KEY LABEL f9 DO tempf9
ON KEY LABEL f8 DO f8screen.spr

STORE ' ' TO m.notes
STORE '       ' TO techassign
SET COLOR OF SCHEME 1 TO

*  ---DIMENSION HOW PAID POPUP WINDOW
DIMENSION pd(6)
STORE 'NOT DISCUSSED' TO pd(1)
STORE 'CHECK' TO pd(2)
STORE 'CASH' TO pd(3)
STORE 'CREDIT CARD' TO pd(4)
STORE 'BILL' TO pd(5)
STORE 'ESTIMATE' TO pd(6)


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
IF USED("timpool")
	SELECT timpool
ELSE
	SELECT 2
	USE (LOCFILE("timpool.dbf","DBF","Where is timpool?"));
		AGAIN ALIAS timpool ;
		ORDER 0
ENDIF


*	SELECT 3
IF USED("advertis")
	SELECT advertis
	SET ORDER TO code
ELSE
	SELECT 3
	USE (LOCFILE("advertis.dbf","DBF","Where is advertis?"));
		AGAIN ALIAS advertis ;
		ORDER code
ENDIF


*	SELECT 4
IF USED("company")
	SELECT company
	SET ORDER TO 0
ELSE
	SELECT 4
	USE (LOCFILE("company.dbf","DBF","Where is company?"));
		AGAIN ALIAS company ;
		ORDER 0
ENDIF


*	SELECT 5
IF USED("timtask")
	SELECT timtask
	SET ORDER TO 0
ELSE
	SELECT 5
	USE (LOCFILE("timtask.dbf","DBF","Where is timtask?"));
		AGAIN ALIAS timtask ;
		ORDER 0
ENDIF


*	SELECT 6
IF USED("timbill")
	SELECT timbill
	SET ORDER TO 0
ELSE
	SELECT 6
	USE (LOCFILE("timbill.dbf","DBF","Where is timbill?"));
		AGAIN ALIAS timbill ;
		ORDER 0
ENDIF


*	SELECT 7
IF USED("billto")
	SELECT billto
	SET ORDER TO 0
ELSE
	SELECT 7
	USE (LOCFILE("billto.dbf","DBF","Where is billto?"));
		AGAIN ALIAS billto ;
		ORDER 0
ENDIF


*	SELECT 8
IF USED("timprefx")
	SELECT timprefx
	SET ORDER TO 0
ELSE
	SELECT 8
	USE (LOCFILE("timprefx.dbf","DBF","Where is timprefx?"));
		AGAIN ALIAS timprefx ;
		ORDER 0
ENDIF



SELECT timwork

@ 0,0 CLEAR TO 1,0
STORE 'TIMSALES.PRG' TO prg_name
STORE '********  SALES INFORMATION  ********' TO TITLE
DO addtitle
SET ECHO OFF
SET TALK OFF
SET INTENSITY ON
SET STATUS OFF
SET EXCLUSIVE OFF
SET CENTURY ON
SET readborder ON
STORE 2 TO sr
STORE '                    ' TO m.ref
STORE '  ' TO mpriority
STORE 00 TO mrefpaid
STORE 'N' TO yn
STORE ' ' TO assigntech

*  ---RESET ALL VARIABLES
DO work_sto

*  ---CREATE PICTURE ON SCREEN
DO workmenu
@ 12,0 CLEAR
@ 15,0 TO 15,80
@ 15,0 SAY 'ENTER JOB DESCRIPTION'
@ 19,0 TO 19,80
STORE drate1 TO mrate
IF DOW(DATE()) = 1
	STORE drate3 TO mrate
ENDIF
IF DOW(DATE()) = 7
	STORE drate2 TO mrate
ENDIF
IF LEFT(DTOC(DATE()),5) = '01/01' .OR. LEFT(DTOC(DATE()),5) = '12/25' .OR. LEFT(DTOC(DATE()),5) = '11/26'
	STORE drate6 TO mrate
ENDIF
IF LEFT(DTOC(DATE()),5) = '12/31' .OR. LEFT(DTOC(DATE()),5) = '12/24'
	STORE drate4 TO mrate
ENDIF



STORE TIME() TO TIME
IF TIME > '16:00:00'
	STORE drate2 TO mrate
ENDIF

IF TIME > '20:00:00'
	STORE drate3 TO mrate
ENDIF
IF TIME < '05:00:00'
	STORE drate3 TO mrate
ENDIF
@ 11,18 SAY '                     LABOR    '
@ 12,44 SAY 'TAX  [ .  %]'
*@ 13,30 SAY 'How will this be paid for? '
STORE SPACE(6) TO prfix
STORE DATE() TO mdate
STORE 2 TO sr
STORE 1 TO numberloops


IF newincident = 'Y'
	STORE newincarea TO marea
	STORE newincphone TO mphone
	STORE newincbarea TO mbarea
	STORE newincbphone TO mbphone
	STORE newincwarea TO mwarea
	STORE newincwphone TO mwphone
	STORE newincref TO m.ref
	STORE 'N' TO newincident
ENDIF


DO WHILE yn = 'N'
	DO WHILE .T.
		CLEAR TYPEAHEAD
		* SET COLOR TO &hiliteatr
		DO WHILE .T.
			DO WHILE .T.
				IF numberloops > 1
					*					RESTORE SCREEN
				ENDIF
				* SET COLOR TO &hiliteatr
				@ 23,0 TO 23,80 CLEAR
				@ 2,68 SAY mdate PICTURE '99/99/9999'
				STORE 0 TO COUNT
				@ 22,0 TO 24,80 SYSTEM
				@ 23,5 SAY 'How did you hear about us ? *  if yellow pages, what page number are we on'
				DO WHILE .T.
					@ 1,60 SAY 'Referral '
					@ 1,69 GET m.ref PICTURE 'XXXXXXXXXXXXXXXXXXXX'
					READ
					IF ALLTRIM(m.ref) = 'E' .and. LEN(ALLTRIM(M.REF)) = 1
						RETURN
					ENDIF
					IF ALLTRIM(m.ref) = 'e' .and. LEN(alltrim(m.ref)) = 1
						RETURN
					ENDIF
					STORE ' ' TO statetitle
					IF LEFT(m.ref,1) = ' ' .AND. LEN(ALLTRIM(m.ref)) = 0
						@ 22,0 TO 24,80 SYSTEM
						@ 23,5 SAY '  A REFERRAL NUMBER MUST BE ENTERED  ( Enter [E] to Exit to Menu )           '
						? CHR(7)
						STORE COUNT + 1 TO COUNT
						LOOP
					ELSE
						SELECT 3
						SET ORDER TO code
						SEEK m.ref
						IF FOUND()
							STORE ALLTRIM(book)+' '+ALLTRIM(state) TO statetitle
						ELSE
							SET ORDER TO ad_page
							SEEK m.ref
							IF FOUND()
								STORE ALLTRIM(book)+' '+ALLTRIM(state) TO statetitle
							ENDIF
						ENDIF
						EXIT
						SELECT 1
					ENDIF
				ENDDO
				IF statetitle = ' '
					* do nothing
				ELSE
					* SET COLOR TO B+/W*

					@ 11,0 TO 13,30 DOUBLE

					* SET COLOR TO &popupatr
					@ 12,3 SAY statetitle

				ENDIF

				@ 23,0 TO 23,80 CLEAR
				DO WHILE .T.
					@ 4,8 GET mwarea PICTURE '(999)'
					@ 4,15 GET mwphone PICTURE '999-9999'



					** DISPLAY PROMPT WINDOW
					* SET COLOR TO &popupatr
					@ 20,0 CLEAR
					@ 20,0 TO 24,79
					@ 21,5 SAY '** Let me enter your information into the computer and I will have the'
					@ 22,5 SAY '   first available technician give you a call as soon as he is available '
					@ 23,5 SAY '** Could you please give me a number where he can reach you.'
					* SET COLOR TO &hiliteatr
					READ
					SELECT 1
					IF numberloops = 1
						IF mwphone <> '        '
							SET ORDER TO  wphone
							STORE ALLTRIM(mwphone) TO mwphone
							LOCATE ALL FOR ALLTRIM(wphone) = mwphone .AND. ALLTRIM(warea) = mwarea
							DO WHILE .NOT. EOF()
								IF .NOT. EOF()
									STORE area TO marea
									STORE ALLTRIM(phone) TO mphone
									STORE barea TO mbarea
									STORE ALLTRIM(bphone) TO mbphone
									STORE name TO mname
									STORE company TO mcompany
									STORE firstname TO mfirstname
									STORE lastname TO mlastname
									STORE bname TO mbname
									STORE bstreet TO mbstreet
									STORE street TO mstreet
									STORE city TO mcity
									STORE bcity TO mbcity
									STORE state TO mstate
									STORE zip TO mzip
									STORE bstate TO mbstate
									STORE bzip TO mbzip
									STORE contact TO mcontact
									STORE prefdisc TO mprefdisc
									STORE technician TO m.technician
									STORE technician TO mtechnician
									@ 1,32 SAY mtechnician
									@ 1,69 SAY m.ref PICTURE 'XXXXXXXXXXXXXXXXXXXX'
									@ 4,33 SAY mbarea PICTURE '(999)'
									@ 4,40 SAY ALLTRIM(mbphone) PICTURE '999-9999'
									@ 4,60 SAY marea PICTURE '(999)'
									@ 4,67 SAY ALLTRIM(mphone) PICTURE '999-9999'
									@ 5,14 SAY mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 6,14 SAY mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 7,14 SAY mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 8,14 SAY mbstate PICTURE '!!'
									@ 8,34 SAY mbzip PICTURE '99999'
									@ 9,14 SAY mcontact PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 9,53 SAY mxstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 5,53 SAY mname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 6,53 SAY mstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 7,53 SAY mcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 8,53 SAY mstate PICTURE '!!'
									@ 8,73 SAY mzip PICTURE '99999'
									CONTINUE

								ENDIF

								IF .NOT. EOF()
									LOOP
								ENDIF
							ENDDO
						ENDIF
					ENDIF
					DO WHILE .T.
						@ 4,33 GET mbarea PICTURE '(999)'
						@ 4,40 GET mbphone PICTURE '999-9999'
						READ
						IF numberloops = 1
							IF mbphone <> '        '
								SET ORDER TO bphone

								LOCATE ALL FOR ALLTRIM(bphone) = mbphone .AND. barea = mbarea
								DO WHILE .NOT. EOF()
									IF .NOT. EOF()
										STORE company TO mcompany
										STORE firstname TO mfirstname
										STORE lastname TO mlastname
										STORE name TO mname
										IF mwphone = '        '
											STORE ALLTRIM(warea) TO mwarea
											STORE ALLTRIM(wphone) TO mwphone
										ENDIF
										STORE area TO marea
										STORE ALLTRIM(phone) TO mphone
										STORE bname TO mbname
										STORE bstreet TO mbstreet
										STORE street TO mstreet
										STORE city TO mcity
										STORE bcity TO mbcity
										STORE state TO mstate
										STORE zip TO mzip
										STORE bstate TO mbstate
										STORE bzip TO mbzip
										STORE prefdisc TO mprefdisc
										STORE contact TO mcontact
										STORE xstreet TO mxstreet
										STORE technician TO mtechnician
										STORE technician TO m.technician
										@ 1,32 SAY mtechnician
										@ 1,69 SAY m.ref PICTURE 'XXXXXXXXXXXXXXXXXXXX'
										@ 4,8 SAY mwarea PICTURE '(999)'
										@ 4,15 SAY mwphone PICTURE '999-9999'
										@ 4,60 SAY marea PICTURE '(999)'
										@ 4,67 SAY ALLTRIM(mphone) PICTURE '999-9999'
										@ 5,14 SAY mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 6,14 SAY mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 7,14 SAY mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 8,14 SAY mbstate PICTURE '!!'
										@ 8,34 SAY mbzip PICTURE '99999'
										@ 9,14 SAY mcontact PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 5,53 SAY mname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 6,53 SAY mstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 7,53 SAY mcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										@ 8,53 SAY mstate PICTURE '!!'
										@ 8,73 SAY mzip PICTURE '99999'
										@ 9,53 SAY mxstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
										CONTINUE

									ENDIF

									IF .NOT. EOF()
										LOOP
									ENDIF
								ENDDO
							ENDIF
						ENDIF
						DO WHILE .T.
							@ 4,60 GET marea PICTURE '(999)'
							@ 4,67 GET mphone PICTURE '999-9999'
							READ
							IF numberloops = 1
								IF mphone > '       '
									SELECT timprefx
									SET ORDER TO 2
									STORE SUBSTR(marea,2,3)+SUBSTR(ALLTRIM(mphone),2,3) TO prfix
									SEEK prfix
									IF .NOT. EOF()
										STORE city TO mcity
										STORE sector TO msector
										STORE zip TO mzip
										STORE prefix TO mprefix
										STORE state TO mstate
										STORE tax TO mtax
										SELECT timwork

									ELSE
										APPEND BLANK
										STORE prfix TO mprefix
										UNLOCK
										SELECT timwork

										STORE SPACE(25) TO mcity
										STORE SPACE(2) TO mstate

									ENDIF
									SELECT timwork
									*									LOCATE FOR ALLTRIM(phone) = ALLTRIM(mphone) .and. ALLTRIM(area) = ALLTRIM(marea)
									DO WHILE .NOT. EOF()
										SET ORDER TO phone DESCENDING
										SEEK mphone
										IF EOF()
											STORE ' '+ALLTRIM(mphone) TO mphone
											SEEK mphone
											IF FOUND()
												REPLACE phone WITH ALLTRIM(phone)
												STORE ALLTRIM(phone) TO mphone
											ENDIF
										ENDIF

										IF EOF()
											EXIT

										ELSE
											SET FILTER TO phone = mphone
											SET ORDER TO area DESCENDING
											SEEK marea
											IF EOF()
												SET FILTER TO
												EXIT
											ELSE
												STORE '       ' TO techassign
												IF mwphone = '        '
													STORE ALLTRIM(warea) TO mwarea
													STORE ALLTRIM(wphone) TO mwphone
												ENDIF
												IF mbphone = '        '
													STORE ALLTRIM(barea) TO mbarea
													STORE ALLTRIM(bphone) TO mbphone
												ENDIF
												*											IF LEFT(ref,1) = 'R'
												*												STORE UPPER(ref) TO m.ref
												*											ELSE
												*												STORE 'R'+UPPER(ref) TO m.ref
												*											ENDIF
												*	STORE '       ' TO mtechnician
												STORE city TO mcity
												STORE name TO mname
												STORE company TO mcompany
												STORE firstname TO mfirstname
												STORE lastname TO mlastname
												STORE bname TO mbname
												STORE bstreet TO mbstreet
												STORE street TO mstreet
												STORE bcity TO mbcity
												STORE state TO mstate
												STORE zip TO mzip
												STORE bstate TO mbstate
												STORE bzip TO mbzip
												STORE contact TO mcontact
												STORE xstreet TO mxstreet
												STORE technician TO techassign
												STORE technician TO mtechnician
												STORE technician TO m.technician
												STORE prefdisc TO mprefdisc
												@ 1,32 SAY mtechnician
												@ 1,69 SAY m.ref PICTURE 'XXXXXXXXXXXXXXXXXXXX'
												@ 4,8 SAY mwarea PICTURE '(999)'
												@ 4,15 SAY mwphone PICTURE '999-9999'
												@ 4,33 SAY mbarea PICTURE '(999)'
												@ 4,40 SAY mbphone PICTURE '999-9999'
												@ 5,14 SAY mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 6,14 SAY mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 7,14 SAY mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 8,14 SAY mbstate PICTURE '!!'
												@ 8,34 SAY mbzip PICTURE '99999'
												@ 9,14 SAY mcontact PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 5,53 SAY mname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 6,53 SAY mstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 7,53 SAY mcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
												@ 8,53 SAY mstate PICTURE '!!'
												@ 8,73 SAY mzip PICTURE '99999'
												@ 9,53 SAY mxstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
											ENDIF
										ENDIF
										@ 21,15 SAY mpriority
										EXIT
									ENDDO
								ENDIF
							ENDIF
							STORE 'N' TO yn
							DO WHILE .T.
								@ 1,50 SAY mtechnician
								@ 7,53 SAY mcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
								@ 8,53 SAY mstate PICTURE '!!'
								@ 8,73 SAY mzip PICTURE '99999'
								@ 11,50 SAY mrate PICTURE '99.99'
								@ 12,50 SAY mtax PICTURE '9.99'
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 24,79
								@ 21,5 SAY '** Is this a business or a residence?                         '
								@ 22,5 SAY "** (IF BUSINESS)   Can I get your business's name please?     "
								@ 23,5 SAY '** (IF RESIDENCE)  Can I get your name please?                '
								* SET COLOR TO &instruatr
								@ 22,5 SAY '   (IF BUSINESS)'
								@ 23,5 SAY '   (IF RESIDENCE)'

								DO WHILE .T.
									DO firstlastname
									IF mname = ' '
										WAIT WINDOW 'Do you want to return to the main menu '+CHR(13)+;
											'and lose all information?    Y / N ' TO yn
										IF UPPER(yn) = 'Y'
											RETURN
										ELSE
											LOOP
										ENDIF
									ENDIF
									@ 5,53 SAY mname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									*									READ
									** DISPLAY PROMPT WINDOW
									@ 20,0 CLEAR
									@ 20,0 TO 25,79
									@ 21,5 SAY '** Could you give me the name of the person that our technician      '
									@ 22,5 SAY '   will need to contact when he arrives?                             '
									@ 23,5 SAY '                                                                     '
									@ 24,5 SAY '    ENTER [-] TO RETURN TO LAST POPUP SCREEN                         '
									* SET COLOR TO &hiliteatr
									@ 9,14 GET mcontact PICTURE '@k!!!!!!!!!!!!!!!!!!!!!!!!!'
									READ
									IF mcontact = '-'
										* do nothing
									ELSE
										EXIT
									ENDIF
								ENDDO
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 25,79
								@ 21,5 SAY '                                                                     '
								@ 22,5 SAY '** Could you give me your street address please?                     '
								@ 23,5 SAY '                                                                     '
								@ 24,5 SAY ' ENTER A BLANK VALUE TO CANCEL AND RETURN TO MAIN MENU              '

								* SET COLOR TO &hiliteatr
								@ 6,53 GET mstreet PICTURE '@k!!!!!!!!!!!!!!!!!!!!!!!!!'
								READ
								IF mstreet = ' '
									RETURN
								ENDIF
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 24,79
								@ 22,5 SAY '**  Could you tell me where to find your street name?               '
								@ 23,5 SAY '                                                                    '

								* SET COLOR TO &instruatr
								@ 21,5 SAY '** (IF GIVEN A STREET NAME INSTEAD OF NUMBER) **                    '
								* SET COLOR TO &hiliteatr
								@ 9,53 GET mxstreet PICTURE '@k!!!!!!!!!!!!!!!!!!!!!!!!!'
								READ
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 24,79
								IF mcity = ' '
									@ 21,5 SAY '** And what city do you live in?'
								ELSE
									@ 21,5 SAY '** And you live in '+mcity
								ENDIF
								@ 22,5 SAY '                                                                     '
								@ 23,5 SAY '                                                                     '
								* SET COLOR TO &hiliteatr
								@ 7,53 GET mcity PICTURE '@k!!!!!!!!!!!!!!!!!!!!!!!!!'
								READ
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 24,79
								@ 21,5 SAY 'And you live in '+mstate
								@ 22,5 SAY '                                                                     '
								@ 23,5 SAY '                                                                     '
								* SET COLOR TO &hiliteatr
								@ 8,53 GET mstate PICTURE '!!'
								READ
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 24,79
								@ 21,5 SAY 'And what is the zip code where you live?                             '
								@ 22,5 SAY '                                                                     '
								@ 23,5 SAY '                                                                     '
								* SET COLOR TO &hiliteatr
								@ 8,73 GET mzip PICTURE '@k99999'
								READ
								IF mzip = '     '
									STORE 'N' TO zipyn
									@ 20,0 CLEAR
									@ 20,0 TO 23,70
									@ 21,5 SAY "Would you like to search for the Zip Code?  (Y / N)  " GET zipyn PICTURE 'X'
									READ
									IF zipyn = 'Y'
										RUN C:\zipcodes\zip.exe
									ENDIF
									@ 20,0 CLEAR
									@ 20,0 TO 24,79
									@ 21,5 SAY 'And what is the zip code where you live?                             '
									@ 22,5 SAY '                                                                     '
									@ 23,5 SAY '                                                                     '
									@ 8,73 GET mzip PICTURE '99999'
									READ
								ENDIF
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								** save SCREEN
								@ 19,0 TO 24,79
								@ 20,5 SAY '** Our labor rates are $'+STR(mrate,5,2)+' per half hour, our time does not    '
								@ 21,5 SAY '   start until we arrive at your door, we do have a one time $24.00  '
								@ 22,5 SAY '   service charge however do we offer a 10% Senior Citizen, Military,'
								@ 23,5 SAY '   and Veterans discounts off our regular labor rates.'
								* SET COLOR TO &hiliteatr
								@ 11,50 GET mrate PICTURE '99.99'
								READ
								** restore screen
								** DISPLAY PROMPT WINDOW
								* SET COLOR TO &popupatr
								@ 20,0 CLEAR
								@ 20,0 TO 24,79
								@ 22,5 SAY '                                                                     '
								@ 23,5 SAY '                                                                     '
								* SET COLOR TO &instruatr
								@ 21,5 SAY ' VERIFY THE TAX RATE FOR THE AREA                                    '
								* SET COLOR TO &hiliteatr
								@ 12,50 GET mtax PICTURE '9.99'
								READ
								** DISPLAY PROMPT WINDOW
								@ 19,0 CLEAR


								STORE 'N' TO ah
								DO WHILE .T.
									@ 14,30 SAY 'Is this an after hour call? 'GET ah PICTURE '!'
									READ
									IF ah = 'N'
										EXIT
									ENDIF
									IF ah = 'Y'
										EXIT
									ENDIF
									*									? CHR(7)
									*									? CHR(7)
									WAIT WINDOW 'You must enter a "Y" or "N" answer'


								ENDDO

								@ 21,0 SAY 'Priority '
								@ 23,0 SAY 'Enter Description          '
								@ 21,15 SAY mpriority
								@ 16,0 GET mjobdesc1
								@ 17,0 GET mjobdesc2
								@ 18,0 GET mjobdesc3
								READ

								****  how will this be paid? ******


								STORE 'HOW WILL YOU BE PAYING TODAY ?' TO titlebar

								DO paymenu

								@ 13,59 SAY mpaid
								IF UPPER(mpaid) = 'BILL'



									@ 23,0 TO 23,80 CLEAR
									@ 23,0 SAY 'Enter Billing Information'
									@ 4,8 GET mwarea PICTURE '(999)'
									@ 4,15 GET mwphone PICTURE '999-9999'
									@ 4,33 GET mbarea PICTURE '(999)'
									@ 4,40 GET mbphone PICTURE '999-9999'
									READ

									**** CODE GOES HERE TO LOOK FOR PAST DUE BILLS, NEED TO NOTIFY AMOUNT AND FLAG EXCESSIVE AGING ****

									SELECT timbill
									********  FIND IF ANY OF THE PHONE NUMBERS MATCH THE BILLING FILE *******

									SET ORDER TO 2
									DO CASE




									ENDCASE

									********  END FIND PHONE NUMBERS *****





									**** END CODE FOR EXCESSIVE AMOUNT OR EXESSIVE AGING *****




									@ 5,14 GET mbname PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 6,14 GET mbstreet PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 7,14 GET mbcity PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
									@ 8,14 GET mbstate PICTURE '!!'
									@ 8,34 GET mbzip PICTURE '99999'
									READ
									CLEAR GETS
									STORE 1 TO sr



									**** CODE GOES HERE FOR CREDIT APPLICATION ON FILE ****
									SELECT billto
								ENDIF

								**** END CODE FOR CREDIT APPLICATION ON FILE *****



								DEFINE WINDOW mreferral FROM 9,30 TO 13,80 SYSTEM
								ACTIVATE WINDOW mreferral
								@ 1,5 SAY 'Edit Referral?  '
								@ 1,35 GET m.ref PICTURE '@kXXXXXXXXXXXXXXXXXXXX'
								READ

								DEACTIVATE WINDOW mskip


								DO WHILE LEFT(m.ref,1) = ' '
									IF LEFT(m.ref,1) = ' '
										WAIT WINDOW 'A REFERRAL NUMBER MUST BE ENTERED  '+CHR(13)+CHR(7)+CHR(7)+' Press any key to continue'


										ACTIVATE WINDOW mreferral
										@ 1,5 SAY 'Enter Referral Number'
										@ 1,35 GET m.ref PICTURE '@kXXXXXXX'
										READ

										DEACTIVATE WINDOW mskip


									ENDIF
								ENDDO
								RELEASE WINDOW mreferral
								@ 1,69 SAY m.ref PICTURE '@kXXXXXXXXXXXXXXXXXXXX'

								STORE UPPER(ah) TO ah
								* SET COLOR TO &promptatr
								CLEAR TYPEAHEAD
								SELECT timwork
								STORE '  ' TO priority
								STORE .F. TO mexit
								DEFINE WINDOW mprimenu FROM 13,30 TO 23,55 SYSTEM
								DO WHILE NOT mexit
									ACTIVATE WINDOW mprimenu
									@ 2,2 PROMPT '01  -  Go For It '
									@ 3,2 PROMPT '02  -  Estimate  '
									@ 4,2 PROMPT '03  -  Cancel    '
									@ 5,2 PROMPT '04  -  Rework    '
									@ 6,2 PROMPT '05  -  Self Gen. '
									@ 7,2 PROMPT '06  -  Other     '
									@ 1,2 PROMPT '00  -  Emergency '

									MENU TO mchoice
									DEACTIVATE WINDOW mprimenu
									DO CASE
									CASE mchoice = 7
										STORE '00' TO mpriority
										STORE .T. TO mexit
										LOOP
									CASE mchoice = 1
										STORE '01' TO mpriority
										STORE .T. TO mexit
										LOOP

									CASE mchoice = 2
										STORE '02' TO mpriority
										STORE .T. TO mexit
										LOOP

									CASE mchoice = 3
										STORE '03' TO mpriority
										STORE .T. TO mexit
										LOOP

									CASE mchoice = 4
										STORE '04' TO mpriority
										STORE .T. TO mexit
										LOOP

									CASE mchoice = 5
										STORE '05' TO mpriority
										STORE .T. TO mexit
										LOOP

									CASE mchoice = 6
										STORE '06' TO mpriority
										STORE .T. TO mexit
										LOOP

									ENDCASE
								ENDDO
								RELEASE WINDOW mprimenu
								@ 21,15 SAY mpriority


								IF mpriority = '02'
									STORE .F. TO mexit
									DEFINE WINDOW mvalidate FROM 3,5 TO 19,95 SYSTEM
									DO WHILE NOT mexit
										ACTIVATE WINDOW mvalidate

										@ 1,5 SAY 'Mr / Mrs ___________ I just need to ask a few questions'
										@ 3,5 SAY '1. Are you the Homeowner or Business Owner ? '
										@ 4,5 SAY '2. Who is the person that will be authorizing the work if you like the estimate?'
										@ 5,5 SAY '3. For a free estimate we require meeting that person. Can we schedule that now?'
										@ 6,5 SAY '4. How many estimates do you have so far ? '
										@ 7,5 SAY '5. If you can give me the estimate amounts that you have recieved,'
										@ 8,5 SAY '   I can see if we can be competetive and beat the price.'
										@ 9,5 SAY '6. If you like our estimate, can the technician start the job while he is there?'
										@ 12,5 PROMPT 'This call Qualifies as an Estimate.'
										@ 13,5 PROMPT 'This call DOES NOT Qualify as an Estimate.'
										MENU TO mchoice
										DEACTIVATE WINDOW mvalidate
										DO CASE
										CASE mchoice = 1
											STORE '02' TO mpriority
											STORE .T. TO mexit
											LOOP
										CASE mchoice = 2
											STORE '01' TO mpriority
											STORE .T. TO mexit
											LOOP
										ENDCASE
									ENDDO
									RELEASE WINDOW mvalidate
									@ 21,15 SAY mpriority
								ENDIF


								IF mpriority = '03'
									STORE 'CNCL' TO mpaid
									STORE 'Y' TO mcleared
									STORE DATE() TO mpddate
									STORE 0.00 TO mamount
									@ 24,0 CLEAR
									@ 24,0 SAY 'Enter Reason for Cancel ' GET mcomments
									READ
								ENDIF
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
	CLEAR TYPEAHEAD
	*	@ 2,57 GET msector PICTURE '!!'
	*	READ

	*** ~< This code assigns a trade code to each Call >~

	*	@ 24,0 CLEAR
	*	STORE ' ' TO mtrade
	*	DO WHILE .T.
	*		@ 24,0 SAY '[A]A Advanced, [P]lumbing or [S]ervice ' GET mtrade PICTURE '!'
	*		READ
	*		IF mtrade = 'A'
	*			EXIT
	*		ENDIF
	*		IF mtrade = 'S'
	*			EXIT
	*		ENDIF
	*		IF mtrade = 'P'
	*			EXIT
	*		ENDIF
	*		@ 23,5 SAY ' ***** A TRADE CODE MUST BE ENTERED ***** '
	*		? CHR(7)
	*		LOOP
	*	ENDDO


	STORE '  ' TO mtrade
	STORE .F. TO mexit
	DEFINE WINDOW choosetrade FROM 13,30 TO 23,55 SYSTEM
	DO WHILE NOT mexit
		ACTIVATE WINDOW choosetrade
		@ 1,2 PROMPT 'ELC  -  Electrical    '
		@ 2,2 PROMPT 'PLM  -  Plumbing      '
		@ 3,2 PROMPT 'HVC  -  Heating / Air '
		@ 4,2 PROMPT 'AEM  -  Energy Mngmt  '
		@ 5,2 PROMPT 'SEC  -  Security      '
		@ 6,2 PROMPT 'TEL  -  Tele / Data   '

		MENU TO mchoice
		DEACTIVATE WINDOW choosetrade
		DO CASE
		CASE mchoice = 1
			STORE 'ELC' TO mtrade
			STORE .T. TO mexit
			LOOP

		CASE mchoice = 2
			STORE 'PLM' TO mtrade
			STORE .T. TO mexit
			LOOP

		CASE mchoice = 3
			STORE 'HVC' TO mtrade
			STORE .T. TO mexit
			LOOP

		CASE mchoice = 4
			STORE 'AEM' TO mtrade
			STORE .T. TO mexit
			LOOP

		CASE mchoice = 5
			STORE 'SEC' TO mtrade
			STORE .T. TO mexit
			LOOP

		CASE mchoice = 6
			STORE 'TEL' TO mtrade
			STORE .T. TO mexit
			LOOP

		ENDCASE
	ENDDO
	RELEASE WINDOW choosetrade




	*** ~< End Trade code >~


	@ 24,0 SAY 'Is This How You Want It To Read?   (Y/N/E) 'GET yn PICTURE '!'
	READ
	@ 24,0 CLEAR
	STORE numberloops + 1 TO numberloops
	IF yn = 'N'

		*		RESTORE SCREEN
	ENDIF
ENDDO YN
IF yn = 'E'
	ON KEY
	CLOSE DATABASES
	RETURN
ENDIF





SELECT timwork
SET ORDER TO name


** --- CREATE SOLD DATE
STORE TIME() TO TIME
STORE SUBSTR(TIME,1,5) TO TIME
STORE DTOC(mdate)+' '+TIME TO msolddate
@ 2,44 SAY mincident PICTURE '!!!!!!!!'

**  SEARCH FOR OTHER JOBS BY THIS CUSTOMER AND ASSIGN A TECH **

IF techassign > ' '
	? CHR(7)
	STORE .F. TO mexit
	DEFINE WINDOW assigntech FROM 1,1 TO 30,54 TITLE 'List of Technicians that have serviced this customer';
		CLOSE FLOAT GROW SHADOW ZOOM SYSTEM
	ACTIVATE WINDOW assigntech
	DISPLAY ALL FOR ALLTRIM(phone) = ALLTRIM(mphone) .AND. DATE > DATE() - 730 FIELDS no,incident,DATE,technician,amount OFF
	@ 0,10 SAY 'Assign a Technician ' GET techassign FUNCTION '@k'
	READ
	RELEASE WINDOW assigntech
	STORE techassign TO mtechnician
	STORE techassign TO m.technician
ENDIF

**  END SEARCH  ***

* --- Save data to database
*STORE lastrec+1 TO lastrec

APPEND BLANK
REPLACE DATE WITH mdate
REPLACE company WITH mcompany
REPLACE firstname WITH mfirstname
REPLACE lastname WITH mlastname
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
REPLACE phone WITH ALLTRIM(mphone)
REPLACE warea WITH mwarea
REPLACE wphone WITH ALLTRIM(mwphone)
REPLACE barea WITH mbarea
REPLACE bphone WITH ALLTRIM(mbphone)
REPLACE rate WITH mrate
REPLACE sector WITH msector
REPLACE contact WITH mcontact
REPLACE xstreet WITH mxstreet
REPLACE jobdesc1 WITH mjobdesc1
REPLACE jobdesc2 WITH mjobdesc2
REPLACE jobdesc3 WITH mjobdesc3
REPLACE solddate WITH msolddate
REPLACE technician WITH mtechnician
REPLACE ref WITH UPPER(ALLTRIM(m.ref))
REPLACE priority WITH mpriority
REPLACE refpaid WITH mrefpaid
REPLACE tax WITH mtax
REPLACE trade WITH mtrade
REPLACE dayofweek WITH CDOW(DATE())
REPLACE notes WITH m.notes
REPLACE notes WITH notes + CHR(13) + mname +' '+marea+' '+mphone
REPLACE notes WITH notes + CHR(13) +  'Sold on '+DTOC(DATE()) +' by '+ operator
REPLACE notes WITH notes + CHR(13) + 'Will pay by ' + mpaid
REPLACE afterhours WITH ah
REPLACE paid WITH mpaid

** ---  CREATE INCIDENT NUMBER
SELECT company
? RLOCK('Company')
STORE DATE() TO mdate
STORE SUBSTR(DTOC(mdate),1,2) TO MONTH
STORE SUBSTR(DTOC(mdate),4,2) TO DAY
STORE SUBSTR(DTOC(mdate),10,1) TO YEAR
STORE last_inc TO mincident
STORE SUBSTR(mincident,4,2) TO lastday
STORE SUBSTR(mincident,6,3) TO lastincday
IF lastday <> DAY
	STORE '0' TO lastincday
ENDIF
IF lastincday = ' '
	STORE '0' TO lastincday
ENDIF
STORE VAL(lastincday) TO nlastincday
STORE nlastincday + 1 TO nlastincday
STORE STR(nlastincday) TO lastincday
STORE LTRIM(lastincday) TO lastincday
STORE YEAR+MONTH+DAY+lastincday TO mincident
REPLACE last_inc WITH mincident
UNLOCK IN company

SELECT timwork
REPLACE incident WITH mincident
STORE RECNO() TO mrecord



IF LEN(mprefix) > 5

	SELECT timprefx
	SET ORDER TO 1
	REPLACE city WITH mcity
	REPLACE sector WITH msector
	REPLACE zip WITH mzip
	REPLACE prefix WITH mprefix
	REPLACE state WITH mstate
	REPLACE tax WITH mtax
	UNLOCK
ENDIF
STORE '          ' TO mschedule
@ 24,5 SAY 'ENTER SCHEDULED DATE ' GET mdate PICTURE '99/99/9999'
@ 24,45 SAY 'TIME ' GET mschedule PICTURE '!!!!!!!!!!'
READ







IF mpriority = '03'
	SELECT timwork
	GOTO mrecord
	REPLACE closedate WITH msolddate
	REPLACE cleared WITH 'Y'
	REPLACE amount WITH 0.00
	REPLACE comments WITH mcomments
	REPLACE paid WITH mpaid
	REPLACE pddate WITH mpddate
	UNLOCK
ELSE
	SELECT timpool

	APPEND BLANK
	REPLACE DATE WITH mdate
	REPLACE solddate WITH msolddate
	REPLACE ref WITH UPPER(m.ref)
	REPLACE priority WITH mpriority
	REPLACE name WITH mname
	REPLACE city WITH mcity
	REPLACE state WITH mstate
	REPLACE incident WITH mincident
	REPLACE sector WITH msector
	REPLACE technician WITH mtechnician
	REPLACE rec_no WITH mrecord
	REPLACE trade WITH mtrade
	REPLACE schedule WITH mschedule
	UNLOCK
ENDIF


SELECT timtask
STORE DATE() TO m.date
STORE TIME() TO m.time
STORE 'Sold on '+DTOC(DATE()) +' by '+ operator TO m.description
STORE '       ' TO m.no
STORE operator TO m.loggedby
STORE mname TO m.jobname
STORE mincident TO m.incident
APPEND BLANK
GATHER MEMVAR

* REPORT FORM TIMSALES TO PRINT

ON KEY
CLOSE DATABASES


PROCEDURE tempf9

DEFINE WINDOW tempf9 FROM 5,10 TO 13,80 SYSTEM TITLE 'F9 NOTES'
ACTIVATE WINDOW tempf9

IF EMPTY(m.notes)
	STORE CHR(13)+CHR(13)+'-- Viewed by '+ALLTRIM(operator)+' on '+DTOC(DATE())+' at '+TIME()+' from sales screen --'+CHR(13) TO m.notes
ELSE
	STORE m.notes + CHR(13)+CHR(13)+'-- Viewed by '+ALLTRIM(operator)+' on '+DTOC(DATE())+' at '+TIME()+' from sales screen --'+CHR(13) TO m.notes
ENDIF
@ 0,0 EDIT m.notes SIZE 7, 69

READ
RELEASE WINDOW


* end of program
*: EOF: TIMSALES.PRG
