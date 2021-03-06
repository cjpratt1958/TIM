*********************************************************************
*	THIS PROGRAM CREATES AN EXCEL SPREADSHEET NAMED CCAUDIT.WK1		*
*	IT EXCLUDES ANYTHING BEFORE 2002, AND TRIES TO SORT OUT ALL		*
*	CREDIT CARD INCOME.												*
*********************************************************************

USE TIMWORK
SET FIELDS TO NO,NAME,AMOUNT,PAID,REF,TECHNICIAN,DATE
SORT ON PAID TO TEMP
USE TEMP
DELETE ALL FOR AMOUNT = 0.00
DELETE ALL FOR LEFT(PAID,4) = 'CHCK'
DELETE ALL FOR LEFT(PAID,4) = 'BILL'
DELETE ALL FOR LEFT(PAID,4) = 'CASH'
DELETE ALL FOR LEFT(PAID,4) = 'COLL'
DELETE ALL FOR LEFT(PAID,4) = 'PAID'
DELETE ALL FOR LEFT(PAID,4) = 'ESTI'
DELETE ALL FOR LEFT(PAID,4) = 'RWRK'
DELETE ALL FOR PAID < 'A'

DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1992'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1993'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1994'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1995'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1996'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1997'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1998'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '1999'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '2000'
DELETE ALL FOR RIGHT(DTOC(DATE),4) = '2001'
BROWSE
CLEAR
STORE 'N' TO YN
@ 12,5 SAY 'WOULD YOU LIKE TO CREATE A SPREADSHEET OF THIS PAGE?  Y/N' GET YN PICTURE '!'
READ
IF YN = 'Y'
	EXPORT TO T:\CORNER~1\TIM\CCAUDIT TYPE WK1
ENDIF	

