close databases

clear
set confirm off
set talk off
set echo off

USE callperday EXCL
ZAP

SELECT 1
USE timwork

SELECT 2
USE callperday


STORE CTOD('08/01/2006') TO m.date
@ 5,5 say 'Enter begin date' get m.date picture '99/99/9999'
read

STORE 1 TO CNT
STORE VAL(SYS(11, dtoc(date())))-VAL(sys(11,DTOC(m.date))) TO m.days
@ 6,5 say 'Enter Number of days' get m.days picture '9999'
read


DO WHILE CNT < m.days+1

	SELECT 1
	STORE CDOW(m.date) TO m.dayofweek

	STORE LEFT(DTOC(m.date),5) TO m.day
	STORE RIGHT(DTOC(m.date),4) TO m.year
	@ 9,4 say m.day + ' ' + m.year
	COUNT ALL FOR LEFT(DTOC(date),5) = m.day .and. RIGHT(DTOC(date),4) = m.year TO m.calls
	SUM ALL AMOUNT FOR LEFT(DTOC(date),5) = m.day .and. RIGHT(DTOC(date),4) = m.year TO m.amount
	if m.amount > 0
		STORE m.amount/m.calls to m.avg
	else
		STORE 0.00 to m.avg
	endif


	SELECT 2
	APPEND BLANK
	GATHER MEMVAR
	STORE CNT + 1 TO CNT
	STORE m.date + 1 TO m.date

ENDDO
select 2
browse

CLOSE DATABASES




