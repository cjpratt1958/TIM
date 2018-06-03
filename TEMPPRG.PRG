public m.wo, m.amount, m.percent
use temp_emp
store '       ' to m.wo
store 0.00 to m.amount
goto top
do while .not. eof()
	store wo to m.wo
	store amount to m.amount
	if amount > 0	
		replace all amount with m.amount for wo = m.wo
		replace all percent with (Other_pay + gross_pay-deduct)/amount for wo = m.wo
		
	endif
	skip 1
enddo
