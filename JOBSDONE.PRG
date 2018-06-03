
use timwork
set fields to name, amount, material, date, no, incident, paid, trade, cancel_cod, discount, special, service, closedate, city, jobdesc1, ref

store 'Workorder #' to SortBy
set order to no

Store 'Amount = 0.00' to FilterBy
set filter to technician = '110' .and. amount = 0.00

report form jobsdone preview
report form jobsdone to print
