SET delimiters off
PUBLIC x1,x2,x3,x4,x5,x6,x7,x8 as integer
ON KEY LABEL ESC EXIT
CLEAR
DO WHILE .t.
	@ 5,10 say 'Press the [ESC] key to quit OR Enter 8 digit Binary Number: [        ]'
 	@ 5,71 get x1 picture '9'
	@ 5,72 get x2 picture '9'
	@ 5,73 get x3 picture '9'
	@ 5,74 get x4 picture '9'
	@ 5,75 get x5 picture '9'
	@ 5,76 get x6 picture '9'
	@ 5,77 get x7 picture '9'
	@ 5,78 get x8 picture '9'
 	read
	@7,55 say INT((x1*1)+(x2*2)+(x3*4)+(x4*8)+(x5*16)+(x6*32)+(x7*64)+(x8*128))
ENDDO WHILE .t.

