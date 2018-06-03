
*:*****************************************************************************
*:
*: Procedure file: C:\TIM\PRG\TIM_OPEN.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 01/28/97, CARL PRATT
*:  Last modified: 05/03/96 at 16:23:54
*:
*:         Set by: TIMDMENU.PRG                      
*:               : TIMADVER.PRG                      
*:
*:           Uses: COMPANY.DBF        
*:
*:      Documented 12:29:27                                FoxDoc version 3.00a
*:*****************************************************************************
* Program.: TIM_OPEN.PRG
* Author..: CARL PRATT
* Date....: 05/03/90
* Notice..: Copyright (c) 1990, TIM SOFTWARE
* Version.: FoxBASE+, revision 2.10
* Notes...: OPEN program for TIMWORK.DBF
*
SET EXCLUSIVE OFF
* ---Declare Global memory variables.
MODIFY WINDOW SCREEN TITLE "Cornerstones"
* on error do errhand





PUBLIC;
   s_level,USER,operator,msecurity,fwork,ftrckst,fstock,freorde,ftemp,frmbrs,;
   fhold,w_num,w_incident,w_tech,w_ref,w_bphone,w_wphone,w_phone,w_priority,;
   s_partno,s_descri,s_naed,t_jobs,r_jobs,r_naed,rm_no,fhistor,hiliteatr,;
   r_desc,ppe,sec,h_jobs,hi_jobs,pr_tech,VERSION,serial,pgdn,pgup,returnkey,;
   delrecord,screenatr,statusatr,windowatr,promptatr,dbfold,dbfnew,dbftim,x,;
   TITLE,prg_name,men_row,men_col,choice,rec,main_dir,uparrow,name,emp_no,;
   popupatr,instruatr
   

   
   PUBLIC;
   mnum,mdate,mincident,msector,mname,mstreet,mcity,mcontact,mzip,mbname,;
   mbstreet,mbcity,mbzip,mphone,mbphone,mwphone,mdiscount,mhours1,mwage,;
   mhours,mcomments,mwork,mrate,mrate1,mservice,marea,mbarea,mwarea,;
   lastrec,mref,mrec_no,lastinc,lastpo,errnum,MESSAGE,NewIncident,NewIncArea,;
   NewIncPhone,NewIncBarea,NewIncBphone,NewIncMwArea,NewIncMwPhone,NewIncRef,m.notes,;
   mFirstName, mLastName, mCompany

   
   PUBLIC;
   mspecial,mamount,mtax,mpreferred,mckn,mprefdisc,mmaterial,mpaid,mpaidno,mtrade,;
   mcleared,mstate,mbstate,tcost,tnum,tname,sr,msolddate,mdispdate,;
   mwo,mpacking,msupp,mpart_no,mdesc,mcost,mqty,mclosedate,m.technician,mtechnician,;
   mjobs,morder,mnaed_no,mprice,monhand,mmfer,mmrkup,msold,mjobdesc1,;
   mjobdesc2,mjobdesc3,mcomodity,munit,mcol3,mxstreet,mpddate,mref,;
   mpriority,dcompany,dstreet,dcity,dstate,dzip,dphone,drate1,drate2,drate3,;
   drate4,drate5,drate6,estyn,estamt,techname,techtitle,appcode,ah,techassign,TempNo,invstate
   

   SELECT 1
USE company share
COUNT TO rn
IF rn < 1
   GO TOP
   APPEND BLANK
   USE
ENDIF
USE company share
GOTO TOP
STORE company    TO dcompany
STORE street     TO dstreet
STORE city       TO dcity
STORE state      TO dstate
STORE zip        TO dzip
STORE phone      TO dphone
STORE CO_RATE1 TO DRATE1
STORE CO_RATE2 TO DRATE2
STORE CO_RATE3 TO DRATE3
STORE CO_RATE4 TO DRATE4
STORE CO_RATE5 TO DRATE5
STORE CO_RATE6 TO DRATE6
STORE SCREEN TO screenatr
STORE PROMPT TO promptatr
STORE hilite TO hiliteatr
STORE POPUP TO popupatr
STORE WINDOW TO windowatr
STORE STATUS TO statusatr
STORE INSTRU TO instruatr
STORE last_inc TO lastinc
STORE last_po TO lastpo
STORE 'N' TO NewIncident

   USE
STORE '2.0' TO VERSION
STORE 'TS900830-0000' TO serial
*store 'TS910626-0009' to serial
STORE 'UD910325-0001' TO serial

   * ---INKEY() constant values.
   pgdn = CHR(3)
   pgup = CHR(18)
   returnkey = CHR(13)
   delrecord = CHR(7)
   STORE SYS(2003) TO main_dir
   
      SET STATUS OFF
   RETURN
   * EOF: TIM_OPEN.PRG
*: EOF: TIM_OPEN.PRG
