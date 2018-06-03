*:******************************************************************************
*:
*: Procedure File Z:\TIM\DOCUMENTATION\TIMINDEX.PRG
*:
*:	Carl Pratt
*:	Cornerstones Electrical Services, Inc.
*:	PO Box 2138
*:	Sandy
*:	Utah
*:	84091
*:	USA
*:	
*:	
*:	
*:	
*:	
*:	
*:
*: Documented using Visual FoxPro Formatting wizard version  .05
*:******************************************************************************
*:   TIMINDEX
Clear
Store '***** R E B U I L D I N G   I N D E X   F I L E S *****' To Title
Store 40 - (Len(serial)/2) To Scol
Store 40 - (Len(Title)/2) To Col
Store 'TIMINDEX.PRG' To prg_name
Do addtitle
Store ' ' To CHOICE
Set Exclusive On
TEXT

            A - Rebuild All files
            B - Rebuild Workorder files
            C - Rebuild Inventory files
            D - Rebuild Personel files
            E - Rebuild Hold files
            F - Rebuild Additional Truck Stock files
            G - Rebuild Reorder files
            H - Rebuild Stock history files
            I - Rebuild Cash Reimburstment files
            J - Rebuild Billing files
            K - Rebuild Tax files
            L - Rebuild Advertising files
            M - Rebuild Cancel codes
            N - Rebuild Dispatch Pool
            O - Rebuild Timecard files
            P - Rebuild Prefix files

ENDTEXT
@ 24,3 Say 'Please enter your choice,   Press [9] to return to menu' Get CHOICE Picture '!'
Read
Set Talk On
* SET ECHO ON
@ 2,0 Clear
Do Case

Case CHOICE = '9'
	Set Talk Off
	Set Exclusive Off
	Set Echo Off
	Return

Case CHOICE = 'A'

	Use employee
	If File('EMPLOYEE.CDX')
		Reindex
	Else
		Index On Name Tag Name Of employee
		Index On emp_no Tag emp_no Of employee
	Endif


	** CARL THIS WOULD NOT RUN taxesw2
	*   use taxesw2
	*	IF FILE('TAXESW2.CDX')
	*		REINDEX
	*	ELSE
	*		index ON EMP_NO TAG pr_tech OF TAXESW2
	*	ENDIF

	Use timwork
	If File('TIMWORK.CDX')
		Reindex
	Else
		Index On no Tag w_num Of timwork
		Index On bphone Tag w_bphone Of timwork
		Index On phone Tag w_phone Of timwork
		Index On wphone Tag w_wphone Of timwork
		Index On incident Tag w_incident Of timwork
		Index On TECHNICIAN Tag w_tech Of timwork
		Index On REF Tag w_ref Of timwork
		Index On priority Tag w_pri Of timwork
	Endif

	Use timhold
	If File('TIMHOLD.CDX')
		Reindex
	Else
		Index On wo Tag h_jobs Of timhold
	Endif

	Use TIMSTOCK
	If File ('TIMSTOCK.CDX')
		Reindex
	Else
		Index On part_no Tag s_partno Of TIMSTOCK
		Index On Descrip Tag s_descri Of TIMSTOCK
		Index On naed_no Tag S_NAED Of TIMSTOCK
		Index On comodity Tag S_COMODI Of TIMSTOCK
	Endif

	Use timtrkst
	If File('TIMTRKST.CDX')
		Reindex
	Else
		Index On wo Tag t_jobs Of timtrkst
	Endif

	Use timreord
	If File('TIMREORD.CDX')
		Reindex
	Else
		Index On wo Tag r_jobs Of timreord
		Index On Descrip Tag r_desc Of timreord
		Index On naed_no Tag r_naed Of timreord
	Endif

	Use timhistr
	If File('timhistr.CDX')
		Reindex
	Else
		Index On wo Tag hi_jobs Of timhistr
	Endif

	Use timrmbrs
	If File('TIMRMBRS.CDX')
		Reindex
	Else
		Index On no Tag rm_no Of timrmbrs
	Endif

	Use timbill
	If File('TIMBILL.CDX')
		Reindex
	Else
		Index On no Tag b_num Of timbill
	Endif

	Use timcancl
	If File('TIMCANCL.CDX')
		Reindex
	Else
		Index On cancel_cod Tag cancel_c Of timcancl
	Endif

	Use TIMUSER
	If File('TIMUSER.CDX')
		Reindex
	Else
		Index On Id Tag timusern Of TIMUSER
	Endif


	Use timpayro
	If File('TIMPAYRO.CDX')
		Reindex
	Else
		Index On wo Tag TPR_WO Of timpayro
	Endif

	Use ADVERTIS
	If File('ADVERTIS.CDX')
		Reindex
	Else
		Index On PHONE_NO Tag PHONE_NO Of ADVERTIS
		Index On DEADLINE Tag DEADLINE Of ADVERTIS
		Index On DISTRIB Tag DISTRIBU Of ADVERTIS
		Index On phone_co Tag phone_co Of ADVERTIS
		Index On COST_MONTH Tag COST_MON Of ADVERTIS
		Index On Code Tag Code Of ADVERTIS
		Index On ad_page Tag ad_page Of ADVERTIS
	Endif

	Use timhold
	If File('TIMHOLD.CDX')
		Reindex
	Else
		Index On wo Tag h_jobs Of timhold
	Endif

	Use timprefx
	If File('TIMPREFX.CDX')
		Reindex
	Else
		Index On PREFIX Tag PREFIX Of timprefx
	Endif





Case CHOICE = 'B'
	Use timwork
	If File('TIMWORK.CDX')
		Reindex
	Else
		Index On no Tag w_num Of timwork
		Index On bphone Tag w_bphone Of timwork
		Index On phone Tag w_phone Of timwork
		Index On wphone Tag w_wphone Of timwork
		Index On incident Tag w_incident Of timwork
		Index On TECHNICIAN Tag w_tech Of timwork
		Index On REF Tag w_ref Of timwork
		Index On priority Tag w_pri Of timwork
	Endif


Case CHOICE = 'C'
	Use TIMSTOCK
	If File ('TIMSTOCK.CDX')
		Reindex
	Else
		Index On part_no Tag s_partno Of TIMSTOCK
		Index On Descrip Tag s_descri Of TIMSTOCK
		Index On naed_no Tag S_NAED Of TIMSTOCK
		Index On comodity Tag S_COMODI Of TIMSTOCK
	Endif


Case CHOICE = 'D'
	Use employee
	If File('EMPLOYEE.CDX')
		Reindex
	Else
		Index On Name Tag Name Of employee
		Index On emp_no Tag emp_no Of employee
	Endif


Case CHOICE = 'E'
	Use timhold
	If File('TIMHOLD.CDX')
		Reindex
	Else
		Index On wo Tag h_jobs Of timhold
	Endif


Case CHOICE = 'F'
	Use timtrkst
	If File('TIMTRKST.CDX')
		Reindex
	Else
		Index On wo Tag t_jobs Of timtrkst
	Endif


Case CHOICE = 'G'
	Use timreord
	If File('TIMREORD.CDX')
		Reindex
	Else
		Index On wo Tag r_jobs Of timreord
		Index On Descrip Tag r_desc Of timreord
		Index On naed_no Tag r_naed Of timreord
	Endif


Case CHOICE = 'H'
	Use timhistr
	If File('timhistr.CDX')
		Reindex
	Else
		Index On wo Tag hi_jobs Of timhistr
	Endif



Case CHOICE = 'I'
	Use timrmbrs
	If File('TIMRMBRS.CDX')
		Reindex
	Else
		Index On no Tag rm_no Of timrmbrs
	Endif


Case CHOICE = 'J'
	Use timbill
	If File('TIMBILL.CDX')
		Reindex
	Else
		Index On no Tag b_num Of timbill
	Endif


Case CHOICE = 'K'
	** CARL THIS WOULD NOT RUN taxesw2
	*   use taxesw2
	*	IF FILE('TAXESW2.CDX')
	*		REINDEX
	*	ELSE
	*		index ON EMP_NO TAG pr_tech OF TAXESW2
	*	ENDIF

Case CHOICE = 'L'
	Use ADVERTIS
	If File('ADVERTIS.CDX')
		Reindex
	Else
		Index On PHONE_NO Tag PHONE_NO Of ADVERTIS
		Index On DEADLINE Tag DEADLINE Of ADVERTIS
		Index On DISTRIB Tag DISTRIBU Of ADVERTIS
		Index On phone_co Tag phone_co Of ADVERTIS
		Index On COST_MONTH Tag COST_MON Of ADVERTIS
		Index On Code Tag Code Of ADVERTIS
		Index On ad_page Tag ad_page Of ADVERTIS
	Endif

Case CHOICE = 'M'
	Use timcancl
	If File('TIMCANCL.CDX')
		Reindex
	Else
		Index On cancel_cod Tag cancel_c Of timcancl
	Endif


Case CHOICE = 'N'
	Use timpool
	If File('TIMpool.CDX')
		Reindex
	Else
		Index On sector Tag sector Of timpool
	Endif

Case CHOICE = 'O'
	Use TIMTIME
	If File('TIMtime.CDX')
		Reindex
	Else
		Index On TECHNICIAN+wo+Dtos(Date) Tag _rty0tylui Of TIMTIME
	Endif

Case CHOICE = 'P'
	Use timprefx
	If File('TIMPREFX.CDX')
		Reindex
	Else
		Index On PREFIX Tag PREFIX Of timprefx
	Endif





Endcase
Set Talk Off
Return
