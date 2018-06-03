*:*****************************************************************************
*:
*: Procedure file: TIMAELOG.PRG
*:         System: TIM ( TRUCK INVENTORY MAINTENANCE)
*:         Author: CARL PRATT
*:      Copyright (c) 04/12/06, CARL PRATT
*:  Last modified: 04/12/06 at 23:02:48
*:
*:         Set by: TIM1MENU.PRG
*:
*:          Calls: TIMAELOG.SPR
*:
*:           Uses: TIMAELOG.DBF
*:
*:        Indexes: TIMAELOG.CDX
*:
*:      Documented 12:29:33                                FoxDoc version 3.00a
*:*****************************************************************************

SET EXCLUSIVE OFF
ON ESCAPE RETURN
STORE DATE() TO M.DATE
DO TIMAELOG.SPR
RETURN