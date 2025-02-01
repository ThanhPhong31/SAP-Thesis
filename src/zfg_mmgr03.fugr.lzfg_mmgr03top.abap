FUNCTION-POOL zfg_mmgr03.                   "MESSAGE-ID ..

* INCLUDE LZFG_MMGR03D...                    " Local class definition

TYPE-POOLS: ole2.
*      value of excel-cell
TYPES: ty_d_itabvalue TYPE ZSTALSMEX_TABLINE-value,
*      internal table containing the excel data
       ty_t_itab      TYPE ZSTALSMEX_TABLINE  OCCURS 0,
*      line type of sender table
       BEGIN OF ty_s_senderline,
         line(4096) TYPE c,
       END OF ty_s_senderline,
*      sender table
       ty_t_sender TYPE ty_s_senderline  OCCURS 0.


CONSTANTS:  GC_ESC              VALUE '"'.

DATA gw_maxcol TYPE int4.
