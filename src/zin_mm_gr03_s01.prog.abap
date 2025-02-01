*&---------------------------------------------------------------------*
*& INCLUDE          ZIN_MM_GR03_S01
*&---------------------------------------------------------------------*
*&           S E L E C T I O N  - S C R E E N
*&---------------------------------------------------------------------*


TABLES: mara, mard, marc.
TABLES: sscrfields. "smp_dyntxt.
TYPE-POOLS: icon.
SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK bl_02 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(20) TEXT-005 ##TEXT_POOL.
    PARAMETERS: rb_dis RADIOBUTTON GROUP rg01 DEFAULT 'X' USER-COMMAND zd1.
    SELECTION-SCREEN COMMENT 33(18) TEXT-003 FOR FIELD rb_dis.
    PARAMETERS: rb_upl RADIOBUTTON GROUP rg01.
    SELECTION-SCREEN COMMENT 58(20) TEXT-004 FOR FIELD rb_upl.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl_02.



SELECTION-SCREEN BEGIN OF BLOCK bl_01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                 smatnr FOR mara-matnr MODIF ID m1,
                 smbrsh FOR mara-mbrsh NO INTERVALS NO-EXTENSION MODIF ID m1,
                 smtart FOR mara-mtart NO INTERVALS NO-EXTENSION MODIF ID m1,
                 swerks FOR marc-werks MODIF ID m1,
                 slgort FOR mard-lgort MODIF ID m1.

  PARAMETERS: pfile TYPE rlgrap-filename MODIF ID m2.
SELECTION-SCREEN END OF BLOCK bl_01.
