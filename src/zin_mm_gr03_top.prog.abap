*&---------------------------------------------------------------------*
*& Include          ZIN_MM_GR03_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&                D E C L A R A T I O N S
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&                       I T A B S
*&---------------------------------------------------------------------*
DATA: gt_upload TYPE STANDARD TABLE OF zstmmgr03upl WITH EMPTY KEY,
      gt_extend TYPE STANDARD TABLE OF zstmmgr3extend WITH EMPTY KEY.

*&---------------------------------------------------------------------*
*&                     C L A S S E S
*&---------------------------------------------------------------------*
CLASS lcl_bal_log DEFINITION DEFERRED.
CLASS lcl_masterdata DEFINITION DEFERRED.
CLASS lcl_event_handler DEFINITION DEFERRED.

DATA: gr_grid    TYPE REF TO cl_gui_alv_grid,
      gr_handler TYPE REF TO lcl_event_handler.
DATA: gr_bal_log TYPE REF TO lcl_bal_log.
DATA: gr_masterdata TYPE REF TO lcl_masterdata.

*&---------------------------------------------------------------------*
*&                     I  N  C  L  U  D  E  S
*&---------------------------------------------------------------------*
INCLUDE zin_mm_gr03_s01.
INCLUDE zin_mm_gr03_d01.
