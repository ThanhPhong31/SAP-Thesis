*&---------------------------------------------------------------------*
*& Include ZINZMM01_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&                D E C L A R A T I O N S
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&                 U P L O A D    F L O W
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&                       I T A B S
*&---------------------------------------------------------------------*
DATA: gt_upload       TYPE STANDARD TABLE OF zstmmgr03upl   WITH EMPTY KEY,
      gt_extend       TYPE STANDARD TABLE OF zstmmgr3extend WITH EMPTY KEY,
      gt_matdes       TYPE STANDARD TABLE OF zstmmgr3matdes WITH EMPTY KEY,
      gt_valtyp       TYPE STANDARD TABLE OF zstmmgr3valtyp WITH NON-UNIQUE KEY primary_key
                                                            COMPONENTS matnr bwkey bwtar,
      gt_check_valtyp TYPE STANDARD TABLE OF zstmmgr3valtyp WITH EMPTY KEY.

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
*&                     V A R I A B L E
*&---------------------------------------------------------------------*
DATA: gv_activetab TYPE int4.
DATA: gc_memid TYPE char20 VALUE 'TABSTRIPSCREEN'.



*&---------------------------------------------------------------------*
*&                 D I S P L A Y    F L O W
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TYPES DECLARATION                                                    *
*----------------------------------------------------------------------*

TYPES: BEGIN OF t_mard,
         matnr TYPE marc-matnr, "material
         werks TYPE mard-werks, "plant
         lgort TYPE mard-lgort, " stor. loc
       END OF t_mard.

TYPES: BEGIN OF ty_material,
         matnr TYPE mara-matnr,
         werks TYPE marc-werks,
         bwkey TYPE mbew-bwkey,
         bwtar TYPE bwtar_d,
         vkorg TYPE tvkwz-vkorg,
         vtweg TYPE tvkwz-vtweg,
       END OF ty_material.

* define type for edit material screen
TYPES: BEGIN OF ty_alvlist,
         cellstyle TYPE lvc_t_styl.
         INCLUDE TYPE zstmmg3d.
TYPES END OF ty_alvlist.

* error flag
TYPES: BEGIN OF ty_flag,
         material_low    TYPE abap_bool,
         material_high   TYPE abap_bool,
         plant_low       TYPE abap_bool,
         plant_high      TYPE abap_bool,
         mattype_low     TYPE abap_bool,
         mattype_high    TYPE abap_bool,
         industry_sector TYPE abap_bool,
         slocs_low       TYPE abap_bool,
         slocs_high      TYPE abap_bool,
       END OF ty_flag.
*----------------------------------------------------------------------*
* Class DECLARATION                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ALV table                                                            *
*----------------------------------------------------------------------*
DATA : go_container TYPE REF TO cl_gui_custom_container ##NEEDED,
       go_alv_table TYPE REF TO cl_gui_alv_grid ##NEEDED.

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION                                          *
*----------------------------------------------------------------------*
DATA ##NEEDED:
  gt_material     TYPE STANDARD TABLE OF ty_material,
  gt_mard         TYPE STANDARD TABLE OF t_mard,
  gt_mafi         TYPE STANDARD TABLE OF zg3mafi,
  gt_display      TYPE STANDARD TABLE OF ty_alvlist,
  gt_selected     TYPE STANDARD TABLE OF ty_alvlist,
  gt_changed      TYPE STANDARD TABLE OF ty_alvlist,
  gt_bapi_makt_ga TYPE STANDARD TABLE OF bapi_makt_ga,
  gt_bapi_marm_ga TYPE STANDARD TABLE OF bapi_marm_ga,
  gt_bapi_mean_ga TYPE STANDARD TABLE OF bapi_mean_ga,
  gt_bapi_mltx_ga TYPE STANDARD TABLE OF bapi_mltx_ga,
  gt_bapireturn   TYPE STANDARD TABLE OF bapireturn,
  gt_classes      TYPE STANDARD TABLE OF bapi1003_alloc_list,
  gt_return       TYPE STANDARD TABLE OF bapiret2,
  gt_bapi_makt    TYPE STANDARD TABLE OF bapi_makt,
  gt_bapi_marm    TYPE STANDARD TABLE OF bapi_marm,
  gt_bapi_marmx   TYPE STANDARD TABLE OF bapi_marmx,
  gt_returnx      TYPE STANDARD TABLE OF bapi_matreturn2.

*----------------------------------------------------------------------*
*      WORK AREAS DECLARATION                                          *
*----------------------------------------------------------------------*
DATA ##NEEDED:
  gs_error        TYPE ty_flag,
  gs_cellstyle    TYPE lvc_s_styl,
  gs_material     TYPE ty_material,
  gs_display      TYPE ty_alvlist,
  gs_bapi_mara    TYPE bapi_mara_ga,
  gs_bapi_marc    TYPE bapi_marc_ga,
  gs_bapi_mbew    TYPE bapi_mbew_ga,
  gs_bapi_mard    TYPE bapi_mard_ga,
  gs_bapi_mvke    TYPE bapi_mvke_ga,
  gs_bapi_mathead TYPE bapimathead,
  gs_bapi_s_mara  TYPE bapi_mara,
  gs_bapi_s_marc  TYPE bapi_marc,
  gs_bapi_s_mard  TYPE bapi_mard,
  gs_bapi_s_mbew  TYPE bapi_mbew,
  gs_bapi_s_mvke  TYPE bapi_mvke,
  gs_bapi_marax   TYPE bapi_marax,
  gs_bapi_marcx   TYPE bapi_marcx,
  gs_bapi_mardx   TYPE bapi_mardx,
  gs_bapi_mbewx   TYPE bapi_mbewx,
  gs_bapi_mvkex   TYPE bapi_mvkex,
  gs_return       TYPE bapiret2.
