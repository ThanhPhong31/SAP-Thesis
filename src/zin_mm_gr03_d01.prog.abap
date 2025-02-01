*&---------------------------------------------------------------------*
*& INCLUDE          ZIN_MM_GR03_D01
*&---------------------------------------------------------------------*
*&            CLASS DEFINITION & INTERFACE
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*& INTERFACE   lif_bapi
*&---------------------------------------------------------------------*
INTERFACE lif_bapi.
  TYPES: ty_t_maktx TYPE STANDARD TABLE OF bapi_makt,
         ty_t_marm  TYPE STANDARD TABLE OF bapi_marm,
         ty_t_marmx TYPE STANDARD TABLE OF bapi_marmx,
         ty_t_mlan  TYPE STANDARD TABLE OF bapi_mlan.

ENDINTERFACE.
*&---------------------------------------------------------------------*
*& INTERFACE   lif_description
*&---------------------------------------------------------------------*
INTERFACE lif_description.
  TYPES: BEGIN OF ty_t001l,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
           lgobe TYPE lgobe,
         END OF ty_t001l.

  TYPES: BEGIN OF ty_t001w,
           werks TYPE werks_d,
           name1 TYPE name1,
         END OF ty_t001w.

  TYPES: ty_t_t001l TYPE HASHED TABLE OF ty_t001l WITH UNIQUE KEY primary_key COMPONENTS werks lgort,
         ty_t_t001w TYPE HASHED TABLE OF ty_t001w WITH UNIQUE KEY primary_key COMPONENTS werks.


  METHODS:
    set_t001l,
    set_t001w.
ENDINTERFACE.
*&---------------------------------------------------------------------*
*& INTERFACE   lif_description
*&---------------------------------------------------------------------*
INTERFACE lif_excel.
  TYPES: ty_t_upload TYPE STANDARD TABLE OF zstmmgr03upl WITH EMPTY KEY,
         ty_t_uom    TYPE STANDARD TABLE OF zstmmgr03oum WITH EMPTY KEY,
         ty_t_tax    TYPE STANDARD TABLE OF zstmmgr03tax WITH EMPTY KEY.

  DATA: t_template TYPE ty_t_upload,
        t_uom      TYPE ty_t_uom,
        t_tax      TYPE ty_t_tax.
ENDINTERFACE.
*&---------------------------------------------------------------------*
*& CLASS   lcl_excel  DEFINITION
*&---------------------------------------------------------------------*
*  Load excel and insert data from excel to itab.
*&---------------------------------------------------------------------*
CLASS lcl_excel DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    INTERFACES lif_excel.
    ALIASES: t_template FOR lif_excel~t_template,
             t_uom      FOR lif_excel~t_uom,
             t_tax      FOR lif_excel~t_tax.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(re_instance) TYPE REF TO lcl_excel.


    CLASS-METHODS f4_file_name
      CHANGING
        ch_filepath TYPE localfile.

    METHODS read_excel
      IMPORTING
        im_file        TYPE localfile
      RETURNING
        VALUE(re_itab) TYPE lif_excel~ty_t_upload
      RAISING
        cx_static_check.


  PRIVATE SECTION.
    CLASS-DATA: o_excel TYPE REF TO lcl_excel.
    DATA: t_excel TYPE zttalsmex_tabline.

    METHODS load_excel
      IMPORTING
        im_file TYPE localfile
      RAISING
        cx_static_check.

    METHODS set_excel_to_itab.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS   lcl_build_fcat DEFINITION
*&---------------------------------------------------------------------*
*  Build fieldcatalogue for Upload template and Extend flow
*&---------------------------------------------------------------------*
CLASS lcl_build_fcat DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    DATA t_fcat TYPE lvc_t_fcat READ-ONLY.


    CLASS-METHODS get_instance
      RETURNING
        VALUE(re_instance) TYPE REF TO lcl_build_fcat.

    METHODS build_fcat
      IMPORTING
        im_struct      TYPE char1 OPTIONAL
      RETURNING
        VALUE(re_fcat) TYPE lvc_t_fcat.

    CLASS-METHODS build_layo
      RETURNING
        VALUE(re_layo) TYPE lvc_s_layo.

    CLASS-METHODS set_status_icon
      CHANGING
        ch_alv TYPE zstmmgr03upl.


  PRIVATE SECTION.
    CLASS-DATA: o_fcat TYPE REF TO lcl_build_fcat.

    METHODS:
      upload_fcat,
      extend_fcat.

    METHODS get_fcat
      IMPORTING
        im_structname  TYPE dd02l-tabname
      RETURNING
        VALUE(re_fcat) TYPE lvc_t_fcat.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_display_alv DEFINITION
*&---------------------------------------------------------------------*
*  Display ALV for Upload flow
*&---------------------------------------------------------------------*
CLASS lcl_display_alv DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_screen,
             scrn_start_col  TYPE i,
             scrn_start_line TYPE i,
             scrn_end_col    TYPE i,
             scrn_end_line   TYPE i,
           END OF ty_screen.
    DATA: ms_screen TYPE ty_screen.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_instance) TYPE REF TO lcl_display_alv.

    METHODS display_alv
      IMPORTING
        i_pf_status TYPE slis_formname
        i_ucomm     TYPE slis_formname
        i_title     TYPE lvc_title  OPTIONAL
        i_layout    TYPE lvc_s_layo OPTIONAL
        it_fcat     TYPE lvc_t_fcat
      EXPORTING
        i_outtab    TYPE STANDARD TABLE.

    METHODS set_screen.


  PRIVATE SECTION.
    CLASS-DATA: mo_display TYPE REF TO lcl_display_alv.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_utils DEFINITION
*&---------------------------------------------------------------------*
*  Upload utilites: This class using static method
*  This class is used to converse input from excel to internal.
*&---------------------------------------------------------------------*
CLASS lcl_utils DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS conv_unit_input
      EXPORTING
        im_unit         TYPE text255
      RETURNING
        VALUE(re_cunit) TYPE meins
      RAISING
        cx_sy_conversion_error.

    CLASS-METHODS conv_date_internal
      IMPORTING
        im_value       TYPE text255
      RETURNING
        VALUE(re_date) TYPE datum
      RAISING
        cx_sy_conversion_no_date_time.

    CLASS-METHODS conv_sled_input
      IMPORTING
        im_value        TYPE char5
      RETURNING
        VALUE(re_iprkz) TYPE dattp.

    CLASS-METHODS conv_char_to_num
      IMPORTING
        im_value         TYPE lvc_value
      RETURNING
        VALUE(re_amount) TYPE ntgew.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_impl_bapi DEFINITION
*&---------------------------------------------------------------------*
* Implement BAPI 'BAPI_MATERIAL_SAVEDATA'
*&---------------------------------------------------------------------*
CLASS lcl_impl_bapi DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_bapi,
      lif_excel.

    TYPES: ty_t_extend TYPE STANDARD TABLE OF zstmmgr3extend WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        im_excel TYPE REF TO lcl_excel OPTIONAL
      RAISING
        cx_sy_create_error.

    METHODS impl_bapi
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS extend_dmatnr
      CHANGING
        c_itab TYPE ty_t_extend.


  PRIVATE SECTION.
    DATA o_excel TYPE REF TO lcl_excel.

    METHODS check_duplicate
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

*    METHODS check_obli_fld
*      CHANGING
*        c_itab TYPE lif_excel~ty_t_upload.

    METHODS creat_matnr
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS extend_matnr
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS fill_creat
      IMPORTING
        is_data              TYPE zstmmgr03upl
      EXPORTING
        headerdata           TYPE bapimathead
        clientdata           TYPE bapi_mara
        clientdatax          TYPE bapi_marax
        plantdata            TYPE bapi_marc
        plantdatax           TYPE bapi_marcx
        storagelocationdata  TYPE bapi_mard
        storagelocationdatax TYPE bapi_mardx
        salesdata            TYPE bapi_mvke
        salesdatax           TYPE bapi_mvkex
        valuationdata        TYPE bapi_mbew
        valuationdatax       TYPE bapi_mbewx
        t_matkt              TYPE lif_bapi=>ty_t_maktx
        t_unitsofmeasure     TYPE lif_bapi=>ty_t_marm
        t_unitsofmeasurex    TYPE lif_bapi=>ty_t_marmx
        t_taxclassifications TYPE lif_bapi=>ty_t_mlan.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_impl_bapi_d DEFINITION
*&---------------------------------------------------------------------*
* Implement BAPI 'BAPI_MATERIAL_SAVEDATA' for DISPLAY FLOW
*&---------------------------------------------------------------------*
CLASS lcl_impl_bapi_d DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: tt_display TYPE STANDARD TABLE OF zmmg3d
                      WITH EMPTY KEY.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_instance) TYPE REF TO lcl_impl_bapi_d.

    METHODS delete_matnr
      IMPORTING
        i_mark TYPE flag
      CHANGING
        c_itab TYPE tt_display.

  PRIVATE SECTION.
    CLASS-DATA: mo_imp_bapi TYPE REF TO lcl_impl_bapi_d.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  bal_log DEFINITION
*&---------------------------------------------------------------------*
*  Bal_log for logging pop up message
*&---------------------------------------------------------------------*
CLASS lcl_bal_log DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    DATA: BEGIN OF ms_bal_log,
            bal_s_log  TYPE bal_s_log,
            log_handle TYPE balloghndl,
          END OF ms_bal_log.

    METHODS constructor.

    METHODS:
      bal_log_create,
      bal_log_display.

    METHODS bal_log_msg_cumulate
      IMPORTING
        i_s_msg TYPE bal_s_msg.

  PRIVATE SECTION.
    METHODS init_bal_log.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_event_handler DEFINITION
*&---------------------------------------------------------------------*
*  Bal_log for logging pop up message
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS on_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed .

    METHODS on_e_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed .
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_masterdata DEFINITION
*&---------------------------------------------------------------------*
*  Get description and text of master data
*&---------------------------------------------------------------------*
CLASS lcl_masterdata DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    INTERFACES lif_description.

    DATA: m_t001l TYPE lif_description~ty_t_t001l READ-ONLY,
          m_t001w TYPE lif_description~ty_t_t001w READ-ONLY.

    METHODS constructor.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS lcl_popup_confirm DEFINITION
*&---------------------------------------------------------------------*
*  Confirmation pop up
*&---------------------------------------------------------------------*
CLASS lcl_popup_confirm DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS is_confirm
      IMPORTING
        im_text       TYPE text100
      RETURNING
        VALUE(re_ans) TYPE boolean.

ENDCLASS.
