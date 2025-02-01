*&---------------------------------------------------------------------*
*& Include ZINZMM01_D01
*&---------------------------------------------------------------------*
*&            CLASS DEFINITIONS & INTERFACES
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

  TYPES: BEGIN OF ty_tvko,
           vkorg TYPE vkorg,
         END OF ty_tvko.

  TYPES: BEGIN OF ty_t134,
           mtart TYPE mtart,
         END OF ty_t134.

  TYPES: BEGIN OF ty_t137,
           mbrsh TYPE mbrsh,
         END OF ty_t137.

  TYPES: BEGIN OF ty_t023,
           matkl TYPE matkl,
         END OF ty_t023.

  TYPES: BEGIN OF ty_tvtw,
           vtweg TYPE vtweg,
         END OF ty_tvtw.

  TYPES: ty_t_t001l TYPE HASHED TABLE OF ty_t001l WITH UNIQUE KEY primary_key COMPONENTS werks lgort,
         ty_t_t001w TYPE HASHED TABLE OF ty_t001w WITH UNIQUE KEY primary_key COMPONENTS werks,
         ty_t_t137  TYPE HASHED TABLE OF ty_t137 WITH UNIQUE KEY primary_key COMPONENTS  mbrsh,
         ty_t_t134  TYPE HASHED TABLE OF ty_t134 WITH UNIQUE KEY primary_key COMPONENTS  mtart,
         ty_t_t023  TYPE HASHED TABLE OF ty_t023 WITH UNIQUE KEY primary_key COMPONENTS  matkl,
         ty_t_tvko  TYPE HASHED TABLE OF ty_tvko WITH UNIQUE KEY primary_key COMPONENTS  vkorg,
         ty_t_tvtw  TYPE HASHED TABLE OF ty_tvtw WITH UNIQUE KEY primary_key COMPONENTS  vtweg.

  METHODS:
    set_t001l,
    set_t001w,
    set_t137,
    set_t134,
    set_t023,
    set_tvko,
    set_tvtw.
ENDINTERFACE.
*&---------------------------------------------------------------------*
*& INTERFACE   lif_description
*&---------------------------------------------------------------------*
INTERFACE lif_excel.
  TYPES: ty_t_upload TYPE STANDARD TABLE OF zstmmgr03upl WITH EMPTY KEY,
         ty_t_uom    TYPE STANDARD TABLE OF zstmmgr03oum WITH EMPTY KEY,
         ty_t_tax    TYPE STANDARD TABLE OF zstmmgr03tax WITH EMPTY KEY,
         ty_t_matdes TYPE STANDARD TABLE OF zstmmgr3matdes WITH EMPTY KEY.

*  DATA: t_template TYPE ty_t_upload,
*        t_uom      TYPE ty_t_uom,
*        t_tax      TYPE ty_t_tax,
*        t_matdes   TYPE ty_t_matdes.
ENDINTERFACE.
*&---------------------------------------------------------------------*
*& CLASS lcl_authority  DEFINITION
*&---------------------------------------------------------------------*
*
*&---------------------------------------------------------------------*
*CLASS lcl_authority DEFINITION CREATE PRIVATE.
*  PUBLIC SECTION.
*   me
*
*ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS   lcl_excel  DEFINITION
*&---------------------------------------------------------------------*
*  Load excel and insert data from excel to itab.
*&---------------------------------------------------------------------*
CLASS lcl_excel DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    INTERFACES lif_excel.
    DATA: t_uom      TYPE lif_excel~ty_t_uom    READ-ONLY,
          t_template TYPE lif_excel~ty_t_upload READ-ONLY,
          t_matdes   TYPE lif_excel~ty_t_matdes READ-ONLY,
          t_tax      TYPE lif_excel~ty_t_tax    READ-ONLY.

    "Activity category
    " F01 : File Create & Extend
    " F02 : File Multi-language
    DATA: v_upltyp TYPE char03 READ-ONLY.


*    CLASS-METHODS get_instance
*      IMPORTING
*        im_file            TYPE localfile
*      RETURNING
*        VALUE(re_instance) TYPE REF TO lcl_excel.

    METHODS constructor
      IMPORTING
        im_file TYPE localfile.

    METHODS set_uplfile.

    CLASS-METHODS f4_file_name
      CHANGING
        ch_filepath TYPE localfile.

    METHODS read_excel
      RAISING
        cx_static_check.


  PRIVATE SECTION.
    DATA: v_filepath TYPE localfile.
    DATA: t_excel TYPE zttalsmex_tabline.

    METHODS load_excel
      IMPORTING
        im_sheets TYPE i
      RAISING
        cx_static_check.

    METHODS set_excel_to_itab.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS   lcl_build_fcat DEFINITION
*&---------------------------------------------------------------------*
*  Build fieldcatalogue for Upload template.
*&---------------------------------------------------------------------*
CLASS lcl_build_fcat DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    DATA t_fcat TYPE lvc_t_fcat READ-ONLY.
    TYPES: ty_t_matdes TYPE STANDARD TABLE OF zstmmgr3matdes WITH EMPTY KEY,
           ty_t_upload TYPE STANDARD TABLE OF zstmmgr03upl WITH EMPTY KEY.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(re_instance) TYPE REF TO lcl_build_fcat.

    METHODS build_fcat
      IMPORTING
        im_struct      TYPE char1 OPTIONAL
      RETURNING
        VALUE(re_fcat) TYPE lvc_t_fcat.

    CLASS-METHODS build_layo
      IMPORTING
        im_flg_upload  TYPE flag OPTIONAL
      RETURNING
        VALUE(re_layo) TYPE lvc_s_layo.

    CLASS-METHODS set_status_icon
      CHANGING
        ch_alv TYPE zstmmgr03upl.

    CLASS-METHODS set_matdes_icon
      CHANGING
        ch_alv TYPE zstmmgr3matdes.

    CLASS-METHODS set_lang_color
      CHANGING
        ch_alv TYPE ty_t_matdes.

    CLASS-METHODS set_mtart_color
      CHANGING
        ch_alv TYPE zstmmgr03upl.

    CLASS-METHODS set_mattyp_color
      CHANGING
        ch_alv TYPE ty_t_upload.

  PRIVATE SECTION.
    CLASS-DATA: o_fcat TYPE REF TO lcl_build_fcat.

    METHODS:
      upload_fcat,
      matdes_fcat,
      extend_fcat,
      valtyp_fcat.

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
        i_top_of_page TYPE slis_formname OPTIONAL
        i_pf_status   TYPE slis_formname OPTIONAL
        i_ucomm       TYPE slis_formname OPTIONAL
        i_title       TYPE lvc_title     OPTIONAL
        i_layout      TYPE lvc_s_layo    OPTIONAL
        it_fcat       TYPE lvc_t_fcat
        it_sort       TYPE lvc_t_sort    OPTIONAL
      EXPORTING
        i_outtab      TYPE STANDARD TABLE.

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

    CLASS-METHODS conv_langu_input
      IMPORTING
        im_value        TYPE text255
      RETURNING
        VALUE(re_langu) TYPE spras
      RAISING
        cx_sy_conversion_error.

    CLASS-METHODS conv_date_external
      IMPORTING
        im_value       TYPE sydatum
      RETURNING
        VALUE(re_date) TYPE char10.

    CLASS-METHODS conv_time_external
      IMPORTING
        im_Value       TYPE syuzeit
      RETURNING
        VALUE(re_time) TYPE char8.

    CLASS-METHODS conv_curr_internal
      IMPORTING
        im_curr         TYPE waers
        im_price        TYPE ck_pvprs_1v
      RETURNING
        VALUE(re_price) TYPE verpr_bapi.
ENDCLASS.
*&---------------------------------------------------------------------*
*& CLASS  lcl_impl_bapi DEFINITION
*&---------------------------------------------------------------------*
* Implement BAPI 'BAPI_MATERIAL_SAVEDATA'
*&---------------------------------------------------------------------*
CLASS lcl_impl_bapi DEFINITION CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    INTERFACES:
      lif_bapi,
      lif_excel.

    TYPES: ty_t_extend TYPE STANDARD TABLE OF zstmmgr3extend WITH EMPTY KEY,
           ty_t_valtyp TYPE STANDARD TABLE OF zstmmgr3valtyp WITH EMPTY KEY.


    METHODS constructor
      IMPORTING
        im_excel TYPE REF TO lcl_excel OPTIONAL
      RAISING
        cx_sy_create_error.

    METHODS extend_dmatnr
      CHANGING
        c_itab TYPE ty_t_extend.

    METHODS add_valtyp
      CHANGING
        c_itab TYPE ty_t_valtyp.

    METHODS implement_bapi
      RAISING
        cx_static_check.

    METHODS get_upload
      RETURNING
        VALUE(re_itab) TYPE lif_excel~ty_t_upload.

    METHODS get_matdes
      RETURNING
        VALUE(re_itab) TYPE lif_excel~ty_t_matdes.


  PRIVATE SECTION.
    DATA o_excel TYPE REF TO lcl_excel.
    DATA: t_upload TYPE lif_excel~ty_t_upload,
          t_matdes TYPE lif_excel~ty_t_matdes.

    METHODS check_duplicate
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS check_valid
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS creat_matnr
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS extend_matnr
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS creat_matdescription
      CHANGING
        c_itab TYPE lif_excel~ty_t_matdes.

    METHODS impl_bapi_main_templ
      CHANGING
        c_itab TYPE lif_excel~ty_t_upload.

    METHODS impl_bapi_matdes
      CHANGING
        c_itab TYPE lif_Excel~ty_t_matdes.

    METHODS fill_bapiupl
      IMPORTING
        is_data              TYPE zstmmgr03upl
        iv_is_extend         TYPE flag          OPTIONAL
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
    TYPES: tt_display TYPE STANDARD TABLE OF ty_alvlist
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
      IMPORTING er_data_changed.

    METHODS on_e_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    METHODS on_valtyp_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

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
          m_t137  TYPE lif_description~ty_t_t137  READ-ONLY,
          m_t001w TYPE lif_description~ty_t_t001w READ-ONLY,
          m_t134  TYPE lif_description~ty_t_t134 READ-ONLY,
          m_t023  TYPE lif_description~ty_t_t023 READ-ONLY,
          m_tvko  TYPE lif_description~ty_t_tvko READ-ONLY,
          m_tvtw  TYPE lif_description~ty_t_tvtw READ-ONLY.

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
