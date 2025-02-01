*&---------------------------------------------------------------------*
*& Include          ZIN_MM_GR03_F01
*&---------------------------------------------------------------------*
*& Handle Selection Screen Functions:
*&  • Function key ( Download Template button )
*&  • Obligatory parameters
*&---------------------------------------------------------------------*






*&---------------------------------------------------------------------*
*&      FORM  FUNC_1000
*&---------------------------------------------------------------------*
*       Set Function key text and icon
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM func_1000 .
  DATA: ls_smp_dyntxt TYPE smp_dyntxt.

  ls_smp_dyntxt = VALUE #(
                            text = TEXT-fc1
                            icon_id = icon_xls
                            icon_text = TEXT-fc1
                          ).

  sscrfields-functxt_01 = ls_smp_dyntxt.
ENDFORM.


*&---------------------------------------------------------------------*
*&      FORM  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*   Set dynamic radio button on Selection Screen
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name = 'PFILE' OR  screen-name = 'SMATNR-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.

    IF rb_dis IS NOT INITIAL AND screen-group1 = 'M2'.
      screen-active = 0 .
    ENDIF.

    IF rb_upl IS NOT INITIAL AND screen-group1 = 'M1'.
      screen-active = 0 .
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& FORM HANDLE_FUNC_1000
*&---------------------------------------------------------------------*
*& Handle function key to download template
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
FORM handle_func_1000 .
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM download_template_excel.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_TEMPLATE_EXCEL
*&---------------------------------------------------------------------*
*& Handle Download Template
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_template_excel .
*  DATA: lv_data_size  TYPE i,
*        lv_doc_type   TYPE char100,
*        lv_doc_format TYPE char100.

  DATA: lv_filename TYPE string VALUE '[ZMM01] Create & Extend Material.XLSX' ##NO_TEXT,
        lv_path     TYPE string,
        lv_fullpath TYPE string.
  DATA: lt_w3mime TYPE STANDARD TABLE OF w3mime WITH EMPTY KEY.
  DATA: lv_filename_c TYPE char50.

  DATA: lv_default_extension TYPE string.

  lv_filename_c = 'ZMM01_CREATE AND EXTEND MATATERIAL.XLSX'.

  "Read data of excel to LT_W3MIME.
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      object_id        = lv_filename_c
*    IMPORTING
*      data_size        = lv_data_size
*      document_type    = lv_doc_type
*      document_format  = lv_doc_format
    TABLES
      data_table       = lt_w3mime
    EXCEPTIONS
      object_not_found = 1
      internal_error   = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE s029 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  "Choose location
  cl_gui_frontend_services=>file_save_dialog(
   EXPORTING
     default_extension         = lv_default_extension
     window_title              =  CONV #( TEXT-t01 )
*     file_filter               =  'Excel(.xls,.xlsx)|*.x|s;*.x|sx'
     file_filter               =  'Excel(.xls,.xlsx)|*.xls' "|s;*.x|sx'
     default_file_name         =  CONV #( lv_filename_c )
     initial_directory         =  'D:\'
   CHANGING
     filename                  = lv_filename
     path                      = lv_path
     fullpath                  = lv_fullpath
   EXCEPTIONS
     cntl_error                = 1
     error_no_gui              = 2
     not_supported_by_gui      = 3
     invalid_default_file_name = 4
     OTHERS                    = 5
       ) ##SUBRC_OK ##NO_TEXT.

  "Cancel upload
  IF strlen( lv_fullpath ) = 0.
    MESSAGE s023 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "Download file
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_fullpath
      filetype                = 'BIN'
    TABLES
      data_tab                = lt_w3mime
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF lv_fullpath IS NOT INITIAL.
    MESSAGE s002.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_OBLIGATORY
*&---------------------------------------------------------------------*
*& Check oblicatory fields on Selection Screen
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_obligatory .

  IF rb_upl IS NOT INITIAL AND pfile IS INITIAL.
    MESSAGE s001 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF rb_dis IS NOT INITIAL AND smatnr IS INITIAL.
    MESSAGE s013 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
