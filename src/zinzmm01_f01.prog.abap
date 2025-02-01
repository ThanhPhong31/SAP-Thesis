*&---------------------------------------------------------------------*
*& Include ZINZMM01_F01
*&---------------------------------------------------------------------*
*& Handle Selection Screen Functions:
*&  • Function key ( Download Template button )
*&  • Obligatory parameters
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      FORM  FUNC_1000
*&---------------------------------------------------------------------*
* Set Function key text and icon
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM func_1000.
  DATA ls_smp_dyntxt TYPE smp_dyntxt.

  ls_smp_dyntxt = VALUE #( text      = TEXT-fc1
                           icon_id   = icon_xls
                           icon_text = TEXT-fc1 ).

  sscrfields-functxt_01 = ls_smp_dyntxt.

  CLEAR ls_smp_dyntxt.
  ls_smp_dyntxt = VALUE #( text      = TEXT-fc3
                           icon_id   = icon_report_template
                           icon_text = TEXT-fc2 ).
  sscrfields-functxt_02 = ls_smp_dyntxt.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*   Set dynamic radio button on Selection Screen
*----------------------------------------------------------------------*
FORM modify_screen.
  DATA lv_field TYPE char1.

  LOOP AT SCREEN.
    IF screen-name = 'SMATNR-LOW'.
      screen-required = 2.
      IF gs_error-material_low IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SMATNR-LOW'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SMATNR-HIGH'.
      IF gs_error-material_high IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SMATNR-HIGH'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SMTART-LOW'.
      screen-required = 2.
      GET PARAMETER ID 'PAR1' FIELD lv_field ##EXISTS.
      IF gs_error-mattype_low IS NOT INITIAL OR lv_field IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SMTART-LOW'.
        SET PARAMETER ID 'PAR1' FIELD space ##EXISTS.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SMTART-HIGH'.
      GET PARAMETER ID 'PAR1' FIELD lv_field ##EXISTS.
      IF gs_error-mattype_high IS NOT INITIAL OR lv_field IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SMTART-HIGH'.
        SET PARAMETER ID 'PAR1' FIELD space ##EXISTS.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SWERKS-LOW'.
      screen-required = 2.
      GET PARAMETER ID 'PAR2' FIELD lv_field ##EXISTS.
      IF gs_error-plant_low IS NOT INITIAL OR lv_field IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SWERKS-LOW'.
        SET PARAMETER ID 'PAR2' FIELD space ##EXISTS.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'SWERKS-HIGH'.
      GET PARAMETER ID 'PAR2' FIELD lv_field ##EXISTS.
      IF gs_error-plant_high IS NOT INITIAL OR lv_field IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SWERKS-HIGH'.
        SET PARAMETER ID 'PAR2' FIELD space ##EXISTS.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SLGORT-LOW'.
      IF gs_error-slocs_low IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SLGORT-LOW'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SLGORT-HIGH'.
      IF gs_error-slocs_high IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SLGORT-HIGH'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'SMBRSH-LOW'.
      IF gs_error-industry_sector IS NOT INITIAL.
        screen-intensified = 1.
        SET CURSOR FIELD 'SMBRSH-LOW'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'PFILE'.
      screen-required = 2.
    ENDIF.

    IF rb_f1 IS NOT INITIAL AND screen-group1 = 'M2'.
      screen-active = 0.
    ENDIF.

    IF rb_f2 IS NOT INITIAL AND screen-group1 = 'M1'.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM HANDLE_FUNC_1000
*&---------------------------------------------------------------------*
*& Handle function key to download template
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
FORM handle_func_1000.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM download_template_matnr.
    WHEN 'FC02'.
      PERFORM download_template_langu.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM HANDLE_TABSTRIP
*&---------------------------------------------------------------------*
*& Handle tabstrip
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
FORM handle_tabstrip.
  CASE sy-ucomm.
    WHEN 'PUSH1'.
      mytab-dynnr     = 9010.
      mytab-activetab = 'BUTTON1'.
    WHEN 'PUSH2'.
      mytab-dynnr     = 9020.
      mytab-activetab = 'BUTTON2'.
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
FORM download_template USING iv_filenam TYPE char50.
  DATA lv_filename          TYPE string.
  DATA lv_path              TYPE string.
  DATA lv_fullpath          TYPE string.
  DATA lt_w3mime            TYPE STANDARD TABLE OF w3mime WITH EMPTY KEY.
  DATA lv_default_extension TYPE string.

  " Read data of excel to LT_W3MIME.
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING  object_id        = iv_filenam
    TABLES     data_table       = lt_w3mime
    EXCEPTIONS object_not_found = 1
               internal_error   = 2
               OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE s029 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  lv_filename = |{ iv_filenam }.XLSX|.

  " Choose location
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING  default_extension         = lv_default_extension
               window_title              = CONV #( TEXT-t01 )
*               file_filter               = 'Excel(.xls,.xlsx)|*.x|s;*.x|sx'
               file_filter               = '(*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|'
               default_file_name         = lv_filename
               initial_directory         = 'D:\'
    CHANGING   filename                  = lv_filename
               path                      = lv_path
               fullpath                  = lv_fullpath
    EXCEPTIONS cntl_error                = 1
               error_no_gui              = 2
               not_supported_by_gui      = 3
               invalid_default_file_name = 4
               OTHERS                    = 5 )
       ##SUBRC_OK ##NO_TEXT.

  " Cancel upload
  IF strlen( lv_fullpath ) = 0.
    MESSAGE s023 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Download file
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING  filename                = lv_fullpath
               filetype                = 'BIN'
    TABLES     data_tab                = lt_w3mime
    EXCEPTIONS file_write_error        = 1
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
    MESSAGE e002 DISPLAY LIKE 'S'.
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
FORM check_obligatory.
  DATA lv_string TYPE string.

  CASE mytab-dynnr.
    WHEN 9010.
      IF smatnr IS INITIAL.
        MESSAGE s013 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

      IF smtart IS INITIAL.
        SET PARAMETER ID 'PAR1' FIELD 'X' ##EXISTS.
        MESSAGE s001 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

      IF swerks IS INITIAL.
        SET PARAMETER ID 'PAR2' FIELD 'X' ##EXISTS.
        MESSAGE s001 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

    WHEN 9020.
      gv_activetab = 9020.
      EXPORT gv_activetab TO MEMORY ID gc_memid.
      IF pfile IS INITIAL.
        SET CURSOR FIELD 'PFILE'.
        MESSAGE s055 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        lv_string = pfile.
        lv_string = condense( val  = lv_string
                              from = ` `
                              to   = `` ).
        lv_string = to_upper( lv_string ).
        " Using regular expressions to validate path input
        FIND FIRST OCCURRENCE OF PCRE '^[^\\]+\\.*\.(XLS|XLSX)$' IN lv_string ##NO_TEXT.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
        MESSAGE s056 DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form download_template_langu
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_template_langu.
  DATA lv_filename TYPE char50 VALUE 'ZMM01- MATERIAL DESCRIPTIONS'.

  PERFORM download_template USING lv_filename.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form download_template_matnr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_template_matnr.
  DATA lv_filename TYPE char50 VALUE 'ZMM01 - CREATE & EXTEND MATERIALS'.

  PERFORM download_template USING lv_filename.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form tabstrip
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM tabstrip.
  DATA lv_repid TYPE syrepid VALUE sy-repid.

  IMPORT gv_activetab FROM MEMORY ID gc_memid.
  FREE MEMORY ID gc_memid.
  button1 = TEXT-007.
  button2 = TEXT-008.
  mytab-prog = lv_repid.
  CASE gv_activetab.
    WHEN 9020.
      mytab-dynnr     = gv_activetab.
      mytab-activetab = 'PUSH2'.

    WHEN OTHERS.
      mytab-dynnr     = 9010.
      mytab-activetab = 'PUSH1'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form check_authority
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_authority.
  CHECK swerks IS NOT INITIAL.
  DATA(lt_werks) = VALUE plants( ).
  TRY.
      SELECT werks FROM t001w
        WHERE werks IN @swerks
        INTO TABLE @lt_werks.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT lt_werks ASSIGNING FIELD-SYMBOL(<lv_werks>).
        AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
                        ID 'ACTVT' FIELD '03'
                        ID 'WERKS' FIELD <lv_werks>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW cx_sy_authorization_error( ).
        ENDIF.
      ENDLOOP.
    CATCH cx_sy_authorization_error.
      MESSAGE e073(/ibs/rb) WITH 'plant' <lv_werks> space.
  ENDTRY.
ENDFORM.
