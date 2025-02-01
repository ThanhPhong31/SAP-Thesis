*&---------------------------------------------------------------------*
*& Include ZINZMM01_P01
*&---------------------------------------------------------------------*
*&                  CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& CLASS   lcl_excel  IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_excel IMPLEMENTATION.
  METHOD constructor.
    me->v_filepath = im_file.
  ENDMETHOD.

  METHOD set_uplfile.
    "Code threading by radio buttons
    CASE 'X'.
      WHEN rb_f1.
        me->v_upltyp = 'F01'.
      WHEN rb_f2.
        me->v_upltyp = 'F02'.
    ENDCASE.
  ENDMETHOD.

  METHOD f4_file_name.
    DATA: lt_tab   TYPE filetable,
          lv_subrc TYPE i.
    DATA: BEGIN OF lty_file,
            filename(200),
          END OF lty_file.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = CONV #( TEXT-t02 )
        default_filename        = '*.XLSX'
        file_filter             = 'Microsoft Excel Files (*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|All Files (*.*)|*.*' ##NO_TEXT
        multiselection          = ' '
      CHANGING
        file_table              = lt_tab
        rc                      = lv_subrc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_tab INTO lty_file INDEX 1.
    IF sy-subrc = 0.
      ch_filepath = lty_file-filename.
    ENDIF.
  ENDMETHOD.

  METHOD read_excel.
    CASE me->v_upltyp.
      WHEN 'F01'.
        me->load_excel( im_sheets = 3 ).
      WHEN 'F02'.
        me->load_excel( im_sheets = 1 ).
    ENDCASE.

    me->set_excel_to_itab( ).
  ENDMETHOD.

  METHOD load_excel.
    DATA: lv_begin_col TYPE i VALUE 2,
          lv_begin_row TYPE i VALUE 1,
          lv_end_col   TYPE i VALUE 99,
          lv_end_row   TYPE i VALUE 200.

    "§ Read excel from file to itab
    CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = me->v_filepath
        i_begin_col             = lv_begin_col
        i_begin_row             = lv_begin_row
        i_end_col               = lv_end_col
        i_end_row               = lv_end_row
        sheets                  = im_sheets
      TABLES
        it_data                 = me->t_excel
      EXCEPTIONS
        inconsistent_parameters = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_rsl_ui_upload_error
                      MESSAGE e003.
    ENDIF.

    DELETE me->t_excel WHERE value IS INITIAL.
  ENDMETHOD.

  METHOD set_excel_to_itab.
    DATA: lt_alv    TYPE me->lif_excel~ty_t_upload,
          lt_uom    TYPE me->lif_excel~ty_t_uom,
          lt_tax    TYPE me->lif_excel~ty_t_tax,
          lt_matdes TYPE me->lif_excel~ty_t_matdes.


    CASE me->v_upltyp.
      WHEN 'F01'.
        PERFORM get_mtemplate_itab USING me->t_excel
                               CHANGING lt_alv.
        IF lt_alv IS INITIAL.
          MESSAGE s036 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.

        "• If data exceed 150 lines -> Dont allow to upload
        IF lines( lt_alv ) > 150.
          MESSAGE s016 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.

        PERFORM get_oum_itab USING me->t_excel
                          CHANGING lt_uom.

        PERFORM get_tax_itab USING me->t_excel
                             CHANGING lt_tax.
        me->t_template = lt_alv.
        me->t_uom = lt_uom.
        me->t_tax = lt_tax.
      WHEN 'F02'.
        PERFORM get_matdes_itab USING me->t_excel
                              CHANGING lt_matdes.
        IF lt_matdes IS INITIAL.
          MESSAGE s036 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.

        IF lines( lt_matdes ) > 150.
          MESSAGE s016 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.

        me->t_matdes = lt_matdes.

    ENDCASE.
  ENDMETHOD.
ENDCLASS. "end LCL_EXCEL

*&---------------------------------------------------------------------*
*& CLASS lcl_build_fcat IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_build_fcat IMPLEMENTATION.
  METHOD get_instance.
    IF o_fcat IS NOT BOUND.
      CREATE OBJECT o_fcat.
    ENDIF.

    re_instance = o_fcat.
  ENDMETHOD.

  METHOD build_fcat.
*&---------------------------------------------*
*   'X': Extend                                *
*   '': Template                               *
*   'U': UOM                                   *
*   'T': Tax Classification                    *
*   'D': Material Description                  *
*   'V': Add Valuation Type                    *
*&---------------------------------------------*
    CASE im_struct.
      WHEN ''.
        me->upload_fcat( ).
      WHEN 'X'.
        me->extend_fcat( ).
      WHEN 'U'.
        t_fcat = me->get_fcat( 'ZSTMMGR03OUM' ).
      WHEN 'T'.
        t_fcat = me->get_fcat( 'ZSTMMGR03TAX' ).
      WHEN 'D'.
        me->matdes_fcat( ).
      WHEN 'V'.
        me->valtyp_fcat( ).
    ENDCASE.

    re_fcat = t_fcat.
  ENDMETHOD.

  METHOD build_layo.
    IF im_flg_upload IS NOT INITIAL.
      re_layo = VALUE #( cwidth_opt = 'A'
                         col_opt    = 'A'
                         zebra = 'X'
                         sel_mode = 'A'
                         ctab_fname = 'T_COLOR'
                         no_f4    = 'X'
                        ).

    ELSE.
      re_layo = VALUE #( cwidth_opt = 'A'
                         col_opt    = 'A'
                         zebra      = 'X'
                         sel_mode   = 'A'
                       ).
    ENDIF.
  ENDMETHOD.

  METHOD set_status_icon.
    CASE ch_alv-status.
      WHEN 'S'.
        ch_alv-icon = icon_green_light.
      WHEN 'D'.
        ch_alv-icon = icon_yellow_light.
      WHEN 'E'.
        ch_alv-icon = icon_red_light.
      WHEN OTHERS.
        ch_alv-icon = icon_light_out.
    ENDCASE.
  ENDMETHOD.

  METHOD set_matdes_icon.
    CASE ch_alv-status.
      WHEN 'S'.
        ch_alv-icon = icon_green_light.
      WHEN 'E'.
        ch_alv-icon = icon_red_light.
      WHEN OTHERS.
        ch_alv-icon = icon_light_out.
    ENDCASE.
  ENDMETHOD.

  METHOD set_mtart_color.
    CASE ch_alv-mtart.
      WHEN 'HAWA'.
        APPEND VALUE #( fname = 'HAWA'    color-col = 5
                        color-int = 1     color-inv = 0  ) TO  ch_alv-t_color.
      WHEN 'ZDIE'.
        APPEND VALUE #( fname = 'ZDIE'    color-col = 7
                        color-int = 1     color-inv = 0  ) TO  ch_alv-t_color.
      WHEN OTHERS.
        APPEND VALUE #( fname = |{ ch_alv-mtart }|    color-col = 7
                        color-int = 0                 color-inv = 0  ) TO  ch_alv-t_color.
    ENDCASE.
  ENDMETHOD.

  METHOD set_lang_color.
    LOOP AT ch_alv INTO DATA(lr_data) GROUP BY ( spras = lr_data-spras ).
      LOOP AT GROUP lr_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        CASE <ls_data>-spras.
          WHEN '쁩'. "Vietnamese
            APPEND VALUE #( fname = 'SPRAS'   color-col = 2
                            color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
          WHEN 'E'. "English
            APPEND VALUE #( fname = 'SPRAS'   color-col = 5
                            color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
          WHEN 'D'. "German
            APPEND VALUE #( fname = 'SPRAS'   color-col = 4
                            color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
          WHEN '3'. "Korean
            APPEND VALUE #( fname = 'SPRAS'   color-col = 7
                            color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
          WHEN 'J'. "Japan
            APPEND VALUE #( fname = 'SPRAS'   color-col = 6
                            color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
          WHEN OTHERS.
            APPEND VALUE #( fname = 'SPRAS'   color-col = 3
                            color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_mattyp_color.
    LOOP AT ch_alv ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE mtart IS NOT INITIAL.
      CASE <ls_data>-mtart.
        WHEN 'HAWA'.
          APPEND VALUE #( fname = 'SPRAS'   color-col = 7
                          color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
        WHEN 'ZDIE'.
          APPEND VALUE #( fname = 'SPRAS'   color-col = 2
                          color-int = 0     color-inv = 1   ) TO  <ls_data>-t_color.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD upload_fcat.
    TYPES: BEGIN OF lty_fields,
             fieldname TYPE lvc_fname,
             label     TYPE lvc_txtcol,
             just      TYPE char1,
             color     TYPE lvc_emphsz,
           END OF lty_fields .
    DATA: lt_field TYPE STANDARD TABLE OF lty_fields WITH EMPTY KEY,
          ls_field TYPE lty_fields.

    lt_field = VALUE #(  ( fieldname = 'ICON'    label = 'Status'  just = 'C'     )
                         ( fieldname = 'MESSAGE' label = 'Message'            color = 'C300' )
                         ( fieldname = 'IS_EXT'  label = 'Extend'  just = 'C' color = 'C511' )
                         ( fieldname = 'STATUS' )
                         ( fieldname = 'WAERS'  )
                         ( fieldname = 'MATNR'  )
                      ) ##NO_TEXT.

    CLEAR t_fcat.
    t_fcat = me->get_fcat( im_structname = 'ZSTMMGR03UPL' ).

    LOOP AT lt_field INTO ls_field.
      READ TABLE t_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WITH KEY fieldname = ls_field-fieldname.
      IF sy-subrc = 0.
        CASE <ls_fcat>-fieldname.
          WHEN 'MATNR'.
            <ls_fcat>-key = 'X'.
          WHEN 'STATUS' OR 'WAERS'.
            <ls_fcat>-tech = 'X'.
            CONTINUE.
        ENDCASE.
        <ls_fcat>-coltext      = ls_field-label.
        <ls_fcat>-just         = ls_field-just.
        <ls_fcat>-emphasize    = ls_field-color.
      ENDIF.
      CLEAR ls_field.
    ENDLOOP.
  ENDMETHOD.

  METHOD valtyp_fcat.
    TYPES: BEGIN OF lty_field,
             fieldname TYPE lvc_fname,
             edit      TYPE flag,
             no_zero   TYPE flag,
             emphasize TYPE char4,
             tech      TYPE flag,
           END OF lty_field.
    DATA lt_field TYPE STANDARD TABLE OF lty_field WITH EMPTY KEY.

    lt_field = VALUE #( ( fieldname = 'BWTAR'   edit = 'X' )
                        ( fieldname = 'BKLAS'   edit = 'X' )
                        ( fieldname = 'PVPRS_1' edit = 'X'  no_zero = 'X' )
                        ( fieldname = 'MATNR'   emphasize = 'C411'  )
                        ( fieldname = 'MAKTX'                       )
                        ( fieldname = 'BWTTY'   emphasize = 'C510'  )
                        ( fieldname = 'BWKEY'   emphasize = 'C510'  )
                        ( fieldname = 'MTART'   tech = 'X'  )
                      ).

    t_fcat = me->get_fcat( 'ZSTMMGR3VALTYP' ).

    LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      READ TABLE lt_field ASSIGNING FIELD-SYMBOL(<ls_field>) WITH KEY fieldname = <ls_fcat>-fieldname.
      IF sy-subrc = 0.
        <ls_fcat>-edit      = <ls_field>-edit.
        <ls_fcat>-no_zero   = <ls_field>-no_zero.
        <ls_fcat>-emphasize = <ls_field>-emphasize.
        <ls_fcat>-tech      = <ls_field>-tech.
        <ls_fcat>-col_opt   = 'X'.
      ENDIF.
*      IF <ls_fcat>-fieldname EQ 'BKLAS'.
*
**§Set status of column BWKEY to editable and set a dropdown handle. (**Copy from BCALV_EDIT_06 - line 91 )
*        <ls_fcat>-edit = 'X'.
*        <ls_fcat>-drdn_hndl = '1'.
*
*
** Field 'checktable' is set to avoid shortdumps that are caused
** by inconsistend data in check tables. You may comment this out
** when the test data of the flight model is consistent in your system.
*        <ls_fcat>-checktable = '!'.        "do not check foreign keys
*      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD matdes_fcat.
    TYPES: BEGIN OF lty_fields,
             fieldname TYPE lvc_fname,
             label     TYPE lvc_txtcol,
             just      TYPE char1,
           END OF lty_fields.
    DATA: lt_fcat TYPE STANDARD TABLE OF lty_fields WITH EMPTY KEY.

    lt_fcat = VALUE #( ( fieldname = 'STATUS' )
                       ( fieldname = 'ICON'     label = 'Status'  just = 'C' )
                       ( fieldname = 'MESSAGE'  label = 'Message'            )
                       ( fieldname = 'MATNR' )
                       ( fieldname = 'SPRAS' )
                       ( fieldname = 'LANGU'  label = 'Language'             )
                     ) ##NO_TEXT.

    CLEAR t_fcat.
    t_fcat = me->get_fcat( im_structname = 'ZSTMMGR3MATDES' ).

    LOOP AT lt_fcat INTO DATA(ls_field).
      READ TABLE t_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>) WITH KEY fieldname = ls_field-fieldname.
      IF sy-subrc = 0.
        <lfs_fcat>-coltext    = ls_field-label.
        <lfs_fcat>-just       = ls_field-just.
        <lfs_fcat>-emphasize  = COND #( WHEN <lfs_fcat>-fieldname = 'MESSAGE' THEN 'C300' ).
        <lfs_fcat>-tech       = COND #( WHEN <lfs_fcat>-fieldname = 'STATUS'  THEN 'X'
                                        WHEN <lfs_fcat>-fieldname = 'SPRAS'   THEN 'X'   ).
        <lfs_fcat>-key        = COND #( WHEN <lfs_fcat>-fieldname = 'MATNR'
                                          OR <lfs_fcat>-fieldname = 'SPRAS'   THEN 'X'    ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extend_fcat.
    TYPES: BEGIN OF lty_fields,
             fieldname TYPE lvc_fname,
           END OF lty_fields.
    DATA: lt_fcat TYPE STANDARD TABLE OF lty_fields WITH EMPTY KEY.

    lt_fcat = VALUE #( ( fieldname = 'WERKSTX' )
                       ( fieldname = 'LGOBE'   )
                       ( fieldname = 'WAERS'   )
                       ( fieldname = 'MATNR'   )
                       ( fieldname = 'MAKTX'   )
                       ( fieldname = 'BWKEY'   )
                     ).
    CLEAR t_fcat.
    t_fcat = me->get_fcat( im_structname = 'ZSTMMGR3EXTEND' ).

    LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      IF NOT line_exists( lt_fcat[ fieldname = <lfs_fcat>-fieldname  ] ).
        <lfs_fcat>-edit = 'X'.
        <lfs_fcat>-f4availabl = 'X'.
      ENDIF.
      CASE <lfs_fcat>-fieldname.
        WHEN 'MATNR'.
          <lfs_fcat>-key = 'X'.
        WHEN 'WERKS'.
*          <lfs_fcat>-emphasize = 'C700'.
          <lfs_fcat>-ref_table = 'ZSTMMGR3EXTEND'.
          <lfs_fcat>-ref_field = 'WERKS'.
        WHEN 'LGORT'.
          <lfs_fcat>-ref_table = 'ZSTMMGR3EXTEND'.
          <lfs_fcat>-ref_field = 'LGORT'.
        WHEN 'STATUS'.
          <lfs_fcat>-tech = 'X'.
        WHEN 'MESSAGE'.
          <lfs_fcat>-tech = 'X'.
        WHEN 'BWTTY'.
          <lfs_fcat>-ref_table = 'ZSTMMGR3EXTEND'.
          <lfs_fcat>-ref_field = 'BWTTY'.
        WHEN 'PVPRS_1'.
          <lfs_fcat>-no_zero = 'X'.
          <lfs_fcat>-f4availabl = ''.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = im_structname
      CHANGING
        ct_fieldcat            = re_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.
ENDCLASS. "end LCL_BUILD_FCAT

*&---------------------------------------------------------------------*
*& CLASS   lcl_display_alv IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_display_alv IMPLEMENTATION.
  METHOD get_instance.
    IF mo_display IS NOT BOUND.
      CREATE OBJECT mo_display.
    ENDIF.

    r_instance = mo_display.
  ENDMETHOD.

  METHOD display_alv.
    DATA: lv_repid TYPE syrepid VALUE sy-repid.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program          = lv_repid
        i_callback_html_top_of_page = i_top_of_page
        i_callback_pf_status_set    = i_pf_status
        i_callback_user_command     = i_ucomm
        i_grid_title                = i_title
        is_layout_lvc               = i_layout
        it_fieldcat_lvc             = it_fcat
        i_save                      = 'A'
        it_sort_lvc                 = it_sort
        i_screen_start_column       = me->ms_screen-scrn_start_col
        i_screen_start_line         = me->ms_screen-scrn_start_line
        i_screen_end_column         = me->ms_screen-scrn_end_col
        i_screen_end_line           = me->ms_screen-scrn_end_line
      TABLES
        t_outtab                    = i_outtab
      EXCEPTIONS
        program_error               = 1
        OTHERS                      = 2.
    IF sy-subrc <> 0.
      MESSAGE s007 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.

  METHOD set_screen.
    me->ms_screen = VALUE #( scrn_start_col  = 10
                             scrn_start_line = 5
                             scrn_end_col    = 120
                             scrn_end_line   = 20   ).
  ENDMETHOD.
ENDCLASS. "end LCL_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& CLASS  lcl_utils DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_utils IMPLEMENTATION.
  METHOD conv_unit_input.
    CONDENSE im_unit NO-GAPS.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = im_unit
      IMPORTING
        output         = re_cunit
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_conversion_unknown_unit.
    ENDIF.
  ENDMETHOD.

  METHOD conv_date_internal.
    DATA: lv_input TYPE char12.
    CHECK im_value IS NOT INITIAL.

    lv_input = im_value.
*    REPLACE ALL OCCURRENCES OF '/' IN lv_input WITH '.' .

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_input
      IMPORTING
        date_internal            = re_date
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_conversion_no_date_time.
    ENDIF.
  ENDMETHOD.

  METHOD conv_sled_input.
    CALL FUNCTION 'CONVERSION_EXIT_PERKZ_INPUT'
      EXPORTING
        input  = im_value
      IMPORTING
        output = re_iprkz.
  ENDMETHOD.

  METHOD conv_char_to_num.
    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        chr             = im_value
      IMPORTING
        num             = re_amount
      EXCEPTIONS
        convt_no_number = 1
        convt_overflow  = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD conv_langu_input.
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = im_value
      IMPORTING
        output           = re_langu
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_conversion_unknown_langu.
    ENDIF.
  ENDMETHOD.

  METHOD conv_date_external.
    TYPES: BEGIN OF lts_date,
             year  TYPE char4,
             month TYPE char2,
             date  TYPE char2,
           END OF lts_date.
    DATA: ls_date TYPE lts_date.

    ls_date = im_value.

    CONCATENATE ls_date-date ls_date-month ls_date-year INTO re_date SEPARATED BY '.'.
  ENDMETHOD.

  METHOD conv_time_external.
    TYPES: BEGIN OF lts_time,
             hours TYPE char2,
             min   TYPE char2,
             sec   TYPE char2,
           END OF lts_time.
    DATA: ls_time TYPE lts_time.

    ls_time = im_value.
    CONCATENATE ls_time-hours ls_time-min ls_time-sec INTO re_time SEPARATED BY ':'.
  ENDMETHOD.

  METHOD conv_curr_internal.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = im_curr
        amount_internal = CONV bapicurr_d( im_price )
      IMPORTING
        amount_external = re_price.
  ENDMETHOD.
ENDCLASS. "end LCL_UTILS
*&---------------------------------------------------------------------*
*& CLASS  lcl_impl_bapi IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_impl_bapi IMPLEMENTATION.
  METHOD constructor.
    CHECK im_excel IS SUPPLIED.
    IF im_excel IS NOT BOUND.
      RAISE EXCEPTION TYPE cx_sy_create_object_error.
    ENDIF.
    o_excel = im_excel.
  ENDMETHOD.

  METHOD implement_bapi.
    CASE o_excel->v_upltyp.
      WHEN 'F01'.
        me->t_upload =  o_excel->t_template.
        me->impl_bapi_main_templ( CHANGING c_itab = me->t_upload ).
      WHEN 'F02'.
        me->t_matdes =  o_excel->t_matdes.
        me->impl_bapi_matdes( CHANGING c_itab = me->t_matdes ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_upload.
    re_itab = me->t_upload .
  ENDMETHOD.

  METHOD get_matdes.
    re_itab = me->t_matdes.
  ENDMETHOD.

  METHOD impl_bapi_main_templ.
    CHECK o_excel->t_template IS NOT INITIAL.
    me->check_valid( CHANGING c_itab = c_itab ).
    me->check_duplicate(  CHANGING  c_itab =  c_itab  ).
    me->creat_matnr(  CHANGING  c_itab =  c_itab  ).
    me->extend_matnr(  CHANGING  c_itab =  c_itab  ).
  ENDMETHOD.

  METHOD impl_bapi_matdes.
    CHECK o_excel->t_matdes IS NOT INITIAL.
    me->creat_matdescription( CHANGING c_itab = c_itab ).
  ENDMETHOD.

  METHOD creat_matdescription.
    DATA: ls_headerdata TYPE bapimathead.
    DATA: ls_makt TYPE bapi_makt,
          lt_makt TYPE STANDARD TABLE OF bapi_makt.
    DATA  ls_return TYPE  bapiret2.
    DATA: lt_line TYPE STANDARD TABLE OF zstmmgr3matdes WITH EMPTY KEY.
    DATA: lv_text TYPE text100.


    "§. Check material exist in DB
    SELECT DISTINCT
           c_itab~matnr,
           makt~spras
      FROM @c_itab AS c_itab
      INNER JOIN mara ON mara~matnr = c_itab~matnr
      LEFT  JOIN makt ON makt~matnr = c_itab~matnr
     INTO TABLE @DATA(lt_matnr).
    "If all materials do not exist in DB
    IF sy-subrc <> 0.
      LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>).
        <lfs_data>-status = 'E'.
        MESSAGE e040 INTO lv_text.
        APPEND lv_text TO <lfs_data>-t_mess.
        CLEAR lv_text.
        lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
        CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.
      ENDLOOP.
      RETURN.
    ENDIF.

    "CHECK duplicate key in file
    LOOP AT c_itab ASSIGNING <lfs_data> WHERE spras IS NOT INITIAL
                                        GROUP BY ( matnr  = <lfs_data>-matnr
                                                   spras  = <lfs_data>-spras )
                                        ASSIGNING FIELD-SYMBOL(<lfs_grp>) .
      lt_line = VALUE #( FOR m IN GROUP <lfs_grp> ( m ) ).

      IF lines( lt_line ) > 1.
        READ TABLE c_itab ASSIGNING FIELD-SYMBOL(<lfs_matdes>) WITH KEY matnr = <lfs_grp>-matnr
                                                                        spras = <lfs_grp>-spras.
        IF sy-subrc = 0.
          <lfs_matdes>-status = 'E'.
          MESSAGE e008 INTO lv_text.
          APPEND lv_text TO <lfs_matdes>-t_mess.
          CLEAR lv_text.
          lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_matdes> ).
          CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.
        ENDIF.
      ENDIF.
      CLEAR lt_line.
    ENDLOOP.

    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_group>) WHERE status IS INITIAL
                                                       GROUP BY ( matnr = <lfs_group>-matnr ).
      "Handle Material number not exist in DB to create description
      IF NOT line_exists( lt_matnr[ matnr = <lfs_group>-matnr ] ).
        LOOP AT GROUP <lfs_group> ASSIGNING <lfs_data>.
          <lfs_data>-status = 'E'.
          MESSAGE e040 INTO lv_text.
          APPEND lv_text TO <lfs_data>-t_mess.
          CLEAR lv_text.
          lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
          CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.
        ENDLOOP.
      ELSE. "Material number exist in DB
        LOOP AT GROUP <lfs_group> ASSIGNING <lfs_data> WHERE status IS INITIAL ##LOOP_ASSIGN.
          READ TABLE lt_matnr INTO DATA(ls_matnr) WITH KEY matnr = <lfs_data>-matnr
                                                           spras = <lfs_data>-spras.
          "Handle MATNR and LANGU already exist in DB.
          "(Delete) Business: need to update mat. description already exists with language
*          IF sy-subrc = 0.
*            <lfs_data>-status = 'E'.
*            MESSAGE e042 WITH ls_matnr-spras INTO lv_text.
*            APPEND lv_text TO <lfs_data>-t_mess.
*            CLEAR lv_text.
*            lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
*          ENDIF.
          "(endDelete)

          "Handle Key is initial
          IF <lfs_data>-spras IS INITIAL.
            <lfs_data>-status = 'E'.
            MESSAGE e043 INTO lv_text.
            APPEND lv_text TO <lfs_data>-t_mess.
            CLEAR lv_text.
            lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
          ENDIF.

          "Handle Description is initial
          IF <lfs_data>-maktx IS INITIAL.
            <lfs_data>-status = 'E'.
            MESSAGE e064 INTO lv_text.
            APPEND lv_text TO <lfs_data>-t_mess.
            CLEAR lv_text.
            lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
          ENDIF.

          "IF error exist continue to next entries, not allow implement BAPI.
          IF <lfs_data>-status = 'E'.
            CONTINUE.
          ENDIF.

          "§. Fill data to struct to implement BAPI
          ls_makt = VALUE #( langu     = <lfs_data>-spras
                             matl_desc = <lfs_data>-maktx
                           ).
          APPEND ls_makt TO lt_makt.
          CLEAR ls_makt.
          ls_headerdata-material = <lfs_data>-matnr.

          "§. Implement BAPI
          CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
            EXPORTING
              headdata            = ls_headerdata
            IMPORTING
              return              = ls_return
            TABLES
              materialdescription = lt_makt.
          "§. Handle return
          IF ls_return-type  <> 'S'. "Return fail
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            "Set message from BAPI and reflect to ALV
            READ TABLE c_itab ASSIGNING <lfs_data> WITH KEY matnr = <lfs_data>-matnr
                                                            spras = <lfs_data>-spras.
            IF sy-subrc = 0.
              <lfs_data>-status = 'E'.
              CONCATENATE ls_return-type '-' ls_return-id '-' ls_return-number '-' ls_return-message INTO lv_text.
              APPEND lv_text TO <lfs_data>-t_mess.
              CLEAR lv_text.
              lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
            ENDIF.
          ELSEIF ls_return-type = 'S'.  "Return success
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            "Set message and reflect to ALV
            READ TABLE c_itab ASSIGNING <lfs_data> WITH KEY matnr = <lfs_data>-matnr
                                                            spras = <lfs_data>-spras.
            IF sy-subrc = 0.
              <lfs_data>-status = 'S'.
              MESSAGE i041 WITH <lfs_data>-matnr INTO <lfs_data>-message ##MG_MISSING.
              lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = <lfs_data> ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      CLEAR: ls_headerdata, ls_return, lt_makt.
    ENDLOOP.

    "Concanate messages
    LOOP AT c_itab ASSIGNING <lfs_data> WHERE message IS INITIAL.
      CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.
    ENDLOOP.
  ENDMETHOD.

  METHOD creat_matnr.
    "BAPI MATERIAL SAVEDATA
    DATA: ls_headerdata           TYPE bapimathead,
          ls_clientdata           TYPE bapi_mara,
          ls_clientdatax          TYPE bapi_marax,
          ls_plantdata            TYPE bapi_marc,
          ls_plantdatax           TYPE bapi_marcx,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx,
          ls_salesdata            TYPE bapi_mvke,
          ls_salesdatax           TYPE bapi_mvkex,
          ls_valuationdata        TYPE bapi_mbew,
          ls_valuationdatax       TYPE bapi_mbewx.

    DATA  ls_return TYPE  bapiret2.

    DATA: lt_matkt              TYPE STANDARD TABLE OF bapi_makt,
          lt_unitsofmeasure     TYPE STANDARD TABLE OF bapi_marm,
          lt_unitsofmeasurex    TYPE STANDARD TABLE OF bapi_marmx,
          lt_taxclassifications TYPE STANDARD TABLE OF bapi_mlan.


    "BAPI CLASSIFICATION
    DATA: lv_objectkey   TYPE bapi1003_key-object,
          lv_objecttable TYPE bapi1003_key-objecttable VALUE 'MARA',
          lv_classnum    TYPE bapi1003_key-classnum,
          lv_classtype   TYPE bapi1003_key-classtype.

    DATA: lt_allocvaluesnumnew  TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
          lt_allocvaluescharnew TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
          lt_allocvaluescurrnew TYPE STANDARD TABLE OF bapi1003_alloc_values_curr.

    DATA: lt_return TYPE STANDARD TABLE OF bapiret2.

    DATA: ls_mess TYPE text255,
          lt_mess TYPE STANDARD TABLE OF text255.


    "Create new MATNR
    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE is_ext IS INITIAL AND status IS INITIAL.

      "§.1. Assign data for BAPI
      me->fill_bapiupl(
        EXPORTING
          is_data              = <lfs_data>
        IMPORTING
          headerdata           = ls_headerdata
          clientdata           = ls_clientdata
          clientdatax          = ls_clientdatax
          plantdata            = ls_plantdata
          plantdatax           = ls_plantdatax
          storagelocationdata  = ls_storagelocationdata
          storagelocationdatax = ls_storagelocationdatax
          salesdata            = ls_salesdata
          salesdatax           = ls_salesdatax
          valuationdata        = ls_valuationdata
          valuationdatax       = ls_valuationdatax
          t_matkt              = lt_matkt
          t_unitsofmeasure     = lt_unitsofmeasure
          t_unitsofmeasurex    = lt_unitsofmeasurex
          t_taxclassifications = lt_taxclassifications
      ).

      "§.2. Implement BAPI
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = ls_headerdata
          clientdata           = ls_clientdata
          clientdatax          = ls_clientdatax
          plantdata            = ls_plantdata
          plantdatax           = ls_plantdatax
          storagelocationdata  = ls_storagelocationdata
          storagelocationdatax = ls_storagelocationdatax
          valuationdata        = ls_valuationdata
          valuationdatax       = ls_valuationdatax
          salesdata            = ls_salesdata
          salesdatax           = ls_salesdatax
        IMPORTING
          return               = ls_return
        TABLES
          materialdescription  = lt_matkt
          unitsofmeasure       = lt_unitsofmeasure
          unitsofmeasurex      = lt_unitsofmeasurex
          taxclassifications   = lt_taxclassifications.

      "§.2.1. Create fail
      IF ls_return-type <> 'S'.
        <lfs_data>-status = 'E'.
        CONCATENATE ls_return-type '-' ls_return-id '-' ls_return-number '-' ls_return-message INTO <lfs_data>-message.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        "§.2.2. Create success
      ELSEIF ls_return-type = 'S'.
        <lfs_data>-status = 'S'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        MESSAGE i010 WITH <lfs_data>-matnr INTO <lfs_data>-message ##MG_MISSING.

        "§.2.2.1. Assign Classification for material which is created successfully
        IF <lfs_data>-klart IS INITIAL.
          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data>  ).
          CONTINUE.
        ENDIF.
        lv_objectkey = <lfs_data>-matnr.
        lv_classnum  = <lfs_data>-artxt.
        lv_classtype = <lfs_data>-klart.

        "§.2.2.1.1 Implement BAPI to create Classification
        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objectkey          = lv_objectkey
            objecttable        = lv_objecttable
            classnum           = lv_classnum
            classtype          = lv_classtype
*           STATUS             = '1'
          TABLES
            allocvaluesnumnew  = lt_allocvaluesnumnew
            allocvaluescharnew = lt_allocvaluescharnew
            allocvaluescurrnew = lt_allocvaluescurrnew
            return             = lt_return.
        IF NOT line_exists( lt_return[ type = 'E' ] ) . "assign classification success
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

        ELSE. "Handle assign classification fail
          LOOP AT lt_return INTO DATA(ls_objcl_return) WHERE type <> 'S'.
            ls_mess = ls_OBJCL_return-message.
            APPEND ls_mess TO lt_mess.
            CLEAR ls_mess.
          ENDLOOP.

          CONCATENATE LINES OF lt_mess INTO <lfs_data>-message.
          CLEAR lt_mess.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF. "END BAPI ASSIGN CLASSIFICATION
      ENDIF. "END BAPI CREATE MATNR

      "Set Icon
      lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data>  ).

      "Clear local variables after used
      CLEAR: ls_headerdata, ls_clientdata, ls_clientdatax,
             ls_plantdata, ls_plantdatax, ls_storagelocationdata, ls_storagelocationdatax,
             ls_valuationdata, ls_valuationdatax, ls_salesdata, ls_salesdatax, ls_return,
             lt_matkt, lt_unitsofmeasure, lt_unitsofmeasurex.
      CLEAR: lv_objectkey, lv_classnum, lv_classtype, lt_return.
    ENDLOOP.
  ENDMETHOD. "END CREAT_MATNR

  METHOD extend_matnr.
    DATA: ls_headerdata           TYPE bapimathead,
          ls_clientdata           TYPE bapi_mara,
          ls_clientdatax          TYPE bapi_marax,
          ls_plantdata            TYPE bapi_marc,
          ls_plantdatax           TYPE bapi_marcx,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx,
          ls_salesdata            TYPE bapi_mvke,
          ls_salesdatax           TYPE bapi_mvkex,
          ls_valuationdata        TYPE bapi_mbew,
          ls_valuationdatax       TYPE bapi_mbewx.
    DATA: lt_unitsofmeasure  TYPE STANDARD TABLE OF bapi_marm,
          lt_unitsofmeasurex TYPE STANDARD TABLE OF bapi_marmx.
    DATA: ls_return TYPE  bapiret2.
    DATA: lv_text TYPE text255.

    "Check MATNR existed in DB
    SELECT itab~matnr,
           mard~werks,
           mard~lgort
      FROM @c_itab AS itab
      INNER JOIN mara ON mara~matnr = itab~matnr
      LEFT  JOIN mard ON mard~matnr = mara~matnr
      WHERE itab~is_ext IS NOT INITIAL
     INTO TABLE @DATA(lt_extend).

    IF sy-subrc <> 0.
      "Handle if all materials do not exist in DB
      LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE is_ext IS NOT INITIAL.
        <lfs_data>-status = 'E'.
        MESSAGE e011 INTO <lfs_data>-message.
        lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data>  ).
      ENDLOOP.
      RETURN.
    ENDIF.


    LOOP AT c_itab ASSIGNING <lfs_data> WHERE is_ext IS NOT INITIAL AND status IS INITIAL.
      "§. Not exist in DB to extend
      IF NOT line_exists( lt_extend[ matnr = <lfs_data>-matnr ] ).
        <lfs_data>-status = 'E'.
        MESSAGE e011 INTO lv_text.
        APPEND lv_text TO <lfs_data>-t_mess. CLEAR lv_text.
        CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.

        "Material already extend to plant & sloc
      ELSEIF line_exists( lt_extend[ matnr = <lfs_data>-matnr werks = <lfs_data>-werks lgort = <lfs_data>-lgort ] ).
        <lfs_data>-status = 'E'.
        MESSAGE e044 WITH <lfs_data>-werks <lfs_data>-lgort INTO lv_text.
        APPEND lv_text TO <lfs_data>-t_mess. CLEAR lv_text.
        CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.

        "§. Implement BAPI to Extend Material
      ELSE.

        "§.1. Assign data for BAPI
        me->fill_bapiupl(
          EXPORTING
            is_data              = <lfs_data>
            iv_is_extend         = 'X'
          IMPORTING
            headerdata           = ls_headerdata
            clientdata           = ls_clientdata
            clientdatax          = ls_clientdatax
            plantdata            = ls_plantdata
            plantdatax           = ls_plantdatax
            storagelocationdata  = ls_storagelocationdata
            storagelocationdatax = ls_storagelocationdatax
            salesdata            = ls_salesdata
            salesdatax           = ls_salesdatax
            valuationdata        = ls_valuationdata
            valuationdatax       = ls_valuationdatax
        ).

        "§.2. Extend Material
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata             = ls_headerdata
            clientdata           = ls_clientdata
            clientdatax          = ls_clientdatax
            plantdata            = ls_plantdata
            plantdatax           = ls_plantdatax
            storagelocationdata  = ls_storagelocationdata
            storagelocationdatax = ls_storagelocationdatax
            valuationdata        = ls_valuationdata
            valuationdatax       = ls_valuationdatax
            salesdata            = ls_salesdata
            salesdatax           = ls_salesdatax
          IMPORTING
            return               = ls_return
          TABLES
            unitsofmeasure       = lt_unitsofmeasure
            unitsofmeasurex      = lt_unitsofmeasurex.
        IF ls_return-type  <> 'S'.
          <lfs_data>-status = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CONCATENATE ls_return-type '-' ls_return-id '-' ls_return-number '-' ls_return-message INTO lv_text.
          APPEND lv_text TO <lfs_data>-t_mess. CLEAR lv_text.
        ELSEIF ls_return-type = 'S'.
          <lfs_data>-status = 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          MESSAGE i012 WITH <lfs_data>-matnr INTO <lfs_data>-message ##MG_MISSING.
        ENDIF.
      ENDIF.

      lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data> ).
      CLEAR: ls_headerdata, ls_storagelocationdata, ls_storagelocationdatax.
    ENDLOOP.

    "Concatenate messages
    LOOP AT c_itab ASSIGNING <lfs_data> WHERE is_ext IS NOT INITIAL
                                          AND status <> 'S'.
      CONCATENATE LINES OF <lfs_data>-t_mess INTO <lfs_data>-message SEPARATED BY '|'.
    ENDLOOP.
  ENDMETHOD. "end EXTEND_MANTR

  METHOD check_valid.
    DATA lv_mess TYPE text50.

    "§ Validate madatatory data with Text tables
    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_upload>) WHERE is_ext IS INITIAL.
      "Validate Industry Sector
      READ TABLE gr_masterdata->m_t137 WITH KEY mbrsh = <lfs_upload>-mbrsh TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        <lfs_upload>-status = 'E'.
        MESSAGE e050 WITH 'Indus.Sector'  INTO lv_mess.
        APPEND lv_mess TO <lfs_upload>-t_mess.
        CLEAR lv_mess.
      ENDIF.

      "Validate Material Type
      READ TABLE gr_masterdata->m_t134 WITH KEY mtart = <lfs_upload>-mtart TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        <lfs_upload>-status = 'E'.
        MESSAGE e050 WITH 'MatType'  INTO lv_mess.
        APPEND lv_mess TO <lfs_upload>-t_mess.
        CLEAR lv_mess.
      ENDIF.

      "Validate Material Group
      READ TABLE gr_masterdata->m_t023 WITH KEY matkl = <lfs_upload>-matkl TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        <lfs_upload>-status = 'E'.
        MESSAGE e050 WITH 'MATGROUP'  INTO lv_mess.
        APPEND lv_mess TO <lfs_upload>-t_mess.
        CLEAR lv_mess.
      ENDIF.

      "Validate Sale Organization
      READ TABLE gr_masterdata->m_tvko WITH KEY vkorg = <lfs_upload>-vkorg TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        <lfs_upload>-status = 'E'.
        MESSAGE e050 WITH 'SaleOrg'  INTO lv_mess.
        APPEND lv_mess TO <lfs_upload>-t_mess.
        CLEAR lv_mess.
      ENDIF.

      "Validate Distribution Channel
      READ TABLE gr_masterdata->m_TVTW WITH KEY vtweg = <lfs_upload>-vtweg TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        <lfs_upload>-status = 'E'.
        MESSAGE e050 WITH 'Dist.Channel'  INTO lv_mess.
        APPEND lv_mess TO <lfs_upload>-t_mess.
        CLEAR lv_mess.
      ENDIF.

      "Set status icon and material type color
      lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_upload> ).
      lcl_build_fcat=>set_mtart_color( CHANGING ch_alv = <lfs_upload> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_duplicate.
    DATA: lt_line TYPE STANDARD TABLE OF zstmmgr03upl,
          lv_text TYPE text255.

    "CHECK duplicate key in DB.
    SELECT itab~matnr, itab~is_ext
      FROM @c_itab AS itab
      INNER JOIN mara ON mara~matnr = itab~matnr
      WHERE itab~is_ext IS INITIAL
     INTO TABLE @DATA(lt_check_dupl).

    IF sy-subrc = 0.
      LOOP AT lt_check_dupl INTO DATA(ls_dupl).
        LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_alv>) WHERE matnr = ls_dupl-matnr AND is_ext IS INITIAL.
          <lfs_alv>-status = 'D'.
          MESSAGE i009 INTO <lfs_alv>-message.
          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_alv> ).
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    "CHECK duplicate key in file
    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_upload>) GROUP BY ( matnr = <lfs_upload>-matnr  )
                                                        ASSIGNING FIELD-SYMBOL(<lfs_group>) .
      lt_line = VALUE #( FOR m IN GROUP <lfs_group> ( m ) ).

      IF lines( lt_line ) > 1.
        LOOP AT me->t_upload ASSIGNING <lfs_alv> WHERE matnr = <lfs_group>-matnr .
          <lfs_alv>-status = 'E'.
          MESSAGE e008 INTO lv_text.
          APPEND lv_text TO <lfs_alv>-t_mess.
          CLEAR lv_text.
          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_alv> ).
        ENDLOOP.
        CLEAR lt_line.
      ENDIF.
    ENDLOOP.

    "CONCATENATE error message
    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_mess>) WHERE status = 'E' AND is_ext IS INITIAL.
      SORT <lfs_mess>-t_mess.
      DELETE ADJACENT DUPLICATES FROM <lfs_mess>-t_mess.
      CONCATENATE LINES OF <lfs_mess>-t_mess INTO <lfs_mess>-message SEPARATED BY '|'.
    ENDLOOP.
  ENDMETHOD. "end check_duplicate

  METHOD add_valtyp.
    DATA: ls_headerdata     TYPE bapimathead,
          ls_valuationdata  TYPE bapi_mbew,
          ls_valuationdatax TYPE bapi_mbewx.
    DATA: ls_return TYPE  bapiret2,
          ls_msg    TYPE bal_s_msg.
    CONSTANTS lc_messclass TYPE syst_msgid VALUE 'ZMGR03'.


    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>).
      "Check Valuation Class
      READ TABLE gt_check_valtyp TRANSPORTING NO FIELDS WITH KEY matnr = <lfs_data>-matnr
                                                                 bwkey = <lfs_data>-werks
                                                                 bwtar = <lfs_data>-bwtar
                                                                 bklas = <lfs_data>-bklas.
      IF sy-subrc = 0.
        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 069.
        ls_msg-msgty = 'E'.
        ls_msg-msgv1 = <lfs_data>-matnr.
        ls_msg-msgv1 = <lfs_data>-bwtar.
        ls_msg-msgv1 = <lfs_data>-bklas.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
        CLEAR ls_msg.
        CONTINUE.
      ENDIF.

      "Check field initial
      IF <lfs_data>-bwtar   IS INITIAL OR <lfs_data>-bklas IS INITIAL OR
         <lfs_data>-pvprs_1 IS INITIAL.

        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 054.
        ls_msg-msgty = 'E'.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
        CLEAR ls_msg.
        CONTINUE.
      ENDIF.

      "Init data for bapi
      DATA: lv_price TYPE bapicurr.
      ls_headerdata = VALUE #( material = <lfs_data>-matnr  account_view = 'X' ).
      ls_valuationdata  = VALUE #( val_area  = <lfs_data>-werks   val_type  = <lfs_data>-bwtar
                                   val_class = <lfs_data>-bklas
                                   moving_pr = lcl_utils=>conv_curr_internal( EXPORTING
                                                  im_curr = <lfs_data>-waers
                                                  im_price = <lfs_data>-pvprs_1 )
                                 ).
      ls_valuationdatax = VALUE #( val_area  = <lfs_data>-werks  val_type  = <lfs_data>-bwtar
                                   val_class = 'X'               moving_pr = 'X'
                                 ).
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata       = ls_headerdata
          valuationdata  = ls_valuationdata
          valuationdatax = ls_valuationdatax
        IMPORTING
          return         = ls_return.
      IF ls_return-type <> 'S'.
        ls_msg-msgid = ls_return-id.
        ls_msg-msgno = ls_return-number.
        ls_msg-msgty = ls_return-type.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 053.
        ls_msg-msgty = 'S'.
        ls_msg-msgv1 = <lfs_data>-matnr.
        ls_msg-msgv2 = <lfs_data>-werks.
        "Reflect on ALV
        PERFORM reflect_alv_success USING <lfs_data>-matnr.
      ENDIF.

      IF ls_msg IS NOT INITIAL.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extend_dmatnr.
    DATA: ls_headerdata           TYPE bapimathead,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx,
          ls_plandata             TYPE bapi_marc,
          ls_plandatax            TYPE bapi_marcx.
    DATA: ls_valuationdata  TYPE bapi_mbew,
          ls_valuationdatax TYPE bapi_mbewx.
    DATA: ls_return TYPE  bapiret2.
    DATA: ls_msg TYPE bal_s_msg.
    CONSTANTS lc_messclass TYPE syst_msgid VALUE 'ZMGR03'.

    "Check MATNR existed in DB
    SELECT itab~matnr,
           mard~werks,
           mard~lgort
      FROM @c_itab AS itab
      INNER JOIN mara ON mara~matnr = itab~matnr
      LEFT  JOIN mard ON mard~matnr = mara~matnr
     INTO TABLE @DATA(lt_extend).


    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE status <> 'E'.
      READ TABLE lt_extend TRANSPORTING NO FIELDS WITH KEY matnr = <lfs_data>-matnr
                                                           werks = <lfs_data>-werks
                                                           lgort = <lfs_data>-lgort.
      IF sy-subrc = 0.
        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 044.
        ls_msg-msgty = 'E'.
        ls_msg-msgv1 = <lfs_data>-werks.
        ls_msg-msgv2 = <lfs_data>-lgort.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
        CONTINUE.
      ENDIF.

      "Check input is initial
      IF <lfs_data>-werks IS INITIAL.
        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 058.
        ls_msg-msgty = 'E'.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
        CONTINUE.
      ENDIF.


      "§. Init data for bapi
      "Header
      ls_headerdata = VALUE #( material = <lfs_data>-matnr storage_view =  'X'
                               account_view = COND #( WHEN <lfs_data>-bwtty IS NOT INITIAL THEN 'X' ) ).
      "Storage Location
      IF <lfs_data>-lgort IS NOT INITIAL.
        ls_storagelocationdata  = VALUE #( plant = <lfs_data>-werks  stge_loc = <lfs_data>-lgort  ).
        ls_storagelocationdatax = VALUE #( plant = <lfs_data>-werks  stge_loc = <lfs_data>-lgort ).
        "Plant
      ELSE.
        ls_plandata  = VALUE #( plant = <lfs_data>-werks ).
        ls_plandatax = VALUE #( plant = <lfs_data>-werks ).
      ENDIF.

      "Valuation Category
      IF <lfs_data>-bwtty IS NOT INITIAL.
        ls_valuationdata  = VALUE #( val_area  = <lfs_data>-werks  val_cat   = <lfs_data>-bwtty
                                     val_class = <lfs_data>-bklas  moving_pr = <lfs_data>-pvprs_1 ).
        ls_valuationdatax = VALUE #( val_area  = <lfs_data>-werks  val_cat   = 'X'
                                     val_class = 'X'               moving_pr = 'X').
      ENDIF.

      "§. Implement BAPI
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = ls_headerdata
          plantdata            = ls_plandata
          plantdatax           = ls_plandatax
          storagelocationdata  = ls_storagelocationdata
          storagelocationdatax = ls_storagelocationdatax
          valuationdata        = ls_valuationdata
          valuationdatax       = ls_valuationdatax
        IMPORTING
          return               = ls_return.
      "Handle return of BAPI
      IF ls_return-type <> 'S'.
*        <lfs_data>-status = 'E'.
        ls_msg-msgid = ls_return-id.
        ls_msg-msgno = ls_return-number.
        ls_msg-msgty = ls_return-type.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "Reflect on ALV
*        PERFORM reflect_alv_err USING <lfs_data>-matnr <lfs_data>-message.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 015.
        ls_msg-msgty = 'S'.
        ls_msg-msgv1 =  <lfs_data>-matnr.

        "Reflect on ALV
        PERFORM reflect_alv_success USING <lfs_data>-matnr.
      ENDIF.

      "Cumulate message for BAL_LOG
      IF ls_msg IS NOT INITIAL.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_bapiupl.
    DATA: ls_oum TYPE zstmmgr03oum.
    DATA: ls_unitsofmeasure  TYPE bapi_marm,
          ls_unitsofmeasurex TYPE bapi_marmx.
    DATA: ls_taxclassifications TYPE bapi_mlan.
    DATA: lv_flg_not_extend TYPE flag VALUE IS INITIAL.

    "Using flag to mark Extend or Create
    " If lv_flg_not_extend = 'X' => Create
    " If lv_flg_not_extend = ''  => Extend
    lv_flg_not_extend = COND #( WHEN iv_is_extend IS NOT INITIAL
                                THEN lv_flg_not_extend
                                ELSE 'X' ).

    "Org. level + "Purchasing
    plantdata  = VALUE #( plant = is_data-werks
                          pur_group  = is_data-ekgrp
                          plnd_delry = is_data-plifz
                          gr_pr_time = is_data-webaz
                          sourcelist = is_data-kordb
                          ind_post_to_insp_stock = is_data-insmk
                          issue_unit = is_data-ausme
                          stgeperiod = is_data-maxlz
                          stge_pd_un = is_data-lzeih
                          availcheck = is_data-mtvfp
                        ).
    plantdatax = VALUE #( plant = is_data-werks
                          pur_group  = COND #( WHEN plantdata-pur_group  IS NOT INITIAL THEN 'X')
                          plnd_delry = COND #( WHEN plantdata-plnd_delry IS NOT INITIAL THEN 'X')
                          gr_pr_time = COND #( WHEN plantdata-gr_pr_time IS NOT INITIAL THEN 'X')
                          sourcelist = COND #( WHEN plantdata-sourcelist IS NOT INITIAL THEN 'X')
                          ind_post_to_insp_stock = COND #( WHEN plantdata-ind_post_to_insp_stock IS NOT INITIAL THEN 'X')
                          issue_unit = COND #( WHEN plantdata-issue_unit IS NOT INITIAL THEN 'X')
                          stgeperiod = COND #( WHEN plantdata-stgeperiod IS NOT INITIAL THEN 'X')
                          stge_pd_un = COND #( WHEN plantdata-stge_pd_un IS NOT INITIAL THEN 'X')
                          availcheck = COND #( WHEN plantdata-availcheck IS NOT INITIAL THEN 'X')
                           ).

    CLEAR storagelocationdata.
    IF is_data-lgort IS NOT INITIAL .
      storagelocationdata  = VALUE #( plant = is_data-werks stge_loc = is_data-lgort ).
      storagelocationdatax = VALUE #( plant = storagelocationdata-plant
                                      stge_loc  = is_data-lgort ).
    ENDIF.

    "Basic data + "Purchasing
    t_matkt = VALUE #( ( langu = COND #( WHEN is_data-spras IS NOT INITIAL
                                         THEN is_data-spras
                                         ELSE sy-langu )
                         matl_desc = is_data-maktx )                         ).

    clientdata = VALUE #( matl_group = is_data-matkl
                          base_uom   = is_data-meins
                          item_cat   = is_data-mtpos_mara
                          size_dim   = is_data-groes
                          unit_of_wt = is_data-gewei
                          net_weight = is_data-ntgew
                          division   = is_data-spart
                          po_unit    = is_data-bstme
                          batch_mgmt = is_data-xchpf
                          pur_valkey = is_data-ekwsl
                          stor_conds = is_data-raube
                          container  = is_data-behvo
                          temp_conds = is_data-tempb
                          minremlife = is_data-mhdrz
                          period_ind_expiration_date = is_data-iprkz
                        ).
    clientdatax = VALUE #( matl_group = COND #( WHEN clientdata-matl_group IS NOT INITIAL THEN 'X' )
                           base_uom   = COND #( WHEN clientdata-base_uom   IS NOT INITIAL THEN 'X' )
                           item_cat   = COND #( WHEN clientdata-item_cat   IS NOT INITIAL THEN 'X' )
                           size_dim   = COND #( WHEN clientdata-size_dim   IS NOT INITIAL THEN 'X' )
                           unit_of_wt = COND #( WHEN clientdata-unit_of_wt IS NOT INITIAL THEN 'X' )
                           net_weight = COND #( WHEN clientdata-net_weight IS NOT INITIAL THEN 'X' )
                           division   = COND #( WHEN clientdata-division   IS NOT INITIAL THEN 'X' )
                           po_unit    = COND #( WHEN clientdata-po_unit    IS NOT INITIAL THEN 'X' )
                           batch_mgmt = COND #( WHEN clientdata-batch_mgmt IS NOT INITIAL THEN 'X' )
                           pur_valkey = COND #( WHEN clientdata-pur_valkey IS NOT INITIAL THEN 'X' )
                           stor_conds = COND #( WHEN clientdata-stor_conds IS NOT INITIAL THEN 'X' )
                           container  = COND #( WHEN clientdata-container  IS NOT INITIAL THEN 'X' )
                           temp_conds = COND #( WHEN clientdata-temp_conds IS NOT INITIAL THEN 'X' )
                           minremlife = COND #( WHEN clientdata-minremlife IS NOT INITIAL THEN 'X' ) ).

    "Sales Org.
    salesdata =  VALUE #( sales_org  = is_data-vkorg
                          distr_chan = is_data-vtweg
                          sales_unit = is_data-vrkme
                          delyg_plnt = is_data-dwerk
                          cash_disc  = is_data-sktof
                          min_order  = is_data-aumng
                          min_dely   = is_data-lfmng
                          dely_unit  = is_data-scmng
                          dely_uom   = is_data-schme    ).
    salesdatax = VALUE #( sales_org  = is_data-vkorg
                          distr_chan = is_data-vtweg
                          sales_unit = COND #( WHEN salesdata-sales_unit IS NOT INITIAL THEN 'X' )
                          delyg_plnt = COND #( WHEN salesdata-delyg_plnt IS NOT INITIAL THEN 'X' )
                          cash_disc  = COND #( WHEN salesdata-cash_disc  IS NOT INITIAL THEN 'X' )
                          min_order  = COND #( WHEN salesdata-min_order  IS NOT INITIAL THEN 'X' )
                          min_dely   = COND #( WHEN salesdata-min_dely   IS NOT INITIAL THEN 'X' )
                          dely_unit  = COND #( WHEN salesdata-dely_unit  IS NOT INITIAL THEN 'X' )
                          dely_uom   = COND #( WHEN salesdata-dely_uom   IS NOT INITIAL THEN 'X' ) ).

    "Tax Classification
    LOOP AT me->o_excel->t_tax INTO DATA(lgr_tax) WHERE matnr = is_data-matnr
                                                  GROUP BY ( matnr = lgr_tax-matnr ).
      LOOP AT GROUP lgr_tax ASSIGNING FIELD-SYMBOL(<lfs_tax>).
        MOVE-CORRESPONDING <lfs_tax> TO ls_taxclassifications.
        APPEND ls_taxclassifications TO t_taxclassifications.
        CLEAR ls_taxclassifications.
      ENDLOOP.
    ENDLOOP.

    "Accounting
    valuationdata =  VALUE #(  val_area   = is_data-bwkey
                               ml_settle  = is_data-mlast
                               val_class  = is_data-bklas
                               price_ctrl = is_data-vprsv
                               price_unit = is_data-vjpei
                               moving_pr  = is_data-verpr
                               std_price  = is_data-stprs
                               future_pr  = is_data-zkprs
                               valid_from = is_data-zkdat
                               val_cat    = is_data-bwtty
                            ).
    valuationdatax = VALUE #(  val_area   = is_data-bwkey
                               ml_settle  = COND #( WHEN valuationdata-ml_settle  IS NOT INITIAL THEN 'X' )
                               val_class  = COND #( WHEN valuationdata-val_class  IS NOT INITIAL THEN 'X' )
                               price_ctrl = COND #( WHEN valuationdata-price_ctrl IS NOT INITIAL THEN 'X' )
                               price_unit = COND #( WHEN valuationdata-price_unit IS NOT INITIAL THEN 'X' )
                               moving_pr  = COND #( WHEN valuationdata-moving_pr  IS NOT INITIAL THEN 'X' )
                               std_price  = COND #( WHEN valuationdata-std_price  IS NOT INITIAL THEN 'X' )
                               future_pr  = COND #( WHEN valuationdata-future_pr  IS NOT INITIAL THEN 'X' )
                               valid_from = COND #( WHEN valuationdata-valid_from IS NOT INITIAL THEN 'X' )
                               val_cat    = COND #( WHEN valuationdata-val_cat    IS NOT INITIAL THEN 'X' ) ).

    "UOM
    CLEAR t_unitsofmeasure.
    LOOP AT me->o_excel->t_uom INTO ls_oum WHERE matnr = is_data-matnr.
      ls_unitsofmeasure = CORRESPONDING #( ls_oum  MAPPING alt_unit   = meinh
                                                           denominatr = umren
                                                           numerator  = umrez
                                                           ean_upc    = ean11
                                                           ean_cat    = numtp
                                                           gross_wt   = brgew
                                                           unit_of_wt = gewei
                                                           length     = laeng
                                                           width      = breit
                                                           height     = hoehe
                                                           unit_dim   = meabm
                                                           volume     = volume
                                                           volumeunit = voleh
                                                           capacity_usage      = capause
                                          ).
      APPEND ls_unitsofmeasure TO t_unitsofmeasure.
      CLEAR ls_unitsofmeasure.
    ENDLOOP.
    APPEND VALUE #( alt_unit   = is_data-meins
                    gross_wt   = is_data-brgew
                    unit_of_wt = is_data-gewei
                   ) TO t_unitsofmeasure.

    CLEAR t_unitsofmeasurex.
    LOOP AT t_unitsofmeasure INTO ls_unitsofmeasure.
      ls_unitsofmeasurex = VALUE #( alt_unit   = ls_unitsofmeasure-alt_unit
                                    denominatr = COND #( WHEN ls_unitsofmeasure-denominatr
                                                              IS NOT INITIAL THEN 'X' )
                                    numerator  = COND #( WHEN ls_unitsofmeasure-numerator
                                                              IS NOT INITIAL THEN 'X' )
                                    ean_upc    = COND #( WHEN ls_unitsofmeasure-ean_upc
                                                              IS NOT INITIAL THEN 'X' )
                                    ean_cat    = COND #( WHEN ls_unitsofmeasure-ean_cat
                                                              IS NOT INITIAL THEN 'X' )
                                    gross_wt   = COND #( WHEN ls_unitsofmeasure-gross_wt
                                                              IS NOT INITIAL THEN 'X' )  "is_data?????
                                    unit_of_wt = COND #( WHEN ls_unitsofmeasure-unit_of_wt
                                                              IS NOT INITIAL THEN 'X' )
                                    length     = COND #( WHEN ls_unitsofmeasure-length
                                                              IS NOT INITIAL THEN 'X' )
                                    width      = COND #( WHEN ls_unitsofmeasure-width
                                                              IS NOT INITIAL THEN 'X' )
                                    height     = COND #( WHEN ls_unitsofmeasure-height
                                                              IS NOT INITIAL THEN 'X' )
                                    unit_dim   = COND #( WHEN ls_unitsofmeasure-unit_dim
                                                              IS NOT INITIAL THEN 'X' )
                                    volume     = COND #( WHEN ls_unitsofmeasure-volume
                                                              IS NOT INITIAL THEN 'X' )
                                    volumeunit = COND #( WHEN ls_unitsofmeasure-volumeunit
                                                              IS NOT INITIAL THEN 'X' )
                                    capacity_usage  = COND #( WHEN ls_unitsofmeasure-capacity_usage
                                                              IS NOT INITIAL THEN 'X' ) ).

      APPEND ls_unitsofmeasurex TO t_unitsofmeasurex.
      CLEAR ls_unitsofmeasurex.
    ENDLOOP.

    "header
    headerdata = VALUE #( material  = is_data-matnr ind_sector = is_data-mbrsh
                          matl_type = is_data-mtart basic_view = lv_flg_not_extend
                          sales_view    = COND #( WHEN salesdata     IS NOT INITIAL THEN 'X' )
                          account_view  = COND #( WHEN valuationdata IS NOT INITIAL THEN 'X' )
                          purchase_view = COND #( WHEN plantdata     IS NOT INITIAL THEN 'X' )
                          storage_view  = COND #( WHEN storagelocationdata IS NOT INITIAL THEN 'X' ) ).
  ENDMETHOD.
ENDCLASS. "end LCL_IMPL_BAPI
*&---------------------------------------------------------------------*
*& CLASS  lcl_impl_bapi_d IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_impl_bapi_d IMPLEMENTATION.
  METHOD get_instance.
    IF mo_imp_bapi IS NOT BOUND.
      CREATE OBJECT mo_imp_bapi.
    ENDIF.

    r_instance = mo_imp_bapi.
  ENDMETHOD.

  METHOD delete_matnr.
    DATA: ls_headerdata  TYPE bapimathead.
    DATA: ls_clientdata           TYPE bapi_mara,
          ls_clientdatax          TYPE bapi_marax,
          ls_plantdata            TYPE bapi_marc,
          ls_plantdatax           TYPE bapi_marcx,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx.
    DATA: ls_return TYPE  bapiret2.

    DATA: BEGIN OF lty_symsgv200,
            msgv1 TYPE msgv1,
            msgv2 TYPE msgv2,
            msgv3 TYPE msgv3,
            msgv4 TYPE msgv4,
          END OF lty_symsgv200.
    DATA: ls_msg TYPE bal_s_msg.
    CONSTANTS lc_messclass TYPE syst_msgid VALUE 'ZMGR03'.


    "§1. Mark for deletion
    IF i_mark IS NOT INITIAL.
      LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>).
        CLEAR: <lfs_data>-status.
        "§1.1. Material aldready deleted.
        IF <lfs_data>-del_flag = 'X'.
          <lfs_data>-status = 'E'.

          ls_msg-msgid = lc_messclass.
          ls_msg-msgno = 017.
          ls_msg-msgv1 = <lfs_data>-matnr.
          ls_msg-msgty = 'E'.

          IF ls_msg IS NOT INITIAL.
            gr_bal_log->bal_log_msg_cumulate( ls_msg ).
          ENDIF.
          CLEAR: ls_msg, lty_symsgv200.
          CONTINUE.
        ENDIF.

        "§1.2. Fill data for bapi structure
        "§1.2.1 Flag at client level
        IF <lfs_data>-werks IS INITIAL.
          ls_clientdata  = VALUE #( del_flag = 'X' ).
          ls_clientdatax = VALUE #( del_flag = 'X' ).

          "§1.2.2 Flag at plant level
        ELSEIF <lfs_data>-lgort IS INITIAL.
          ls_plantdata  = VALUE #( plant = <lfs_data>-werks del_flag = 'X' ).
          ls_plantdatax = VALUE #( plant = <lfs_data>-werks del_flag = 'X' ).

          "§1.2.3 Flag at storage location level
        ELSEIF <lfs_data>-lgort IS NOT INITIAL.
          ls_storagelocationdata  = VALUE #( plant = <lfs_data>-werks stge_loc
                                                   = <lfs_data>-lgort del_flag = 'X' ).
          ls_storagelocationdatax = VALUE #( plant = <lfs_data>-werks stge_loc
                                                   = <lfs_data>-lgort del_flag = 'X' ).
        ENDIF.

        ls_headerdata = VALUE #( material      = <lfs_data>-matnr
                                 basic_view    = COND #( WHEN ls_clientdata IS NOT INITIAL THEN 'X' )
                                 purchase_view = COND #( WHEN ls_plantdata  IS NOT INITIAL THEN 'X' )
                                 storage_view  = COND #( WHEN ls_storagelocationdata IS NOT INITIAL THEN 'X' ) ).

        "§1.3. Implement bapi to delete material
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata             = ls_headerdata
            clientdata           = ls_clientdata
            clientdatax          = ls_clientdatax
            plantdata            = ls_plantdata
            plantdatax           = ls_plantdatax
            storagelocationdata  = ls_storagelocationdata
            storagelocationdatax = ls_storagelocationdatax
          IMPORTING
            return               = ls_return.
        IF ls_return-type  <> 'S'.
          <lfs_data>-status = 'E'.
          lty_symsgv200 = ls_return-message.
          MOVE-CORRESPONDING lty_symsgv200 TO ls_msg.
          ls_msg-msgid = ls_return-id.
          ls_msg-msgno = ls_return-number.
          ls_msg-msgty = ls_return-type.
          CLEAR lty_symsgv200.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        ELSEIF ls_return-type = 'S'.
          <lfs_data>-del_flag = 'X'.
          <lfs_data>-status = 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          ls_msg-msgid = lc_messclass.
          ls_msg-msgno = 019.
          ls_msg-msgv1 = <lfs_data>-matnr.
          ls_msg-msgty = 'S'.
        ENDIF.

        <lfs_data>-del_flag_level =  VALUE #( client = ls_clientdata-del_flag
                                              plant  = ls_plantdata-del_flag
                                              sloc   = ls_storagelocationdata-del_flag ).

        "§1.4. Append to bal_log
        IF ls_msg IS NOT INITIAL.
          gr_bal_log->bal_log_msg_cumulate( ls_msg ).
        ENDIF.
        CLEAR: ls_headerdata, ls_clientdata,
               ls_clientdata, ls_clientdatax,
               ls_plantdata, ls_plantdatax,
               ls_storagelocationdata, ls_storagelocationdatax,
               lty_symsgv200, ls_msg.
      ENDLOOP. "End mark for deletion

      "§2. Unmark for deletion
    ELSE.
      LOOP AT c_itab ASSIGNING <lfs_data>.
        "§2.1. Material aldready unmark.
        IF <lfs_data>-del_flag <> 'X'.
          <lfs_data>-status = 'E'.

          ls_msg-msgid = lc_messclass.
          ls_msg-msgno = 018.
          ls_msg-msgv1 = <lfs_data>-matnr.
          ls_msg-msgty = 'E'.

          IF ls_msg IS NOT INITIAL.
            gr_bal_log->bal_log_msg_cumulate( ls_msg ).
          ENDIF.
          CLEAR: ls_msg.
          CONTINUE.
        ENDIF.

        "§2.2. Fill data for bapi structures
        "§2.2.1. Unmark at client level
        IF <lfs_data>-del_flag_level-client IS NOT INITIAL.
          ls_clientdata  = VALUE #( del_flag = '' ).
          ls_clientdatax = VALUE #( del_flag = 'X' ).

          "§2.2.2. Unmark at plant level
        ELSEIF <lfs_data>-del_flag_level-plant IS NOT INITIAL.
          ls_plantdata  = VALUE #( plant = <lfs_data>-werks del_flag = '' ).
          ls_plantdatax = VALUE #( plant = <lfs_data>-werks del_flag = 'X' ).

          "§2.2.3. Unmark at storage location level
        ELSEIF <lfs_data>-del_flag_level-sloc  IS NOT INITIAL.
          ls_storagelocationdata  = VALUE #( plant = <lfs_data>-werks stge_loc = <lfs_data>-lgort del_flag = '' ).
          ls_storagelocationdatax = VALUE #( plant = <lfs_data>-werks stge_loc = <lfs_data>-lgort del_flag = 'X' ).
        ENDIF.

        ls_headerdata  = VALUE #( material      = <lfs_data>-matnr
                                  basic_view    = COND #( WHEN ls_clientdatax IS NOT INITIAL THEN 'X' )
                                  purchase_view = COND #( WHEN ls_plantdatax  IS NOT INITIAL THEN 'X' )
                                  storage_view  = COND #( WHEN ls_storagelocationdatax IS NOT INITIAL THEN 'X' ) ).

        "§2.3. Implement bapi to revert deleted material
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata             = ls_headerdata
            clientdata           = ls_clientdata
            clientdatax          = ls_clientdatax
            plantdata            = ls_plantdata
            plantdatax           = ls_plantdatax
            storagelocationdata  = ls_storagelocationdata
            storagelocationdatax = ls_storagelocationdatax
          IMPORTING
            return               = ls_return.
        IF ls_return-type  <> 'S'.
          <lfs_data>-status = 'E'.

          lty_symsgv200 = ls_return-message.
          MOVE-CORRESPONDING lty_symsgv200 TO ls_msg.
          ls_msg-msgid = ls_return-id.
          ls_msg-msgno = ls_return-number.
          ls_msg-msgty = ls_return-type.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSEIF ls_return-type = 'S'.
          CLEAR <lfs_data>-del_flag.
          <lfs_data>-status = 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          <lfs_data>-del_flag_level =  VALUE #( client = ls_clientdata-del_flag
                                                plant  = ls_plantdata-del_flag
                                                sloc   = ls_storagelocationdata-del_flag ).

          ls_msg-msgid = lc_messclass.
          ls_msg-msgno = 020.
          ls_msg-msgv1 = <lfs_data>-matnr.
          ls_msg-msgty = 'S'.
        ENDIF.

        "§2.4. Append to bal_log
        IF ls_msg IS NOT INITIAL.
          gr_bal_log->bal_log_msg_cumulate( ls_msg ).
        ENDIF.
        CLEAR: ls_headerdata, ls_clientdata,
               ls_clientdata, ls_clientdatax,
               ls_plantdata, ls_plantdatax,
               ls_storagelocationdata, ls_storagelocationdatax,
               lty_symsgv200, ls_msg.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS. "end LCL_IMPL_BAPI_D
*&---------------------------------------------------------------------*
*& CLASS  lcl_bal_log IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_bal_log IMPLEMENTATION.
  METHOD constructor.
    me->init_bal_log( ).
  ENDMETHOD.

  METHOD init_bal_log.
    ms_bal_log-bal_s_log-extnumber = TEXT-t07.
    ms_bal_log-bal_s_log-aluser    = sy-uname.
    ms_bal_log-bal_s_log-alprog    = sy-repid.
  ENDMETHOD.

  METHOD bal_log_create.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ms_bal_log-bal_s_log
      IMPORTING
        e_log_handle            = ms_bal_log-log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD bal_log_msg_cumulate.
    CALL FUNCTION 'BAL_LOG_MSG_CUMULATE'
      EXPORTING
        i_log_handle     = ms_bal_log-log_handle
        i_s_msg          = i_s_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD bal_log_display.
    DATA: ls_display_profile TYPE bal_s_prof,
          lt_log_handle      TYPE bal_t_logh.

    "Click Cancel -> Not display Bal log
    IF sy-ucomm = '&AC1'.
      RETURN.
    ENDIF.

    "Define which BAL_LOG
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile.

    "Define length of message output
    READ TABLE ls_display_profile-mess_fcat WITH KEY ref_field = 'T_MSG' ASSIGNING FIELD-SYMBOL(<l_mess_fcat>).
    IF sy-subrc = 0.
      <l_mess_fcat>-outputlen = '000200'.
    ENDIF.

    INSERT ms_bal_log-log_handle INTO TABLE lt_log_handle.

    "Display BAL_LOG
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = ls_display_profile
        i_t_log_handle       = lt_log_handle
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Refresh BAL_LOG after Display
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = ms_bal_log-log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS. "end BAL_LOG
*&---------------------------------------------------------------------*
*& CLASS  lcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed .
    PERFORM modify_f4_text USING er_data_changed.
  ENDMETHOD.

  METHOD on_e_data_changed .
    PERFORM validate_input USING er_data_changed.
  ENDMETHOD.

  METHOD on_valtyp_data_changed.
    PERFORM validate_price USING er_data_changed.
  ENDMETHOD.

ENDCLASS. "end lcl_event_handler
*&---------------------------------------------------------------------*
*& CLASS  lcl_masterdata IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_masterdata IMPLEMENTATION.
  METHOD constructor.
    me->lif_description~set_t001l( ).
    me->lif_description~set_t001W( ).
    me->lif_description~set_t137( ).
    me->lif_description~set_t134( ).
    me->lif_description~set_t023( ).
    me->lif_description~set_tvko( ).
    me->lif_description~set_tvtw( ).
  ENDMETHOD.

  METHOD lif_description~set_t001l.
    SELECT werks,
           lgort,
           lgobe
      FROM t001l
     INTO TABLE @me->m_t001l.
  ENDMETHOD.

  METHOD lif_description~set_t001w.
    SELECT werks,
           name1
    FROM t001w
   INTO TABLE @me->m_t001W.
  ENDMETHOD.

  METHOD lif_description~set_t137.
    SELECT mbrsh
      FROM t137
    INTO TABLE @me->m_t137.
  ENDMETHOD.

  METHOD lif_description~set_t134.
    SELECT mtart
      FROM t134
    INTO TABLE @me->m_t134.
  ENDMETHOD.

  METHOD lif_description~set_t023.
    SELECT matkl
      FROM t023
    INTO TABLE @me->m_t023.
  ENDMETHOD.

  METHOD lif_description~set_tvko.
    SELECT vkorg
      FROM tvko
     INTO TABLE @me->m_tvko.
  ENDMETHOD.

  METHOD lif_description~set_tvtw.
    SELECT vtweg
      FROM tvtw
    INTO TABLE @me->m_tvtw.
  ENDMETHOD.
ENDCLASS. "end lcl_masterdata
*&---------------------------------------------------------------------*
*& CLASS lcl_popup IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_popup_confirm IMPLEMENTATION.
  METHOD is_confirm.
    DATA: lv_answer TYPE char1.
    DATA: lv_title  TYPE text40,
          lv_icon_y TYPE icon-name,
          lv_icon_n TYPE icon-name.

    lv_title = TEXT-t08.
    lv_icon_y = icon_okay.
    lv_icon_n = icon_cancel.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = lv_title
        text_question         = im_text
        text_button_1         = 'Yes'
        icon_button_1         = lv_icon_y
        text_button_2         = 'No'
        icon_button_2         = lv_icon_n
        default_button        = '2'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      re_ans = abap_false.
    ENDIF.

    IF lv_answer = 1.
      re_ans = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
