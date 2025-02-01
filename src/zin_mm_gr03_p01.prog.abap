*&---------------------------------------------------------------------*
*& INCLUDE          ZIN_MM_GR03_P01
*&---------------------------------------------------------------------*
*&                  CLASS IMPLEMENTATION
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& CLASS   lcl_excel  IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_excel IMPLEMENTATION.
  METHOD get_instance.
    IF o_excel IS NOT BOUND.
      CREATE OBJECT o_excel.
    ENDIF.

    re_instance = o_excel.
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
    TRY.
        me->load_excel( im_file  ).
      CATCH cx_static_check INTO DATA(lr_cx).
        RAISE EXCEPTION lr_cx.
    ENDTRY.

    me->set_excel_to_itab( ).
    re_itab = me->t_template.
  ENDMETHOD.

  METHOD load_excel.
    DATA: lv_begin_col TYPE i VALUE 2,
          lv_begin_row TYPE i VALUE 5,
          lv_end_col   TYPE i VALUE 99,
          lv_end_row   TYPE i VALUE 200,
          lv_sheets    TYPE i VALUE 3.

    CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = im_file
        i_begin_col             = lv_begin_col
        i_begin_row             = lv_begin_row
        i_end_col               = lv_end_col
        i_end_row               = lv_end_row
        sheets                  = lv_sheets
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
    DATA: ls_alv TYPE zstmmgr03upl,
          lt_alv TYPE me->lif_excel~ty_t_upload.
    DATA: ls_uom TYPE zstmmgr03oum,
          lt_uom TYPE me->lif_excel~ty_t_uom.
    DATA: ls_tax TYPE zstmmgr03tax,
          lt_tax TYPE me->lif_excel~ty_t_tax.

    DATA: lt_fcat_template TYPE lvc_t_fcat,
          lt_fcat_uom      TYPE lvc_t_fcat,
          lt_fcat_tax      TYPE lvc_t_fcat,
          ls_fcat          TYPE lvc_s_fcat.

    DATA: lr_cx    TYPE REF TO cx_root,
          lv_fname TYPE lvc_fname,
          lv_waers TYPE waers VALUE 'VND'.


    "Set field catalog of 3 sheets.
    lt_fcat_template =  lcl_build_fcat=>get_instance( )->build_fcat(  ).
    lt_fcat_uom      =  lcl_build_fcat=>get_instance( )->build_fcat( 'U' ).
    lt_fcat_tax      =  lcl_build_fcat=>get_instance( )->build_fcat( 'T' ).

    LOOP AT me->t_excel INTO DATA(lr_excel) WHERE norow > 0004
                                            GROUP BY (  sheetno = lr_excel-sheetno
                                                          norow = lr_excel-norow
                                                      sheetname = lr_excel-sheetname  ).
      "§.Sheet 1: Main Template
      LOOP AT GROUP lr_excel INTO DATA(ls_group) WHERE sheetname = 'Main Template'
                                                   AND norow > 0007.
        READ TABLE lt_fcat_template INTO ls_fcat WITH KEY col_pos = ls_group-col + 2.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CLEAR lv_fname.
        CONCATENATE 'LS_ALV-' ls_fcat-fieldname
               INTO lv_fname.
        ASSIGN (lv_fname) TO FIELD-SYMBOL(<fname>).

        TRY.
            CASE ls_fcat-fieldname.
              WHEN 'MATNR'.
                CONDENSE ls_group-value NO-GAPS.
                TRANSLATE ls_group-value TO UPPER CASE.
                FIND PCRE '^(P|L|T)[A-Z]{2}\d{5}|(SE)(P|T|L)(01|02|03|04)\d{3}$' IN ls_group-value ##NO_TEXT.
                IF sy-subrc <> 0.
                  MESSAGE e024 INTO ls_alv-message.
                  ls_alv-status  = 'E'.
                ENDIF.

              WHEN 'MAKTX'.
                <fname> = ls_group-value.
                CONTINUE.
              WHEN 'IPRKZ'.
                <fname> = lcl_utils=>conv_sled_input( im_value = CONV #( ls_group-value ) ).
                CONTINUE.
              WHEN 'IS_EXT'.
                IF ls_group-value = 'TRUE'.
                  <fname> = 'X'.
                ENDIF.
                CONTINUE.
            ENDCASE.


            CASE ls_fcat-datatype.
              WHEN 'UNIT'.
                <fname> = lcl_utils=>conv_unit_input( IMPORTING im_unit = ls_group-value ).

              WHEN 'DATS'.
                <fname> = lcl_utils=>conv_date_internal( im_value = ls_group-value ).

              WHEN 'CURR'.
                <fname> = ls_group-value / 100.

              WHEN OTHERS.
                CONDENSE ls_group-value NO-GAPS.
                TRANSLATE ls_group-value TO UPPER CASE.

                <fname> = ls_group-value.
            ENDCASE.
          CATCH cx_sy_zerodivide
                cx_sy_conversion_error
                cx_sy_regex_too_complex
                cx_root                           INTO lr_cx.
            ls_alv-message = lr_cx->get_text( ).
            ls_alv-status  = 'E'.
        ENDTRY.
        UNASSIGN <fname>.
      ENDLOOP.

      IF ls_alv-matnr IS NOT INITIAL.
        lcl_build_fcat=>set_status_icon( CHANGING ch_alv = ls_alv ).
        ls_alv-waers = lv_waers.
        APPEND ls_alv TO lt_alv.
      ENDIF.
      CLEAR: lr_cx, ls_alv.

      "• If data exceed 150 lines -> Dont allow to upload
      IF lines( lt_alv ) > 150.
        MESSAGE e016.
      ENDIF.

      "§.Sheet 2: Alternative UOM
      LOOP AT GROUP lr_excel INTO ls_group WHERE sheetname = 'Alternative UOM'
                                             AND norow > 0004.
        READ TABLE lt_fcat_uom INTO ls_fcat WITH KEY col_pos = ls_group-col.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CLEAR lv_fname.
        CONCATENATE 'LS_UOM-' ls_fcat-fieldname
               INTO lv_fname.
        ASSIGN (lv_fname) TO <fname>.

        CONDENSE ls_group-value NO-GAPS.
        TRANSLATE ls_group-value TO UPPER CASE.
        CASE ls_fcat-datatype.
          WHEN 'UNIT'.
            TRY.
                <fname> = lcl_utils=>conv_unit_input( IMPORTING im_unit = ls_group-value ).
              CATCH cx_sy_conversion_error ##NO_HANDLER.
            ENDTRY.

          WHEN OTHERS.
            TRY.
                <fname> = ls_group-value.
              CATCH cx_sy_conversion_no_number ##NO_HANDLER.
            ENDTRY.
        ENDCASE.
      ENDLOOP.

      IF ls_uom-matnr IS NOT INITIAL.
        APPEND ls_uom TO lt_uom.
      ENDIF.
      CLEAR: lr_cx, ls_uom.

*      §.Sheet 3: Tax Classification
      LOOP AT GROUP lr_excel INTO ls_group WHERE sheetname = 'Tax Classification'
                                             AND norow > 0004.
        READ TABLE lt_fcat_tax INTO ls_fcat WITH KEY col_pos = ls_group-col.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CLEAR lv_fname.
        CONCATENATE 'LS_TAX-' ls_fcat-fieldname
               INTO lv_fname.
        ASSIGN (lv_fname) TO <fname>.

        CONDENSE ls_group-value NO-GAPS.
        TRANSLATE ls_group-value TO UPPER CASE.

        <fname> = ls_group-value.
      ENDLOOP.

      IF ls_tax-matnr IS NOT INITIAL.
        APPEND ls_tax TO lt_tax.
      ENDIF.
      CLEAR: ls_tax.
    ENDLOOP.

    me->t_template = lt_alv.
    me->t_uom = lt_uom.
    me->t_tax = lt_tax.
  ENDMETHOD.
ENDCLASS. "end LCL_EXCEL

*&---------------------------------------------------------------------*
*& CLASS   lcl_build_fcat IMPLEMENTATION
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
*&---------------------------------------------*

    CASE im_struct.
      WHEN ''.
        me->upload_fcat( ).
      WHEN 'X'.
        me->extend_fcat( ).
      WHEN 'U'.
        t_fcat =  me->get_fcat( 'ZSTMMGR03OUM' ).
      WHEN 'T'.
        t_fcat = me->get_fcat( 'ZSTMMGR03TAX' ).
    ENDCASE.

    re_fcat = t_fcat.
  ENDMETHOD.

  METHOD build_layo.
    re_layo = VALUE #( cwidth_opt ='X'
                       zebra = 'X'
                       sel_mode = 'A' ).

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

  METHOD upload_fcat.
    TYPES: BEGIN OF ts_fields,
             fieldname TYPE lvc_fname,
             label     TYPE lvc_txtcol,
             just      TYPE char1,
           END OF ts_fields .
    DATA: lt_field TYPE STANDARD TABLE OF ts_fields WITH EMPTY KEY,
          ls_field TYPE ts_fields.

    lt_field = VALUE #(  ( fieldname = 'ICON'    label = 'Status'  just = 'C' )
                         ( fieldname = 'MESSAGE' label = 'Message' )
                         ( fieldname = 'IS_EXT'  label = 'Extend' )
                         ( fieldname = 'STATUS' )
                         ( fieldname = 'WAERS'  )
                         ( fieldname = 'MATNR'  )
                      ) ##NO_TEXT.

    CLEAR t_fcat.
    t_fcat = me->get_fcat( im_structname = 'ZSTMMGR03UPL' ).

    LOOP AT lt_field INTO ls_field.
      READ TABLE t_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WITH KEY fieldname = ls_field-fieldname.
      IF sy-subrc = 0.
        IF <ls_fcat>-fieldname = 'STATUS' OR <ls_fcat>-fieldname = 'WAERS'.
          <ls_fcat>-tech = 'X'.
          CONTINUE.
        ENDIF.
        <ls_fcat>-coltext = ls_field-label.
        <ls_fcat>-just    = ls_field-just.

        CASE <ls_fcat>-fieldname.
          WHEN 'MATNR'.
            <ls_fcat>-key = 'X'.
          WHEN 'MESSAGE'.
            <ls_fcat>-emphasize = 'C300'.
        ENDCASE.

      ENDIF.
      CLEAR ls_field.
    ENDLOOP.
  ENDMETHOD.

  METHOD extend_fcat.
    CLEAR t_fcat.
    t_fcat = me->get_fcat( im_structname = 'ZSTMMGR3EXTEND' ).

    LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      CASE <lfs_fcat>-fieldname.
        WHEN 'MATNR'.
          <lfs_fcat>-key = 'X'.
        WHEN 'WERKS'.
          <lfs_fcat>-edit = 'X'.
          <lfs_fcat>-ref_table = 'ZSTMMGR3EXTEND'.
          <lfs_fcat>-ref_field = 'WERKS'.
          <lfs_fcat>-f4availabl = 'X'.
        WHEN 'LGORT'.
          <lfs_fcat>-edit = 'X'.
          <lfs_fcat>-ref_table = 'ZSTMMGR3EXTEND'.
          <lfs_fcat>-ref_field = 'LGORT'.
          <lfs_fcat>-f4availabl = 'X'.
        WHEN 'STATUS'.
          <lfs_fcat>-tech = 'X'.
        WHEN 'MESSAGE'.
          <lfs_fcat>-tech = 'X'.
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
    DATA: ls_variant TYPE disvariant.

    ls_variant = VALUE #( report = lv_repid
                          handle = '001' ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = lv_repid
        i_callback_pf_status_set = i_pf_status
        i_callback_user_command  = i_ucomm
        i_grid_title             = i_title
        is_layout_lvc            = i_layout
        it_fieldcat_lvc          = it_fcat
        i_save                   = 'A'
        i_screen_start_column    = me->ms_screen-scrn_start_col
        i_screen_start_line      = me->ms_screen-scrn_start_line
        i_screen_end_column      = me->ms_screen-scrn_end_col
        i_screen_end_line        = me->ms_screen-scrn_end_line
      TABLES
        t_outtab                 = i_outtab
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
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
    CHECK im_value IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_DATEX_INPUT'
      EXPORTING
        input  = im_value
      IMPORTING
        output = re_date.

    IF re_date IS INITIAL.
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

  METHOD impl_bapi.
    me->check_duplicate(  CHANGING  c_itab =  c_itab  ).
*    me->check_obli_fld(  CHANGING  c_itab =  c_itab  ).
    me->creat_matnr(  CHANGING  c_itab =  c_itab  ).
    me->extend_matnr(  CHANGING  c_itab =  c_itab  ).
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
          lt_allocvaluescharnew TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_char,
          lt_allocvaluescurrnew TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_curr.

    DATA: lt_return TYPE STANDARD TABLE OF bapiret2.

    DATA: ls_mess TYPE text255,
          lt_mess TYPE STANDARD TABLE OF text255.



    "Create new MATNR
    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE is_ext IS INITIAL AND status IS INITIAL.

      "§.1. Assign data for BAPI
      me->fill_creat(
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


      "§.2. Implement bapi
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
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        MESSAGE i010 WITH <lfs_data>-matnr INTO <lfs_data>-message ##MG_MISSING.

        "§.2.2.1. Assign Classification for material that's created sucessfully
        IF <lfs_data>-klart IS INITIAL.
          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data>  ).
          CONTINUE.
        ENDIF.
        lv_objectkey = <lfs_data>-matnr.
        lv_classnum  = <lfs_data>-artxt.
        lv_classtype = <lfs_data>-klart.

        ""§.2.2.1.1 Implement BAPI to create Classification
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
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        ELSE. "assign classification fail
          LOOP AT lt_return INTO DATA(ls_OBJCL_return) WHERE type <> 'S'.
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

      CLEAR: ls_headerdata, ls_clientdata, ls_clientdatax, ls_plantdata, ls_plantdatax, ls_storagelocationdata, ls_storagelocationdatax,
             ls_valuationdata, ls_valuationdatax, ls_salesdata, ls_salesdatax, ls_return, lt_matkt, lt_unitsofmeasure, lt_unitsofmeasurex.
      CLEAR: lv_objectkey, lv_classnum, lv_classtype, lt_return.
    ENDLOOP.
  ENDMETHOD. "END CREAT_MATNR

  METHOD extend_matnr.
    DATA: ls_headerdata           TYPE bapimathead,
          ls_plantdata            TYPE bapi_marc,
          ls_plantdatax           TYPE bapi_marcx,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx.
    DATA: ls_return TYPE  bapiret2.



    "Check MATNR existed in DB
    SELECT itab~matnr
      FROM @c_itab AS itab
      INNER JOIN mara ON mara~matnr = itab~matnr
      WHERE itab~is_ext IS NOT INITIAL
     INTO TABLE @DATA(lt_extend).

    IF sy-subrc <> 0.
      LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE is_ext IS NOT INITIAL
                                                          AND status IS INITIAL.
        <lfs_data>-status = 'E'.
        MESSAGE e011 INTO <lfs_data>-message.
        lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data>  ).
      ENDLOOP.
      RETURN.
    ENDIF.

    LOOP AT c_itab ASSIGNING <lfs_data> WHERE is_ext IS NOT INITIAL AND status IS INITIAL.
      IF NOT line_exists( lt_extend[ matnr = <lfs_data>-matnr ] ). "Not exist in DB to extend
        <lfs_data>-status = 'E'.
        MESSAGE e011 INTO <lfs_data>-message.

      ELSE. "Extend Material

        IF <lfs_data>-lgort IS NOT INITIAL.
          ls_storagelocationdata  = VALUE #( plant = <lfs_data>-werks  stge_loc = <lfs_data>-lgort  ).
          ls_storagelocationdatax = VALUE #( plant = <lfs_data>-werks  stge_loc = <lfs_data>-lgort ).
        ELSE.
          ls_plantdata  =  VALUE #( plant = <lfs_data>-werks ).
          ls_plantdatax =  VALUE #( plant = <lfs_data>-werks ).
        ENDIF.
        ls_headerdata = VALUE #( material     = <lfs_data>-matnr
                                 basic_view   = COND #( WHEN <lfs_data>-lgort IS INITIAL THEN 'X' )
                                 storage_view = COND #( WHEN <lfs_data>-lgort IS NOT INITIAL THEN 'X' )
                               ).

        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata             = ls_headerdata
            plantdata            = ls_plantdata
            plantdatax           = ls_plantdatax
            storagelocationdata  = ls_storagelocationdata
            storagelocationdatax = ls_storagelocationdatax
          IMPORTING
            return               = ls_return.

        IF ls_return-type  <> 'S'.
          <lfs_data>-status = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CONCATENATE ls_return-type '-' ls_return-id '-' ls_return-number '-' ls_return-message INTO <lfs_data>-message.
        ELSEIF ls_return-type = 'S'.
          <lfs_data>-status = 'S'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
          MESSAGE i012 WITH <lfs_data>-matnr INTO <lfs_data>-message ##MG_MISSING.
        ENDIF.
      ENDIF.

      lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_data> ).
      CLEAR: ls_headerdata, ls_storagelocationdata, ls_storagelocationdatax.
    ENDLOOP.
  ENDMETHOD. "end EXTEND_MANTR

  METHOD check_duplicate.
    DATA: lt_line TYPE STANDARD TABLE OF zstmmgr03upl.

    "CHECK duplicate key in file
    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_upload>) WHERE status IS INITIAL
                                                        GROUP BY ( matnr = <lfs_upload>-matnr  )
                                                        ASSIGNING FIELD-SYMBOL(<lfs_group>) .
      lt_line = VALUE #( FOR m IN GROUP <lfs_group> ( m ) ).

      IF lines( lt_line ) > 1.
        READ TABLE gt_upload ASSIGNING FIELD-SYMBOL(<lfs_alv>) WITH KEY matnr = <lfs_group>-matnr.
        IF sy-subrc = 0.
          <lfs_alv>-status = 'E'.
          MESSAGE e008 INTO <lfs_alv>-message.
          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_alv> ).
        ENDIF.
      ENDIF.

      CLEAR lt_line.
    ENDLOOP.

    "CHECK duplicate key in DB.
    SELECT itab~matnr
      FROM @c_itab AS itab
      INNER JOIN mara ON mara~matnr = itab~matnr
     WHERE itab~is_ext IS INITIAL
       AND status IS INITIAL
     INTO TABLE @DATA(lt_check_dupl).

    IF sy-subrc = 0.
      LOOP AT lt_check_dupl INTO DATA(ls_dupl).
        READ TABLE c_itab  WITH KEY matnr = ls_dupl-matnr ASSIGNING <lfs_alv>.
        IF sy-subrc = 0.
          <lfs_alv>-status = 'D'.
          MESSAGE i009 INTO <lfs_alv>-message.

          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_alv> ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD. "end check_duplicate

*  METHOD check_obli_fld.
*    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_alv>) WHERE ( vkorg IS INITIAL OR vtweg IS INITIAL )
*                                                       AND   status IS INITIAL.
*      <lfs_alv>-status = 'E'.
*      MESSAGE i021 INTO <lfs_alv>-message.
*      lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_alv> ).
*    ENDLOOP.
*  ENDMETHOD. "end check_obli_fld

  METHOD extend_dmatnr.
    DATA: ls_headerdata           TYPE bapimathead,
          ls_storagelocationdata  TYPE bapi_mard,
          ls_storagelocationdatax TYPE bapi_mardx.
    DATA: ls_return TYPE  bapiret2.

    DATA: BEGIN OF lty_symsgv200,
            msgv1 TYPE symsgv1,
            msgv2 TYPE symsgv2,
            msgv3 TYPE symsgv3,
            msgv4 TYPE symsgv4,
          END OF lty_symsgv200.
    DATA: ls_msg TYPE bal_s_msg.
    CONSTANTS lc_messclass TYPE syst_msgid VALUE 'ZMGR03'.




    LOOP AT c_itab ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE status <> 'E'.
      "Init data for bapi
      ls_headerdata = VALUE #( material = <lfs_data>-matnr storage_view = 'X' ).
      ls_storagelocationdata  = VALUE #( plant = <lfs_data>-werks  stge_loc = <lfs_data>-lgort  ).
      ls_storagelocationdatax = VALUE #( plant = <lfs_data>-werks  stge_loc = <lfs_data>-lgort ).

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = ls_headerdata
          storagelocationdata  = ls_storagelocationdata
          storagelocationdatax = ls_storagelocationdatax
        IMPORTING
          return               = ls_return.
      IF ls_return-type <> 'S'.
        <lfs_data>-status = 'E'.
        lty_symsgv200 = <lfs_data>-message = ls_return-message.
        MOVE-CORRESPONDING lty_symsgv200 TO ls_msg.
        ls_msg-msgid = ls_return-id.
        ls_msg-msgno = ls_return-number.
        ls_msg-msgty = ls_return-type.
        CLEAR lty_symsgv200.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.


        MESSAGE i015 WITH <lfs_data>-matnr INTO lty_symsgv200.
        MOVE-CORRESPONDING lty_symsgv200 TO ls_msg.
        ls_msg-msgid = lc_messclass.
        ls_msg-msgno = 000.
        ls_msg-msgty = 'S'.

        "Reflect on ALV
        READ TABLE gt_upload ASSIGNING FIELD-SYMBOL(<lfs_upload>) WITH KEY matnr = <lfs_data>-matnr.
        IF sy-subrc = 0.
          <lfs_upload>-werks = <lfs_data>-werks.
          <lfs_upload>-lgort = <lfs_data>-lgort.

          <lfs_upload>-status = 'S'.
          MESSAGE i012 INTO <lfs_upload>-message.

          lcl_build_fcat=>set_status_icon( CHANGING ch_alv = <lfs_upload> ).
        ENDIF.
      ENDIF.

      IF ls_msg IS NOT INITIAL.
        gr_bal_log->bal_log_msg_cumulate( ls_msg ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD fill_creat.
    DATA: ls_oum TYPE zstmmgr03oum.
    DATA: ls_unitsofmeasure  TYPE bapi_marm,
          ls_unitsofmeasurex TYPE bapi_marmx.
    DATA: ls_taxclassifications TYPE bapi_mlan.


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
                           ).

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
    LOOP AT me->o_excel->lif_excel~t_tax INTO DATA(lgr_tax) WHERE matnr = is_data-matnr
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
                               valid_from = is_data-zkdat ).
    valuationdatax = VALUE #(  val_area   = is_data-bwkey
                               ml_settle  = COND #( WHEN valuationdata-ml_settle  IS NOT INITIAL THEN 'X' )
                               val_class  = COND #( WHEN valuationdata-val_class  IS NOT INITIAL THEN 'X' )
                               price_ctrl = COND #( WHEN valuationdata-price_ctrl IS NOT INITIAL THEN 'X' )
                               price_unit = COND #( WHEN valuationdata-price_unit IS NOT INITIAL THEN 'X' )
                               moving_pr  = COND #( WHEN valuationdata-moving_pr  IS NOT INITIAL THEN 'X' )
                               std_price  = COND #( WHEN valuationdata-std_price  IS NOT INITIAL THEN 'X' )
                               future_pr  = COND #( WHEN valuationdata-future_pr  IS NOT INITIAL THEN 'X' )
                               valid_from = COND #( WHEN valuationdata-valid_from IS NOT INITIAL THEN 'X' ) ).

    "UOM
    LOOP AT me->o_excel->lif_excel~t_uom INTO ls_oum WHERE matnr = is_data-matnr.
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
      ls_unitsofmeasure-gross_wt = is_data-brgew.
      APPEND ls_unitsofmeasure TO t_unitsofmeasure.
      CLEAR ls_unitsofmeasure.
    ENDLOOP.

    LOOP AT t_unitsofmeasure INTO ls_unitsofmeasure.
      ls_unitsofmeasurex = VALUE #( alt_unit   = ls_unitsofmeasure-alt_unit
                                    denominatr = COND #( WHEN ls_unitsofmeasure-denominatr IS NOT INITIAL THEN 'X' )
                                    numerator  = COND #( WHEN ls_unitsofmeasure-numerator  IS NOT INITIAL THEN 'X' )
                                    ean_upc    = COND #( WHEN ls_unitsofmeasure-ean_upc    IS NOT INITIAL THEN 'X' )
                                    ean_cat    = COND #( WHEN ls_unitsofmeasure-ean_cat    IS NOT INITIAL THEN 'X' )
                                    gross_wt   = COND #( WHEN ls_unitsofmeasure-gross_wt   IS NOT INITIAL THEN 'X' )  "is_data?????
                                    unit_of_wt = COND #( WHEN ls_unitsofmeasure-unit_of_wt IS NOT INITIAL THEN 'X' )
                                    length     = COND #( WHEN ls_unitsofmeasure-length     IS NOT INITIAL THEN 'X' )
                                    width      = COND #( WHEN ls_unitsofmeasure-width      IS NOT INITIAL THEN 'X' )
                                    height     = COND #( WHEN ls_unitsofmeasure-height     IS NOT INITIAL THEN 'X' )
                                    unit_dim   = COND #( WHEN ls_unitsofmeasure-unit_dim   IS NOT INITIAL THEN 'X' )
                                    volume     = COND #( WHEN ls_unitsofmeasure-volume     IS NOT INITIAL THEN 'X' )
                                    volumeunit = COND #( WHEN ls_unitsofmeasure-volumeunit IS NOT INITIAL THEN 'X' )
                                    capacity_usage  = COND #( WHEN ls_unitsofmeasure-capacity_usage IS NOT INITIAL THEN 'X' ) ).

      APPEND ls_unitsofmeasurex TO t_unitsofmeasurex.
      CLEAR ls_unitsofmeasurex.
    ENDLOOP.

    "header
    headerdata = VALUE #( material = is_data-matnr  ind_sector = is_data-mbrsh  matl_type = is_data-mtart  basic_view = 'X'
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
          ls_storagelocationdata  = VALUE #( plant = <lfs_data>-werks stge_loc = <lfs_data>-lgort del_flag = 'X' ).
          ls_storagelocationdatax = VALUE #( plant = <lfs_data>-werks stge_loc = <lfs_data>-lgort del_flag = 'X' ).
        ENDIF.

        ls_headerdata  = VALUE #( material      = <lfs_data>-matnr
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
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

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
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

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

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile.


    READ TABLE ls_display_profile-mess_fcat WITH KEY ref_field = 'T_MSG' ASSIGNING FIELD-SYMBOL(<l_mess_fcat>).
    IF sy-subrc = 0.
      <l_mess_fcat>-outputlen = '000200'.
    ENDIF.

    INSERT ms_bal_log-log_handle INTO TABLE lt_log_handle.

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
ENDCLASS. "end lcl_event_handler
*&---------------------------------------------------------------------*
*& CLASS  lcl_masterdata IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_masterdata IMPLEMENTATION.
  METHOD constructor.
    me->lif_description~set_t001l( ).
    me->lif_description~set_t001W( ).
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
































**&---------------------------------------------------------------------*
**& CLASS   lcl_excel_1sheet IMPLEMENTATION ( NO USE )
**&---------------------------------------------------------------------*
*CLASS lcl_excel_1sheet IMPLEMENTATION.
*METHOD get_instance.
*  IF mo_excel IS NOT BOUND.
*    CREATE OBJECT mo_excel.
*  ENDIF.
*
*  r_instance = mo_excel.
*ENDMETHOD.
*
*METHOD set_upl_info.
*  DATA: lo_table_descr  TYPE REF TO cl_abap_tabledescr,
*        lo_struct_descr TYPE REF TO cl_abap_structdescr.
*
*  "Get Number of column
*  lo_table_descr  ?= cl_abap_tabledescr=>describe_by_data( p_data = it_xls ).
*  lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
*
*  "Set excel upload information
*  me->lif_excel_1sheet~ts_xls_info = VALUE #( zfile     = i_filepath
*                                       ztemplate = 'UPL_XLS'
*                                       zcol      = lines( lo_struct_descr->components )
*                                       zstruct   = 'ZST_MM_GR03'
*                                    ).
*ENDMETHOD.
*
*METHOD f4_file_name.
*  DATA: lt_tab   TYPE filetable,
*        lv_subrc TYPE i.
*
*  DATA: BEGIN OF file,
*          filename(200),
*        END OF file.
*
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    EXPORTING
*      window_title            = CONV #( TEXT-t02 )
*      default_filename        = '*.XLSX'
*      multiselection          = ' '
*    CHANGING
*      file_table              = lt_tab
*      rc                      = lv_subrc
*    EXCEPTIONS
*      file_open_dialog_failed = 1
*      cntl_error              = 2
*      error_no_gui            = 3
*      not_supported_by_gui    = 4
*      OTHERS                  = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  READ TABLE lt_tab INTO file INDEX 1.
*  IF sy-subrc = 0.
*    c_filepath = file-filename.
*  ENDIF.
*ENDMETHOD.
*
*METHOD process_file_upload.
**    DATA: lt_alv TYPE me->lif_excel_1sheet~tt_alv.
*  DATA: lt_alv TYPE REF TO data.
*
*  me->load_excel(   EXPORTING
*                      i_file  = i_filepath
*                    IMPORTING
*                     e_excel = t_xsl
*                 ).
*
*  me->set_upl_info(  i_filepath = i_filepath
*                     it_xls     = t_xsl
*                  ).
*
*  me->t_excel = me->convert_xsl( t_xsl ).
*
*ENDMETHOD.
*
*METHOD convert_xsl.
*  DATA: ls_excel TYPE zstgr3mapxls,
*        ls_alv   TYPE zst_mm_gr03.
*  DATA: ls_field TYPE ztbgr3mapxls,
*        ls_type  TYPE ztbgr3mapxls,
*        ls_name  TYPE ztbgr3mapxls.
*
*  DATA: lv_index TYPE char6,
*        lv_do    TYPE numc4,
*        lv_text  TYPE text20,
*        lv_mess  TYPE text150.
*
*  DATA: lw_data TYPE REF TO data.
*
*  CONSTANTS: C_num  TYPE text100 VALUE 'Không phải là số'.
*
*  DATA: lv_excel TYPE char20 VALUE 'LS_EXCEL-C',
*        lv_name  TYPE char20 VALUE 'LS_NAME-C',
*        lv_type  TYPE char20 VALUE 'LS_TYPE-C',
*        lv_field TYPE char20 VALUE 'LS_FIELD-C',
*        lw_alv   TYPE char20 VALUE '<FS_LW_ALV>-'.
*  DATA: lw_excel_assign TYPE string,
*        lw_type_assign  TYPE string,
*        lw_name_assign  TYPE string,
*        lw_map_assign   TYPE string,
*        lw_alv_assign   TYPE string.
*
*  FIELD-SYMBOLS: <fs_row>    TYPE any,
*                 <fs_excel>  TYPE any,
*                 <fs_type>   TYPE any,
*                 <fs_name>   TYPE any,
*                 <fs_mp>     TYPE any,
*                 <fs_data>   TYPE any,
*                 <fs_r_itab> TYPE STANDARD TABLE,
*                 <fs_lw_alv> TYPE any.
*
*  BREAK-POINT ID zgr3_upl.
*
*  "Get template
*  SELECT *
*    FROM ztbgr3mapxls
*    WHERE tcode = @sy-tcode
*      AND ztemplate = 'UPL_XLS'
*   INTO TABLE @DATA(lt_mapping).
*
*    READ TABLE lt_mapping WITH KEY zmapping = '01' INTO ls_field. "Technical field name
*    READ TABLE lt_mapping WITH KEY zmapping = '02' INTO ls_type.  "Type
*    READ TABLE lt_mapping WITH KEY zmapping = '03' INTO ls_NAME.  "Column name
*
*    CREATE DATA r_itab TYPE STANDARD TABLE OF (me->lif_excel_1sheet~ts_xls_info-zstruct).
*    CREATE DATA lw_data TYPE (me->lif_excel_1sheet~ts_xls_info-zstruct).
*
*    ASSIGN r_itab->* TO <fs_r_itab>.
*    ASSIGN lw_data->* TO <fs_lw_alv>.
*
**    BREAK dev-206.
*    "Process data type in file excel
*    LOOP AT it_excel INTO ls_excel.
*
*      ADD 1 TO lv_index.
*      lv_text = |EXCEL LINE { lv_index }:|.
*      DO me->lif_excel_1sheet~ts_xls_info-zcol TIMES.
*        ADD 1 TO lv_do.
*        lw_excel_assign = |{ lv_excel }{ lv_do }|.
*        lw_type_assign  = |{ lv_type }{ lv_do }|.
*        lw_map_assign   = |{ lv_field }{ lv_do }|.
*        lw_name_assign  = |{ lv_name }{ lv_do }|.
*
*        ASSIGN:
*         (lw_excel_assign) TO <fs_excel>,  "Column of work area LS_EXCEL
*         (lw_type_assign)  TO <fs_type>,   "Column type
*         (lw_name_assign)  TO <fs_name>,   "Column type
*         (lw_map_assign)   TO <fs_mp>.
*
*        IF <fs_mp> IS ASSIGNED.
*          lw_alv_assign = |{ lw_alv }{ <fs_mp> }|.
*          ASSIGN: (lw_alv_assign) TO <fs_data>.
*        ENDIF.
*
**        ASSIGN COMPONENT lv_do OF STRUCTURE ls_excel TO <fs_row>.
*
*        IF <fs_type> IS ASSIGNED.
**          CHECK <fs_row>   IS ASSIGNED.
*          CHECK <fs_excel> IS ASSIGNED.
*          CHECK <fs_data>  IS ASSIGNED.
*          CHECK <fs_name>  IS ASSIGNED.
*
*          CASE <fs_type>. "Validate data.
*            WHEN 'C'.
*              PERFORM xls_to_sap USING <fs_mp> CHANGING <fs_excel>.
*
*              <fs_data> = <fs_excel>.
*
*            WHEN 'N'.
*              IF <fs_excel> CO '0123456789.,+- '.
*                <fs_data> = <fs_excel>.
*              ELSE.
*                lv_mess = |{ lv_text }{ <fs_name> }{ c_num }|.
*              ENDIF.
*          ENDCASE.
*
*          IF <fs_mp> = 'MESSAGE'.
*            <fs_data> = lv_mess.
*          ENDIF.
*
*        ENDIF.
*        UNASSIGN: "<fs_name>
*                  <fs_excel>,
*                  <fs_type>,
*                  <fs_name>.
*
*      ENDDO.
*
*      APPEND <fs_lw_alv> TO <fs_r_itab>.
*      CLEAR: <fs_lw_alv>, lv_do, lv_mess.
*    ENDLOOP.
*
*
*
*  ENDMETHOD.
*
*  METHOD load_excel.
*    DATA: l_path TYPE string.
*    DATA: l_data_excl     TYPE truxs_t_text_data,
**          l_tab_converted TYPE STANDARD TABLE OF me->lif_excel_1sheet~ts_mara_upl WITH EMPTY KEY.
*          l_tab_converted TYPE STANDARD TABLE OF zstgr3mapxls WITH EMPTY KEY.
*
*    l_path = i_file.
*
*    "Display a progress bar when executing a particular code logic.
*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*      EXPORTING
*        percentage = 35
*        text       = 'UPLOADING DATA ...'.
*
*    "Get data uploaded
*    CALL FUNCTION 'GUI_UPLOAD'
*      EXPORTING
*        filename                = l_path
*        filetype                = 'ASC'
*        has_field_separator     = 'X'
*      TABLES
*        data_tab                = l_data_excl
*      EXCEPTIONS
*        file_open_error         = 1
*        file_read_error         = 2
*        no_batch                = 3
*        gui_refuse_filetransfer = 4
*        invalid_type            = 5
*        no_authority            = 6
*        unknown_error           = 7
*        bad_data_format         = 8
*        header_not_allowed      = 9
*        separator_not_allowed   = 10
*        header_too_long         = 11
*        unknown_dp_error        = 12
*        access_denied           = 13
*        dp_out_of_memory        = 14
*        disk_full               = 15
*        dp_timeout              = 16
*        OTHERS                  = 17.
*    IF sy-subrc <> 0.
*
*    ENDIF.
*
*    "Convert xls to itab
*    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*      EXPORTING
*        i_field_seperator    = 'X'
*        i_line_header        = 'X'
*        i_tab_raw_data       = l_data_excl
*        i_filename           = i_file
*      TABLES
*        i_tab_converted_data = l_tab_converted
*      EXCEPTIONS
*        conversion_failed    = 1
*        OTHERS               = 2.
*    IF sy-subrc <> 0.
*      MESSAGE s003 DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*
*    ELSE.
**      DELETE l_tab_converted FROM 1 TO 3.
*      DELETE l_tab_converted WHERE c0001 IS INITIAL. "Matnr is initial
*      e_excel = l_tab_converted.
*
*    ENDIF.
*
*    " End file
*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*      EXPORTING
*        percentage = 100
*        text       = 'UPLOAD COMPLETED'.
*
*
*  ENDMETHOD.
*ENDCLASS.
