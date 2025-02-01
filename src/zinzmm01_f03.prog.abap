*&---------------------------------------------------------------------*
*& Include ZINZMM01_F03
*&---------------------------------------------------------------------*
*& Description:     UPLOAD EXCEL FLOW
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form UPLOAD_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM upload_material.
  DATA lr_excel TYPE REF TO lcl_excel.

  " Init Bal_log
  gr_bal_log = NEW #( ).
  TRY.
      CASE 'X'.
        WHEN rb_f1.
          lr_excel = NEW #( im_file = pfile ).
          lr_excel->set_uplfile( ).
          lr_excel->read_excel( ).

          " Init master data description
          gr_masterdata = NEW #( ).

          " Implement BAPI
          DATA(lr_impl_bapi) = NEW lcl_impl_bapi( lr_excel ).
          lr_impl_bapi->implement_bapi( ).

          gt_upload = lr_impl_bapi->get_upload( ).

          " insert additional fields data into DB
          PERFORM ins_add_to_db.

        WHEN rb_f2.
          lr_excel = NEW #( im_file = pfile ).
          lr_excel->set_uplfile( ).
          lr_excel->read_excel( ).

          " Implement BAPI
          lr_impl_bapi = NEW lcl_impl_bapi( lr_excel ).
          lr_impl_bapi->implement_bapi( ).

          gt_matdes = lr_impl_bapi->get_matdes( ).

        WHEN OTHERS.
          RAISE EXCEPTION NEW cx_sy_dyn_call_param_not_found( ).
      ENDCASE.

    CATCH cx_sy_create_object_error
          cx_static_check
          cx_sy_dyn_call_param_not_found INTO DATA(lr_cx).
      MESSAGE lr_cx TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_UPLOAD
*&---------------------------------------------------------------------*
*& Display Upload ALV
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_upload.
  DATA lv_lines       TYPE int4.
  DATA ls_layo        TYPE lvc_s_layo.
  DATA lt_fcat        TYPE lvc_t_fcat.
  DATA lv_gui_status  TYPE slis_formname.
  DATA lv_ucomm       TYPE slis_formname.
  DATA lv_top_of_page TYPE slis_formname.
  FIELD-SYMBOLS <lfs_itab> TYPE ANY TABLE.

  CASE 'X'.
    WHEN rb_f1.
      ls_layo = lcl_build_fcat=>build_layo( 'X' ).
      lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( ).

      lv_gui_status = 'UPLOAD_STATUS'.
      lv_ucomm      = 'HANDLE_UPL_UCOM'.
      ASSIGN gt_upload TO <lfs_itab>.
    WHEN rb_f2.
      ls_layo = lcl_build_fcat=>build_layo( ).
      lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'D' ).
      ASSIGN gt_matdes TO <lfs_itab>.
  ENDCASE.

  lv_top_of_page = 'ALV_HTML_TOP_OF_PAGE'.

  "§. Display number of rows on ALV
  lv_lines = lines( <lfs_itab> ).
  MESSAGE s024(msitem) WITH lv_lines.

  "§. Display ALV
  lcl_display_alv=>get_instance(
                )->display_alv( EXPORTING i_top_of_page = lv_top_of_page
                                          i_pf_status   = lv_gui_status
                                          i_ucomm       = lv_ucomm
                                          i_layout      = ls_layo
                                          it_fcat       = lt_fcat
                                IMPORTING i_outtab      = <lfs_itab> ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  UPLOAD_STATUS
*&---------------------------------------------------------------------*
*& Set pf-status for Upload ALV
*&---------------------------------------------------------------------*
FORM upload_status USING t_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'ZGUI_DISPLAY' EXCLUDING t_extab.

  IF gr_grid IS NOT BOUND.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING e_grid = gr_grid.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  HANDLE_UPL_UCOM
*&---------------------------------------------------------------------*
*& Handle ucomm for Upload ALV
*&---------------------------------------------------------------------*
FORM handle_upl_ucom USING r_ucomm     LIKE sy-ucomm
                           rs_selfield TYPE slis_selfield ##NEEDED ##CALLED.

  DATA lv_text TYPE text100.

  CASE r_ucomm.
    WHEN '&EXT'.
      "§. Display Pop-up Confirmation
      MESSAGE i026 INTO lv_text.
      IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_false.
        MESSAGE s023 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      "§. Create BAL_LOG
      gr_bal_log->bal_log_create( ).
      "§. Extend Material
      PERFORM extend_matnr.
      "§. Display BAL_LOG
      gr_bal_log->bal_log_display( ).

    WHEN '&VALTYP'.
      "§. Display Pop-up Confirmation
      MESSAGE i051 INTO lv_text.
      IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_false.
        MESSAGE s023 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      "§. Create BAL_LOG
      gr_bal_log->bal_log_create( ).
      "§. Add Valuation Type to Material(s)
      PERFORM add_valtype.
      "§. Display BAL_LOG
      gr_bal_log->bal_log_display( ).
  ENDCASE.
  FREE gr_grid.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALV_HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& Handle Header of ALV
*&---------------------------------------------------------------------*
FORM alv_html_top_of_page USING top TYPE REF TO cl_dd_document ##CALLED.
  DATA lo_doctable TYPE REF TO cl_dd_table_element.
  DATA lo_docarea  TYPE REF TO cl_dd_table_area ##NEEDED.
  DATA lo_column1  TYPE REF TO cl_dd_area.
  DATA: BEGIN OF lty_t_hd_info,
          title  TYPE char50,
          usname TYPE char50,
          date   TYPE char50,
          Infor  TYPE char100,
        END OF lty_t_hd_info.
  DATA lv_c_success TYPE int4.
  DATA lv_c_err     TYPE int4.
  DATA lv_upload    TYPE char3.

  " Set information to Header
  "§. Count success and fail entries
  IF gt_upload IS NOT INITIAL.
    lv_c_success = REDUCE #( INIT l_count TYPE int4
                             FOR ls IN gt_upload WHERE ( status = 'S' )
                             NEXT l_count += 1 ).

    lv_c_err = REDUCE #( INIT l_count TYPE int4
                         FOR ls IN gt_upload WHERE ( status = 'E' )
                         NEXT l_count += 1 ).
  ELSEIF gt_matdes IS NOT INITIAL.
    lv_c_success = REDUCE #( INIT l_count TYPE int4
                             FOR ls_matdes IN gt_matdes WHERE ( status = 'S' )
                             NEXT l_count += 1 ).

    lv_c_err = REDUCE #( INIT l_count TYPE int4
                         FOR ls_matdes IN gt_matdes WHERE ( status = 'E' )
                         NEXT l_count += 1 ).
  ENDIF.

  "§. Set thread
  lv_upload = COND #( WHEN rb_f1 IS NOT INITIAL THEN 'F01'
                      WHEN rb_f2 IS NOT INITIAL THEN 'F02' ).

  "§. Get date and time
  DATA(lv_date) = lcl_utils=>conv_date_external( sy-datum ).
  DATA(lv_time) = lcl_utils=>conv_time_external( sy-uzeit ).

  lty_t_hd_info = VALUE #( title  = SWITCH #( lv_upload
                                              WHEN 'F01' THEN TEXT-t05
                                              WHEN 'F02' THEN TEXT-t09 )
                           usname = |Upload by: { sy-uname }|
                           date   = |Upload on: { lv_date } - Time: { lv_time }|
                           infor  = |Success: { lv_c_success }, Error: { lv_c_err }| )
                        ##NO_TEXT.

  " Create table contain Report name
  top->add_table( EXPORTING no_of_columns               = 1
                            border                      = '0'
                            cell_background_transparent = 'X'
                            width                       = '100%'
                  IMPORTING table                       = lo_doctable
                            tablearea                   = lo_docarea ).

  " Add column to table
  lo_doctable->add_column( IMPORTING column = lo_column1 ).

  lo_doctable->set_column_style( col_no        = 1
                                 sap_align     = 'Left'
                                 sap_fontstyle = 'Arial' ) ##NO_TEXT.

  " Add title text
  lo_column1->new_line( ).
  lo_column1->add_text( text         = CONV #( lty_t_hd_info-title )
                        sap_fontsize = '16'
                        sap_style    = cl_dd_area=>heading ).

  lo_column1->new_line( ).
  " Create table area
  top->add_table( EXPORTING no_of_columns               = 1
                            border                      = '0'
                            cell_background_transparent = 'X'
                            width                       = '100%'
                  IMPORTING table                       = lo_doctable
                            tablearea                   = lo_docarea ) ##NO_TEXT.
  " Add column to table
  lo_doctable->add_column( IMPORTING column = lo_column1 ).

  " Add username text
  lo_column1->add_text( text          = CONV #( lty_t_hd_info-usname )
                        sap_fontstyle = 'Arial'
                        sap_fontsize  = cl_dd_area=>medium ) ##NO_TEXT.

  " Add date and time text
  lo_column1->new_line( ).
  lo_column1->add_text( text          = CONV #( lty_t_hd_info-date )
                        sap_fontstyle = 'Arial'
                        sap_fontsize  = cl_dd_area=>medium ) ##NO_TEXT.

  " Add number of success and fail entries
  lo_column1->new_line( ).
  lo_column1->add_text( text          = CONV #( lty_t_hd_info-infor )
                        sap_fontstyle = 'Arial'
                        sap_fontsize  = cl_dd_area=>medium ) ##NO_TEXT.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form extend_matnr
*&---------------------------------------------------------------------*
*& ONLY extend for Material duplicate in DB!
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM extend_matnr.
  DATA lt_row_no TYPE lvc_t_row.
  DATA ls_extend TYPE zstmmgr3extend.
  DATA lt_extend TYPE STANDARD TABLE OF zstmmgr3extend
                 WITH UNIQUE SORTED KEY prim COMPONENTS matnr.

  "§.Get selected row(s)
  CHECK gr_grid IS BOUND.
  gr_grid->get_selected_rows( IMPORTING et_index_rows = lt_row_no ).
  IF lt_row_no IS INITIAL.
    MESSAGE e004.
  ENDIF.

  "§.Append selected rows to another global itab - GT_EXTEND
  LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<l_row_no>).
    ASSIGN gt_upload[ <l_row_no>-index ] TO FIELD-SYMBOL(<ls_alv>).
    IF sy-subrc = 0.
      IF <ls_alv>-status = 'E'.
        MESSAGE e014.
      ENDIF.
      IF <ls_alv>-mtart CP '*DIE*'.
        MESSAGE e062.
      ENDIF.
      ls_extend = CORRESPONDING #( <ls_alv> EXCEPT message werks lgort ).
      APPEND ls_extend TO lt_extend.
      CLEAR ls_extend.
    ENDIF.
  ENDLOOP.

  gt_extend = lt_extend.
  CLEAR lt_extend.

  "§.Display Extend ALV
  PERFORM display_extend USING gt_extend.
  CLEAR gt_extend.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_extend
*&---------------------------------------------------------------------*
*& Display pop-up Extend ALV
*&---------------------------------------------------------------------*
*&      --> LT_EXTEND
*&---------------------------------------------------------------------*
FORM display_extend USING it_extend TYPE STANDARD TABLE.
  DATA lt_fcat  TYPE lvc_t_fcat.
  DATA ls_layo  TYPE lvc_s_layo.

  DATA lv_title TYPE lvc_title.

  lv_title = TEXT-t06.

  "§.Build Extend Fieldcatalog + Layout
  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'X' ).
  ls_layo = lcl_build_fcat=>get_instance( )->build_layo( ).

  "§.Display Extend ALV
  FREE gr_grid.
  DATA(lr_display_alv) = lcl_display_alv=>get_instance( ).
  lr_display_alv->set_screen( ).
  lr_display_alv->display_alv( EXPORTING i_pf_status = 'EXTEND_STATUS'
                                         i_ucomm     = 'HANDLE_EXT_UCOM'
                                         i_layout    = ls_layo
                                         i_title     = lv_title
                                         it_fcat     = lt_fcat
                               IMPORTING i_outtab    = it_extend ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  EXTEND_STATUS
*&---------------------------------------------------------------------*
*& Set pf-status for Extend ALV
*&---------------------------------------------------------------------*
FORM extend_status USING t_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'ZGUI_EXTEND' EXCLUDING t_extab.

  IF gr_grid IS NOT BOUND.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING e_grid = gr_grid.
  ENDIF.

  "§.Init object for Grid Event Handler
  gr_handler = NEW #( ).

  "§.Register edit event
  gr_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  gr_grid->set_ready_for_input( i_ready_for_input = 1 ).

  "§.Handle event
  SET HANDLER gr_handler->on_data_changed FOR gr_grid.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  HANDLE_EXT_UCOM
*&---------------------------------------------------------------------*
*& Handle ucomm for Extend pop-up ALV
*&---------------------------------------------------------------------*
FORM handle_ext_ucom USING r_ucomm     LIKE sy-ucomm
                           rs_selfield TYPE slis_selfield ##NEEDED ##CALLED.

  IF gr_grid IS BOUND.
    gr_grid->check_changed_data( ).
  ENDIF.

  CASE r_ucomm.
    WHEN '&SUBMIT'.
      PERFORM submit_extend.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INS_ADD_TO_DB
*&---------------------------------------------------------------------*
*& Insert successful created materials to DB ZG3MAFI
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ins_add_to_db.
  DATA ls_mafi TYPE zg3mafi.
  DATA lt_mafi TYPE STANDARD TABLE OF zg3mafi WITH UNIQUE SORTED KEY prim COMPONENTS matnr.

  IF gt_upload IS INITIAL.
    RETURN.
  ENDIF.

  "§1. Move data from ALV to itab structure like DB
  LOOP AT gt_upload INTO DATA(ls_upload) WHERE status = 'S' AND is_ext IS INITIAL.
    MOVE-CORRESPONDING ls_upload TO ls_mafi.
    TRY.
        APPEND ls_mafi TO lt_mafi.
        CLEAR ls_mafi.
      CATCH cx_sy_itab_duplicate_key ##NO_HANDLER.
    ENDTRY.
  ENDLOOP.

  "§2. Insert to DB
  IF lt_mafi IS NOT INITIAL.
    TRY.
        INSERT zg3mafi FROM TABLE @lt_mafi.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      CATCH cx_sy_open_sql_db ##NO_HANDLER.
    ENDTRY.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SUBMIT_EXTEND
*&---------------------------------------------------------------------*
*& Submit button on Extend ALV
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM submit_extend.
  " Extend duplicate MATNR on ALV
  DATA(lr_bapi) = NEW lcl_impl_bapi( ).
  lr_bapi->extend_dmatnr( CHANGING c_itab = gt_extend ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DEL_MATNR
*&---------------------------------------------------------------------*
*& Delete selected rows on Display ALV.
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM del_matnr USING i_mark TYPE flag.
  DATA lt_index TYPE lvc_t_row.
  DATA lt_sel   TYPE STANDARD TABLE OF ty_alvlist WITH EMPTY KEY.
  DATA lv_text  TYPE text100.

  " Display Pop-up Confirmation
  CASE i_mark.
    WHEN 'X'.
      MESSAGE i027 INTO lv_text.
    WHEN OTHERS.
      MESSAGE i028 INTO lv_text.
  ENDCASE.

  "§1. Get selected rows
  go_alv_table->get_selected_rows( IMPORTING et_index_rows = lt_index ).

  IF lt_index IS INITIAL.
    MESSAGE e004.
  ENDIF.
  IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_false.
    RETURN.
  ENDIF.

  LOOP AT lt_index ASSIGNING FIELD-SYMBOL(<fs_index>).
    TRY.
        lt_sel = VALUE #( BASE lt_sel
                          ( gt_display[ <fs_index>-index ] ) ).
      CATCH cx_sy_itab_line_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.

  "§2. Init bal log
  gr_bal_log = NEW #( ).
  gr_bal_log->bal_log_create( ).

  "§3. Implement bapi to Delete material
  lcl_impl_bapi_d=>get_instance(
                )->delete_matnr( EXPORTING i_mark = |{ i_mark }|
                                 CHANGING  c_itab = lt_sel ).

  "§4.Reflect delete flag to ALV
  LOOP AT lt_sel INTO DATA(ls_sel).
    ASSIGN gt_display[ matnr = ls_sel-matnr
                       lgort = ls_sel-lgort
                       bwkey = ls_sel-bwkey
                       bwtar = ls_sel-bwtar ] TO FIELD-SYMBOL(<ls_alv>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    <ls_alv>-del_flag = ls_sel-del_flag.
    IF <ls_alv>-del_flag IS NOT INITIAL.
      <ls_alv>-del_icon = icon_incomplete.
    ELSE.
      CLEAR <ls_alv>-del_icon.
    ENDIF.
    <ls_alv>-status = ls_sel-status.
    PERFORM set_d_icon CHANGING <ls_alv>.
  ENDLOOP.

  "§5. Display log
  gr_bal_log->bal_log_display( ).

  "§6. Refresh ALV
  go_alv_table->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = 'X'
                                                                            col = 'X' )
                                                  i_soft_refresh = 'X'
                                       EXCEPTIONS finished       = 1
                                                  OTHERS         = 2  ).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form modify_f4_text
*&---------------------------------------------------------------------*
*& Auto fill description after input Plant and Sloc
*&---------------------------------------------------------------------*
*& --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM modify_f4_text USING io_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  "§.Fill Plant and Valuation Area description
  LOOP AT io_data_changed->mt_mod_cells INTO DATA(ls_data_changed) WHERE fieldname = 'WERKS'.
    IF NOT line_exists( gt_extend[ ls_data_changed-row_id ] ).
      CONTINUE.
    ENDIF.

    TRY.
        io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = 'WERKSTX'
            i_value     = VALUE #( gr_masterdata->m_t001w[ werks = ls_data_changed-value ]-name1 ) )
        ##WARN_OK.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
    io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                  i_tabix     = ls_data_changed-tabix
                                  i_fieldname = 'BWKEY'
                                  i_value     = ls_data_changed-value ).
  ENDLOOP.

  "§.Fill LGORT Description
  LOOP AT io_data_changed->mt_mod_cells INTO ls_data_changed WHERE fieldname = 'LGORT'.
    ASSIGN gt_extend[ ls_data_changed-row_id ] TO FIELD-SYMBOL(<ls_extend>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF <ls_extend>-werks IS INITIAL.
      io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                    i_tabix     = ls_data_changed-tabix
                                    i_fieldname = 'LGORT'
                                    i_value     = '' )
       ##WARN_OK.
      MESSAGE s070.
    ENDIF.
    TRY.
        io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = 'LGOBE'
            i_value     = VALUE #( gr_masterdata->m_t001l[ lgort = ls_data_changed-value ]-lgobe ) ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form set_d_icon
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_ALV>
*&---------------------------------------------------------------------*
FORM set_d_icon CHANGING cs_alv TYPE ty_alvlist.
  CASE cs_alv-status.
    WHEN 'E'.
      cs_alv-icon = icon_red_light.
    WHEN 'S'.
      cs_alv-icon = icon_green_light.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_matdes_itab
*&---------------------------------------------------------------------*
*& Fill MATDES itab from excel
*&---------------------------------------------------------------------*
*&      --> ME_>T_EXCEL
*&      <-- LT_MATDES
*&---------------------------------------------------------------------*
FORM get_matdes_itab USING    it_excel  TYPE zttalsmex_tabline
                     CHANGING ct_matdes TYPE STANDARD TABLE.

  DATA lt_fcat   TYPE lvc_t_fcat.
  DATA ls_fcat   TYPE lvc_S_fcat.
  DATA lv_fname  TYPE lvc_fname.
  DATA ls_matdes TYPE zstmmgr3matdes.

  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'D' ).
  LOOP AT it_excel INTO DATA(lr_excel) WHERE     sheetno = 1
                                             AND norow   > 0004
       GROUP BY ( norow = lr_excel-norow   ).
    LOOP AT GROUP lr_excel INTO DATA(ls_group).
      READ TABLE lt_fcat INTO ls_fcat WITH KEY col_pos = ls_group-col + 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lv_fname.
      CONCATENATE 'LS_MATDES-' ls_fcat-fieldname
                  INTO lv_fname.
      ASSIGN (lv_fname) TO FIELD-SYMBOL(<fname>).

      CASE ls_fcat-fieldname.
        WHEN 'MATNR'.
          CONDENSE ls_group-value NO-GAPS.
          TRANSLATE ls_group-value TO UPPER CASE.
        WHEN 'LANGU'.
          TRY.
              CONDENSE ls_group-value NO-GAPS.
              TRANSLATE ls_group-value TO UPPER CASE.
              <fname> = ls_group-value.
              ls_matdes-spras = lcl_utils=>conv_langu_input( CONV #( ls_matdes-langu ) ).
              CONTINUE.
            CATCH cx_sy_conversion_error INTO DATA(lr_cx).
              ls_matdes-status = 'E'.
              APPEND lr_cx->get_text( ) TO ls_matdes-t_mess.
              lcl_build_fcat=>set_matdes_icon( CHANGING ch_alv = ls_matdes ).
          ENDTRY.
      ENDCASE.

      <fname> = ls_group-value.
    ENDLOOP.

    IF ls_matdes-matnr IS NOT INITIAL.
      APPEND ls_matdes TO ct_matdes.
      CLEAR ls_matdes.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_mtemplate_itab
*&---------------------------------------------------------------------*
*& Fill Main template into itab
*&---------------------------------------------------------------------*
*&      --> ME_>T_EXCEL
*&      <-- LT_ALV
*&---------------------------------------------------------------------*
FORM get_mtemplate_itab USING    it_excel TYPE zttalsmex_tabline
                        CHANGING ct_alv   TYPE STANDARD TABLE.

  DATA lt_fcat    TYPE lvc_t_fcat.
  DATA ls_fcat    TYPE lvc_S_fcat.
  DATA ls_alv     TYPE zstmmgr03upl.
  DATA lv_message TYPE text100.
  DATA lr_cx      TYPE REF TO cx_root.
  DATA lv_fname   TYPE lvc_fname.
  DATA lv_waers   TYPE waers VALUE 'VND'.

  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( ).
  LOOP AT it_excel INTO DATA(lr_excel) WHERE     norow   > 0007
                                             AND sheetno = 1
       GROUP BY ( sheetno = lr_excel-sheetno
                  norow   = lr_excel-norow  ).
    "§.Sheet 1: Main Template
    LOOP AT GROUP lr_excel INTO DATA(ls_group).
      READ TABLE lt_fcat INTO ls_fcat WITH KEY col_pos = ls_group-col + 2.
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
              " Using regular expressions to validate Material code format
              FIND FIRST OCCURRENCE OF PCRE '^(P|L|T)[A-Z]{2}\d{5}|SE(P|T|L)(01|02|03|04)\d{3}|RA\d{6}' IN ls_group-value ##NO_TEXT.
              IF sy-subrc <> 0.
                MESSAGE e024 INTO lv_message.
                APPEND lv_message TO ls_alv-t_mess.
                CLEAR lv_message.
                ls_alv-status = 'E'.
              ENDIF.
            WHEN 'MAKTX'.
              <fname> = ls_group-value.
              CONTINUE.
            WHEN 'IPRKZ'.
              <fname> = lcl_utils=>conv_sled_input( CONV #( ls_group-value ) ).
              CONTINUE.
            WHEN 'IS_EXT'.
              IF ls_group-value = 'TRUE'.
                <fname> = 'X'.
              ENDIF.
              CONTINUE.
          ENDCASE.

          IF ls_fcat-fieldname CP 'ATT*'.
            <fname> = ls_group-value.
            CONTINUE.
          ENDIF.

          CASE ls_fcat-datatype.
            WHEN 'UNIT'.
              <fname> = lcl_utils=>conv_unit_input( IMPORTING im_unit = ls_group-value ).

            WHEN 'DATS'.
              <fname> = lcl_utils=>conv_date_internal( ls_group-value ).

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
              cx_root INTO lr_cx.
*          ls_alv-message = lr_cx->get_text( ).
          APPEND lr_cx->get_text( ) TO ls_alv-t_mess.
          ls_alv-status = 'E'.
      ENDTRY.
      UNASSIGN <fname>.
    ENDLOOP.

    IF ls_alv-matnr IS NOT INITIAL.
      lcl_build_fcat=>set_status_icon( CHANGING ch_alv = ls_alv ).
      ls_alv-waers = lv_waers.
      APPEND ls_alv TO ct_alv.
      CLEAR ls_alv.
    ENDIF.
    CLEAR: lr_cx,
           ls_alv.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_oum_itab
*&---------------------------------------------------------------------*
*& Fill sheet 2 ( OUM ) into itab
*&---------------------------------------------------------------------*
*&      --> ME_>T_EXCEL
*&      <-- LT_OUM
*&---------------------------------------------------------------------*
FORM get_oum_itab USING    it_excel TYPE zttalsmex_tabline
                  CHANGING ct_uom   TYPE STANDARD TABLE.

  DATA lt_fcat  TYPE lvc_t_fcat.
  DATA ls_fcat  TYPE lvc_S_fcat.
  DATA ls_uom   TYPE zstmmgr03oum.
  DATA lv_fname TYPE lvc_fname.

  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'U' ).
  LOOP AT it_excel INTO DATA(lr_excel) WHERE     norow   > 0004
                                             AND sheetno = 2
       GROUP BY ( sheetno = lr_excel-sheetno
                  norow   = lr_excel-norow ).
    LOOP AT GROUP lr_excel INTO DATA(ls_group).
      READ TABLE lt_fcat INTO ls_fcat WITH KEY col_pos = ls_group-col.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lv_fname.
      CONCATENATE 'LS_UOM-' ls_fcat-fieldname
                  INTO lv_fname.
      ASSIGN (lv_fname) TO FIELD-SYMBOL(<fname>).

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
      APPEND ls_uom TO ct_uom.
    ENDIF.
    CLEAR ls_uom.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_tax_itab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ME_>T_EXCEL
*&      <-- LT_TAX
*&---------------------------------------------------------------------*
FORM get_tax_itab USING    it_excel TYPE zttalsmex_tabline
                  CHANGING ct_tax   TYPE STANDARD TABLE.

  DATA lt_fcat  TYPE lvc_t_fcat.
  DATA ls_fcat  TYPE lvc_S_fcat.
  DATA ls_tax   TYPE zstmmgr03tax.
  DATA lv_fname TYPE lvc_fname.

  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'T' ).
  LOOP AT it_excel INTO DATA(lr_excel) WHERE     norow   > 0009
                                             AND sheetno = 3
       GROUP BY ( sheetno   = lr_excel-sheetno
                  norow     = lr_excel-norow
                  sheetname = lr_excel-sheetname  ).
    " §.Sheet 3: Tax Classification
    LOOP AT GROUP lr_excel INTO DATA(ls_group).
      READ TABLE lt_fcat INTO ls_fcat WITH KEY col_pos = ls_group-col.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lv_fname.
      CONCATENATE 'LS_TAX-' ls_fcat-fieldname
                  INTO lv_fname.
      ASSIGN (lv_fname) TO FIELD-SYMBOL(<fname>).

      CONDENSE  ls_group-value NO-GAPS.
      TRANSLATE ls_group-value TO UPPER CASE.

      <fname> = ls_group-value.
    ENDLOOP.

    IF ls_tax-matnr IS NOT INITIAL.
      APPEND ls_tax TO ct_tax.
      CLEAR ls_tax.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form add_valtype
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_valtype.
  DATA lt_row_no   TYPE lvc_t_row.
  DATA ls_valuatyp TYPE zstmmgr3valtyp.
  DATA lt_valuatyp TYPE zttmmgr3valtyp.

  CHECK gr_grid IS BOUND.
  " Get selected rows
  gr_grid->get_selected_rows( IMPORTING et_index_rows = lt_row_no ).
  IF lt_row_no IS INITIAL.
    MESSAGE e004.
  ENDIF.

  LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<l_row_no>).
    ASSIGN gt_upload[ <l_row_no>-index ] TO FIELD-SYMBOL(<ls_alv>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    " Handle select Error lines
    IF <ls_alv>-status = 'E'.
      MESSAGE e063.
    ENDIF.
    " Handle select Material type 'SERVICE' to add Valuation type
    IF <ls_alv>-mtart CP '*DIE*'.
      MESSAGE e061.
    ENDIF.
    ls_valuatyp = CORRESPONDING #( <ls_alv> EXCEPT werks bklas bwkey ).
    APPEND ls_valuatyp TO lt_valuatyp.
    CLEAR ls_valuatyp.
  ENDLOOP.

  " Get all available plants of materials.
  PERFORM get_all_plant USING    lt_valuatyp
                        CHANGING gt_valtyp.
  CLEAR lt_valuatyp.

  " Display Valuation Type ALV
  PERFORM display_valtype USING gt_valtyp.
  CLEAR gt_valtyp.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_valtype
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_EXTEND
*&---------------------------------------------------------------------*
FORM display_valtype USING it_valtype TYPE STANDARD TABLE.
  DATA lt_fcat  TYPE lvc_t_fcat.
  DATA ls_layo  TYPE lvc_s_layo.
  DATA lt_sort  TYPE lvc_t_sort.
  DATA lv_title TYPE lvc_title.

  lv_title = TEXT-t10.

  "§.Build Extend Fieldcatalog + Layout
  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'V' ).
  ls_layo = lcl_build_fcat=>get_instance( )->build_layo( ).

  "§. Sort ALV.
  lt_sort = VALUE #( up = 'X'
                     ( spos = '01' fieldname = 'MATNR' )
                     ( spos = '02' fieldname = 'WERKS' ) ).

  "§.Display Valuation Type ALV
  FREE gr_grid.
  DATA(lr_display_alv) = lcl_display_alv=>get_instance( ).
  lr_display_alv->set_screen( ).
  lr_display_alv->display_alv( EXPORTING i_pf_status = 'VALUATYP_STATUS'
                                         i_ucomm     = 'HANDLE_VALTYP_UCOM'
                                         i_layout    = ls_layo
                                         i_title     = lv_title
                                         it_fcat     = lt_fcat
                                         it_sort     = lt_sort
                               IMPORTING i_outtab    = it_valtype ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  VALUATYP_STATUS
*&---------------------------------------------------------------------*
*& Set pf-status for Extend ALV
*&---------------------------------------------------------------------*
FORM valuatyp_status USING t_extab TYPE slis_t_extab ##CALLED.
*  DATA: lt_drdn_bwkey TYPE lvc_t_drop. "
  SET PF-STATUS 'ZGUI_VALTYP' EXCLUDING t_extab.

  IF gr_grid IS NOT BOUND.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING e_grid = gr_grid.
  ENDIF.

  gr_handler = NEW #( ).

  "§. Set dropdown list value
*  PERFORM set_drdn_table CHANGING lt_drdn_bwkey.
*  gr_grid->set_drop_down_table(   it_drop_down  =   lt_drdn_bwkey  ).

  "§. Register Edit event
  gr_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  gr_grid->set_ready_for_input( i_ready_for_input = 1 ).

  "§. Handle event
  SET HANDLER gr_handler->on_valtyp_data_changed FOR gr_grid.

  "§. Refresh ALV
  gr_grid->refresh_table_display( is_stable      = VALUE #( row = 'X'
                                                            col = 'X' )
                                  i_soft_refresh = 'X' ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  HANDLE_EXT_UCOM
*&---------------------------------------------------------------------*
*& Handle ucomm for Extend pop-up ALV
*&---------------------------------------------------------------------*
FORM handle_valtyp_ucom USING r_ucomm     LIKE sy-ucomm
                              rs_selfield TYPE slis_selfield ##NEEDED ##CALLED.

  IF gr_grid IS BOUND.
    gr_grid->check_changed_data( ).
  ENDIF.

  CASE r_ucomm.
    WHEN '&SUBMIT'.
      PERFORM submit_valtyp.
      LEAVE TO SCREEN 0.

    WHEN '&ADD'.
      PERFORM insert_row.

    WHEN '&DROW'.
      PERFORM delete_row.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form submit_valtyp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM submit_valtyp.
  DATA lt_row_no   TYPE lvc_t_row.
  DATA ls_valuatyp TYPE zstmmgr3valtyp.
  DATA lt_valuatyp TYPE STANDARD TABLE OF zstmmgr3valtyp WITH EMPTY KEY.

  " Get selected rows
  CHECK gr_grid IS BOUND.
  gr_grid->get_selected_rows( IMPORTING et_index_rows = lt_row_no ).
  IF lt_row_no IS INITIAL.
    MESSAGE e004 DISPLAY LIKE 'S'.
  ENDIF.

  LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<l_row_no>).
    ASSIGN gt_valtyp[ <l_row_no>-index ] TO FIELD-SYMBOL(<ls_alv>).
    IF sy-subrc = 0.
      ls_valuatyp = CORRESPONDING #( <ls_alv> ).
      APPEND ls_valuatyp TO lt_valuatyp.
      CLEAR ls_valuatyp.
    ENDIF.
  ENDLOOP.

  " Submit add Price and Valuation type
  DATA(lr_bapi) = NEW lcl_impl_bapi( ).
  lr_bapi->add_valtyp( CHANGING c_itab = lt_valuatyp ).
  CLEAR gt_valtyp.
ENDFORM.

*&---------------------------------------------------------------------*
*&Form  set_drdn_table
*&---------------------------------------------------------------------*
* This Perform is copied from prog BCALV_EDIT_06 - line 203
* NO USE but Not delete because maybe use in future...
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_drdn_table CHANGING ct_bwkey_drdn TYPE lvc_t_drop ##CALLED.
  " §1.Define a dropdown table and pass it to ALV.
  "    One listbox is referenced by a handle, e.g., '1'.
  "    For each entry that shall appear in this listbox
  "    you have to append a line to the dropdown table
  "    with handle '1'.
  "    This handle can be assigned to several columns
  "    of the output table using the field catalog.
  DATA ls_dropdown TYPE lvc_s_drop.

  SELECT bklas FROM t025
    WHERE kkref IN ( '0001', '0005' )
    INTO TABLE @DATA(lt_t025).

  LOOP AT lt_t025 ASSIGNING FIELD-SYMBOL(<ls_bklas>).
    ls_dropdown-handle = '1'.
    ls_dropdown-value  = <ls_bklas>-bklas.
    APPEND ls_dropdown TO ct_bwkey_drdn.
    CLEAR ls_dropdown.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form reflect_alv_success
*&---------------------------------------------------------------------*
*& Reflect Success to ALV
*&---------------------------------------------------------------------*
*&      --> <LFS_DATA>_MATNR
*&---------------------------------------------------------------------*
FORM reflect_alv_success USING iv_matnr TYPE matnr.
  ASSIGN gt_upload[ matnr = iv_matnr ] TO FIELD-SYMBOL(<lfs_upload>).
  IF sy-subrc = 0.
    <lfs_upload>-status = 'S'.
    MESSAGE i012 INTO <lfs_upload>-message.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_price
*&---------------------------------------------------------------------*
*& Validate price: PRICE must not smaller than 0
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM validate_price USING io_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  LOOP AT io_data_changed->mt_mod_cells INTO DATA(ls_data_changed) WHERE fieldname = 'PVPRS_1'.
    IF ls_data_changed-value < '0'.
      io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                    i_tabix     = ls_data_changed-tabix
                                    i_fieldname = 'PVPRS_1'
                                    i_value     = '0' ).
      MESSAGE s057 DISPLAY LIKE 'E'.
    ENDIF.
  ENDLOOP.

  CLEAR ls_data_changed.
  LOOP AT io_data_changed->mt_mod_cells INTO ls_data_changed WHERE fieldname = 'BWTAR'.
    DATA(lt_valtyp) = gt_valtyp.
    ASSIGN gt_valtyp[ ls_data_changed-row_id ] TO FIELD-SYMBOL(<lfs_valtyp>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    DELETE lt_valtyp INDEX ls_data_changed-row_id.
    IF line_exists( lt_valtyp[ matnr = <lfs_valtyp>-matnr
                               werks = <lfs_valtyp>-werks
                               bwtar = ls_data_changed-value ] ).
      io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                    i_tabix     = ls_data_changed-tabix
                                    i_fieldname = 'BWTAR'
                                    i_value     = '' )
              ##WARN_OK.
      MESSAGE s071 WITH ls_data_changed-value DISPLAY LIKE 'E'.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form insert_row
*&---------------------------------------------------------------------*
*& Insert Row in ALV Valuation Type
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM insert_row.
  DATA lt_row_no   TYPE lvc_t_row.
  DATA ls_valuatyp TYPE zstmmgr3valtyp.
  DATA lt_valuatyp TYPE STANDARD TABLE OF zstmmgr3valtyp WITH EMPTY KEY.

  CHECK gr_grid IS BOUND.
  gr_grid->get_selected_rows( IMPORTING et_index_rows = lt_row_no ).
  IF lt_row_no IS INITIAL.
    MESSAGE e004.
  ENDIF.

  LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<l_row_no>).
    ASSIGN gt_valtyp[ <l_row_no>-index ] TO FIELD-SYMBOL(<ls_alv>).
    IF sy-subrc = 0.
      ls_valuatyp = CORRESPONDING #( <ls_alv> EXCEPT bwtar pvprs_1 ).
      APPEND ls_valuatyp TO lt_valuatyp.
      CLEAR ls_valuatyp.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF lt_valuatyp TO gt_valtyp.
  SORT gt_valtyp BY matnr
                    werks.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form delete_row
*&---------------------------------------------------------------------*
*& Delete Row in ALV Valuation Type
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_row.
  DATA lt_row_no TYPE lvc_t_row.

  CHECK gr_grid IS BOUND.

  gr_grid->get_selected_rows( IMPORTING et_index_rows = lt_row_no ).
  IF lt_row_no IS INITIAL.
    MESSAGE e004.
  ENDIF.

  SORT lt_row_no DESCENDING BY index.

  LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<l_row_no>).
    DELETE gt_valtyp INDEX <l_row_no>-index.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_all_plant
*&---------------------------------------------------------------------*
*& Get all plant to add valuation type
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_all_plant USING    it_valtyp TYPE zttmmgr3valtyp
                   CHANGING gt_valtyp.

  SELECT DISTINCT it_valtyp~*,
                  marc~werks,
                  mbew~bwtty,
                  mbew~bwkey
    FROM @it_valtyp AS it_valtyp
           LEFT JOIN
             marc ON marc~matnr = it_valtyp~matnr
               LEFT JOIN
                 t001w ON t001w~werks = marc~werks
                   LEFT JOIN
                     mbew ON  mbew~matnr = it_valtyp~matnr
                          AND mbew~bwkey = t001w~bwkey
    WHERE mbew~bwtty IS NOT INITIAL
      AND marc~werks IS NOT INITIAL
    INTO CORRESPONDING FIELDS OF TABLE @gt_valtyp ##ITAB_KEY_IN_SELECT.
  IF sy-subrc <> 0.
    MESSAGE e060.
  ENDIF.

  SELECT it_valtyp~matnr,
         mbew~bwkey,
         mbew~bwtar
    FROM @it_valtyp AS it_valtyp
           INNER JOIN
             mbew ON mbew~matnr = it_valtyp~matnr
    INTO CORRESPONDING FIELDS OF TABLE @gt_check_valtyp ##ITAB_KEY_IN_SELECT
                                                       ##TOO_MANY_ITAB_FIELDS.
ENDFORM.
