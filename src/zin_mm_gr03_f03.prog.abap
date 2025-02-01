*&---------------------------------------------------------------------*
*& Include          ZIN_MM_GR03_F03
*&---------------------------------------------------------------------*
*& Description:     UPLOAD EXCEL FLOW
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form INIT_OBJET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_objet .
  TRY.
      "Get data from excel file
      gt_upload = lcl_excel=>get_instance( )->read_excel( pfile ).

      "Implement bapi
      DATA(lo_impl_bapi) = NEW lcl_impl_bapi( lcl_excel=>get_instance( ) ).
      lo_impl_bapi->impl_bapi( CHANGING c_itab = gt_upload ).

      "Init master description
      gr_masterdata = NEW #( ).

      "Init bal log
      gr_bal_log = NEW #( ).
    CATCH cx_sy_create_object_error
          cx_static_check           INTO DATA(lr_cx).
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
  DATA: lv_lines TYPE int4.
  DATA: ls_layo TYPE lvc_s_layo,
        lt_fcat TYPE lvc_t_fcat.
  DATA: l_title TYPE lvc_title.

  l_title = TEXT-t05.

  "§. Build layout and fieldcatalog
  ls_layo = lcl_build_fcat=>build_layo( ).
  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( ).

  "§. Display number of rows on ALV
  lv_lines = lines( gt_upload ).
  MESSAGE s024(msitem) WITH lv_lines.

  "§. Display ALV
  lcl_display_alv=>get_instance(
                )->display_alv(
                         EXPORTING
                            i_pf_status = 'UPLOAD_STATUS'
                            i_ucomm     = 'HANDLE_UPL_UCOM'
                            i_title     = l_title
                            i_layout    = ls_layo
                            it_fcat     = lt_fcat
                         IMPORTING
                            i_outtab    = gt_upload
                   ).
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
      IMPORTING
        e_grid = gr_grid.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  HANDLE_UPL_UCOM
*&---------------------------------------------------------------------*
*& Handle ucomm for Upload ALV
*&---------------------------------------------------------------------*
FORM handle_upl_ucom USING r_ucomm      LIKE sy-ucomm
                           rs_selfield  TYPE slis_selfield ##NEEDED ##CALLED.
  DATA: lv_text TYPE text100.
  MESSAGE i026 INTO lv_text.


  CASE r_ucomm.
    WHEN '&EXT'.
      IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_false.
        RETURN.
      ENDIF.

      gr_bal_log->bal_log_create( ).
      PERFORM extend_matnr.
      gr_bal_log->bal_log_display( ).
      FREE gr_grid.
  ENDCASE.

*  check gr_grid is BOUND.
*  gr_grid->refresh_table_display(       EXPORTING  is_stable      = VALUE #( row = 'X' col = 'X' )
*                                                   i_soft_refresh = 'X'
*                                        EXCEPTIONS finished = 1
*                                                   OTHERS   = 2  ).
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form extend_matnr
*&---------------------------------------------------------------------*
*& ONLY extend for Material duplicate in DB!
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM extend_matnr .
  DATA: lt_row_no TYPE lvc_t_row.
  DATA: ls_extend TYPE zstmmgr3extend,
        lt_extend TYPE STANDARD TABLE OF zstmmgr3extend
                  WITH UNIQUE SORTED KEY prim COMPONENTS matnr.


  CHECK gr_grid IS BOUND.
  gr_grid->get_selected_rows( IMPORTING et_index_rows = lt_row_no ).
  IF lt_row_no IS INITIAL.
    MESSAGE e004.
  ENDIF.

  LOOP AT lt_row_no ASSIGNING FIELD-SYMBOL(<l_row_no>).
    READ TABLE gt_upload INDEX <l_row_no>-index ASSIGNING FIELD-SYMBOL(<ls_alv>).
    IF sy-subrc = 0 AND <ls_alv>-status <> 'E'.
      ls_extend = CORRESPONDING #( <ls_alv> EXCEPT message werks lgort ).
      APPEND ls_extend TO lt_extend.
      CLEAR ls_extend.
    ENDIF.
  ENDLOOP.

  IF line_exists( lt_extend[ status = 'E' ] ).
    MESSAGE e014.
  ENDIF.

  gt_extend = lt_extend.
  CLEAR lt_extend.

  "Display Extend ALV
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
FORM display_extend  USING it_extend TYPE STANDARD TABLE.
  DATA: lt_fcat TYPE lvc_t_fcat,
        ls_layo TYPE lvc_s_layo.

  DATA: lv_title TYPE lvc_title.

  lv_title = TEXT-t06.

  "§.Build Extend Fieldcatalog + Layout
  lt_fcat = lcl_build_fcat=>get_instance( )->build_fcat( 'X' ).
  ls_layo = lcl_build_fcat=>get_instance( )->build_layo( ).

  "§.Display Extend ALV
  FREE gr_grid.
  DATA(lr_display_alv) = lcl_display_alv=>get_instance(  ).
  lr_display_alv->set_screen( ).
  lr_display_alv->display_alv(
                     EXPORTING
                       i_pf_status = 'EXTEND_STATUS'
                       i_ucomm     = 'HANDLE_EXT_UCOM'
                       i_layout    = ls_layo
                       i_title     = lv_title
                       it_fcat     = lt_fcat
                     IMPORTING
                       i_outtab    = it_extend
                   ).
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
      IMPORTING
        e_grid = gr_grid.
  ENDIF.

  gr_handler = NEW #( ).

  gr_grid->register_edit_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
 ).

  gr_grid->set_ready_for_input( i_ready_for_input = 1 ).

  SET HANDLER:
    gr_handler->on_data_changed FOR gr_grid.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form  HANDLE_EXT_UCOM
*&---------------------------------------------------------------------*
*& Handle ucomm for Extend pop-up ALV
*&---------------------------------------------------------------------*
FORM handle_ext_ucom USING r_ucomm      LIKE sy-ucomm
                           rs_selfield  TYPE slis_selfield ##NEEDED ##CALLED.

  IF gr_grid IS BOUND.
    gr_grid->check_changed_data( ).
  ENDIF.

  CASE r_ucomm.
    WHEN '&SUBMIT'.
      PERFORM submit.
      LEAVE TO SCREEN 0.
  ENDCASE.
*  gr_grid->refresh_table_display(       EXPORTING  is_stable      = VALUE #( row = 'X' col = 'X' )
*                                                   i_soft_refresh = 'X'
*                                        EXCEPTIONS finished = 1
*                                                   OTHERS   = 2  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process .
  PERFORM ins_add_to_db.
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
  DATA: ls_mafi TYPE zg3mafi,
        lt_mafi TYPE STANDARD TABLE OF zg3mafi WITH UNIQUE SORTED KEY prim COMPONENTS matnr.


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
*& Form SUBMIT
*&---------------------------------------------------------------------*
*& Submit button on Extend ALV
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM submit .
  "Extend duplicate MATNR on ALV
  DATA(lr_bapi) = NEW lcl_impl_bapi( ).
  lr_bapi->extend_dmatnr(
    CHANGING
      c_itab = gt_extend
  ).
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
  DATA: lt_index TYPE lvc_t_row.
  DATA: lt_sel TYPE STANDARD TABLE OF zmmg3d WITH EMPTY KEY.
  DATA: lv_text TYPE text100.

  CASE i_mark.
    WHEN 'X'.
      MESSAGE i027 INTO lv_text.
    WHEN OTHERS.
      MESSAGE i028 INTO lv_text.
  ENDCASE.
  IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_false.
    RETURN.
  ENDIF.

  "§1. Get selected rows
  go_alv_table->get_selected_rows(
    IMPORTING
      et_index_rows =   lt_index
  ).

  IF lt_index IS INITIAL.
    MESSAGE e004.
  ENDIF.

  LOOP AT lt_index ASSIGNING FIELD-SYMBOL(<fs_index>).
    TRY .
        lt_sel = VALUE #( BASE lt_sel ( gt_display[ <fs_index>-index ] ) ).
      CATCH cx_sy_itab_line_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.

  "§2. Init bal log
  gr_bal_log = NEW #( ).
  gr_bal_log->bal_log_create( ).

  "§3. Implement bapi to Delete material
  lcl_impl_bapi_d=>get_instance(
                )->delete_matnr(
                    EXPORTING
                      i_mark = |{ i_mark }|
                    CHANGING
                      c_itab = lt_sel
                  ).

  "§4.Reflect ALV
  LOOP AT lt_sel INTO DATA(ls_sel).
    READ TABLE gt_display ASSIGNING FIELD-SYMBOL(<ls_alv>) WITH KEY matnr = ls_sel-matnr
                                                                    lgort = ls_sel-lgort.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    <ls_alv>-del_flag = ls_sel-del_flag.
    IF <ls_alv>-del_flag IS NOT INITIAL.
      <ls_alv>-del_icon = icon_incomplete.
    ELSE.
      CLEAR   <ls_alv>-del_icon .
    ENDIF.
    <ls_alv>-status   = ls_sel-status.
    PERFORM set_d_icon CHANGING <ls_alv>.
  ENDLOOP.

  "§5. Display log
  gr_bal_log->bal_log_display( ).

  "§5. Refresh ALV
  go_alv_table->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = 'X' col = 'X' )
                                                  i_soft_refresh = 'X'
                                      EXCEPTIONS finished = 1
                                                 OTHERS   = 2  ).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_wekrstx
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM modify_f4_text  USING io_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  "Display plant description
  LOOP AT io_data_changed->mt_mod_cells INTO DATA(ls_data_changed) WHERE fieldname = 'WERKS'.
    READ TABLE gt_extend INDEX ls_data_changed-row_id TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      TRY.
          io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = 'WERKSTX'
            i_value     = VALUE #( gr_masterdata->m_t001w[ werks = ls_data_changed-value ]-name1 )
          ).
        CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDLOOP.

  "DISPLAY LGORT DESCRIPTION
  LOOP AT io_data_changed->mt_mod_cells INTO ls_data_changed WHERE fieldname = 'LGORT'.
    READ TABLE gt_extend INDEX ls_data_changed-row_id TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      TRY.
          io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = 'LGOBE'
            i_value     = VALUE #( gr_masterdata->m_t001l[ lgort = ls_data_changed-value ]-lgobe )
          ).
        CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_d_icon
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_ALV>
*&---------------------------------------------------------------------*
FORM set_d_icon CHANGING cs_alv TYPE zmmg3d.
  CASE cs_alv-status.
    WHEN 'E'.
      cs_alv-icon = icon_red_light.
    WHEN 'S'.
      cs_alv-icon = icon_green_light.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form lgort_f4  ( NO USE )
*&---------------------------------------------------------------------*
*& Search help for sloc
*&---------------------------------------------------------------------*
*&      --> <LS_EXTEND>_WERKS
*&      <-- LT_LGORT
*&---------------------------------------------------------------------*
*FORM lgort_f4  USING    i_werks TYPE werks_d
*               CHANGING ct_lgort TYPE STANDARD TABLE.
*  SELECT lgort,
*         lgobe
*    FROM t001l
*    WHERE werks = @i_werks
*   INTO TABLE @ct_lgort.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form set_f4 ( NO USE )
**&---------------------------------------------------------------------*
**& Set search help for sloc
**&---------------------------------------------------------------------*
**&      <-- LT_F4
**&---------------------------------------------------------------------*
*FORM set_f4  CHANGING ct_f4 TYPE lvc_t_f4.
*
*  ct_f4 = VALUE #( ( fieldname = 'LGORT'  register = 'X'
*                     getbefore = space    chngeafter = space ) ).
*ENDFORM.
