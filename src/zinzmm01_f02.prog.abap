*&---------------------------------------------------------------------*
*& Include ZINZMM01_F02
*&---------------------------------------------------------------------*
*&       HANDLE DISPLAY FLOW                                                              *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form d_mat
*&---------------------------------------------------------------------*
*& handle display flow
*&---------------------------------------------------------------------*
FORM display_material.
  IF gs_error IS NOT INITIAL.
    MESSAGE s073 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  PERFORM get_data. " Get all the data for the import parameters of bapi_material_get_all
  LOOP AT gt_material INTO gs_material.
    PERFORM bapi_material_getall. "* Get all the data for the materials given in the selection screen.
    IF NOT line_exists( gt_bapireturn[ type = 'E' ] ).
      PERFORM append_data USING    gt_display
                          CHANGING gs_display. " append data to alv table for display
    ENDIF.
  ENDLOOP.
  PERFORM display_data.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_material
*&---------------------------------------------------------------------*
*& Form to get the data for the import parameters of  bapi_material_getall.
*&---------------------------------------------------------------------*
FORM get_data.
  SELECT mara~matnr,
         marc~werks,
         mbew~bwkey,
         mbew~bwtar,
         mvke~vkorg,
         mvke~vtweg
    FROM mara
    LEFT JOIN marc  ON mara~matnr = marc~matnr
    LEFT JOIN t001w ON t001w~werks = marc~werks
    LEFT JOIN mbew  ON  mbew~matnr = mara~matnr
                    AND mbew~bwkey = t001w~bwkey
    LEFT JOIN  mvke ON mvke~matnr = mara~matnr
    INTO CORRESPONDING FIELDS OF TABLE @gt_material
    WHERE mara~matnr IN @smatnr
      AND marc~werks IN @swerks
      AND mara~mbrsh IN @smbrsh
      AND mara~mtart IN @smtart.
  SORT gt_material BY matnr
                      werks ASCENDING.
  IF sy-subrc <> 0.
    MESSAGE s006 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  " get attribute
  SELECT * FROM zg3mafi
    INTO CORRESPONDING FIELDS OF TABLE @gt_mafi
    FOR ALL ENTRIES IN @gt_material
    WHERE matnr = @gt_material-matnr.
  SORT gt_mafi BY matnr ASCENDING.
  " get storage locs.
  SELECT mard~matnr,
         mard~werks,
         mard~lgort
    FROM mard
    INTO CORRESPONDING FIELDS OF TABLE @gt_mard
    FOR ALL ENTRIES IN @gt_material
    WHERE mard~matnr  = @gt_material-matnr
      AND mard~werks  = @gt_material-werks
      AND mard~lgort IN @slgort.
  SORT gt_mard BY matnr
                  werks
                  lgort ASCENDING.
  IF sy-subrc <> 0.
    MESSAGE s006 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_material_detail
*&---------------------------------------------------------------------*
*& Form to get the data from database tables MARA, MARC, MPOP, MPGD,MBEW,
*  MLGN, MVKE, MLGT for the material
*&---------------------------------------------------------------------*
FORM bapi_material_getall.
  DATA lv_objectkey  TYPE bapi1003_key-object.
  DATA lv_material   TYPE bapi_mara_ga-material.
  DATA lv_val_area   TYPE bapi_mbew_ga-val_area.
  DATA lv_val_type   TYPE bapi_mbew_ga-val_type.
  DATA lv_plant      TYPE bapi_marc_ga-plant.
  DATA lv_stgel      TYPE bapi_mard_ga-stge_loc.
  DATA lv_salesorg   TYPE bapi_mvke_ga-sales_org.
  DATA lv_distr_chan TYPE bapi_mvke_ga-distr_chan.
  CONSTANTS lc_objecttable_imp TYPE bapi1003_key-objecttable VALUE 'MARA'.

  " fetch data to bapi parameters
  lv_material = gs_material-matnr.
  lv_plant    = gs_material-werks.
  lv_val_area = gs_material-bwkey.
  lv_val_type = gs_material-bwtar.
  lv_salesorg = gs_material-vkorg.
  lv_distr_chan = gs_material-vtweg.
  lv_objectkey = gs_material-matnr.
  IF gt_mard IS NOT INITIAL.

    READ TABLE gt_mard INTO DATA(ls_mard) WITH KEY matnr = gs_material-matnr
                                                   werks = gs_material-werks BINARY SEARCH.
    IF sy-subrc = 0.
      lv_stgel = ls_mard-lgort.
    ENDIF.
  ENDIF.

  " call bapi to get material number
  CALL FUNCTION 'BAPI_MATERIAL_GETALL'
    EXPORTING
      material                  = lv_material
      valuationarea             = lv_val_area
      valuationtype             = lv_val_type
      plant                     = lv_plant
      storagelocation           = lv_stgel
      salesorganisation         = lv_salesorg
      distributionchannel       = lv_distr_chan
    IMPORTING
      clientdata                = gs_bapi_mara
      plantdata                 = gs_bapi_marc
      storagelocationdata       = gs_bapi_mard
      valuationdata             = gs_bapi_mbew
      salesdata                 = gs_bapi_mvke
    TABLES
      materialdescription       = gt_bapi_makt_ga
      unitsofmeasure            = gt_bapi_marm_ga
      internationarticlenumbers = gt_bapi_mean_ga
      materialtext              = gt_bapi_mltx_ga
      return                    = gt_bapireturn.

  " call bapi to get material classification
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp   = lv_objectkey
      objecttable_imp = lc_objecttable_imp
      classtype_imp   = '023'
    TABLES
      alloclist       = gt_classes
      return          = gt_return.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY MATERIAL ON ALV
*&---------------------------------------------------------------------*
FORM display_data.
  DATA lt_field_cat TYPE lvc_t_fcat.
  DATA ls_layout    TYPE lvc_s_layo.
  DATA lv_lines     TYPE i.

  gr_bal_log = NEW #( ).

  PERFORM pepare_field_cat CHANGING lt_field_cat.
  PERFORM perpare_layout CHANGING ls_layout.
  PERFORM display_material_alv USING ls_layout
                                     lt_field_cat.
  lv_lines = lines( gt_display ).
  MESSAGE s024(msitem) WITH lv_lines.
  CALL SCREEN 9000.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form pepare_field_cat
*&---------------------------------------------------------------------*
*& FIELD CATALOG INITIAL
*&---------------------------------------------------------------------*
FORM pepare_field_cat CHANGING ch_t_field_cat TYPE lvc_t_fcat.
  TYPES: BEGIN OF lty_field,
           fieldname TYPE lvc_fname,
           label     TYPE lvc_txtcol,
         END OF lty_field.
  DATA lt_field_tech TYPE STANDARD TABLE OF lty_field WITH EMPTY KEY.

  lt_field_tech = VALUE #( ( fieldname = 'MESSAGE'    )
                           ( fieldname = 'STATUS'     )
                           ( fieldname = 'DEL_FLAG'   )
                           ( fieldname = 'ICON'        label = 'Status' )
                           ( fieldname = 'DEL_ICON'    label = 'Flag Material for Deletion' )
                           ( fieldname = 'CLIENT'     )
                           ( fieldname = 'PLANT'      )
                           ( fieldname = 'SLOC'       ) )
                          ##NO_TEXT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSTMMG3D'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = ch_t_field_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  LOOP AT ch_t_field_cat INTO DATA(ls_cfield).
    ls_cfield-colddictxt = 'L'.
    ls_cfield-selddictxt = 'L'.
    CASE ls_cfield-fieldname.
      WHEN 'MATNR' OR 'ICON' OR 'DEL_ICON' OR 'MBRSH'
        OR 'MTART' OR 'WERKS' OR 'LGORT'.
        ls_cfield-fix_column = 'X'.
      WHEN 'WAERS'.
        ls_cfield-no_out = 'X'.
    ENDCASE.
    MODIFY ch_t_field_cat FROM ls_cfield.
  ENDLOOP.
  LOOP AT lt_field_tech INTO DATA(ls_field).
    ASSIGN ch_t_field_cat[ fieldname = ls_field-fieldname ] TO FIELD-SYMBOL(<fs_fcat>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    <fs_fcat>-coltext   = ls_field-label.
    <fs_fcat>-scrtext_l = <fs_fcat>-coltext.
    <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l.
    <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m
                          .
    <fs_fcat>-just      = 'C'.
    CASE <fs_fcat>-fieldname.
      WHEN 'ICON'.
        CONTINUE.
      WHEN 'DEL_ICON'.
        CONTINUE.
      WHEN OTHERS.
        <fs_fcat>-tech = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form perpare_layout
*&---------------------------------------------------------------------*
*& ALV LAYOUT INITIAL
*&---------------------------------------------------------------------*

FORM perpare_layout CHANGING ch_ls_layout TYPE lvc_s_layo.
  ch_ls_layout-cwidth_opt = 'A'.
  ch_ls_layout-col_opt    = 'A'.
  ch_ls_layout-zebra      = abap_true.
  ch_ls_layout-sel_mode   = 'A'.
  ch_ls_layout-no_f4      = 'X'.
  ch_ls_layout-grid_title = TEXT-t03.
  ch_ls_layout-smalltitle = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_material_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display_material_alv USING im_s_layout  TYPE lvc_s_layo
                                it_field_cat TYPE lvc_t_fcat.

  DATA ls_variant TYPE disvariant.

  go_container = NEW cl_gui_custom_container( container_name = 'ALV_CONT'
                                              repid          = sy-repid
                                              dynnr          = '9000' ).
  go_alv_table = NEW cl_gui_alv_grid( i_parent = go_container  ).

  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.
  go_alv_table->set_table_for_first_display( EXPORTING  is_layout                     = im_s_layout   " Layout
                                                        is_variant                    = ls_variant              " Layout
                                                        i_save                        = 'A'            " Save Layout
                                             CHANGING   it_outtab                     = gt_display    " Output Table
                                                        it_fieldcatalog               = it_field_cat " Field Catalog
                                             EXCEPTIONS invalid_parameter_combination = 1                " Wrong Parameter
                                                        program_error                 = 2                " Program Errors
                                                        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
                                                        OTHERS                        = 4 ).
  IF sy-subrc <> 0.
    MESSAGE s007 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form append_data
*&---------------------------------------------------------------------*
*& append data retrived from bapi to internal table
*&---------------------------------------------------------------------*

FORM append_data USING    i_table TYPE ANY TABLE
                 CHANGING c_gs    TYPE ty_alvlist.

  FIELD-SYMBOLS <lfs_table> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lfs_gs>    TYPE ty_alvlist.

  ASSIGN i_table TO <lfs_table>.
  ASSIGN c_gs TO <lfs_gs>.
  CLEAR <lfs_gs>.
  DATA lv_conv_price TYPE bapi_price.
  " Client data
  <lfs_gs>-matnr      = gs_bapi_mara-material.
  <lfs_gs>-mbrsh      = gs_bapi_mara-ind_sector.
  <lfs_gs>-mtart      = gs_bapi_mara-matl_type.
  <lfs_gs>-meins      = gs_bapi_mara-base_uom.
  <lfs_gs>-matkl      = gs_bapi_mara-matl_group.
  <lfs_gs>-mtpos_mara = gs_bapi_mara-item_cat.
  <lfs_gs>-groes      = gs_bapi_mara-size_dim.
  <lfs_gs>-ntgew      = gs_bapi_mara-net_weight.
  <lfs_gs>-spart      = gs_bapi_mara-division.
  <lfs_gs>-bstme      = gs_bapi_mara-po_unit.
  <lfs_gs>-xchpf      = gs_bapi_mara-batch_mgmt.
  <lfs_gs>-ekwsl      = gs_bapi_mara-pur_valkey.
  <lfs_gs>-raube      = gs_bapi_mara-stor_conds.
  <lfs_gs>-behvo      = gs_bapi_mara-container.
  <lfs_gs>-tempb      = gs_bapi_mara-temp_conds.
  <lfs_gs>-mhdrz      = gs_bapi_mara-minremlife.
  <lfs_gs>-iprkz      = gs_bapi_mara-period_ind_expiration_date.

  " Plant Data
  <lfs_gs>-mtvfp      = gs_bapi_marc-availcheck.
  <lfs_gs>-werks      = gs_bapi_marc-plant.
  <lfs_gs>-ekgrp      = gs_bapi_marc-pur_group.
  <lfs_gs>-plifz      = gs_bapi_marc-plnd_delry.
  <lfs_gs>-webaz      = gs_bapi_marc-gr_pr_time.
  <lfs_gs>-kordb      = gs_bapi_marc-sourcelist.
  <lfs_gs>-insmk      = gs_bapi_marc-ind_post_to_insp_stock.
  <lfs_gs>-ausme      = gs_bapi_marc-issue_unit.
  <lfs_gs>-maxlz      = gs_bapi_marc-stgeperiod.
  <lfs_gs>-lzeih      = gs_bapi_marc-stge_pd_un.

  " Storage Location Data
  <lfs_gs>-lgort      = gs_bapi_mard-stge_loc.

  " Valuation Data
  <lfs_gs>-bwtty      = gs_bapi_mbew-val_cat.
  <lfs_gs>-waers      = gs_bapi_mbew-currency.
  <lfs_gs>-bwkey      = gs_bapi_mbew-val_area.
  <lfs_gs>-bwtar      = gs_bapi_mbew-val_type.
  <lfs_gs>-bklas      = gs_bapi_mbew-val_class.
  <lfs_gs>-mlast      = gs_bapi_mbew-ml_settle.
  <lfs_gs>-vprsv      = gs_bapi_mbew-price_ctrl.
  <lfs_gs>-vjpei      = gs_bapi_mbew-pr_unit_py.
  <lfs_gs>-zkdat      = gs_bapi_mbew-valid_from.
  " convert to internal price for FUTURE PRICE
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = gs_bapi_mbew-currency
      amount_external      = gs_bapi_mbew-future_pr
      max_number_of_digits = 23
    IMPORTING
      amount_internal      = lv_conv_price.
  <lfs_gs>-zkprs = lv_conv_price.
  CLEAR lv_conv_price.

  " convert to internal price for moving price
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = gs_bapi_mbew-currency
      amount_external      = gs_bapi_mbew-moving_pr
      max_number_of_digits = 23
    IMPORTING
      amount_internal      = lv_conv_price.
  <lfs_gs>-verpr = lv_conv_price.
  CLEAR lv_conv_price.

  " convert to internal price for standard price
  CLEAR lv_conv_price.
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = gs_bapi_mbew-currency
      amount_external      = gs_bapi_mbew-std_price
      max_number_of_digits = 23
    IMPORTING
      amount_internal      = lv_conv_price.
  <lfs_gs>-stprs = lv_conv_price.
  CLEAR lv_conv_price.

  " Sales Data
  <lfs_gs>-vkorg = gs_bapi_mvke-sales_org.
  <lfs_gs>-vtweg = gs_bapi_mvke-distr_chan.
  <lfs_gs>-vrkme = gs_bapi_mvke-sales_unit.
  <lfs_gs>-dwerk = gs_bapi_mvke-delyg_plnt.
  <lfs_gs>-sktof = gs_bapi_mvke-cash_disc.
  <lfs_gs>-aumng = gs_bapi_mvke-min_order.
  <lfs_gs>-lfmng = gs_bapi_mvke-min_dely.
  <lfs_gs>-scmng = gs_bapi_mvke-dely_unit.
  <lfs_gs>-schme = gs_bapi_mvke-dely_uom.

  " Additional Data
  READ TABLE gt_mafi INTO DATA(ls_mafi) WITH KEY matnr = <lfs_gs>-matnr BINARY SEARCH.
  <lfs_gs>-att1           = ls_mafi-att1.
  <lfs_gs>-att2           = ls_mafi-att2.
  <lfs_gs>-att3           = ls_mafi-att3.
  <lfs_gs>-att4           = ls_mafi-att4.
  <lfs_gs>-att5           = ls_mafi-att5.
  <lfs_gs>-att6           = ls_mafi-att6.
  <lfs_gs>-att7           = ls_mafi-att7.
  <lfs_gs>-att8           = ls_mafi-att8.
  <lfs_gs>-att9           = ls_mafi-att9.
  <lfs_gs>-att10          = ls_mafi-att10.
  <lfs_gs>-att11          = ls_mafi-att11.
  <lfs_gs>-att12          = ls_mafi-att12.
  <lfs_gs>-att13          = ls_mafi-att13.
  <lfs_gs>-att14          = ls_mafi-att14.
  <lfs_gs>-att15          = ls_mafi-att15.
  <lfs_gs>-att16          = ls_mafi-att16.
  <lfs_gs>-att17          = ls_mafi-att17.
  <lfs_gs>-att18          = ls_mafi-att18.
  <lfs_gs>-att19          = ls_mafi-att19.
  <lfs_gs>-att20          = ls_mafi-att20.
  <lfs_gs>-att21          = ls_mafi-att21.
  <lfs_gs>-att22          = ls_mafi-att22.
  <lfs_gs>-att23          = ls_mafi-att23.
  <lfs_gs>-att24          = ls_mafi-att24.
  <lfs_gs>-att25          = ls_mafi-att25.
  <lfs_gs>-att26          = ls_mafi-att26.

  "{ begin TrucTT add
  <lfs_gs>-del_flag       = COND #( WHEN gs_bapi_mara-del_flag IS NOT INITIAL
                                      OR gs_bapi_marc-del_flag IS NOT INITIAL
                                      OR gs_bapi_mard-del_flag IS NOT INITIAL
                                    THEN 'X' ).
  <lfs_gs>-del_flag_level = VALUE #( client = gs_bapi_mara-del_flag
                                     plant  = gs_bapi_marc-del_flag
                                     sloc   = gs_bapi_mard-del_flag ).

  " classification
  READ TABLE gt_classes INTO DATA(ls_class) WITH KEY object = <lfs_gs>-matnr.
  IF sy-subrc = 0.
    <lfs_gs>-class = ls_class-classnum.
    <lfs_gs>-klart = ls_class-classtype.
  ENDIF.

  " Material Description
  READ TABLE gt_bapi_makt_ga INTO DATA(ls_makt) WITH KEY langu = sy-langu.
  IF sy-subrc = 0.
    <lfs_gs>-maktx = ls_makt-matl_desc.
    <lfs_gs>-spras = ls_makt-langu.
  ENDIF.

*  " Material Gross Weight
  READ TABLE gt_bapi_marm_ga INTO DATA(ls_marm) WITH KEY alt_unit = <lfs_gs>-meins.

  IF sy-subrc = 0.
    <lfs_gs>-brgew = ls_marm-gross_wt.
    <lfs_gs>-gewei = ls_marm-unit_of_wt.
  ENDIF.

  "{begin TrucTT - 2024.11.22 -  ADD  - Assign icon DELETE for material deleted.
  IF <lfs_gs>-del_flag IS NOT INITIAL.
    <lfs_gs>-del_icon = icon_incomplete.
  ENDIF.
  "}end add

  APPEND <lfs_gs> TO <lfs_table>.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_selected_rows
*&---------------------------------------------------------------------*
FORM get_selected_rows.
  DATA lt_row_ids TYPE lvc_t_row. " Table containing selected row indexes
  DATA lv_row     TYPE lvc_s_row. " Work area for a selected row index.

  CLEAR gt_selected.

  " Get list of selected rows
  go_alv_table->get_selected_rows( IMPORTING et_index_rows = lt_row_ids ).

  " Browse the list of selected row indexes
  LOOP AT lt_row_ids INTO lv_row.
    READ TABLE gt_display INTO DATA(ls_select) INDEX lv_row-index.
    IF sy-subrc = 0.
      CLEAR:ls_select-icon,
             ls_select-status.
      APPEND ls_select TO gt_selected. " add row(s) it_sel
    ENDIF.
  ENDLOOP.
  IF gt_selected IS INITIAL.
    MESSAGE e035.
  ENDIF.

  LOOP AT gt_selected ASSIGNING FIELD-SYMBOL(<ls_selected>).

    IF <ls_selected>-bwtty IS NOT INITIAL.
      IF <ls_selected>-bwtar IS NOT INITIAL.
        CLEAR gs_cellstyle.
        gs_cellstyle-fieldname = 'BWTTY'.
        gs_cellstyle-style     = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gs_cellstyle TO <ls_selected>-cellstyle.
        CLEAR gs_cellstyle.
        gs_cellstyle-fieldname = 'STPRS'.
        gs_cellstyle-style     = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gs_cellstyle TO <ls_selected>-cellstyle.
      ELSE.
        CLEAR gs_cellstyle.
        gs_cellstyle-fieldname = 'STPRS'.
        gs_cellstyle-style     = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gs_cellstyle TO <ls_selected>-cellstyle.
      ENDIF.
    ELSE.
      IF <ls_selected>-vprsv = 'V'.
        CLEAR gs_cellstyle.
        gs_cellstyle-fieldname = 'STPRS'.
        gs_cellstyle-style     = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gs_cellstyle TO <ls_selected>-cellstyle.
      ELSEIF <ls_selected>-vprsv = 'S'.
        CLEAR gs_cellstyle.
        gs_cellstyle-fieldname = 'VERPR'.
        gs_cellstyle-style     = cl_gui_alv_grid=>mc_style_disabled.
        APPEND gs_cellstyle TO <ls_selected>-cellstyle.
      ENDIF.
    ENDIF.

  ENDLOOP.

  " Truc test Authority check
  LOOP AT gt_selected ASSIGNING <ls_selected>.
    AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
                    ID 'ACTVT' FIELD '02'
                    ID 'WERKS' FIELD <ls_selected>-werks.
    IF sy-subrc <> 0.
      MESSAGE e073(/ibs/rb) WITH 'plant' <ls_selected>-werks space.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_second_alv
*&---------------------------------------------------------------------*
*
FORM display_selected_rows.
  DATA lt_fcat    TYPE lvc_t_fcat.
  DATA ls_slayout TYPE lvc_s_layo.

  PERFORM edit_layout CHANGING ls_slayout.
  PERFORM edit_field_cat CHANGING lt_fcat.
  PERFORM display_sel_alv USING ls_slayout
                                lt_fcat.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_Edt_UCOM
*&---------------------------------------------------------------------*
FORM handle_edt_ucom USING r_ucomm     LIKE sy-ucomm
                           rs_selfield TYPE slis_selfield ##NEEDED ##CALLED.

  DATA lv_text TYPE text100.

  FREE gt_changed.
  CASE r_ucomm.
    WHEN '&SUBMIT'.
      PERFORM get_changed_rows.
      IF gt_changed IS INITIAL.
        MESSAGE e046.
        LEAVE LIST-PROCESSING.
      ENDIF.
      MESSAGE i045 INTO lv_text.
      IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_true.
        PERFORM save_edited_data.
        PERFORM refresh_alv.
      ENDIF.
      gr_grid->refresh_table_display( ).
    WHEN 'CANC' OR '&BACK'.
      MESSAGE i052 INTO lv_text.
      PERFORM get_changed_rows.
      IF gt_changed IS NOT INITIAL.
        IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_true.
          LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN '&CANC' OR '&EXIT'.
      MESSAGE i052 INTO lv_text.
      PERFORM get_changed_rows.
      IF gt_changed IS NOT INITIAL.
        IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_true.
          LEAVE PROGRAM.
        ENDIF.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_edited_data
*&---------------------------------------------------------------------*
FORM save_edited_data.
  LOOP AT gt_changed ASSIGNING FIELD-SYMBOL(<lfs_changed>). " gs_selected.
    " fill bapi
    PERFORM prepare_data USING <lfs_changed>.
    " implement bapi
    PERFORM bapi_material_savedata CHANGING <lfs_changed>.
    IF <lfs_changed>-bwkey IS NOT INITIAL.
      IF <lfs_changed>-bwtty IS NOT INITIAL AND <lfs_changed>-bwtar IS INITIAL.
        CONTINUE.
      ELSE.
        PERFORM bapi_matval_pricechange CHANGING <lfs_changed>.
      ENDIF.
    ENDIF.
    PERFORM mod_att USING <lfs_changed>.
*    CLEAR gs_selected.
    gr_grid->refresh_table_display( ).
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form edit_field_cat
*&---------------------------------------------------------------------*
*& field catalog for edit material
*&---------------------------------------------------------------------*
FORM edit_field_cat CHANGING ch_t_fcat TYPE lvc_t_fcat.
  " Call Function Module to create Field Catalog from table structure
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSTMMG3D'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = ch_t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  " Customize Field Catalog: lock fields from editing, f4 help
  LOOP AT ch_t_fcat INTO DATA(ls_field).
    ls_field-colddictxt = 'L'.
    ls_field-selddictxt = 'L'.
    CASE ls_field-fieldname.
      WHEN 'MATNR' OR 'MBRSH' OR 'MTART'
        OR 'WERKS' OR 'LGORT'.
        ls_field-fix_column = 'X'.
      WHEN 'MEINS' OR 'MATKL' OR 'KLART' OR 'SPART'
        OR 'VKORG' OR 'VTWEG' OR 'EKGRP' OR 'BWKEY'
        OR 'VPRSV' OR 'VJPEI' OR 'BWTAR '
        OR 'CLASS' OR 'MLAST' OR 'SPRAS'.
      WHEN 'DEL_ICON' OR 'STATUS' OR 'DEL_FLAG'
        OR 'CLIENT' OR 'PLANT' OR 'SLOC'.
        ls_field-tech = 'X'.
      WHEN 'ICON'.
        ls_field-coltext    = ls_field-scrtext_s = ls_field-scrtext_m = ls_field-scrtext_L = 'Status' ##NO_TEXT.
        ls_field-fix_column = 'X'.
      WHEN 'MESSAGE'.
        ls_field-coltext    = ls_field-scrtext_s = ls_field-scrtext_m = ls_field-scrtext_l = 'Message' ##NO_TEXT.
        ls_field-fix_column = 'X'.
      WHEN 'WAERS'.
        ls_field-no_out = 'X'.
      WHEN 'MTVFP'.
        ls_field-f4availabl ='X'.
        ls_field-checktable = 'X'.
        ls_field-edit = 'X'.
      WHEN 'BWTTY'.
        ls_field-f4availabl ='X'.
        ls_field-checktable = 'X'.
        ls_field-edit = 'X'.
      WHEN 'BKLAS'.
        ls_field-f4availabl ='X'.
        ls_field-checktable = 'X'.
        ls_field-edit = 'X'.
      WHEN OTHERS.
        ls_field-edit = 'X'.
    ENDCASE.
    MODIFY ch_t_fcat FROM ls_field.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bapi_material_savedata
*&---------------------------------------------------------------------*
*& CALL BAPI to save edited material data
*&---------------------------------------------------------------------*

FORM bapi_material_savedata CHANGING ch_t_edit TYPE ty_alvlist.
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata             = gs_bapi_mathead
      clientdata           = gs_bapi_s_mara
      clientdatax          = gs_bapi_marax
      plantdata            = gs_bapi_s_marc
      plantdatax           = gs_bapi_marcx
      storagelocationdata  = gs_bapi_s_mard
      storagelocationdatax = gs_bapi_mardx
      valuationdata        = gs_bapi_s_mbew
      valuationdatax       = gs_bapi_mbewx
      salesdata            = gs_bapi_s_mvke
      salesdatax           = gs_bapi_mvkex
    IMPORTING
      return               = gs_return
    TABLES
      materialdescription  = gt_bapi_makt
      unitsofmeasure       = gt_bapi_marm
      unitsofmeasurex      = gt_bapi_marmx
      returnmessages       = gt_returnx.

  IF gs_return-type <> 'S'.
    ch_t_edit-status  = 'E'.
    ch_t_edit-message = gs_return-message.
    ch_t_edit-icon    = icon_red_light.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSEIF gs_return-type = 'S'.
    ch_t_edit-status = 'S'.
    MESSAGE s025 INTO ch_t_edit-message.
    ch_t_edit-icon = icon_green_light.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
  LOOP AT gt_selected TRANSPORTING NO FIELDS WHERE     matnr = ch_t_edit-matnr
                                                   AND werks = ch_t_edit-werks
                                                   AND lgort = ch_t_edit-lgort
                                                   AND bwkey = ch_t_edit-bwkey
                                                   AND bwtar = ch_t_edit-bwtar.
    MODIFY gt_selected FROM ch_t_edit.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form prepare_datax
*&---------------------------------------------------------------------*
*& Prepare structurex to fill in bapi_material_savedata
*&---------------------------------------------------------------------*
FORM prepare_data USING i_sel TYPE ty_alvlist.
  DATA lv_conv_price TYPE bapi_price.

  " head data
  gs_bapi_mathead = VALUE #( material      = i_sel-matnr
                             ind_sector    = i_sel-mbrsh
                             matl_type     = i_sel-mtart
                             basic_view    = 'X'
                             sales_view    = 'X'
                             account_view  = 'X'
                             purchase_view = 'X'
                             storage_view  = 'X' ).
  " cilent data
  gs_bapi_s_mara = VALUE #( matl_group = i_sel-matkl
                            base_uom   = i_sel-meins
                            item_cat   = i_sel-mtpos_mara
                            size_dim   = i_sel-groes
                            net_weight = i_sel-ntgew
                            division   = i_sel-spart
                            po_unit    = i_sel-bstme
                            batch_mgmt = i_sel-xchpf
                            pur_valkey = i_sel-ekwsl
                            stor_conds = i_sel-raube
                            container  = i_sel-behvo
                            temp_conds = i_sel-tempb
                            minremlife = i_sel-mhdrz ).
  gs_bapi_marax = VALUE #( matl_group           = 'X'
                           base_uom             = 'X'
                           item_cat             = 'X'
                           size_dim             = 'X'
                           net_weight           = 'X'
                           division             = 'X'
                           po_unit              = 'X'
                           batch_mgmt           = 'X'
                           pur_valkey           = 'X'
                           stor_conds           = 'X'
                           container            = 'X'
                           temp_conds           = 'X'
                           minremlife           = 'X'
                           quarantine_period_un = 'X'  ).

  " plant data
  gs_bapi_s_marc = VALUE #( plant                  = i_sel-werks
                            availcheck             = i_sel-mtvfp
                            pur_group              = i_sel-ekgrp
                            plnd_delry             = i_sel-plifz
                            gr_pr_time             = i_sel-webaz
                            sourcelist             = i_sel-kordb
                            ind_post_to_insp_stock = i_sel-insmk
                            issue_unit             = i_sel-ausme
                            stgeperiod             = i_sel-maxlz
                            stge_pd_un             = i_sel-lzeih
                            period_ind             = i_sel-iprkz   ).

  gs_bapi_marcx = VALUE #( plant                  = i_sel-werks
                           availcheck             = 'X'
                           pur_group              = 'X'
                           plnd_delry             = 'X'
                           gr_pr_time             = 'X'
                           sourcelist             = 'X'
                           ind_post_to_insp_stock = 'X'
                           issue_unit             = 'X'
                           stgeperiod             = 'X'
                           stge_pd_un             = 'X'
                           period_ind             = 'X' ).

  " storage locs data
  IF i_sel-lgort IS NOT INITIAL. " TrucTT add code
    gs_bapi_s_mard = VALUE #( plant    = i_sel-werks
                              stge_loc = i_sel-lgort ).

    gs_bapi_mardx = VALUE #(
        plant    = gs_bapi_s_mard-plant
        stge_loc = COND #( WHEN gs_bapi_s_mard-stge_loc IS NOT INITIAL THEN gs_bapi_s_mard-stge_loc ) ).
  ENDIF.

  " material desscription data
  gt_bapi_makt = VALUE #( ( langu     = COND #( WHEN i_sel-spras IS NOT INITIAL THEN i_sel-spras ELSE sy-langu )
                            matl_desc = i_sel-maktx ) ).

  " sale data
  gs_bapi_s_mvke = VALUE #( sales_org  = i_sel-vkorg
                            distr_chan = i_sel-vtweg
                            sales_unit = i_sel-vrkme
                            delyg_plnt = i_sel-dwerk
                            cash_disc  = i_sel-sktof
                            min_order  = i_sel-aumng
                            min_dely   = i_sel-lfmng
                            dely_unit  = i_sel-scmng
                            dely_uom   = i_sel-schme  ).

  gs_bapi_mvkex = VALUE #( sales_org  = i_sel-vkorg
                           distr_chan = i_sel-vtweg
                           sales_unit = 'X'
                           delyg_plnt = 'X'
                           cash_disc  = 'X'
                           min_order  = 'X'
                           min_dely   = 'X'
                           dely_unit  = 'X'
                           dely_uom   = 'X'   ).

  " valuation data

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = i_sel-waers
      amount_internal = i_sel-zkprs
    IMPORTING
      amount_external = lv_conv_price.

  gs_bapi_s_mbew = VALUE #( val_area   = i_sel-bwkey
                            ml_settle  = i_sel-mlast
                            val_class  = i_sel-bklas
                            val_cat    = i_sel-bwtty
                            price_ctrl = i_sel-vprsv
                            price_unit = i_sel-vjpei
                            future_pr  = lv_conv_price
                            valid_from = i_sel-zkdat ).

  gs_bapi_mbewx = VALUE #( val_area   = i_sel-bwkey
                           ml_settle  = 'X'
                           val_class  = 'X'
                           val_cat    = 'X'
                           price_ctrl = 'X'
                           price_unit = 'X'
                           future_pr  = 'X'
                           valid_from = 'X'  ).

  " unit of measure
  gt_bapi_marm = VALUE #( ( gross_wt   = i_sel-brgew
                            alt_unit   = i_sel-meins
                            unit_of_wt = i_sel-gewei ) ).
  "
  gt_bapi_marmx = VALUE #( ( gross_wt   = 'X'
                             unit_of_wt = 'X'
                             alt_unit   = i_sel-meins ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form refresh_alv
*&---------------------------------------------------------------------*
*& refresh data at Display screen by using bapi to get latest data
*&---------------------------------------------------------------------*

FORM refresh_alv.
  DATA ls_refresh TYPE ty_alvlist.
  DATA lt_refresh TYPE STANDARD TABLE OF ty_alvlist.

  CONSTANTS lc_objecttable_imp TYPE bapi1003_key-objecttable VALUE 'MARA'.

  " select material's new attribute data
  SELECT * FROM zg3mafi
    INTO CORRESPONDING FIELDS OF TABLE @gt_mafi
    FOR ALL ENTRIES IN @gt_changed
    WHERE matnr = @gt_changed-matnr.

  LOOP AT gt_changed INTO DATA(ls_changed) GROUP BY ( matnr = ls_changed-matnr ).
    LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE matnr = ls_changed-matnr.
      DATA lv_objectkey  TYPE bapi1003_key-object.
      DATA lv_material   TYPE bapi_mara_ga-material.
      DATA lv_val_area   TYPE bapi_mbew_ga-val_area.
      DATA lv_val_type   TYPE bapi_mbew_ga-val_type.
      DATA lv_plant      TYPE bapi_marc_ga-plant.
      DATA lv_stgel      TYPE bapi_mard_ga-stge_loc.
      DATA lv_salesorg   TYPE bapi_mvke_ga-sales_org.
      DATA lv_distr_chan TYPE bapi_mvke_ga-distr_chan.

      " fetch data to bapi parameters
      lv_material = <lfs_data>-matnr.
      lv_plant    = <lfs_data>-werks.
      lv_stgel    = <lfs_data>-lgort.
      lv_val_area = <lfs_data>-bwkey.
      lv_val_type = <lfs_data>-bwtar.
      lv_salesorg = <lfs_data>-vkorg.
      lv_distr_chan = <lfs_data>-vtweg.
      lv_objectkey = <lfs_data>-matnr.

      " call bapi to get material's new data
      CALL FUNCTION 'BAPI_MATERIAL_GETALL'
        EXPORTING
          material                  = lv_material
          valuationarea             = lv_val_area
          valuationtype             = lv_val_type
          plant                     = lv_plant
          storagelocation           = lv_stgel
          salesorganisation         = lv_salesorg
          distributionchannel       = lv_distr_chan
        IMPORTING
          clientdata                = gs_bapi_mara
          plantdata                 = gs_bapi_marc
          storagelocationdata       = gs_bapi_mard
          valuationdata             = gs_bapi_mbew
          salesdata                 = gs_bapi_mvke
        TABLES
          materialdescription       = gt_bapi_makt_ga
          unitsofmeasure            = gt_bapi_marm_ga
          internationarticlenumbers = gt_bapi_mean_ga
          materialtext              = gt_bapi_mltx_ga
          return                    = gt_bapireturn.

      CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
        EXPORTING
          objectkey_imp   = lv_objectkey
          objecttable_imp = lc_objecttable_imp
          classtype_imp   = '023'
        TABLES
          alloclist       = gt_classes
          return          = gt_return.

      " append new data
      PERFORM append_data USING    lt_refresh
                          CHANGING ls_refresh.
    ENDLOOP.
    DELETE gt_display WHERE matnr = ls_changed-matnr.
  ENDLOOP.
  APPEND LINES OF lt_refresh TO gt_display.
  SORT gt_display BY matnr ASCENDING.
  go_alv_table->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = 'X'
                                                                            col = 'X' )
                                                  i_soft_refresh = 'X'
                                       EXCEPTIONS finished       = 1
                                                  OTHERS         = 2  ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  EXTEND_STATUS
*&---------------------------------------------------------------------*
FORM edit_status USING t_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'ZGUI_EDIT' EXCLUDING t_extab.

  FREE gr_grid.

  IF gr_grid IS NOT BOUND.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = gr_grid.
  ENDIF.

  gr_handler = NEW #( ).
  gr_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  gr_grid->set_ready_for_input( i_ready_for_input = 1 ).
  SET HANDLER gr_handler->on_e_data_changed FOR gr_grid.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form edit_layout
*&---------------------------------------------------------------------*
FORM edit_layout CHANGING ch_ls_slayout TYPE lvc_s_layo.
  ch_ls_slayout-cwidth_opt = 'A'.
  ch_ls_slayout-col_opt    = 'A'.
  ch_ls_slayout-zebra      = abap_true.
  ch_ls_slayout-no_rowins  = abap_true.
  ch_ls_slayout-no_rowmove = abap_true.
  ch_ls_slayout-no_rowmark = abap_true.
  ch_ls_slayout-stylefname = 'CELLSTYLE'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_sel_alv
*&---------------------------------------------------------------------*
*&  DISPLAY SELECTED MATERIAL
*&---------------------------------------------------------------------*

FORM display_sel_alv USING i_s_layout  TYPE lvc_s_layo
                           i_field_cat TYPE lvc_t_fcat.

  DATA l_title TYPE lvc_title VALUE 'Edit Material(s)' ##NO_TEXT.

  DATA(lr_display_alv) = lcl_display_alv=>get_instance( ).

  lr_display_alv->display_alv( EXPORTING i_pf_status = 'EDIT_STATUS'
                                         i_ucomm     = 'HANDLE_EDT_UCOM'
                                         i_title     = l_title
                                         it_fcat     = i_field_cat
                                         i_layout    = i_s_layout
                               IMPORTING i_outtab    = gt_selected ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_input
*&---------------------------------------------------------------------*

FORM validate_input USING io_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA lv_weight         TYPE ntgew.
  DATA lv_storcon        TYPE raube.
  DATA lr_fieldname      TYPE RANGE OF lvc_fname.
  DATA lv_movprice       TYPE verpr.
  DATA lv_stdprice       TYPE stprs.
  DATA lr_fieldname_mara TYPE RANGE OF lvc_fname.

  DATA: BEGIN OF lty_mara,
          matnr      TYPE matnr,
          maktx      TYPE maktx,
          mtpos_mara TYPE mtpos_mara,
          groes      TYPE groes,
          gewei      TYPE gewei,
          brgew      TYPE brgew,
          ntgew      TYPE ntgew,
          spart      TYPE spart,
          vrkme      TYPE vrkme,
          bstme      TYPE bstme,
          schme      TYPE schme,
          xchpf      TYPE xchpf,
          ekwsl      TYPE ekwsl,
          raube      TYPE raube,
          behvo      TYPE behvo,
          tempb      TYPE tempb,
          mhdrz      TYPE mhdrz,
          iprkz      TYPE dattp,
          dwerk      TYPE dwerk_ext,
          sktof      TYPE sktof,
          aumng      TYPE minau,
          lfmng      TYPE minlf,
          scmng      TYPE schrt,
          plifz      TYPE plifz,
          att1       TYPE zattribute1,
          att2       TYPE zattribute2,
          att3       TYPE zattribute3,
          att4       TYPE zattribute4,
          att5       TYPE zattribute5,
          att6       TYPE zattribute6,
          att7       TYPE zattribute7,
          att8       TYPE zattribute8,
          att9       TYPE zattribute9,
          att10      TYPE zattribute10,
          att11      TYPE zattribute11,
          att12      TYPE zattribute12,
          att13      TYPE zattribute13,
          att14      TYPE zattribute14,
          att15      TYPE zattribute15,
          att16      TYPE zattribute16,
          att17      TYPE zattribute17,
          att18      TYPE zattribute18,
          att19      TYPE zattribute19,
          att20      TYPE zattribute20,
          att21      TYPE zattribute21,
          att22      TYPE zattribute22,
          att23      TYPE zattribute23,
          att24      TYPE zattribute24,
          att25      TYPE zattribute25,
          att26      TYPE zattribute26,
        END OF lty_mara.
  FIELD-SYMBOLS <lfs_mod_rows> TYPE STANDARD TABLE.

  lr_fieldname_mara = VALUE #( sign   = 'I'
                               option = 'EQ'
                               ( low = 'MAKTX' )
                               ( low = 'MTPOS_MARA' )
                               ( low = 'GROES' )
                               ( low = 'GEWEI' ) "?
                               ( low = 'BRGEW' )
                               ( low = 'NTGEW' )
                               ( low = 'SPART' )
                               ( low = 'VRKME' )
                               ( low = 'BSTME' )
                               ( low = 'SCHME' )
                               ( low = 'XCHPF' )
                               ( low = 'EKWSL' )
                               ( low = 'RAUBE' )
                               ( low = 'BEHVO' )
                               ( low = 'TEMPB' )
                               ( low = 'MHDRZ' )
                               ( low = 'IPRKZ' )
                               ( low = 'DWERK' )
                               ( low = 'SKTOF' )
                               ( low = 'AUMNG' )
                               ( low = 'LFMNG' )
                               ( low = 'SCMNG' )
                               ( low = 'PLIFZ' )
                               ( low = 'ATT1' )
                               ( low = 'ATT2' )
                               ( low = 'ATT3' )
                               ( low = 'ATT4' )
                               ( low = 'ATT5' )
                               ( low = 'ATT6' )
                               ( low = 'ATT7' )
                               ( low = 'ATT8' )
                               ( low = 'ATT9' )
                               ( low = 'ATT10' )
                               ( low = 'ATT11' )
                               ( low = 'ATT12' )
                               ( low = 'ATT13' )
                               ( low = 'ATT14' )
                               ( low = 'ATT15' )
                               ( low = 'ATT16' )
                               ( low = 'ATT17' )
                               ( low = 'ATT18' )
                               ( low = 'ATT19' )
                               ( low = 'ATT20' )
                               ( low = 'ATT21' )
                               ( low = 'ATT22' )
                               ( low = 'ATT23' )
                               ( low = 'ATT24' )
                               ( low = 'ATT25' )
                               ( low = 'ATT26' ) ).

  lr_fieldname = VALUE #( sign   = 'I'
                          option = 'EQ'
                          ( low = 'NTGEW' )
                          ( low = 'BRGEW' )
                          ( low = 'LFMNG' )
                          ( low = 'AUMNG' )
                          ( low = 'RAUBE' )
                          ( low = 'DWERK' )
                          ( low = 'STPRS' )
                          ( low = 'VERPR' )
                          ( low = 'MTVFP' ) ).

  LOOP AT io_data_changed->mt_mod_cells INTO DATA(ls_data_changed) WHERE fieldname IN lr_fieldname.
    READ TABLE gt_selected INDEX ls_data_changed-row_id INTO DATA(ls_selected).
    IF sy-subrc = 0.
      CASE ls_data_changed-fieldname.
        WHEN 'NTGEW'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).
          IF lv_weight <= ls_selected-brgew.
            CONTINUE.
          ENDIF.
          CLEAR lv_weight.
          io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                        i_tabix     = ls_data_changed-tabix
                                        i_fieldname = ls_data_changed-fieldname
                                        i_value     = ls_selected-ntgew ).
          MESSAGE s031 DISPLAY LIKE 'E'.

        WHEN 'BRGEW'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          IF lv_weight >= ls_selected-ntgew.
            CONTINUE.
          ENDIF.
          CLEAR lv_weight.
          io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                        i_tabix     = ls_data_changed-tabix
                                        i_fieldname = ls_data_changed-fieldname
                                        i_value     = ls_selected-brgew ).
          MESSAGE s032 DISPLAY LIKE 'E'.

        WHEN 'LFMNG'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          IF lv_weight <= ls_selected-aumng.
            CONTINUE.
          ENDIF.
          CLEAR lv_weight.
          io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                        i_tabix     = ls_data_changed-tabix
                                        i_fieldname = ls_data_changed-fieldname
                                        i_value     = ls_selected-lfmng ).
          MESSAGE s033 DISPLAY LIKE 'E'.

        WHEN 'AUMNG'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          IF lv_weight >= ls_selected-lfmng.
            CONTINUE.
          ENDIF.
          io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                        i_tabix     = ls_data_changed-tabix
                                        i_fieldname = ls_data_changed-fieldname
                                        i_value     = ls_selected-aumng ).
          MESSAGE s034 DISPLAY LIKE 'E'.
        WHEN 'RAUBE'.
          lv_storcon = ls_data_changed-value.
          SELECT COUNT(*)
            UP TO 1 ROWS
            FROM t142t
            WHERE raube = @lv_storcon.
          IF sy-subrc <> 0.
            MESSAGE s048 DISPLAY LIKE 'E' WITH lv_storcon.
          ENDIF.
        WHEN 'VERPR'.
          lv_movprice = lcl_utils=>conv_char_to_num( ls_data_changed-value ).
          IF ls_selected-bwtty IS NOT INITIAL AND ls_selected-bwtar IS INITIAL.
            io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                          i_tabix     = ls_data_changed-tabix
                                          i_fieldname = ls_data_changed-fieldname
                                          i_value     = ls_selected-verpr ).
            MESSAGE s066 DISPLAY LIKE 'E'.
          ENDIF.
          IF lv_movprice < 0.
            io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                          i_tabix     = ls_data_changed-tabix
                                          i_fieldname = ls_data_changed-fieldname
                                          i_value     = ls_selected-verpr ).
            MESSAGE s057 DISPLAY LIKE 'E'.
          ENDIF.
        WHEN 'STPRS'.
          lv_stdprice = lcl_utils=>conv_char_to_num( ls_data_changed-value ).
          IF lv_stdprice < 0.
            io_data_changed->modify_cell( i_row_id    = ls_data_changed-row_id
                                          i_tabix     = ls_data_changed-tabix
                                          i_fieldname = ls_data_changed-fieldname
                                          i_value     = ls_selected-stprs ).
            MESSAGE s057 DISPLAY LIKE 'E'.
          ENDIF.
        WHEN 'MTVFP'.
          IF ls_selected-mtart <> 'ZRAW'.
            MESSAGE s072 DISPLAY LIKE 'W' WITH ls_selected-mtart.
          ENDIF.
      ENDCASE.
      CLEAR lv_weight.
    ENDIF.

    gr_grid->refresh_table_display( is_stable      = VALUE #( col = 'X'
                                                              row = 'X' )
                                    i_soft_refresh = 'X'   ).
  ENDLOOP.

  LOOP AT io_data_changed->mt_mod_cells INTO ls_data_changed WHERE fieldname IN lr_fieldname_mara.
    ASSIGN io_data_changed->mp_mod_rows->* TO <lfs_mod_rows>.
    ASSIGN <lfs_mod_rows>[ 1 ] TO FIELD-SYMBOL(<lfs_row>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING <lfs_row> TO lty_mara.
    LOOP AT gt_selected INTO DATA(lr_selected) WHERE matnr = lty_mara-matnr  GROUP BY ( matnr = lr_selected-matnr ).
      LOOP AT GROUP lr_selected ASSIGNING FIELD-SYMBOL(<ls_selected>).
        <ls_selected> = CORRESPONDING #( BASE ( <ls_selected> ) lty_mara ).
      ENDLOOP.
    ENDLOOP.
    gr_grid->refresh_table_display( is_stable      = VALUE #( col = 'X'
                                                              row = 'X' )
                                    i_soft_refresh = 'X'   ).
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form mod_att
*&---------------------------------------------------------------------*
*& MODIFY MATERIAL ATTRIBUTE
*&---------------------------------------------------------------------*

FORM mod_att USING i_sel.
  DATA ls_mafi TYPE zg3mafi.
  DATA lt_mafi TYPE STANDARD TABLE OF zg3mafi.

  MOVE-CORRESPONDING i_sel TO ls_mafi.
  TRY.
      APPEND ls_mafi TO lt_mafi.
      CLEAR ls_mafi.
    CATCH cx_sy_itab_duplicate_key ##NO_HANDLER.
  ENDTRY.
  IF lt_mafi IS NOT INITIAL.
    TRY.
        MODIFY zg3mafi FROM TABLE @lt_mafi.
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
*& Form validate_material
*&---------------------------------------------------------------------*
*& validate material number in selection screen
*&---------------------------------------------------------------------*

FORM validate_material.
  CLEAR: gs_error-material_high,
         gs_error-material_low.
  IF smatnr IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT smatnr INTO DATA(ls_data).
    IF ls_data-low IS NOT INITIAL.
      IF ls_data-option <> 'CP'.
        SELECT COUNT(*)
          UP TO 1 ROWS
          FROM mara
          WHERE matnr = @ls_data-low.
        IF sy-subrc <> 0.
          gs_error-material_low = 'X'.
          MESSAGE s022 DISPLAY LIKE 'E' WITH ls_data-low.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_data-high IS NOT INITIAL.
      SELECT COUNT(*)
        UP TO 1 ROWS
        FROM mara
        WHERE matnr = @ls_data-high.
      IF sy-subrc <> 0.
        gs_error-material_high = 'X'.
        MESSAGE s022 DISPLAY LIKE 'E' WITH ls_data-high.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_plant
*&---------------------------------------------------------------------*
*& validate plant in selection screen
*&---------------------------------------------------------------------*
FORM validate_plant.
  CLEAR: gs_error-plant_high,
         gs_error-plant_low.
  IF swerks IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT swerks INTO DATA(ls_data).
    IF ls_data-low IS NOT INITIAL.
      IF ls_data-option <> 'CP'.
        SELECT COUNT(*)
          UP TO 1 ROWS
          FROM t001w
          WHERE werks = @ls_data-low.
        IF sy-subrc <> 0.
          gs_error-plant_low = 'X'.
          MESSAGE s038 DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_data-high IS NOT INITIAL.
      SELECT COUNT(*)
        UP TO 1 ROWS
        FROM t001w
        WHERE werks = @ls_data-high.
      IF sy-subrc <> 0.
        gs_error-plant_high = 'X'.
        MESSAGE s038 DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_sloc
*&---------------------------------------------------------------------*
*& validate storage location in selection screen
*&---------------------------------------------------------------------*

FORM validate_sloc.
  CLEAR: gs_error-slocs_high,
         gs_error-slocs_low.
  IF slgort IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT slgort INTO DATA(ls_data).
    IF ls_data-low IS NOT INITIAL.
      IF ls_data-option <> 'CP'.
        SELECT COUNT(*)
          UP TO 1 ROWS
          FROM t001l
          WHERE lgort = @ls_data-low.
        IF sy-subrc <> 0.
          gs_error-slocs_low = 'X'.
          MESSAGE s039 DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_data-high IS NOT INITIAL.
      SELECT COUNT(*)
        UP TO 1 ROWS
        FROM t001l
        WHERE lgort = @ls_data-high.
      IF sy-subrc <> 0.
        gs_error-slocs_high = 'X'.
        MESSAGE s039 DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_mat_type
*&---------------------------------------------------------------------*
*& validate material type
*&---------------------------------------------------------------------*
FORM validate_mat_type.
  CLEAR: gs_error-mattype_low,
         gs_error-mattype_high.
  IF smtart IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT smtart INTO DATA(ls_data).
    IF ls_data-low IS NOT INITIAL.
      IF ls_data-option <> 'CP'.
        SELECT COUNT(*)
          UP TO 1 ROWS
          FROM t134
          WHERE mtart = @ls_data-low.
        IF sy-subrc <> 0.
          gs_error-mattype_low = 'X'.
          MESSAGE s068 DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_data-high IS NOT INITIAL.
      SELECT COUNT(*)
        UP TO 1 ROWS
        FROM t134
        WHERE mtart = @ls_data-low.
      IF sy-subrc <> 0.
        gs_error-mattype_high = 'X'.
        MESSAGE s068 DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_ind_sector
*&---------------------------------------------------------------------*
*& validate industry sector
*&---------------------------------------------------------------------*
FORM validate_ind_sector.
  CLEAR gs_error-industry_sector.
  IF smbrsh IS NOT INITIAL.
    SELECT COUNT(*)
      UP TO 1 ROWS
      FROM t137
      WHERE mbrsh IN @smbrsh.
    IF sy-subrc <> 0.
      gs_error-industry_sector = 'X'.
      MESSAGE s067 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_changed_rows
*&---------------------------------------------------------------------*
FORM get_changed_rows.
  DATA ls_selected_temp TYPE ty_alvlist. " Temporaly WA to store value
  DATA ls_display_temp  TYPE ty_alvlist. " Temporaly WA to store value

  LOOP AT gt_selected INTO DATA(ls_selected).
    READ TABLE gt_display INTO DATA(ls_display) WITH KEY matnr = ls_selected-matnr
                                                         werks = ls_selected-werks
                                                         lgort = ls_selected-lgort
                                                         bwkey = ls_selected-bwkey
                                                         bwtar = ls_selected-bwtar.
    MOVE-CORRESPONDING ls_selected TO ls_selected_temp.
    MOVE-CORRESPONDING ls_display TO  ls_display_temp.

    FREE : ls_selected_temp-icon,
           ls_selected_temp-message,
           ls_selected_temp-status,
           ls_selected_temp-cellstyle,
           ls_display_temp-status,
           ls_display_temp-icon,
           ls_display_temp-message,
           ls_display_temp-cellstyle,
           ls_selected-icon,
           ls_selected-message,
           ls_selected-status.

    IF ls_selected_temp <> ls_display_temp.
      MODIFY gt_selected FROM ls_selected TRANSPORTING icon message status.
      APPEND ls_selected TO gt_changed.
    ELSE.
      ls_selected-status = 'i'.
      MESSAGE i047 INTO ls_selected-message.
      ls_selected-icon = icon_yellow_light.
      MODIFY gt_selected FROM ls_selected.
    ENDIF.
  ENDLOOP.
  gr_grid->refresh_table_display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bapi_matval_pricechange
*&---------------------------------------------------------------------*
*& change material prices
*&---------------------------------------------------------------------*

FORM bapi_matval_pricechange CHANGING ch_t_edit TYPE ty_alvlist.
  DATA lv_matnr      TYPE bapi_matval_key-material.
  DATA lv_valarea    TYPE bapi_matval_key-val_area.
  DATA lv_valtype    TYPE bapi_matval_key-val_type.
  DATA ls_date       TYPE bapi_matval_pricedate.
  DATA lv_conv_price TYPE bapi_price.
  DATA ls_price      TYPE bapi_matval_prices ##NEEDED.
  DATA lt_price      TYPE STANDARD TABLE OF bapi_matval_prices.
  DATA lt_return     TYPE STANDARD TABLE OF bapiret2.

  " unconvert price
  DATA(lv_uncv_price) = COND #( WHEN ch_t_edit-vprsv = 'V'
                                THEN ch_t_edit-verpr
                                ELSE ch_t_edit-stprs ).

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = ch_t_edit-waers
      amount_internal = lv_uncv_price
    IMPORTING
      amount_external = lv_conv_price.

  " append material data to bapi data
  lv_matnr   = ch_t_edit-matnr.
  lv_valarea = ch_t_edit-bwkey.
  lv_valtype = ch_t_edit-bwtar.
  ls_date-price_date = sy-datum.
  ls_price = VALUE #( curr_type  = '10'
                      currency   = ch_t_edit-waers
                      price      = lv_conv_price
                      price_unit = ch_t_edit-vjpei ).

  APPEND ls_price TO lt_price.
  CALL FUNCTION 'BAPI_MATVAL_PRICE_CHANGE'
    EXPORTING
      material      = lv_matnr
      valuationarea = lv_valarea
      valuationtype = lv_valtype
      pricedate     = ls_date
    TABLES
      prices        = lt_price
      return        = lt_return.

  READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
  IF sy-subrc = 0.
    ch_t_edit-status  = 'E'.
    ch_t_edit-icon    = icon_red_light.
    ch_t_edit-message = ls_return-message.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
  LOOP AT gt_selected TRANSPORTING NO FIELDS WHERE     matnr = ch_t_edit-matnr
                                                   AND werks = ch_t_edit-werks
                                                   AND lgort = ch_t_edit-lgort
                                                   AND bwkey = ch_t_edit-bwkey
                                                   AND bwtar = ch_t_edit-bwtar.
    MODIFY gt_selected FROM ch_t_edit.
  ENDLOOP.
ENDFORM.
