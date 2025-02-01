*&---------------------------------------------------------------------*
*& Include          ZIN_MM_GR03_F02
*&---------------------------------------------------------------------*
*&       HANDLE DISPLAY FLOW                                                              *
*&---------------------------------------------------------------------*
INCLUDE zin_mm_gr03_pai_9000.
INCLUDE zin_mm_gr03_pbo_9000.
INCLUDE zin_mm_gr03_dtop.

*&---------------------------------------------------------------------*
*& * lcl_alv_handler
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*& Form d_mat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM d_mat .

  PERFORM get_data. "Get all the data for the import parameters of bapi_material_get_all
  IF gt_marc IS NOT INITIAL.
    LOOP AT gt_marc INTO gs_marc.
      PERFORM bapi_material_getall. "* Get all the data for the materials given in the selection screen.
      PERFORM append_data USING gt_display
                          CHANGING gs_display. "append data to alv table for display
    ENDLOOP.
    PERFORM display_data.
  ELSE.
    MESSAGE s006 DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_material
*&---------------------------------------------------------------------*
*& Form to get the data for the import parameters of  bapi_material_getall.
*&---------------------------------------------------------------------*
FORM get_data.

* get material
  SELECT matnr,
         mbrsh,
         mtart
  FROM mara
  INTO CORRESPONDING FIELDS OF TABLE @gt_mara
  WHERE matnr IN @smatnr
        AND mbrsh IN @smbrsh
        AND mtart IN @smtart.
  IF sy-subrc <> 0.
    MESSAGE i022  DISPLAY LIKE 'E' WITH smatnr-low smatnr-high .
    LEAVE LIST-PROCESSING.
  ENDIF.
* get plant
  SELECT marc~matnr,
         marc~werks
  FROM marc
  FOR ALL ENTRIES IN @gt_mara
  WHERE marc~matnr = @gt_mara-matnr
        AND marc~werks IN @swerks
  INTO CORRESPONDING FIELDS OF TABLE @gt_marc .
  SORT gt_marc ASCENDING BY matnr.

* get storage locs.
  SELECT mard~matnr,
         mard~werks,
         mard~lgort
 FROM mard
 FOR ALL ENTRIES IN @gt_marc
 WHERE mard~matnr = @gt_marc-matnr
   AND mard~werks = @gt_marc-werks
   AND mard~lgort IN @slgort
 INTO CORRESPONDING FIELDS OF TABLE @gt_mard .
* get valuation area & type
  SELECT matnr,
         bwkey,
         bwtar
  FROM mbew
  INTO CORRESPONDING FIELDS OF TABLE @gt_mbew
  FOR ALL ENTRIES IN @gt_mara
  WHERE matnr = @gt_mara-matnr.
* get sale org. and
  SELECT werks,
        vkorg,
        vtweg
  FROM tvkwz
  INTO CORRESPONDING FIELDS OF TABLE @gt_tvkwz
  FOR ALL ENTRIES IN @gt_marc
  WHERE werks = @gt_marc-werks.
* get attribute
  SELECT *
  FROM zg3mafi
  INTO CORRESPONDING FIELDS OF TABLE @gt_mafi
  FOR ALL ENTRIES IN @gt_mara
  WHERE matnr = @gt_mara-matnr.
* get characteristic

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_material_detail
*&---------------------------------------------------------------------*
*& Form to get the data from database tables MARA, MARC, MPOP, MPGD,MBEW,
*  MLGN, MVKE, MLGT for the material
*&---------------------------------------------------------------------*
FORM bapi_material_getall.
  DATA:
    lv_objectkey  TYPE bapi1003_key-object,
    lv_material   TYPE bapi_mara_ga-material,
    lv_val_area   TYPE bapi_mbew_ga-val_area,
    lv_val_type   TYPE bapi_mbew_ga-val_type,
    lv_plant      TYPE bapi_marc_ga-plant,
    lv_stgel      TYPE bapi_mard_ga-stge_loc,
    lv_salesorg   TYPE bapi_mvke_ga-sales_org,
    lv_distr_chan TYPE bapi_mvke_ga-distr_chan.
*fetch data to bapi parameters
  lv_material  = gs_marc-matnr.
  lv_plant     = gs_marc-werks.
  lv_objectkey = gs_marc-matnr.
  IF gt_mard IS NOT INITIAL.
    READ TABLE gt_mard INTO gs_mard WITH KEY  matnr = gs_marc-matnr
                                           werks = gs_marc-werks.
    IF sy-subrc = 0.
      lv_stgel = gs_mard-lgort .
    ENDIF.
  ENDIF.

  READ TABLE gt_mard INTO gs_mard WITH KEY  matnr = gs_marc-matnr
                                            werks = gs_marc-werks.
  IF sy-subrc = 0.
    lv_stgel = gs_mard-lgort.
  ENDIF.
  READ TABLE gt_mbew INTO gs_mbew WITH KEY matnr = gs_marc-matnr
                                           bwkey = gs_marc-werks .
  IF sy-subrc = 0.
    lv_val_area = gs_mbew-bwkey.
    lv_val_type = gs_mbew-bwtar.
  ENDIF.

  READ TABLE gt_tvkwz INTO gs_tvkwz WITH KEY werks = gs_marc-werks.

  IF sy-subrc = 0.
    lv_salesorg   = gs_tvkwz-vkorg.
    lv_distr_chan = gs_tvkwz-vtweg.
  ENDIF.


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
      objecttable_imp = 'MARA'
      classtype_imp   = '023'
*     KEYDATE         = SY-DATUM
*     LANGUAGE        = SY-LANGU
*     OBJECTKEY_IMP_LONG       =
    TABLES
      alloclist       = gt_classes
*     ALLOCVALUESCHAR =
*     ALLOCVALUESCURR =
*     ALLOCVALUESNUM  =
      return          = gt_return.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& DISPLAY MATERIAL ON ALV
*&---------------------------------------------------------------------*
FORM display_data .
  DATA: lt_field_cat TYPE lvc_t_fcat,
        ls_layout    TYPE lvc_s_layo.

  gr_bal_log = NEW #( ).

  PERFORM pepare_field_cat CHANGING lt_field_cat.
  PERFORM perpare_layout   CHANGING ls_layout.
  PERFORM display_material_alv USING  ls_layout
                                      lt_field_cat.

  "{ add TrucTT 2024.11.22  *Note: move position from perform DISPLAY_MATERIAL to this postition.
  CALL SCREEN 9000. "} end Add

ENDFORM.
*&---------------------------------------------------------------------*
*& Form pepare_field_cat
*&---------------------------------------------------------------------*
*& FIELD CATALOG INITIAL
*&---------------------------------------------------------------------*
FORM pepare_field_cat  CHANGING ch_t_field_cat TYPE lvc_t_fcat.
  "{begin TrucTT - 2024.11.22  12:52:00 - add
  TYPES: BEGIN OF lty_field,
           fieldname TYPE lvc_fname,
           label     TYPE lvc_txtcol,
         END OF lty_field.
  DATA: lt_field_tech TYPE STANDARD TABLE OF lty_field WITH EMPTY KEY.

  lt_field_tech = VALUE #( ( fieldname = 'MESSAGE'    )
                           ( fieldname = 'STATUS'     )
                           ( fieldname = 'DEL_FLAG'   )
                           ( fieldname = 'DEL_FLAG_LEVEL'   )
                           ( fieldname = 'ICON'        label = 'Status' )
                           ( fieldname = 'DEL_ICON'    label = 'Flag Material for Deletion' )
                          ) ##NO_TEXT. "} end Add

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMG3D'
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

  "{begin TrucTT - 2024.11.22  12:52:00 - add
  LOOP AT lt_field_tech INTO DATA(ls_field).
    READ TABLE ch_t_field_cat WITH KEY fieldname = ls_field-fieldname ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF sy-subrc = 0.
      <fs_fcat>-scrtext_s =
      <fs_fcat>-scrtext_m =
      <fs_fcat>-scrtext_l = <fs_fcat>-coltext = ls_field-label.
      <fs_fcat>-just = 'C'.
      CASE <fs_fcat>-fieldname.
        WHEN 'ICON'.
          CONTINUE.
        WHEN 'DEL_ICON'.
          CONTINUE.
        WHEN OTHERS.
          <fs_fcat>-tech = 'X'.
      ENDCASE.
    ENDIF.
  ENDLOOP. "} end Add
ENDFORM.
*&---------------------------------------------------------------------*
*& Form perpare_layout
*&---------------------------------------------------------------------*
*& ALV LAYOUT INITIAL
*&---------------------------------------------------------------------*

FORM perpare_layout  CHANGING ch_ls_layout TYPE lvc_s_layo.
  ch_ls_layout-cwidth_opt = abap_true.
  ch_ls_layout-zebra = abap_true.
  ch_ls_layout-sel_mode = 'A'.
  ch_ls_layout-no_f4 = 'X'.
  "{ begin   TrucTT   2024.11.22    - Add
  ch_ls_layout-grid_title = TEXT-t03.
  "} end Add
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_material_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display_material_alv USING im_s_layout   TYPE lvc_s_layo
                                it_field_cat  TYPE lvc_t_fcat.
  "{ begin    TrucTT19    2024.11.21  - Replace -
  go_container = NEW cl_gui_custom_container( container_name = 'ALV_CONT'
                                              repid = sy-repid
                                              dynnr = '9000' ).
  go_alv_table = NEW cl_gui_alv_grid( i_parent = go_container  )." } by {

*  go_alv_table = NEW cl_gui_alv_grid( i_parent = cl_gui_custom_container=>screen0 ). "} end Replace


  go_alv_table->set_table_for_first_display(
     EXPORTING
       is_layout                     =  im_s_layout   " Layout
       i_save                        =  'X'            " Save Layout
     CHANGING
       it_outtab                     =  gt_display    " Output Table
       it_fieldcatalog               =  it_field_cat " Field Catalog
     EXCEPTIONS
       invalid_parameter_combination = 1                " Wrong Parameter
       program_error                 = 2                " Program Errors
       too_many_lines                = 3                " Too many Rows in Ready for Input Grid
       OTHERS                        = 4
     ).

*  CALL SCREEN 9000. "*Note: move position from line 215 this Include to perform DISPLAY_DATA include F02.
  IF sy-subrc <> 0.
    "{ begin  TrucTT 2024.11.22  Add
    MESSAGE s007 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING. "} end Add
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form append_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*

FORM append_data USING i_table TYPE ANY TABLE
                 CHANGING c_gs TYPE zmmg3d.

  FIELD-SYMBOLS: <lfs_table> TYPE STANDARD TABLE,
                 <lfs_gs>    TYPE zmmg3d.
  ASSIGN i_table TO <lfs_table>.
  ASSIGN c_gs TO <lfs_gs>.
  CLEAR <lfs_gs>.

* Client data

  <lfs_gs>-matnr = gs_bapi_mara-material.
  <lfs_gs>-mbrsh = gs_bapi_mara-ind_sector.
  <lfs_gs>-mtart = gs_bapi_mara-matl_type.
  <lfs_gs>-meins = gs_bapi_mara-base_uom.
  <lfs_gs>-matkl = gs_bapi_mara-matl_group.
  <lfs_gs>-mtpos_mara = gs_bapi_mara-item_cat.
  <lfs_gs>-groes = gs_bapi_mara-size_dim.
  <lfs_gs>-ntgew = gs_bapi_mara-net_weight.
  <lfs_gs>-spart = gs_bapi_mara-division.
  <lfs_gs>-bstme = gs_bapi_mara-po_unit.
  <lfs_gs>-xchpf = gs_bapi_mara-batch_mgmt.
  <lfs_gs>-ekwsl = gs_bapi_mara-pur_valkey.
  <lfs_gs>-raube = gs_bapi_mara-stor_conds.
  <lfs_gs>-behvo = gs_bapi_mara-container.
  <lfs_gs>-tempb = gs_bapi_mara-temp_conds.
  <lfs_gs>-mhdrz = gs_bapi_mara-minremlife.
  <lfs_gs>-iprkz = gs_bapi_mara-period_ind_expiration_date.

* Plant Data
*  <fs_gs>-del_flag = gs_bapi_marc-del_flag.
  <lfs_gs>-werks = gs_bapi_marc-plant.
  <lfs_gs>-ekgrp = gs_bapi_marc-pur_group.
  <lfs_gs>-plifz = gs_bapi_marc-plnd_delry.
  <lfs_gs>-webaz = gs_bapi_marc-gr_pr_time.
  <lfs_gs>-kordb = gs_bapi_marc-sourcelist.
  <lfs_gs>-insmk = gs_bapi_marc-ind_post_to_insp_stock.
  <lfs_gs>-ausme = gs_bapi_marc-issue_unit.
  <lfs_gs>-maxlz = gs_bapi_marc-stgeperiod.
  <lfs_gs>-lzeih = gs_bapi_marc-stge_pd_un.

* Storage Location Data
  <lfs_gs>-lgort = gs_bapi_mard-stge_loc.

* Valuation Data
  <lfs_gs>-bwkey = gs_bapi_mbew-val_area.
  <lfs_gs>-bklas = gs_bapi_mbew-val_class.
  <lfs_gs>-mlast = gs_bapi_mbew-ml_settle.
  <lfs_gs>-vprsv = gs_bapi_mbew-price_ctrl.
  <lfs_gs>-vjpei = gs_bapi_mbew-pr_unit_py.
  <lfs_gs>-verpr = gs_bapi_mbew-moving_pr.
  <lfs_gs>-stprs = gs_bapi_mbew-std_price.
  <lfs_gs>-zkprs = gs_bapi_mbew-future_pr.
  <lfs_gs>-zkdat = gs_bapi_mbew-valid_from.

* Sales Data
  <lfs_gs>-vkorg = gs_bapi_mvke-sales_org.
  <lfs_gs>-vtweg = gs_bapi_mvke-distr_chan.
  <lfs_gs>-vrkme = gs_bapi_mvke-sales_unit.
  <lfs_gs>-dwerk = gs_bapi_mvke-delyg_plnt.
  <lfs_gs>-sktof = gs_bapi_mvke-cash_disc.
  <lfs_gs>-aumng = gs_bapi_mvke-min_order.
  <lfs_gs>-lfmng = gs_bapi_mvke-min_dely.
  <lfs_gs>-scmng = gs_bapi_mvke-dely_unit.
  <lfs_gs>-schme = gs_bapi_mvke-dely_uom.

* Additional Data
  READ TABLE gt_mafi INTO gs_mafi WITH KEY matnr = <lfs_gs>-matnr.
  <lfs_gs>-att1 = gs_mafi-att1.
  <lfs_gs>-att2 = gs_mafi-att2.
  <lfs_gs>-att3 = gs_mafi-att3.
  <lfs_gs>-att4 = gs_mafi-att4.
  <lfs_gs>-att5 = gs_mafi-att5.
  <lfs_gs>-att6 = gs_mafi-att6.
  <lfs_gs>-att7 = gs_mafi-att7.
  <lfs_gs>-att8 = gs_mafi-att8.
  <lfs_gs>-att9 = gs_mafi-att9.
  <lfs_gs>-att10 = gs_mafi-att10.
  <lfs_gs>-att11 = gs_mafi-att11.
  <lfs_gs>-att12 = gs_mafi-att12.
  <lfs_gs>-att13 = gs_mafi-att13.
  <lfs_gs>-att14 = gs_mafi-att14.
  <lfs_gs>-att15 = gs_mafi-att15.
  <lfs_gs>-att16 = gs_mafi-att16.
  <lfs_gs>-att17 = gs_mafi-att17.
  <lfs_gs>-att18 = gs_mafi-att18.
  <lfs_gs>-att19 = gs_mafi-att19.
  <lfs_gs>-att20 = gs_mafi-att20.
  <lfs_gs>-att21 = gs_mafi-att21.
  <lfs_gs>-att22 = gs_mafi-att22.
  <lfs_gs>-att23 = gs_mafi-att23.
  <lfs_gs>-att24 = gs_mafi-att24.
  <lfs_gs>-att25 = gs_mafi-att25.
  <lfs_gs>-att26 = gs_mafi-att26.

  "{ begin TrucTT add
  <lfs_gs>-del_flag = COND #( WHEN gs_bapi_mara-del_flag IS NOT INITIAL
                                  OR gs_bapi_marc-del_flag IS NOT INITIAL
                                  OR gs_bapi_mard-del_flag IS NOT INITIAL
                                THEN 'X'
                               ).
  <lfs_gs>-del_flag_level = VALUE #( client = gs_bapi_mara-del_flag
                                       plant  = gs_bapi_marc-del_flag
                                       sloc   = gs_bapi_mard-del_flag
                                      ).

* classification
  READ TABLE gt_classes INTO DATA(ls_class) WITH KEY object = <lfs_gs>-matnr.
  IF sy-subrc = 0.
    <lfs_gs>-class = ls_class-classnum.
    <lfs_gs>-klart = ls_class-classtype.
  ENDIF.


*   Material Description
  READ TABLE gt_bapi_makt_ga INTO gs_bapi_makt WITH KEY langu = sy-langu.
  IF sy-subrc = 0.
    <lfs_gs>-maktx = gs_bapi_makt-matl_desc.
    <lfs_gs>-spras = gs_bapi_makt-langu.
  ENDIF.

*  " Material Gross Weight
  READ TABLE gt_bapi_marm_ga INTO gs_bapi_marm WITH KEY alt_unit = <lfs_gs>-meins.

  IF sy-subrc = 0.
    <lfs_gs>-brgew = gs_bapi_marm-gross_wt.
    <lfs_gs>-gewei = gs_bapi_marm-unit_of_wt.
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
FORM get_selected_rows .
  DATA: lt_row_ids TYPE lvc_t_row,    " Table chứa các index dòng được chọn
        lv_row     TYPE lvc_s_row.    " Work area cho một index dòng được chọn.
  DATA: lv_text  TYPE text100.



  "{TrucTT add
  MESSAGE i030 INTO lv_text.
  IF xsdbool( lcl_popup_confirm=>is_confirm( lv_text ) ) = abap_false.
    RETURN.
  ENDIF."} end Add

  CLEAR gt_selected.

  " Lấy list các dòng được chọn
  CALL METHOD go_alv_table->get_selected_rows
    IMPORTING
      et_index_rows = lt_row_ids.

  " Duyệt list các index dòng được chọn
  LOOP AT lt_row_ids INTO lv_row.
    " Đọc d (it_alv_mat) dựa trên index
    READ TABLE gt_display INTO gs_selected INDEX lv_row-index.
    IF sy-subrc = 0.
      APPEND gs_selected TO gt_selected. " Thêm dòng vào it_sel
    ENDIF.
  ENDLOOP.
  IF gt_selected IS INITIAL.
    MESSAGE e035 DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_second_alv
*&---------------------------------------------------------------------*
*
FORM display_selected_rows.

  DATA: lt_fcat    TYPE lvc_t_fcat,
        ls_slayout TYPE lvc_s_layo.


  PERFORM edit_layout  CHANGING ls_slayout.
  PERFORM edit_field_cat CHANGING lt_fcat.
  PERFORM display_sel_alv USING  ls_slayout
                                  lt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_Edt_UCOM
*&---------------------------------------------------------------------*
FORM handle_edt_ucom USING r_ucomm      LIKE sy-ucomm
                           rs_selfield  TYPE slis_selfield ##NEEDED ##CALLED.

  CASE r_ucomm.
    WHEN '&SUBMIT'.

      PERFORM save_edited_data.
      PERFORM refresh_alv.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_popup_data
*&---------------------------------------------------------------------*
FORM save_edited_data.


*  LOOP AT gt_selected INTO gs_selected.
  LOOP AT gt_selected ASSIGNING FIELD-SYMBOL(<lfs_selected>). " gs_selected.
    "{ begin    TrucTT    2024.11.20 17:58:00  REPLACE {
*    PERFORM prepare_data. } by {

    "fill bapi
    PERFORM prepare_data USING <lfs_selected>.
    "} end

    "implement bapi
    PERFORM bapi_material_savedata CHANGING <lfs_selected>.

    PERFORM mod_att USING <lfs_selected>.
*    CLEAR gs_selected.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_field_catalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM edit_field_cat CHANGING ch_t_fcat TYPE lvc_t_fcat .
  " Gọi Function Module để tạo Field Catalog từ cấu trúc bảng
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMG3D'
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

  " Tùy chỉnh Field Catalog: khóa các field không cho chỉnh sửa
  LOOP AT ch_t_fcat INTO DATA(ls_field).
    CASE ls_field-fieldname.
      WHEN 'MATNR' OR 'MBRSH' OR 'MTART' OR 'WERKS'  OR  'LGORT'
            OR 'MEINS'  OR 'MATKL' OR 'SPRAS' OR 'KLART'
            OR 'VKORG'  OR 'VTWEG' OR 'EKGRP' OR 'BWKEY' OR 'BKLAS'
            OR 'STPRS'  OR 'VERPR' OR 'VPRSV' OR 'VJPEI' OR 'ZKPRS'
            OR 'MESSAGE' OR 'BWTAR' OR 'CLASS'. " Danh sách field cần khóa
        ls_field-edit = ' '. " Không cho chỉnh sửa

      WHEN 'DEL_ICON' OR 'STATUS' OR 'DEL_FLAG'.
        ls_field-tech = 'X'.
      WHEN 'ICON'.
        ls_field-coltext = ls_field-scrtext_s = ls_field-scrtext_m = ls_field-scrtext_L = 'Status' ##NO_TEXT.

      WHEN OTHERS.
        ls_field-edit = 'X'. " Cho phép chỉnh sửa
        ls_field-f4availabl = ''.
    ENDCASE.
    MODIFY ch_t_fcat FROM ls_field.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bapi_material_savedata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM bapi_material_savedata CHANGING ch_t_upload TYPE zmmg3d.

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
  "{begin   TrucTT   2024.11.29    ADD
  IF gs_return-type <> 'S'.
    ch_t_upload-status = 'E'.
    ch_t_upload-message = gs_return-message.
    ch_t_upload-icon = icon_red_light.

  ELSEIF  gs_return-type = 'S'.
    ch_t_upload-status = 'S'.
    MESSAGE s025 INTO ch_t_upload-message.
    ch_t_upload-icon = icon_green_light.
  ENDIF.

  gr_grid->refresh_table_display( ).


*  MESSAGE gs_return-message TYPE gs_return-type.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form prepare_datax
*&---------------------------------------------------------------------*

FORM prepare_data USING i_sel TYPE  zmmg3d.

*head data
  gs_bapi_mathead = VALUE #( material = i_sel-matnr
                            ind_sector = i_sel-mbrsh
                            matl_type = i_sel-mtart
                           basic_view = 'X'
                           sales_view    = 'X'
                           account_view  = 'X'
                           purchase_view = 'X'
                           storage_view  = 'X'
                             ).
* cilent data
  gs_bapi_s_mara = VALUE #(
                            matl_group = i_sel-matkl
                            base_uom = i_sel-meins
                            item_cat =  i_sel-mtpos_mara
                            size_dim =  i_sel-groes
                          net_weight = i_sel-ntgew
                          division = i_sel-spart
                          po_unit  =  i_sel-bstme
                          batch_mgmt = i_sel-xchpf
                          pur_valkey = i_sel-ekwsl
                          stor_conds = i_sel-raube
                          container = i_sel-behvo
                          temp_conds = i_sel-tempb
                          minremlife = i_sel-mhdrz
*                          quarantine_period_un = i_sel-qqtimeuom
                        ).
  gs_bapi_marax = VALUE #(
                           matl_group = 'X'
                           base_uom ='X'
                             item_cat = 'X'
                             size_dim = 'X'
                           net_weight = 'X'
                             division = 'X'
                              po_unit  =  'X'
                           batch_mgmt = 'X'
                           pur_valkey  =  'X'
                           stor_conds = 'X'
                            container  =  'X'
                           temp_conds = 'X'
                            minremlife  =  'X'
                            quarantine_period_un = 'X'  ).

*plant data
  gs_bapi_s_marc = VALUE #( plant = i_sel-werks
                           pur_group = i_sel-ekgrp
                           plnd_delry = i_sel-plifz
                           gr_pr_time = i_sel-webaz
                           sourcelist = i_sel-kordb
                           ind_post_to_insp_stock = i_sel-insmk
                           issue_unit = i_sel-ausme
                           stgeperiod = i_sel-maxlz
                           stge_pd_un = i_sel-lzeih
                           period_ind = i_sel-iprkz   ).


  gs_bapi_marcx = VALUE #( plant = i_sel-werks
                            pur_group = 'X'
                            plnd_delry = 'X'
                            gr_pr_time =  'X'
                            sourcelist = 'X'
                            ind_post_to_insp_stock =  'X'
                            issue_unit = 'X'
                            stgeperiod = 'X'
                            stge_pd_un = 'X'
                            period_ind = 'X' ).

* storage locs data
  IF i_sel-lgort IS NOT INITIAL. "TrucTT add code
    gs_bapi_s_mard  = VALUE #( plant = i_sel-werks
                               stge_loc = i_sel-lgort ).


    gs_bapi_mardx = VALUE #( plant =  gs_bapi_s_mard-plant
                             stge_loc = COND #( WHEN gs_bapi_s_mard-stge_loc IS NOT INITIAL THEN gs_bapi_s_mard-stge_loc ) ).
  ENDIF.

* materiscription data
  gt_bapi_makt = VALUE #( ( langu = COND #( WHEN i_sel-spras IS NOT INITIAL THEN i_sel-spras ELSE sy-langu )
                            matl_desc = i_sel-maktx ) ).

* sale data
  gs_bapi_s_mvke =  VALUE #( sales_org = i_sel-vkorg
                             distr_chan = i_sel-vtweg
                             sales_unit = i_sel-vrkme
                             delyg_plnt = i_sel-dwerk
                            cash_disc = i_sel-sktof
                            min_order  = i_sel-aumng
                            min_dely   = i_sel-lfmng
                            dely_unit  = i_sel-scmng
                            dely_uom = i_sel-schme  ).
  gs_bapi_mvkex = VALUE #(  sales_org = i_sel-vkorg
                            distr_chan = i_sel-vtweg
                            sales_unit =  'X'
                            delyg_plnt = 'X'
                            cash_disc = 'X'
                            min_order = 'X'
                            min_dely = 'X'
                            dely_unit = 'X'
                            dely_uom = 'X'   ).


* valuation data
  gs_bapi_s_mbew =  VALUE #(  val_area = i_sel-bwkey
                              ml_settle = i_sel-mlast
                              val_class = i_sel-bklas
                              price_ctrl = i_sel-vprsv
                              price_unit = i_sel-vjpei
                               moving_pr = i_sel-verpr
                               std_price = i_sel-stprs
                               future_pr = i_sel-zkprs
                                valid_from = i_sel-zkdat ).
  gs_bapi_mbewx = VALUE #(  val_area = i_sel-bwkey
                            ml_settle = 'X'
                            val_class = 'X'
                            price_ctrl =  'X'
                            price_unit =  'X'
                            moving_pr = 'X'
                            std_price = 'X'
                            future_pr = 'X'
                            valid_from =  'X'  ).

* unit of measure
  gt_bapi_marm = VALUE #( (  gross_wt = i_sel-brgew
                             alt_unit = i_sel-meins
                             unit_of_wt = i_sel-gewei ) ).
*
  gt_bapi_marmx = VALUE #( ( gross_wt = 'X'
                            unit_of_wt = 'X'
                            alt_unit = i_sel-meins ) ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form refresh_alv
*&---------------------------------------------------------------------*

FORM refresh_alv .
  DATA: ls_refresh TYPE zmmg3d,
        lt_refresh TYPE STANDARD TABLE OF zmmg3d.


* select material's new attribute data
  SELECT *
  FROM zg3mafi
  INTO CORRESPONDING FIELDS OF TABLE @gt_mafi
  FOR ALL ENTRIES IN @gt_selected
  WHERE matnr = @gt_selected-matnr.

  LOOP AT gt_selected INTO gs_selected GROUP BY ( matnr = gs_selected ).
    LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<lfs_data>) WHERE matnr = gs_selected-matnr.
      DATA:
        lv_objectkey  TYPE bapi1003_key-object,
        lv_material   TYPE bapi_mara_ga-material,
        lv_val_area   TYPE bapi_mbew_ga-val_area,
        lv_val_type   TYPE bapi_mbew_ga-val_type,
        lv_plant      TYPE bapi_marc_ga-plant,
        lv_stgel      TYPE bapi_mard_ga-stge_loc,
        lv_salesorg   TYPE bapi_mvke_ga-sales_org,
        lv_distr_chan TYPE bapi_mvke_ga-distr_chan.
*fetch data to bapi parameters
      lv_material  = <lfs_data>-matnr.
      lv_plant     = <lfs_data>-werks.
      lv_stgel     = <lfs_data>-lgort.
      lv_val_area  = <lfs_data>-bwkey.
      lv_salesorg  = <lfs_data>-vkorg.
      lv_distr_chan = <lfs_data>-vtweg.
      lv_objectkey = <lfs_data>-matnr.

*call bapi to get material's new data
      CALL FUNCTION 'BAPI_MATERIAL_GETALL'
        EXPORTING
          material                  = lv_material
          valuationarea             = lv_val_area
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
          objecttable_imp = 'MARA'
          classtype_imp   = '023'
*         KEYDATE         = SY-DATUM
*         LANGUAGE        = SY-LANGU
*         OBJECTKEY_IMP_LONG       =
        TABLES
          alloclist       = gt_classes
*         ALLOCVALUESCHAR =
*         ALLOCVALUESCURR =
*         ALLOCVALUESNUM  =
          return          = gt_return.

* append new data
      PERFORM  append_data USING lt_refresh CHANGING ls_refresh.
    ENDLOOP.
    DELETE gt_display WHERE matnr = gs_selected-matnr.
  ENDLOOP.
  APPEND LINES OF lt_refresh TO gt_display.
  SORT gt_display BY matnr ASCENDING.
  go_alv_table->refresh_table_display( EXPORTING  is_stable      = VALUE #( row = 'X' col = 'X' )
                                                  i_soft_refresh = 'X'
                                       EXCEPTIONS finished = 1
                                                  OTHERS   = 2  ).

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

  gr_grid->register_edit_event(
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  gr_grid->set_ready_for_input( i_ready_for_input = 1 ).
  SET HANDLER:
     gr_handler->on_e_data_changed FOR gr_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form edit_layout
*&---------------------------------------------------------------------*
FORM edit_layout  CHANGING ch_ls_slayout TYPE lvc_s_layo.
  ch_ls_slayout-cwidth_opt = abap_true.
  ch_ls_slayout-zebra = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_sel_alv
*&---------------------------------------------------------------------*
*&  DISPLAY SELECTED MATERIAL
*&---------------------------------------------------------------------*

FORM display_sel_alv  USING i_s_layout   TYPE lvc_s_layo
                            i_field_cat  TYPE lvc_t_fcat.

  DATA: l_title TYPE lvc_title VALUE 'Edit Material(s)' ##NO_TEXT.
  DATA(lr_display_alv) = lcl_display_alv=>get_instance(  ).


  lr_display_alv->display_alv(
                     EXPORTING
                       i_pf_status = 'EDIT_STATUS'
                       i_ucomm     = 'HANDLE_EDT_UCOM'
                       i_title     = l_title
                       it_fcat     = i_field_cat
                       i_layout    = i_s_layout
                     IMPORTING
                       i_outtab    = gt_selected
                   ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_input
*&---------------------------------------------------------------------*

FORM validate_input  USING io_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA: lv_weight         TYPE ntgew,
        lr_fieldname      TYPE RANGE OF lvc_fname,
        lr_fieldname_mara TYPE RANGE OF lvc_fname.

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
        END OF lty_mara.
  FIELD-SYMBOLS: <lfs_mod_rows> TYPE STANDARD TABLE.

  lr_fieldname_mara = VALUE #(  ( sign = 'I' option = 'EQ' low = 'MAKTX' )
                                ( sign = 'I' option = 'EQ' low = 'MTPOS_MARA' )
                                ( sign = 'I' option = 'EQ' low = 'GROES' )
                                ( sign = 'I' option = 'EQ' low = 'GEWEI' ) "?
                                ( sign = 'I' option = 'EQ' low = 'BRGEW' )
                                ( sign = 'I' option = 'EQ' low = 'NTGEW' )
                                ( sign = 'I' option = 'EQ' low = 'SPART' )
                                ( sign = 'I' option = 'EQ' low = 'VRKME' )
                                ( sign = 'I' option = 'EQ' low = 'BSTME' )
                                ( sign = 'I' option = 'EQ' low = 'SCHME' )
                                ( sign = 'I' option = 'EQ' low = 'XCHPF' )
                                ( sign = 'I' option = 'EQ' low = 'EKWSL' )
                                ( sign = 'I' option = 'EQ' low = 'RAUBE' )
                                ( sign = 'I' option = 'EQ' low = 'BEHVO' )
                                ( sign = 'I' option = 'EQ' low = 'TEMPB' )
                                ( sign = 'I' option = 'EQ' low = 'MHDRZ' )
                                ( sign = 'I' option = 'EQ' low = 'IPRKZ' )
                        ).

  lr_fieldname = VALUE #( ( sign = 'I' option = 'EQ' low = 'NTGEW' )
                          ( sign = 'I' option = 'EQ' low = 'BRGEW' )
                          ( sign = 'I' option = 'EQ' low = 'LFMNG' )
                          ( sign = 'I' option = 'EQ' low = 'AUMNG' )
                        ).

  LOOP AT io_data_changed->mt_mod_cells INTO DATA(ls_data_changed) WHERE fieldname IN lr_fieldname.
    READ TABLE gt_selected INDEX ls_data_changed-row_id INTO DATA(ls_selected).
    IF sy-subrc = 0.
      CASE ls_data_changed-fieldname.
        WHEN 'NTGEW'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          CHECK lv_weight  > ls_selected-brgew.
          CLEAR lv_weight.
          io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = ls_data_changed-fieldname
            i_value     = ls_selected-ntgew
          ).
          MESSAGE s031 DISPLAY LIKE 'E'.

        WHEN 'BRGEW'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          CHECK lv_weight  < ls_selected-ntgew.
          CLEAR lv_weight.
          io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = ls_data_changed-fieldname
            i_value     = ls_selected-brgew
          ).

          MESSAGE s032 DISPLAY LIKE 'E'.
        WHEN 'LFMNG'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          CHECK lv_weight  > ls_selected-aumng.
          CLEAR lv_weight.
          io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = ls_data_changed-fieldname
            i_value     = ls_selected-lfmng
          ).

          MESSAGE s033 DISPLAY LIKE 'E'.
        WHEN 'AUMNG'.
          lv_weight = lcl_utils=>conv_char_to_num( ls_data_changed-value ).

          CHECK lv_weight  < ls_selected-lfmng.
          io_data_changed->modify_cell(
            i_row_id    = ls_data_changed-row_id
            i_tabix     = ls_data_changed-tabix
            i_fieldname = ls_data_changed-fieldname
            i_value     = ls_selected-aumng
          ).

          MESSAGE s034 DISPLAY LIKE 'E'.
      ENDCASE.
      CLEAR lv_weight.
    ENDIF.

    gr_grid->refresh_table_display( is_stable = VALUE #( col = 'X' row = 'X' )
                                    i_soft_refresh = 'X'   ).
  ENDLOOP.

  LOOP AT io_data_changed->mt_mod_cells INTO ls_data_changed WHERE fieldname IN lr_fieldname_mara.
    ASSIGN io_data_changed->mp_mod_rows->* TO <lfs_mod_rows>.
    READ TABLE <lfs_mod_rows> ASSIGNING FIELD-SYMBOL(<lfs_row>) INDEX 1.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING <lfs_row> TO lty_mara.
    LOOP AT gt_selected INTO DATA(lr_selected) WHERE matnr = lty_mara-matnr  GROUP BY ( matnr = lr_selected-matnr ).
      LOOP AT GROUP lr_selected ASSIGNING FIELD-SYMBOL(<ls_selected>).
        <ls_selected> = CORRESPONDING #( BASE ( <ls_selected> ) lty_mara ).
      ENDLOOP.
    ENDLOOP.
    gr_grid->refresh_table_display( is_stable = VALUE #( col = 'X' row = 'X' )
                                    i_soft_refresh = 'X'   ).
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form mod_att
*&---------------------------------------------------------------------*
*& MODIFY MATERIAL ATTRIBUTE
*&---------------------------------------------------------------------*

FORM mod_att USING i_sel.
  DATA: ls_mafi TYPE zg3mafi,
        lt_mafi TYPE STANDARD TABLE OF zg3mafi.

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
