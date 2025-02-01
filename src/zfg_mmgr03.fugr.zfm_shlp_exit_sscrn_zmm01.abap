FUNCTION zfm_shlp_exit_sscrn_zmm01.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------
  TYPES: BEGIN OF lts_sscrn,
           matnr TYPE matnr,
           mtart TYPE mtart,
           werks TYPE werks,
         END OF lts_sscrn.
  DATA: lt_sh_sscrn TYPE STANDARD TABLE OF lts_sscrn.

  IF callcontrol-step = 'SELECT'.
    SELECT mara~matnr,
           mara~mtart,
           mard~werks
      FROM mara
      LEFT JOIN mard ON mard~matnr = mara~matnr
     INTO TABLE @lt_sh_sscrn.

    REFRESH record_tab.
    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = lt_sh_sscrn
      CHANGING
        shlp               = shlp
        callcontrol        = callcontrol
      EXCEPTIONS
        illegal_structure  = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
    ENDIF.

    callcontrol-step = 'DISP' .
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
ENDFUNCTION.
