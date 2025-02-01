FUNCTION zfm_shlp_exit_bklas.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------
  TYPES: BEGIN OF lts_bklas,
           mtart TYPE mtart,
           bklas TYPE bklas,
           bkbez TYPE bkbez,
         END OF lts_bklas.
  DATA: lt_sh_bklas TYPE STANDARD TABLE OF lts_bklas.

  IF callcontrol-step = 'SELECT'.
    SELECT
           t134~mtart,
           t025~bklas,
           t025t~bkbez
      FROM t134
      LEFT JOIN t025  ON t025~kkref  = t134~kkref
      LEFT JOIN t025t ON t025t~bklas = t025~bklas
                     AND t025t~spras = @sy-langu
      WHERE t134~kkref IS NOT INITIAL
    INTO TABLE @lt_sh_bklas.

    REFRESH record_tab.
    CALL FUNCTION 'F4UT_RESULTS_MAP'
      EXPORTING
        apply_restrictions = 'X'
      TABLES
        shlp_tab           = shlp_tab
        record_tab         = record_tab
        source_tab         = lt_sh_bklas
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
