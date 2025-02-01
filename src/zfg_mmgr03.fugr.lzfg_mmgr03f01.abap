*----------------------------------------------------------------------*
***INCLUDE LZFG_MMGR03F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form separated_to_intern_convert
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> EXCEL_TAB
*&      --> INTERN
*&      --> LD_SEPARATOR
*&      --> SHEETNO
*&      --> SHEETNAME
*&---------------------------------------------------------------------*
FORM separated_to_intern_convert  TABLES i_tab       TYPE ty_t_sender
                                         i_intern    TYPE ty_t_itab
                                  USING  i_separator TYPE c
                                         i_sheetno   TYPE i
                                         i_sheetname TYPE text30.
  DATA: l_sic_tabix LIKE sy-tabix,
        l_sic_col   TYPE kcd_ex_col.
  DATA: l_fdpos     LIKE sy-fdpos.
  DATA: lw_string   TYPE string.
  REFRESH i_intern.
  LOOP AT i_tab.
    lw_string = i_tab.
    IF lw_string CO cl_abap_char_utilities=>horizontal_tab.
      CONTINUE.
    ENDIF.
    l_sic_tabix = sy-tabix.
    l_sic_col = 0.
    WHILE i_tab CA i_separator.
      l_fdpos = sy-fdpos.
      l_sic_col = l_sic_col + 1.
      PERFORM line_to_cell_separat TABLES i_intern
                                   USING  i_tab l_sic_tabix l_sic_col
                                          i_separator l_fdpos i_sheetno i_sheetname.
    ENDWHILE.
    IF gw_maxcol < l_sic_col.
      gw_maxcol = l_sic_col.
    ENDIF.
    IF i_tab <> space.
      CLEAR i_intern.
      i_intern-sheetno = i_sheetno.
      i_intern-sheetname = i_sheetname.
      i_intern-norow = l_sic_tabix.
      i_intern-col = l_sic_col + 1.
      i_intern-value = i_tab.
      APPEND i_intern.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM LINE_TO_CELL_SEPARAT TABLES I_INTERN    TYPE TY_T_ITAB
                          USING  I_LINE
                                 I_ROW       LIKE SY-TABIX
                                 CH_CELL_COL TYPE KCD_EX_COL
                                 I_SEPARATOR TYPE C
                                 I_FDPOS     LIKE SY-FDPOS
                                 I_SHEETNO   TYPE I
                                 I_SHEETNAME TYPE TEXT30.
  DATA: L_STRING   TYPE TY_S_SENDERLINE.
  DATA  L_SIC_INT  TYPE I.
  CLEAR I_INTERN.
  L_SIC_INT = I_FDPOS.
  I_INTERN-SHEETNO = I_SHEETNO.
  I_INTERN-SHEETNAME = I_SHEETNAME.
  I_INTERN-NOROW = I_ROW.
  L_STRING = I_LINE.
  I_INTERN-COL = CH_CELL_COL.
* csv Dateien mit separator in Zelle: --> ;"abc;cd";
  IF ( I_SEPARATOR = ';' OR  I_SEPARATOR = ',' ) AND
       L_STRING(1) = GC_ESC.
    PERFORM LINE_TO_CELL_ESC_SEP USING L_STRING
                                       L_SIC_INT
                                       I_SEPARATOR
                                       I_INTERN-VALUE.
  ELSE.
    IF L_SIC_INT > 0.
      I_INTERN-VALUE = I_LINE(L_SIC_INT).
    ENDIF.
  ENDIF.
  DATA(LW_COL) = CH_CELL_COL.
  IF L_SIC_INT > 0 OR LW_COL <= GW_MAXCOL.
*  IF L_SIC_INT > 0.
    APPEND I_INTERN.
  ENDIF.
  L_SIC_INT = L_SIC_INT + 1.
  I_LINE = I_LINE+L_SIC_INT.
ENDFORM.

*---------------------------------------------------------------------*
FORM LINE_TO_CELL_ESC_SEP USING I_STRING
                                I_SIC_INT      TYPE I
                                I_SEPARATOR    TYPE C
                                I_INTERN_VALUE TYPE TY_D_ITABVALUE.
  DATA: L_INT         TYPE I,
        L_CELL_END(2).
  FIELD-SYMBOLS: <L_CELL>.
  L_CELL_END = GC_ESC.
  L_CELL_END+1 = I_SEPARATOR .
  IF I_STRING CS GC_ESC.
    I_STRING = I_STRING+1.
    IF I_STRING CS L_CELL_END.
      L_INT = SY-FDPOS.
      ASSIGN I_STRING(L_INT) TO <L_CELL>.
      I_INTERN_VALUE = <L_CELL>.
      L_INT = L_INT + 2.
      I_SIC_INT = L_INT.
      I_STRING = I_STRING+L_INT.
    ELSEIF I_STRING CS GC_ESC.
*     letzte Celle
      L_INT = SY-FDPOS.
      ASSIGN I_STRING(L_INT) TO <L_CELL>.
      I_INTERN_VALUE = <L_CELL>.
      L_INT = L_INT + 1.
      I_SIC_INT = L_INT.
      I_STRING = I_STRING+L_INT.
      L_INT = STRLEN( I_STRING ).
      IF L_INT > 0 . MESSAGE X001(KX) . ENDIF.
    ELSE.
      MESSAGE X001(KX) . "was ist mit csv-Format
    ENDIF.
  ENDIF.
ENDFORM.
