*&---------------------------------------------------------------------*
*& Include          ZIN_MM_GR03_PAI_9000
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMM_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_comm_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA: lv_mark TYPE flag ##NEEDED.
  CASE sy-ucomm.
    WHEN 'EDIT'.
      PERFORM get_selected_rows.
      PERFORM display_selected_rows.
    WHEN '&DEL'.
      lv_mark = 'X' ##NEEDED.
      PERFORM del_matnr USING lv_mark.
    WHEN '&UDEL'.
      CLEAR lv_mark.
      PERFORM del_matnr USING lv_mark.

  ENDCASE.
ENDMODULE.
