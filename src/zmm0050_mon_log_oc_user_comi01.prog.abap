*----------------------------------------------------------------------*
***INCLUDE ZMM0050_MON_LOG_OC_USER_COMI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'BACK_0100'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE ok_code.
    WHEN 'CLS_0101'. "Desaparecer Subscreen.
      CLEAR: zmmtt_header_log. "Limpia estructura que muestra en dynpro 0101
      gv_dynnr = 0102. "Muestra dynpro vacia
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
