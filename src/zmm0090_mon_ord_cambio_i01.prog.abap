*&---------------------------------------------------------------------*
*&  Include           ZMM0090_MON_ORD_CAMBIO_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
CASE ok_code.
    WHEN 'BACK_100'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT_100'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL_100'.
      LEAVE TO SCREEN 0.
    when 'FRSH_100'.
      clear: it_ch_or.
      SELECT * FROM ZMMTT_0040_CH_OR INTO TABLE it_ch_or
       WHERE  proyecto IN so_proy
       AND  folio  IN so_folio
       AND  fecha_ejec IN so_fecha.
       IF sy-subrc ne 0.
   LEAVE TO SCREEN 0.
  ENDIF.
ENDCASE.



endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0200 input.
CASE ok_code.
    WHEN 'BACK_0200'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT_0200'.
      LEAVE TO SCREEN 0.
  ENDCASE.
endmodule.                 " USER_COMMAND_0200  INPUT
