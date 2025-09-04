*----------------------------------------------------------------------*
***INCLUDE ZMM0040_CONTR_PRESU_LOG_USEI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK_100'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT_100'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL_100'.
      LEAVE TO SCREEN 0.
    when 'FRSH_100'.
      clear: it_zreg_log.
      SELECT * FROM zreg_log INTO TABLE it_zreg_log
  WHERE  namepgr = v_prgn
    AND  aedat  IN so_aedat
    AND  cputm  IN so_cputm
    AND  ernam  IN so_ernam.
        IF sy-subrc ne 0.
   LEAVE TO SCREEN 0.
  ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
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
