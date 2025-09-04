*&---------------------------------------------------------------------*
*&  Include           ZLO4220_MON_EMB_CLI
*&---------------------------------------------------------------------*
CLASS lcl_alv100 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA ls_reg_log LIKE LINE OF it_zreg_log.
    DATA ls_log_det LIKE LINE OF it_log_det.
    CLEAR: ls_reg_log.

    CLEAR: it_log_det, wa_zreg_log.
    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE it_zreg_log INTO wa_zreg_log INDEX e_row-index.
    CHECK sy-subrc EQ 0.


    SELECT * FROM zlog_det INTO TABLE it_log_det
    WHERE id_referencia = wa_zreg_log-id_referencia.

    IF sy-subrc NE 0.
      MESSAGE 'No se encontro registro en Tabla ZLOG_DET segun ID_REFERENCIA' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      CALL SCREEN 0200.
    ENDIF.

  ENDMETHOD.                    "hand_dblclk

*--> Inicio Modificación RSDK906912 - 2.
  "hand_tool_bar
  METHOD hand_tool_bar.

  ENDMETHOD.                    "hand_tool_bar

  METHOD hand_cmd.

  ENDMETHOD.                    "hand_cmd
*<-- Fin Modificación RSDK906912 - 2.


ENDCLASS.                    "lcl_alv100 IMPLEMENTATION
