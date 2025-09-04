*&---------------------------------------------------------------------*
*&  Include           ZLO0090_MON_EMB_CLI
*&---------------------------------------------------------------------*
CLASS lcl_alv100 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA ls_ch_or LIKE LINE OF it_ch_or2.
*    DATA ls_log_det LIKE LINE OF it_log_det.
    CLEAR: ls_ch_or.

    CLEAR: wa_ch_or2.
    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE it_ch_or2 INTO wa_ch_or2 INDEX e_row-index.
    CHECK sy-subrc EQ 0.
    CASE e_column.
      WHEN 'FOLIO'.
        SELECT SINGLE * FROM zmmtt_0040_ch_or INTO zmmtt_0040_ch_or
        WHERE folio = wa_ch_or2-folio.
        IF sy-subrc NE 0.
          MESSAGE 'No se encontro registro en Tabla ZMMTT_0040_CH_OR segun  "ID Proyecto"'
           TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          V_CURR = zmmtt_0040_ch_or-IMPORTE_AU.
          CALL SCREEN 0200.
        ENDIF.
      WHEN 'DECREMENTAL'.
        PERFORM f_call_me23 uSING WA_CH_OR2-DECREMENTAL.

      WHEN 'INCREMENTAL'.
        PERFORM f_call_me23 uSING WA_CH_OR2-INCREMENTAL.
    ENDCASE.


  ENDMETHOD.                    "hand_dblclk


ENDCLASS.                    "lcl_alv100 IMPLEMENTATION
