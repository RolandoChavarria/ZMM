*&---------------------------------------------------------------------*
*&  Include           ZMM0050_MON_LOG_OC_CLI
*&---------------------------------------------------------------------*
CLASS lcl_hand_0100 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA ls_det LIKE LINE OF it_detail_log.

    CLEAR: ls_det.
    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE it_detail_log INTO ls_det INDEX e_row-index.
    CHECK sy-subrc EQ 0.

    "Se identifica en que columna fue el doble click
    "En base a eso se tiene una reacción
    CASE e_column.
      WHEN 'ID_CONTROL'.
        CHECK ls_det-id_control IS NOT INITIAL.

        READ TABLE it_header_log INTO zmmtt_header_log
        WITH KEY id_control = ls_det-id_control.

        gv_dynnr = 0101.

      WHEN 'PEDIDO'.
        CHECK ls_det-pedido is NOT INITIAL.

        PERFORM f_call_me23 USING ls_det-pedido.

    ENDCASE.
  ENDMETHOD.                    "hand_dblclk
ENDCLASS.                    "lcl_hand_0100 IMPLEMENTATION
