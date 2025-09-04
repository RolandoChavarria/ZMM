*&---------------------------------------------------------------------*
*&  Include           ZMM0090_MON_ORD_CAMBIO_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ESTATUS_GUI_100_01'.
  SET TITLEBAR 'TITLE_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_0100 OUTPUT.
  IF gr_alv100 IS INITIAL AND gr_cnt100 IS INITIAL.
    PERFORM f_crt_fcat_0100 CHANGING gt_fc100.
    PERFORM f_crt_layot_0100 CHANGING gs_layo100.
   PERFORM f_crt_srt_0100 CHANGING gt_srt100.


    CREATE OBJECT gr_cnt100
      EXPORTING
        container_name = 'CC_0100'.

    CREATE OBJECT gr_alv100
      EXPORTING
        i_parent = gr_cnt100.

    gr_alv100->set_table_for_first_display(
     EXPORTING is_layout       = gs_layo100
     CHANGING  it_outtab       = IT_CH_OR2
               it_fieldcatalog = gt_fc100
               it_sort         = gt_srt100
                  ).

*   Registro para manejo de eventos
    CREATE OBJECT gr_hnd100.
    SET HANDLER gr_hnd100->hand_dblclk FOR gr_alv100."Manejo de doble click en ALV
  ELSE.
    gr_alv100->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.

ENDMODULE.                 " ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0200 output.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'TITLE_0200'.

endmodule.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CLEAR_OK output.
CLEAR ok_code.
  CLEAR ok_code.
endmodule.                 " CLEAR_OK  OUTPUT
