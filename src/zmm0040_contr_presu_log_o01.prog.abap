*----------------------------------------------------------------------*
***INCLUDE ZMM0040_CONTR_PRESU_LOG_STAO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ESTATUS_GUI_100_01'.
  SET TITLEBAR 'TITLE_0100' WITH v_prg_name.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_0100 OUTPUT.



ENDMODULE.                 " ALV_0100  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_ok OUTPUT.
  CLEAR ok_code.
  CLEAR ok_code.
ENDMODULE.                 " CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'TITLE_0200' WITH wa_zreg_log-id_referencia
                                 wa_zreg_log-aedat
                                 wa_zreg_log-cputm.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_0200 OUTPUT.

  IF gr_alv200 IS INITIAL AND gr_cnt200 IS INITIAL.
    PERFORM f_crt_fcat_0200 CHANGING gt_fc200.
    PERFORM f_crt_layot_0200 CHANGING gs_layo200.
    CREATE OBJECT gr_cnt200
      EXPORTING
        container_name = 'CC_0200'.

    CREATE OBJECT gr_alv200
      EXPORTING
        i_parent = gr_cnt200.

    gr_alv200->set_table_for_first_display(
     EXPORTING is_layout       = gs_layo200
     CHANGING  it_outtab       = it_log_det
               it_fieldcatalog = gt_fc200 ).
  ELSE.
    gr_alv200->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.



ENDMODULE.                 " ALV_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0100_  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_0100_ OUTPUT.
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
     CHANGING  it_outtab       = it_zreg_log
               it_fieldcatalog = gt_fc100
               it_sort         = gt_srt100 ).

*   Registro para manejo de eventos
    CREATE OBJECT gr_hnd100.
    SET HANDLER gr_hnd100->hand_dblclk FOR gr_alv100."Manejo de doble click en ALV
  ELSE.
    gr_alv100->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.
ENDMODULE.                 " ALV_0100_  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_CRT_LAYOT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYO200  text
*----------------------------------------------------------------------*
FORM f_crt_layot_0200 CHANGING p_layo TYPE lvc_s_layo.
  CLEAR p_layo.
  p_layo-cwidth_opt = x.
ENDFORM.                    " F_CRT_LAYOT_0200
