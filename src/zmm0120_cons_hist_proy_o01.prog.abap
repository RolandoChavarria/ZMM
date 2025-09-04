*&---------------------------------------------------------------------*
*&  Include           ZMM0120_CONS_HIST_PROY_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZMM0120_S0100_01'.
  SET TITLEBAR  'ZMM0120_T0100_01' WITH gv_proyecto.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F_CONSTRUYE_ALV_0100_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_construye_alvs_0100 OUTPUT.

  PERFORM f_recupera_registros.
  PERFORM f_crea_alv_0100_1.
  PERFORM f_crea_alv_0100_2.
  PERFORM f_crea_alv_0100_3.
  PERFORM f_crea_alv_0100_4.

ENDMODULE.                 " F_CONSTRUYE_ALV_0100_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_ok OUTPUT.

  CLEAR ok_code.

ENDMODULE.                 " CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
* >>> RSDK907480 >>>
  DATA: lwa_det LIKE LINE OF git_nivel_detalle,
        lv_bloq TYPE flag,
        lv_msg TYPE bapi_msg.
  CLEAR: lwa_det, lv_bloq, lv_msg.
  READ TABLE git_nivel_detalle INTO lwa_det INDEX 1.
  READ TABLE git_bproy TRANSPORTING NO FIELDS
  WITH KEY proyecto = lwa_det-proyecto.
  IF sy-subrc EQ 0.
    lv_bloq = abap_true.
    CONCATENATE text-m01 lwa_det-proyecto text-m02
    INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.
* <<< RSDK907480 <<<
  SET PF-STATUS 'ZMM0120_S0200_01'.
* >>> RSDK907480 >>>
*  IF gv_edicion IS NOT INITIAL AND gv_edit IS NOT INITIAL.
  IF gv_edicion IS NOT INITIAL AND gv_edit IS NOT INITIAL
  AND  lv_bloq IS INITIAL."Proyecto no bloqueado p/Edición
* <<< RSDK907480 <<<
    SET PF-STATUS 'ZMM0120_S0200_02'.
  ENDIF.

  IF gv_edicion IS NOT INITIAL AND gv_edit IS INITIAL.
    SET PF-STATUS 'ZMM0120_S0200_03'.
  ENDIF.

  SET TITLEBAR  'ZMM0120_T0200_01' WITH gv_proyecto.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK_2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_ok_2 OUTPUT.

  CLEAR ok_code2.

ENDMODULE.                 " CLEAR_OK_2  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0200_DETALLE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_0200_detalle OUTPUT.
  PERFORM f_create_alv_0200.
ENDMODULE.                 " ALV_0200_DETALLE  OUTPUT
