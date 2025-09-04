*----------------------------------------------------------------------*
***INCLUDE ZMM0050_MON_LOG_OC_STATUS_0O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'. " El estatus son botones de toolBar y sus comandos
  SET TITLEBAR 'TITLE_0100'."Titulo de la ventana
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*   Limpiar variable donde cacha eventos en dynpro
*----------------------------------------------------------------------*
MODULE clear_ok OUTPUT.
  CLEAR ok_code.
ENDMODULE.                 " CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
*     ALV 0100
*----------------------------------------------------------------------*
MODULE alv_0100 OUTPUT.
  IF gr_dalv IS INITIAL."Si la variable de ALV aun no contiene uno
    PERFORM f_crt_fcat_det.
    PERFORM f_set_layout_det.

    "Instancia Objeto de Custom Container donde aparecera el ALV
    CREATE OBJECT gr_dcnt
      EXPORTING
        container_name = 'CC_0100'."Custom Container colocado en dynpro

    "Instancia ALV y se le pasa en que Custom Container debe aparecer
    CREATE OBJECT gr_dalv
      EXPORTING
        i_parent      = gr_dcnt"Variable ref del containr
        i_appl_events = 'X'.

    "Llamamos Metodo de clase ALV para indicar que itab y otras caracteristicas
    "del ALV
    gr_dalv->set_table_for_first_display(
      EXPORTING is_layout       = gs_dlayo "Otras caracteristicas de ALV en gral.
      CHANGING  it_outtab       = it_detail_log "Itab a mostrar en ALV
                it_fieldcatalog = gt_dfcat ). "Field Catalog de ALV

    CREATE OBJECT gr_hand_alv."Clase que implementa eventos de ALV
    SET HANDLER gr_hand_alv->hand_dblclk FOR gr_dalv."Manejo de doble click en ALV

  ELSE.
    "Si ya la variable GR_dALV contiene un ALV solo se hacer un refresh de lo
    "Que se muestra en pantalla contra lo que contiene la itab IT_DETAIL_LOG
    gr_dalv->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.
ENDMODULE.                 " ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_DYNNR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_dynnr OUTPUT.
  IF gv_dynnr IS INITIAL.
    gv_dynnr = 0102."Mostramos dynpro vacia
  ENDIF.
ENDMODULE.                 " INIT_DYNNR  OUTPUT
