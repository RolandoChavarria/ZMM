*&---------------------------------------------------------------------*
*& Report  ZMM0040_CONTR_PRESU_LOG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE zmm0040_contr_presu_log_top             .    " global Data
INCLUDE zmm0040_contr_presu_log_f01.
INCLUDE zmm0040_contr_presu_log_o01.
INCLUDE zmm0040_contr_presu_log_i01.
INCLUDE zlo4220_mon_emb_cli                     .    " LCL Impl.



*** EVENTOS de Programa Ejecutable - Logica del programa
INITIALIZATION.
  so_aedat = 'IBT'.
  so_aedat-low = sy-datum.
  SO_AEDAT-HIGH = SY-DATUM.
  APPEND so_aedat.

START-OF-SELECTION.

  CASE 'X'.
    WHEN prg_1.
      v_prgn = 'ZMMFM_0010_CAT_PROVDRS_WEB'.
      v_prg_name = 'Catalogo de Proveedores.'.
    WHEN prg_2.
      v_prgn = 'ZMMFM_0020_GET_PO'.
      v_prg_name = 'Ordenes de Compra por Fecha.'.
    WHEN prg_3.
      v_prgn = 'ZMMFM_0025_GET_PO'.
      v_prg_name = 'Ordenes de Compra por Pedido.'.
    WHEN prg_4.
      v_prgn = 'ZMMFM_0030_CRT_HOJA_SERV'.
      v_prg_name = 'Hoja de Entrada de Servicios.'.
    WHEN prg_5.
      v_prgn = 'ZMMFM_0040_CHANGE_ORDERS'.
      v_prg_name = 'Ordenes de Cambio.'.
*-- JCG 20.03.13 Reemplazo de Orden de Decremental por Finalizar Orden
*    WHEN prg_6.
*      v_prgn = 'ZMMFM_0080_ORDEN_DECREMENTA1'.
*      v_prg_name = 'Ordenes de Cambio Decremental.'.

    WHEN prg_6.
      v_prgn = 'ZMMFM_0080_FINALIZAR_ORDEN'.
      v_prg_name = 'Finalizar Orden.'.
*--
    WHEN prg_7.
      v_prgn = 'ZMMFM_0070_ORDEN_INCREMENTA'.
      v_prg_name = 'Ordenes de Cambio Incremental.'.
    WHEN prg_8.
      v_prgn = 'ZMMFM_0020_PENALIZACION_PO'.
      v_prg_name = 'Entrada de Penalizacion.'.
    WHEN prg_9.
      v_prgn = 'ZMMFM_0090_DISPONIBLE'.
      v_prg_name = 'Consulta de Presupuesto.'.
*-- IJOF 25.03.13 RFC de cancelacion de orden
    WHEN prg_10.
      v_prgn = 'ZMMFM_0080_DESMARCA_FIN_ORDEN'.
      v_prg_name = 'Desmarca fin de entrega final.'.
*--
*-- JCG 17.04.13 RFC de Traspaso de Saldo
    WHEN prg_11.
      v_prgn = 'ZMMFM_0100_TRASPASO_SALDO'.
      v_prg_name = 'Traspaso de Saldo.'.
*--
  ENDCASE.

  SELECT * FROM zreg_log INTO TABLE it_zreg_log
  WHERE  namepgr = v_prgn
    AND  aedat  IN so_aedat
    AND  cputm  IN so_cputm
    AND  ernam  IN so_ernam.

  IF sy-subrc <> 0.
    MESSAGE 'No se encontraron Registros segun Filtros'
    TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 100.
  ENDIF.
