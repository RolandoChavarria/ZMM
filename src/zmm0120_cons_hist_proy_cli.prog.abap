*&---------------------------------------------------------------------*
*&  Include           ZMM0120_CONS_HIST_PROY_CLI
*&---------------------------------------------------------------------*
CLASS lcl_alv100dc_1 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA: lwa_t_e_pep LIKE LINE OF git_t_e_pep,
          lwa_hist_part LIKE LINE OF git_hist_part,
          lwa_nivel_detalle LIKE LINE OF git_nivel_detalle.

    CLEAR: git_nivel_detalle[], git_nivel_detalle.

    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE git_t_e_pep INTO lwa_t_e_pep INDEX e_row-index.

    SORT git_hist_part BY elemento_pep.
    LOOP AT git_hist_part INTO lwa_hist_part WHERE elemento_pep = lwa_t_e_pep-elemento_pep.

      lwa_nivel_detalle-proyecto = lwa_hist_part-proyecto.
      lwa_nivel_detalle-elemento_pep = lwa_hist_part-elemento_pep.
      lwa_nivel_detalle-proveedor = lwa_hist_part-proveedor.
      lwa_nivel_detalle-factura = lwa_hist_part-factura.
      lwa_nivel_detalle-desc_proyecto = lwa_hist_part-desc_proyecto.
      lwa_nivel_detalle-clas_caratula = lwa_hist_part-clas_caratula.
      lwa_nivel_detalle-t_registro = lwa_hist_part-t_registro.
      lwa_nivel_detalle-clas_partida = lwa_hist_part-clas_partida.
      lwa_nivel_detalle-f_contabilizacion = lwa_hist_part-f_contabilizacion.
      lwa_nivel_detalle-n_proveedor = lwa_hist_part-n_proveedor.
      lwa_nivel_detalle-d_factura = lwa_hist_part-d_factura.
      lwa_nivel_detalle-t_cabecero = lwa_hist_part-t_cabecero.
      lwa_nivel_detalle-importe = lwa_hist_part-importe.
      lwa_nivel_detalle-moneda = lwa_hist_part-moneda.

      APPEND lwa_nivel_detalle TO git_nivel_detalle.
      CLEAR: lwa_nivel_detalle.
    ENDLOOP.

    IF git_nivel_detalle[] IS NOT INITIAL.
      CALL SCREEN 0200.
    ELSE.
      MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "hand_dblclk
ENDCLASS.                    "lcl_alv100dc_1 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_alv100dc_2 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv100dc_2 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA: lwa_t_prov LIKE LINE OF git_t_prov,
          lwa_hist_part LIKE LINE OF git_hist_part,
          lwa_nivel_detalle LIKE LINE OF git_nivel_detalle.

    CLEAR: git_nivel_detalle[], git_nivel_detalle.

    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE git_t_prov INTO lwa_t_prov INDEX e_row-index.

    SORT git_hist_part BY proveedor.
    LOOP AT git_hist_part INTO lwa_hist_part WHERE proveedor = lwa_t_prov-proveedor.
      lwa_nivel_detalle-proyecto = lwa_hist_part-proyecto.
      lwa_nivel_detalle-elemento_pep = lwa_hist_part-elemento_pep.
      lwa_nivel_detalle-proveedor = lwa_hist_part-proveedor.
      lwa_nivel_detalle-factura = lwa_hist_part-factura.
      lwa_nivel_detalle-desc_proyecto = lwa_hist_part-desc_proyecto.
      lwa_nivel_detalle-clas_caratula = lwa_hist_part-clas_caratula.
      lwa_nivel_detalle-t_registro = lwa_hist_part-t_registro.
      lwa_nivel_detalle-clas_partida = lwa_hist_part-clas_partida.
      lwa_nivel_detalle-f_contabilizacion = lwa_hist_part-f_contabilizacion.
      lwa_nivel_detalle-n_proveedor = lwa_hist_part-n_proveedor.
      lwa_nivel_detalle-d_factura = lwa_hist_part-d_factura.
      lwa_nivel_detalle-t_cabecero = lwa_hist_part-t_cabecero.
      lwa_nivel_detalle-importe = lwa_hist_part-importe.
      lwa_nivel_detalle-moneda = lwa_hist_part-moneda.

      APPEND lwa_nivel_detalle TO git_nivel_detalle.
      CLEAR: lwa_nivel_detalle.
    ENDLOOP.

    IF git_nivel_detalle[] IS NOT INITIAL.
      CALL SCREEN 0200.
    ELSE.
      MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "hand_dblclk
ENDCLASS.                    "lcl_alv100dc_2 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_alv100dc_3 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv100dc_3 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA: lwa_t_cc LIKE LINE OF git_t_cc,
          lwa_hist_part LIKE LINE OF git_hist_part,
          lwa_nivel_detalle LIKE LINE OF git_nivel_detalle.

    CLEAR: git_nivel_detalle[], git_nivel_detalle.

    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE git_t_cc INTO lwa_t_cc INDEX e_row-index.

    SORT git_hist_part BY clas_caratula.
    LOOP AT git_hist_part INTO lwa_hist_part WHERE clas_caratula = lwa_t_cc-clas_caratula.
      lwa_nivel_detalle-proyecto = lwa_hist_part-proyecto.
      lwa_nivel_detalle-elemento_pep = lwa_hist_part-elemento_pep.
      lwa_nivel_detalle-proveedor = lwa_hist_part-proveedor.
      lwa_nivel_detalle-factura = lwa_hist_part-factura.
      lwa_nivel_detalle-desc_proyecto = lwa_hist_part-desc_proyecto.
      lwa_nivel_detalle-clas_caratula = lwa_hist_part-clas_caratula.
      lwa_nivel_detalle-t_registro = lwa_hist_part-t_registro.
      lwa_nivel_detalle-clas_partida = lwa_hist_part-clas_partida.
      lwa_nivel_detalle-f_contabilizacion = lwa_hist_part-f_contabilizacion.
      lwa_nivel_detalle-n_proveedor = lwa_hist_part-n_proveedor.
      lwa_nivel_detalle-d_factura = lwa_hist_part-d_factura.
      lwa_nivel_detalle-t_cabecero = lwa_hist_part-t_cabecero.
      lwa_nivel_detalle-importe = lwa_hist_part-importe.
      lwa_nivel_detalle-moneda = lwa_hist_part-moneda.

      APPEND lwa_nivel_detalle TO git_nivel_detalle.
      CLEAR: lwa_nivel_detalle.
    ENDLOOP.

    IF git_nivel_detalle[] IS NOT INITIAL.
      CALL SCREEN 0200.
    ELSE.
      MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "hand_dblclk
ENDCLASS.                    "lcl_alv100dc_3 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_alv100dc_4 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv100dc_4 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA: lwa_t_cp LIKE LINE OF git_t_cp,
          lwa_hist_part LIKE LINE OF git_hist_part,
          lwa_nivel_detalle LIKE LINE OF git_nivel_detalle.

    CLEAR: git_nivel_detalle[], git_nivel_detalle.

    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE git_t_cp INTO lwa_t_cp INDEX e_row-index.

    SORT git_hist_part BY clas_partida.
    LOOP AT git_hist_part INTO lwa_hist_part WHERE clas_partida = lwa_t_cp-clas_partida.
      lwa_nivel_detalle-proyecto = lwa_hist_part-proyecto.
      lwa_nivel_detalle-elemento_pep = lwa_hist_part-elemento_pep.
      lwa_nivel_detalle-proveedor = lwa_hist_part-proveedor.
      lwa_nivel_detalle-factura = lwa_hist_part-factura.
      lwa_nivel_detalle-desc_proyecto = lwa_hist_part-desc_proyecto.
      lwa_nivel_detalle-clas_caratula = lwa_hist_part-clas_caratula.
      lwa_nivel_detalle-t_registro = lwa_hist_part-t_registro.
      lwa_nivel_detalle-clas_partida = lwa_hist_part-clas_partida.
      lwa_nivel_detalle-f_contabilizacion = lwa_hist_part-f_contabilizacion.
      lwa_nivel_detalle-n_proveedor = lwa_hist_part-n_proveedor.
      lwa_nivel_detalle-d_factura = lwa_hist_part-d_factura.
      lwa_nivel_detalle-t_cabecero = lwa_hist_part-t_cabecero.
      lwa_nivel_detalle-importe = lwa_hist_part-importe.
      lwa_nivel_detalle-moneda = lwa_hist_part-moneda.

      APPEND lwa_nivel_detalle TO git_nivel_detalle.
      CLEAR: lwa_nivel_detalle.
    ENDLOOP.

    IF git_nivel_detalle[] IS NOT INITIAL.
      CALL SCREEN 0200.
    ELSE.
      MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "hand_dblclk
ENDCLASS.                    "lcl_alv100dc_3 IMPLEMENTATION
