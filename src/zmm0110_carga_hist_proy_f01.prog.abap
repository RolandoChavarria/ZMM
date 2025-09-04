*&---------------------------------------------------------------------*
*&  Include           ZMM0110_CARGA_HIST_PROY_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_F4_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ARCH  text
*----------------------------------------------------------------------*
FORM f_f4_path  USING    p_p_arch.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = p_arch
      def_path         = p_arch
      mask             = '*.*'
      mode             = 'O'
      title            = 'Selección del Fichero'
    IMPORTING
      filename         = p_arch
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.                                                    " F_F4_PATH
*&---------------------------------------------------------------------*
*&      Form  F_VAL_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_val_archivo .
  IF p_arch IS NOT INITIAL.
    IF NOT p_arch CS '.XLS'.
      MESSAGE text-003 TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VAL_FILE
*&---------------------------------------------------------------------*
*&      Form  F_CARGA_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_carga_archivo .
  DATA: lwa_hist_part LIKE LINE OF git_hist_part,
        lwa_hist_part2 LIKE LINE OF git_hist_part.
  DATA: v_str_name TYPE dd02l-tabname.

  CLEAR: git_hist_part[], git_log_error[].

  v_str_name = 'ZMMWA_HIST_PART'.

  CALL FUNCTION 'ZMF_LEE_ARCHIVO_EXCEL'
    EXPORTING
      i_structure = v_str_name
      i_fieldname = p_arch
      i_begin_col = gv_begin_col
      i_begin_row = gv_begin_row
      i_end_col   = gv_end_col
      i_end_row   = gv_end_row
      i_sheet     = '1'
    TABLES
      it_itab     = git_hist_part.

  SORT git_hist_part BY proyecto elemento_pep proveedor factura.

  LOOP AT git_hist_part INTO lwa_hist_part.
    IF  lwa_hist_part2-proyecto = lwa_hist_part-proyecto
    AND lwa_hist_part2-elemento_pep = lwa_hist_part-elemento_pep
    AND lwa_hist_part2-proveedor = lwa_hist_part-proveedor
    AND lwa_hist_part2-factura = lwa_hist_part-factura.

      git_log_error-proyecto = lwa_hist_part-proyecto.
      git_log_error-elemento_pep = lwa_hist_part-elemento_pep.
      git_log_error-proveedor = lwa_hist_part-proveedor.
      git_log_error-factura = lwa_hist_part-factura.
      git_log_error-mensaje_error = 'Existe mas de 1 Registro con la misma llave en el archivo'.
      APPEND git_log_error.
      CLEAR git_log_error.

    ENDIF.

    lwa_hist_part2 = lwa_hist_part.

  ENDLOOP.

  IF git_log_error[] IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM git_log_error COMPARING proyecto elemento_pep proveedor factura.
  ENDIF.
  IF git_hist_part IS INITIAL.
    MESSAGE text-004 TYPE 'E'.
  ENDIF.

ENDFORM.                    " F_CARGA_ARCHIVO
*&---------------------------------------------------------------------*
*&      Form  F_VAL_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_val_datos .

  DATA: lwa_hist_part LIKE LINE OF git_hist_part,
        lwa_hist_part2 LIKE LINE OF git_hist_part2,
        lv_fecha TYPE c LENGTH 8,
        lv_error TYPE c LENGTH 100,
        lv_val_importe TYPE c LENGTH 1.

  LOOP AT git_hist_part INTO lwa_hist_part.

    git_log_error-proyecto = lwa_hist_part-proyecto.
    git_log_error-elemento_pep = lwa_hist_part-elemento_pep.
    git_log_error-proveedor = lwa_hist_part-proveedor.
    git_log_error-factura = lwa_hist_part-factura.


    IF lwa_hist_part-proyecto IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Proyecto'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-elemento_pep IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Elemento PEP'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-proveedor IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Proveedor'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-factura IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Num. Factura'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-desc_proyecto IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Desc. Proyecto'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-t_registro IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Tipo de Registro'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-n_proveedor IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Nom. Proveedor'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-d_factura IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Denom. Factura'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-t_cabecero IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Txt. Cabecero'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-moneda IS INITIAL.
      git_log_error-mensaje_error = 'Es obligatorio el campo Moneda'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-f_contabilizacion IS NOT INITIAL.
      CONCATENATE lwa_hist_part-f_contabilizacion+6(4) lwa_hist_part-f_contabilizacion+3(2)
                    lwa_hist_part-f_contabilizacion+0(2) INTO lv_fecha.

      CALL FUNCTION 'RP_CHECK_DATE'
        EXPORTING
          date         = lv_fecha
        EXCEPTIONS
          date_invalid = 1.

      IF sy-subrc NE 0.
        CONCATENATE 'La fecha' lwa_hist_part-f_contabilizacion 'no es valida'
        INTO lv_error SEPARATED BY ' '.
        git_log_error-mensaje_error = lv_error.
        APPEND git_log_error.
        CLEAR: lv_error.
      ENDIF.
    ELSE.
      git_log_error-mensaje_error = 'No se especifico una Fecha de contabilización'.
      APPEND git_log_error.
    ENDIF.

    IF lwa_hist_part-importe IS NOT INITIAL.
      IF lwa_hist_part-importe CO '1234567890-. '.
      ELSE.
        CONCATENATE 'El importe:' lwa_hist_part-importe 'No tiene un formato valido'
        INTO lv_error SEPARATED BY ' '.
        git_log_error-mensaje_error = lv_error.
        APPEND git_log_error.
        CLEAR: lv_error.
      ENDIF.
    ELSE.
      git_log_error-mensaje_error = 'No se especifico un Importe'.
      APPEND git_log_error.
    ENDIF.

    IF git_log_error-mensaje_error IS INITIAL.

      CONCATENATE lwa_hist_part-f_contabilizacion+6(4)
                  lwa_hist_part-f_contabilizacion+3(2)
                  lwa_hist_part-f_contabilizacion+0(2)
                  INTO lwa_hist_part-f_contabilizacion.

      lwa_hist_part2-proyecto =  lwa_hist_part-proyecto.
      lwa_hist_part2-elemento_pep = lwa_hist_part-elemento_pep.
      lwa_hist_part2-proveedor = lwa_hist_part-proveedor.
      lwa_hist_part2-factura = lwa_hist_part-factura.
      lwa_hist_part2-desc_proyecto = lwa_hist_part-desc_proyecto.
      lwa_hist_part2-clas_caratula = lwa_hist_part-clas_caratula.
      lwa_hist_part2-t_registro =  lwa_hist_part-t_registro.
      lwa_hist_part2-clas_partida = lwa_hist_part-clas_partida.
      lwa_hist_part2-f_contabilizacion = lwa_hist_part-f_contabilizacion.
      lwa_hist_part2-n_proveedor = lwa_hist_part-n_proveedor.
      lwa_hist_part2-d_factura = lwa_hist_part-d_factura.
      lwa_hist_part2-t_cabecero = lwa_hist_part-t_cabecero.
      lwa_hist_part2-importe = lwa_hist_part-importe.
      lwa_hist_part2-moneda = lwa_hist_part-moneda.
      APPEND lwa_hist_part2 TO git_hist_part2.
    ENDIF.

    CLEAR: lv_fecha, git_log_error, lwa_hist_part2, lv_val_importe.
  ENDLOOP.

ENDFORM.                    " F_VAL_DATOS
*&---------------------------------------------------------------------*
*&      Form  F_ALIMENTA_ZMMTT_HIST_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alimenta_zmmtt_hist_part .

  DATA: lit_hist_part TYPE TABLE OF zmmtt_hist_part,
        lwa_hist_part LIKE LINE OF lit_hist_part.

  IF git_log_error[] IS INITIAL AND git_hist_part2 IS NOT INITIAL.

    SELECT * FROM zmmtt_hist_part INTO TABLE lit_hist_part
    FOR ALL ENTRIES IN git_hist_part2[]
    WHERE proyecto = git_hist_part2-proyecto
    AND   elemento_pep = git_hist_part2-elemento_pep
    AND   proveedor = git_hist_part2-proveedor
    AND   factura = git_hist_part2-factura.

    IF lit_hist_part[] IS INITIAL.
      INSERT zmmtt_hist_part  FROM TABLE git_hist_part2.
      MESSAGE text-005 TYPE 'S'.
    ELSE.
      CLEAR: git_log_error.
      LOOP AT lit_hist_part INTO lwa_hist_part.
        git_log_error-proyecto = lwa_hist_part-proyecto.
        git_log_error-elemento_pep = lwa_hist_part-elemento_pep.
        git_log_error-proveedor = lwa_hist_part-proveedor.
        git_log_error-factura = lwa_hist_part-factura.
        git_log_error-mensaje_error = 'Ya existe un registro con la misma llave en la tabla ZMMTT_HIST_PART'.
        APPEND git_log_error.
        CLEAR git_log_error.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_ALIMENTA_ZMMTT_HIST_PART
*&---------------------------------------------------------------------*
*&      Form  F_GENERA_LOG_ERRORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_genera_log_errores .

  IF git_log_error[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.                    " F_GENERA_LOG_ERRORES
*&---------------------------------------------------------------------*
*&      Form  F_GENERA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_genera_alv .
  IF gr_alv100 IS INITIAL AND gr_cnt100 IS INITIAL.
    PERFORM f_crt_fcat_0100.
    PERFORM f_layout_alv_100.

    CREATE OBJECT gr_cnt100
      EXPORTING
        container_name = 'CC_0100'.

    CREATE OBJECT gr_alv100
      EXPORTING
        i_parent = gr_cnt100.

    gr_alv100->set_table_for_first_display(
   EXPORTING is_layout       = gs_layo100
*               it_toolbar_excluding  = git_exclude
   CHANGING  it_outtab       = git_log_error[]
             it_fieldcatalog = gt_fc100 ).

  ELSE.
*    Se refresca tabla
    gr_alv100->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.



ENDFORM.                    " F_GENERA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0100 .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc100.

  CLEAR: gt_fc100, gwa_fc100.

  gwa_fc100-fieldname = 'PROYECTO'.
  gwa_fc100-tabname   = 'GIT_LOG_ERROR'.
  gwa_fc100-outputlen = '25'.
  gwa_fc100-edit      = ''.
  APPEND gwa_fc100 TO gt_fc100.
  CLEAR gwa_fc100 .

  gwa_fc100-fieldname = 'ELEMENTO_PEP'.
  gwa_fc100-tabname   = 'GIT_LOG_ERROR'.
  gwa_fc100-outputlen = '25'.
  gwa_fc100-edit      = ''.
  APPEND gwa_fc100 TO gt_fc100.
  CLEAR gwa_fc100 .

  gwa_fc100-fieldname = 'PROVEEDOR'.
  gwa_fc100-tabname   = 'GIT_LOG_ERROR'.
  gwa_fc100-outputlen = '11'.
  gwa_fc100-edit      = ''.
  APPEND gwa_fc100 TO gt_fc100.
  CLEAR gwa_fc100 .

  gwa_fc100-fieldname = 'FACTURA'.
  gwa_fc100-tabname   = 'GIT_LOG_ERROR'.
  gwa_fc100-outputlen = '25'.
  gwa_fc100-edit      = ''.
  APPEND gwa_fc100 TO gt_fc100.
  CLEAR gwa_fc100 .

  gwa_fc100-fieldname = 'MENSAJE_ERROR'.
  gwa_fc100-tabname   = 'GIT_LOG_ERROR'.
  gwa_fc100-outputlen = '100'.
  gwa_fc100-edit      = ''.
  APPEND gwa_fc100 TO gt_fc100.
  CLEAR gwa_fc100 .

  READ TABLE gt_fc100 ASSIGNING <fc> WITH KEY fieldname = 'PROYECTO'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'ID Proyecto'.
    <fc>-scrtext_l = 'ID Proyecto'.
    <fc>-scrtext_s = 'ID Proyecto'.
    <fc>-scrtext_m = 'ID Proyecto'.
  ENDIF.

  READ TABLE gt_fc100 ASSIGNING <fc> WITH KEY fieldname = 'ELEMENTO_PEP'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Elemento PEP'.
    <fc>-scrtext_l = 'Elemento PEP'.
    <fc>-scrtext_s = 'Elemento PEP'.
    <fc>-scrtext_m = 'Elemento PEP'.
  ENDIF.

  READ TABLE gt_fc100 ASSIGNING <fc> WITH KEY fieldname = 'PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'No. Proveedor'.
    <fc>-scrtext_l = 'No. Proveedor'.
    <fc>-scrtext_s = 'No. Proveedor'.
    <fc>-scrtext_m = 'No. Proveedor'.
  ENDIF.

  READ TABLE gt_fc100 ASSIGNING <fc> WITH KEY fieldname = 'FACTURA'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'No. Factura'.
    <fc>-scrtext_l = 'No. Factura'.
    <fc>-scrtext_s = 'No. Factura'.
    <fc>-scrtext_m = 'No. Factura'.
  ENDIF.

  READ TABLE gt_fc100 ASSIGNING <fc> WITH KEY fieldname = 'MENSAJE_ERROR'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Mensaje de Error'.
    <fc>-scrtext_l = 'Mensaje de Error'.
    <fc>-scrtext_s = 'Mensaje de Error'.
    <fc>-scrtext_m = 'Mensaje de Error'.
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv_100 .

  CLEAR gs_layo100.
  gs_layo100-no_rowmark = x.

ENDFORM.                    " F_LAYOUT_ALV_100
*&---------------------------------------------------------------------*
*&      Form  F_ZUTIL_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_zutil_parameters .

  SELECT * FROM zutil_parameters INTO TABLE git_zutil
  WHERE zreport = 'ZMM0110_CARGA_HIST_PROY_XLS'.

  READ TABLE git_zutil INTO gwa_zutil WITH KEY zfield = 'I_BEGIN_COL'.
  gv_begin_col = gwa_zutil-zchar.
  CLEAR gwa_zutil.

  IF gv_begin_col IS INITIAL.
    MESSAGE text-006 TYPE 'E'.
  ENDIF.

  READ TABLE git_zutil INTO gwa_zutil WITH KEY zfield = 'I_BEGIN_ROW'.
  gv_begin_row = gwa_zutil-zchar.
  CLEAR gwa_zutil.

  IF gv_begin_row IS INITIAL.
    MESSAGE text-006 TYPE 'E'.
  ENDIF.

  READ TABLE git_zutil INTO gwa_zutil WITH KEY zfield = 'I_END_COL'.
  gv_end_col = gwa_zutil-zchar.
  CLEAR gwa_zutil.

  IF gv_end_col IS INITIAL.
    MESSAGE text-006 TYPE 'E'.
  ENDIF.

  READ TABLE git_zutil INTO gwa_zutil WITH KEY zfield = 'I_END_ROW'.
  gv_end_row = gwa_zutil-zchar.
  CLEAR gwa_zutil.

  IF gv_end_row IS INITIAL.
    MESSAGE text-006 TYPE 'E'.
  ENDIF.


ENDFORM.                    " F_ZUTIL_PARAMETERS
