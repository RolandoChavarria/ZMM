*&---------------------------------------------------------------------*
*&  Include           ZMM0100_INFO_COMP_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CREA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_excel .
* Crea Excel
  CREATE OBJECT application 'excel.application'.
  CALL METHOD OF application 'Workbooks' = workbook.
  CALL METHOD OF workbook 'Add'.
ENDFORM.                    " F_CREA_EXCEL
*&---------------------------------------------------------------------*
*&      Form  F_ADD_HOJA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_add_hoja .
* Agrega una Hoja
  CALL METHOD OF application 'Worksheets' = sheet.
  CALL METHOD OF sheet 'Add'.
ENDFORM.                    " F_ADD_HOJA
*&---------------------------------------------------------------------*
*&      Form  F_CREA_HOJA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_hoja .
* Crea la primer hoja
  CALL METHOD OF application 'Worksheets' = sheet
    EXPORTING
    #1 = 1.
ENDFORM.                    " F_CREA_HOJA
*&---------------------------------------------------------------------*
*&      Form  F_SET_NAME_HOJA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_name_hoja .
* Nombre de la Hoja
  SET PROPERTY OF sheet 'Name' = 'REPORTE'.
  CALL METHOD OF sheet 'Activate'.
ENDFORM.                    " F_SET_NAME_HOJA
*&---------------------------------------------------------------------*
*&      Form  F_LLENA_ETIQUETAS_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llena_etiquetas_header .

  FIELD-SYMBOLS <fs_header>  LIKE LINE OF gt_fc100h.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 1.
  git_header-pedido = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 2.
  git_header-desc_ped = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 3.
  git_header-no_prov = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 4.
  git_header-desc_prov = <fs_header>-reptext.

  git_header-moneda = text-m03.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 5.
  git_header-contratado = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 6.
  git_header-fondo_garan = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 7.
  git_header-total_amortizar = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 8.
  git_header-neto_contratado = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 9.
  git_header-anticipo = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 10.
  git_header-amort_acum = <fs_header>-reptext.

  READ TABLE gt_fc100h ASSIGNING <fs_header> INDEX 11.
  git_header-amort_fg = <fs_header>-reptext.

  APPEND git_header.
ENDFORM.                    " F_LLENA_ETIQUETAS_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_LLENA_CAMPOS_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llena_campos_header .

  DATA lwa_header LIKE LINE OF it_header.

  PERFORM f_llena_etiquetas_header.

  CLEAR git_header.

  LOOP AT it_header INTO lwa_header WHERE pedido EQ gv_ebeln.
    git_header-pedido          = lwa_header-pedido.
    git_header-desc_ped        = lwa_header-desc_ped.
    git_header-no_prov         = lwa_header-no_prov.
    git_header-desc_prov       = lwa_header-desc_prov.
    git_header-moneda          = gv_moneda.
    git_header-contratado      = lwa_header-contratado.
    git_header-fondo_garan     = lwa_header-fondo_garan.
    git_header-total_amortizar = lwa_header-total_amortizar.
    git_header-neto_contratado = lwa_header-neto_contratado.
    git_header-anticipo        = lwa_header-anticipo.
    git_header-amort_acum      = lwa_header-amort_acum.
    git_header-amort_fg        = lwa_header-amort_fg.

    CONCATENATE: '$' git_header-contratado INTO git_header-contratado,
                 '$' git_header-fondo_garan INTO git_header-fondo_garan,
                 '$' git_header-total_amortizar
                     INTO git_header-total_amortizar,
                 '$' git_header-neto_contratado
                     INTO git_header-neto_contratado,
                 '$' git_header-anticipo INTO git_header-anticipo,
                 '$' git_header-amort_acum INTO git_header-amort_acum,
                 '$' git_header-amort_fg INTO git_header-amort_fg.

    APPEND git_header.
    CLEAR: lwa_header, git_header.

  ENDLOOP.
ENDFORM.                    " F_LLENA_CAMPOS_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_LLENA_CAMPOS_DETALLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llena_campos_detalle .

  DATA: lwa_detalle  LIKE LINE OF it_detail_p,
        lv_lines     TYPE i,
        lv_index     TYPE i,
        lv_tabix     TYPE i,
        lv_gasto     TYPE dmbtr,
        lv_fg        TYPE dmbtr,
        lv_amortiz   TYPE dmbtr,
        lv_netocont  TYPE dmbtr,
        lv_verifi    TYPE dmbtr,
        lv_total     TYPE dmbtr,
            "Acumuladores
        lv_agasto    TYPE dmbtr,
        lv_afg       TYPE dmbtr,
        lv_aamortiz  TYPE dmbtr,
        lv_anetocont TYPE dmbtr,
        lv_averifi   TYPE dmbtr,
        lv_atotal    TYPE dmbtr.

  FIELD-SYMBOLS: <fs_detalle> LIKE LINE OF it_detail_p,
                 <fs_det_fch> LIKE LINE OF it_detail_p.

  CLEAR: lv_gasto, lv_fg, lv_amortiz, lv_netocont, lv_verifi, lv_total,
         lv_agasto, lv_afg, lv_aamortiz, lv_anetocont, lv_averifi,
         lv_atotal, lv_lines, lv_index.

  PERFORM f_llena_etiquetas_detalle.

  CLEAR git_detalle.

  DESCRIBE TABLE it_detail_p LINES lv_lines.

  LOOP AT it_detail_p INTO lwa_detalle.
    lv_tabix = sy-tabix.
    IF lv_tabix NE lv_lines.
      lv_index = lv_tabix + 1.
      READ TABLE it_detail_p ASSIGNING <fs_detalle> INDEX lv_index.
    ELSE.
      READ TABLE it_detail_p ASSIGNING <fs_detalle> INDEX lv_tabix.
    ENDIF.

    READ TABLE it_detail_p ASSIGNING <fs_det_fch> INDEX lv_tabix.

    IF sy-subrc EQ 0.
      IF <fs_det_fch>-bldat IS INITIAL.
        lwa_detalle-bldat = space.
      ENDIF.
      IF <fs_det_fch>-fch_ver IS INITIAL.
        lwa_detalle-fch_ver = space.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lwa_detalle-srvpos
      IMPORTING
        output = lwa_detalle-srvpos.

    git_detalle-bldat    = lwa_detalle-bldat.
    git_detalle-xblnr    = lwa_detalle-xblnr.
    git_detalle-srvpos   = lwa_detalle-srvpos.
    git_detalle-asktx    = lwa_detalle-asktx.
    git_detalle-menge    = lwa_detalle-menge.
    git_detalle-g_dmbtr  = lwa_detalle-g_dmbtr.
    git_detalle-r_dmbtr  = lwa_detalle-r_dmbtr.
    git_detalle-a_dmbtr  = lwa_detalle-a_dmbtr.
    git_detalle-rf_dmbtr = lwa_detalle-rf_dmbtr.
    git_detalle-vf_belnr = lwa_detalle-vf_belnr.
    git_detalle-dc_belnr = lwa_detalle-dc_belnr.
    git_detalle-fch_ver  = lwa_detalle-fch_ver.
    git_detalle-dmbtr    = lwa_detalle-dmbtr.
    git_detalle-mwskz    = lwa_detalle-mwskz.
    git_detalle-total    = lwa_detalle-total.

    IF NOT git_detalle-bldat IS INITIAL OR
       NOT git_detalle-fch_ver IS INITIAL.

      IF git_detalle-bldat IS NOT INITIAL.
        PERFORM f_conver_fech_bldat.
      ENDIF.
      IF git_detalle-fch_ver IS NOT INITIAL.
        PERFORM f_conver_fech_fch_ver.
      ENDIF.

    ELSE.
      PERFORM f_conver_fech_bldat.
      PERFORM f_conver_fech_fch_ver.
    ENDIF.

    lv_gasto    = lv_gasto    + lwa_detalle-g_dmbtr.
    lv_fg       = lv_fg       + lwa_detalle-r_dmbtr.
    lv_amortiz  = lv_amortiz  + lwa_detalle-a_dmbtr.
    lv_netocont = lv_netocont + lwa_detalle-rf_dmbtr.
    lv_verifi   = lv_verifi   + lwa_detalle-dmbtr.
    lv_total    = lv_total    + lwa_detalle-total.

    APPEND git_detalle.
    CLEAR git_detalle.

    IF <fs_detalle>-xblnr NE lwa_detalle-xblnr OR lv_tabix EQ lv_lines.
      "Se agrega una linea
      git_detalle-xblnr    = lwa_detalle-xblnr.
      git_detalle-g_dmbtr  = lv_gasto.
      git_detalle-r_dmbtr  = lv_fg.
      git_detalle-a_dmbtr  = lv_amortiz.
      git_detalle-rf_dmbtr = lv_netocont.
      git_detalle-dmbtr    = lv_verifi.
      git_detalle-total    = lv_total.

* Se asignan valores a acumuladores.
      lv_agasto    = lv_agasto    + lv_gasto.
      lv_afg       = lv_afg       + lv_fg.
      lv_aamortiz  = lv_aamortiz  + lv_amortiz.
      lv_anetocont = lv_anetocont + lv_netocont.
      lv_averifi   = lv_averifi   + lv_verifi.
      lv_atotal    = lv_atotal    + lv_total.

      APPEND git_detalle.

      CLEAR: lv_gasto, lv_fg, lv_amortiz, lv_netocont, lv_verifi,
             lv_total, git_detalle.
    ENDIF.

    IF lv_tabix EQ lv_lines.
* Se agrega linea final.
      git_detalle-menge    = text-m04.
      git_detalle-g_dmbtr  = lv_agasto.
      git_detalle-r_dmbtr  = lv_afg.
      git_detalle-a_dmbtr  = lv_aamortiz.
      git_detalle-rf_dmbtr = lv_anetocont.
      git_detalle-dmbtr    = lv_averifi.
      git_detalle-total    = lv_atotal.

      APPEND git_detalle.

      CLEAR: lv_agasto, lv_afg, lv_aamortiz, lv_anetocont, lv_averifi,
             lv_atotal, git_detalle.
    ENDIF.

    CLEAR lwa_detalle.
  ENDLOOP.
ENDFORM.                    " F_LLENA_CAMPOS_DETALLE
*&---------------------------------------------------------------------*
*&      Form  F_LLENA_ETIQUETAS_DETALLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llena_etiquetas_detalle .

  FIELD-SYMBOLS: <fs_detalle> LIKE LINE OF gt_fc100d.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 9.
  git_detalle-bldat = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 10.
  git_detalle-xblnr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 11.
  git_detalle-srvpos = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 12.
  git_detalle-asktx = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 13.
  git_detalle-menge = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 14.
  git_detalle-g_dmbtr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 15.
  git_detalle-r_dmbtr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 16.
  git_detalle-a_dmbtr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 17.
  git_detalle-rf_dmbtr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 18.
  git_detalle-vf_belnr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 19.
  git_detalle-dc_belnr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 20.
  git_detalle-fch_ver = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 21.
  git_detalle-dmbtr = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 22.
  git_detalle-mwskz = <fs_detalle>-reptext.

  READ TABLE gt_fc100d ASSIGNING <fs_detalle> INDEX 23.
  git_detalle-total = <fs_detalle>-reptext.

  APPEND git_detalle.

ENDFORM.                    " F_LLENA_ETIQUETAS_DETALLE
*&---------------------------------------------------------------------*
*&      Form  F_CONVER_FECH_BLDAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_conver_fech_bldat .
  CONCATENATE git_detalle-bldat+6(2) '.'
              git_detalle-bldat+4(2) '.'
              git_detalle-bldat+0(4) INTO git_detalle-bldat.
ENDFORM.                    " F_CONVER_FECH_BLDAT
*&---------------------------------------------------------------------*
*&      Form  F_CONVER_FECH_FCH_VER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_conver_fech_fch_ver .
  CONCATENATE git_detalle-fch_ver+6(2) '.'
              git_detalle-fch_ver+4(2) '.'
              git_detalle-fch_ver+0(4) INTO git_detalle-fch_ver.
ENDFORM.                    " F_CONVER_FECH_FCH_VER
*&---------------------------------------------------------------------*
*&      Form  F_CREA_ARCH_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_arch_excel .
  DATA: lv_contador TYPE i,
        lv_tabix    TYPE i,
        lv_lines    TYPE i,
        lv_flag     TYPE i,
        lv_index    TYPE i,
        lv_path     TYPE bapi_msg,
        lv_folder   TYPE string.

  FIELD-SYMBOLS <fs_detalle> LIKE LINE OF git_detalle.

  CLEAR: lv_contador, lv_tabix, lv_lines, lv_index.

  PERFORM f_crea_det_excel.

* Recorre Tabla de Header.
  IF git_header[] IS NOT INITIAL.
    DESCRIBE TABLE git_header LINES lv_lines.
    lv_contador = 4.

    LOOP AT git_header.
      lv_tabix = sy-tabix.
      PERFORM f_llena_header_excel    USING lv_tabix lv_lines
                                   CHANGING lv_contador.
    ENDLOOP.
  ENDIF.

  CLEAR: lv_tabix, lv_lines, lv_index.

* Recorre Tabla de Detalle.
  IF git_detalle[] IS NOT INITIAL.
    DESCRIBE TABLE git_detalle LINES lv_lines.
    lv_contador = lv_contador + 3.

    LOOP AT git_detalle.
      lv_tabix = sy-tabix.
      IF lv_tabix NE lv_lines AND lv_tabix GT 1.
        lv_index = lv_tabix + 1.
        READ TABLE git_detalle ASSIGNING <fs_detalle> INDEX lv_index.
        IF <fs_detalle>-xblnr NE git_detalle-xblnr.
          lv_flag = 1.
        ELSE.
          lv_flag = 0.
        ENDIF.
      ENDIF.

      IF lv_tabix EQ lv_lines.
        lv_flag = 1.
      ENDIF.

      PERFORM f_llena_detalle_excel     USING lv_tabix lv_lines lv_flag
                                     CHANGING lv_contador.
      CLEAR lv_flag.
    ENDLOOP.
  ENDIF.

  IF sy-subrc EQ 0.
    PERFORM f_get_filename CHANGING lv_folder lv_path.
    IF NOT lv_path IS INITIAL.
      PERFORM f_conver_path CHANGING lv_path.
      IF sy-subrc EQ 0.
        PERFORM f_guarda_arch_excel USING lv_path.
        IF sy-subrc EQ 0.
          MESSAGE text-m07 TYPE 'S'.
        ELSE.
          MESSAGE text-m08 TYPE 'E' DISPLAY LIKE 'S'.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE text-m06 TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_CREA_ARCH_EXCEL
*&---------------------------------------------------------------------*
*&      Form  F_CREA_DET_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_det_excel .
  PERFORM f_crea_excel.
  PERFORM f_add_hoja.
  PERFORM f_crea_hoja.
  PERFORM f_set_name_hoja.
ENDFORM.                    " F_CREA_DET_EXCEL
*&---------------------------------------------------------------------*
*&      Form  F_LLENA_HEADER_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_CONTADOR  text
*----------------------------------------------------------------------*
FORM f_llena_header_excel    USING plv_tabix    TYPE i
                                   plv_lines    TYPE i
                          CHANGING plv_contador TYPE i.

  PERFORM f_rellena_celda USING 'D' plv_tabix plv_contador
                                    git_header-pedido plv_lines 0.
  PERFORM f_rellena_celda USING 'E' plv_tabix plv_contador
                                    git_header-desc_ped plv_lines 0.
  PERFORM f_rellena_celda USING 'F' plv_tabix plv_contador
                                    git_header-no_prov plv_lines 0.
  PERFORM f_rellena_celda USING 'G' plv_tabix plv_contador
                                    git_header-desc_prov plv_lines 0.
  PERFORM f_rellena_celda USING 'H' plv_tabix plv_contador
                                    git_header-moneda plv_lines 0.
  PERFORM f_rellena_celda USING 'I' plv_tabix plv_contador
                                    git_header-contratado plv_lines 0.
  PERFORM f_rellena_celda USING 'J' plv_tabix plv_contador
                                    git_header-fondo_garan plv_lines 0.
  PERFORM f_rellena_celda USING 'K' plv_tabix plv_contador
                                    git_header-total_amortizar
                                    plv_lines 0.
  PERFORM f_rellena_celda USING 'L' plv_tabix plv_contador
                                    git_header-neto_contratado
                                    plv_lines 0.
  PERFORM f_rellena_celda USING 'M' plv_tabix plv_contador
                                    git_header-anticipo plv_lines 0.
  PERFORM f_rellena_celda USING 'N' plv_tabix plv_contador
                                    git_header-amort_acum plv_lines 0.
  PERFORM f_rellena_celda USING 'O' plv_tabix plv_contador
                                    git_header-amort_fg plv_lines 0.
  plv_contador = plv_contador + 1.
ENDFORM.                    " F_LLENA_HEADER_EXCEL
*&---------------------------------------------------------------------*
*&      Form  F_RELLENA_CELDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0941   text
*      -->P_PLV_CONTADOR  text
*      -->P_GIT_HEADER_PEDIDO  text
*----------------------------------------------------------------------*
FORM f_rellena_celda  USING columna index fila texto registro flag.
*rellenamos la celda
  CALL METHOD OF application 'cells' = cells
    EXPORTING
    #1 = fila
    #2 = columna.
  SET PROPERTY OF cells 'value' = texto.

  IF fila EQ 4 OR fila EQ 9 OR ( index EQ registro AND flag EQ 1 ).
    PERFORM f_formato_celda_cab.

    IF index EQ registro.
      PERFORM f_formato_cells_cab_fin.
    ENDIF.
  ELSEIF fila EQ 5.
    PERFORM f_formato_cells_cab USING columna.
  ELSEIF fila EQ 10.
    PERFORM f_formato_cell_det USING columna.
  ELSEIF fila GT 10.
    PERFORM f_formato_reg_nor USING columna.
  ENDIF.

  IF flag EQ 1.
    PERFORM f_cell_font_cant.
  ENDIF.

ENDFORM.                    " F_RELLENA_CELDA
*&---------------------------------------------------------------------*
*&      Form  F_FORMATO_CELDA_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_formato_celda_cab .
  PERFORM f_cell_font.
  PERFORM f_cell_fondo.
ENDFORM.                    " F_FORMATO_CELDA_CAB
*&---------------------------------------------------------------------*
*&      Form  F_FORMATO_CELLS_CAB_FIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_formato_cells_cab_fin .
  PERFORM f_borde_arr_neg.
ENDFORM.                    " F_FORMATO_CELLS_CAB_FIN
*&---------------------------------------------------------------------*
*&      Form  F_FORMATO_CELLS_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_formato_cells_cab USING columna.
  IF columna EQ 'D'.
    PERFORM f_borde_izq_neg.
    PERFORM f_borde_der_nor.
    PERFORM f_borde_arr_neg.
    PERFORM f_borde_aba_neg.
  ELSEIF columna NE 'O'.
    PERFORM f_borde_izq_nor.
    PERFORM f_borde_der_nor.
    PERFORM f_borde_arr_neg.
    PERFORM f_borde_aba_neg.
  ELSEIF columna EQ 'O'.
    PERFORM f_borde_izq_nor.
    PERFORM f_borde_der_neg.
    PERFORM f_borde_arr_neg.
    PERFORM f_borde_aba_neg.
  ENDIF.
ENDFORM.                    " F_FORMATO_CELLS_CAB
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_DER_NOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_der_nor .
* Derecho
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 10.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 2.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_DER_NOR
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_IZQ_NEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_izq_neg .
* Izquierdo
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 7.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 3.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_IZQ_NEG
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_ARR_NEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_arr_neg .
* Arriba
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 8.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 3.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_ARR_NEG
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_ABA_NEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_aba_neg .
* Abajo
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 9.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 3.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_ABA_NEG
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_IZQ_NOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_izq_nor .
* Izquierdo
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 7.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 2.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_IZQ_NOR
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_DER_NEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_der_neg .
* Derecho
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 10.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 3.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_DER_NEG
*&---------------------------------------------------------------------*
*&      Form  F_FORMATO_CELL_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COLUMNA  text
*----------------------------------------------------------------------*
FORM f_formato_cell_det  USING columna.
  IF columna EQ 'B'.
    PERFORM f_borde_izq_neg.
    PERFORM f_borde_der_nor.
    PERFORM f_borde_arr_neg.
    PERFORM f_borde_aba_nor.
  ELSEIF columna NE 'P'.
    PERFORM f_borde_izq_nor.
    PERFORM f_borde_der_nor.
    PERFORM f_borde_arr_neg.
    PERFORM f_borde_aba_nor.
  ELSEIF columna EQ 'P'.
    PERFORM f_borde_izq_nor.
    PERFORM f_borde_der_neg.
    PERFORM f_borde_arr_neg.
    PERFORM f_borde_aba_nor.
  ENDIF.
ENDFORM.                    " F_FORMATO_CELL_DET
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_ABA_NOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_aba_nor .
* Abajo
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 9.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 2.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_ABA_NOR
*&---------------------------------------------------------------------*
*&      Form  F_FORMATO_REG_NOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COLUMNA  text
*----------------------------------------------------------------------*
FORM f_formato_reg_nor  USING columna.
  IF columna EQ 'B'.
    PERFORM f_borde_izq_neg.
    PERFORM f_borde_der_nor.
    PERFORM f_borde_arr_nor.
    PERFORM f_borde_aba_nor.
  ELSEIF columna NE 'P'.
    PERFORM f_borde_izq_nor.
    PERFORM f_borde_der_nor.
    PERFORM f_borde_arr_nor.
    PERFORM f_borde_aba_nor.
  ELSEIF columna EQ 'P'.
    PERFORM f_borde_izq_nor.
    PERFORM f_borde_der_neg.
    PERFORM f_borde_arr_nor.
    PERFORM f_borde_aba_nor.
  ENDIF.
ENDFORM.                    " F_FORMATO_REG_NOR
*&---------------------------------------------------------------------*
*&      Form  F_BORDE_ARR_NOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_borde_arr_nor .
* Arriba
  CALL METHOD OF cells 'BORDERS' = borders
    EXPORTING
    #1 = 8.
  SET PROPERTY OF borders 'LineStyle' = 1.
  SET PROPERTY OF borders 'WEIGHT' = 2.
  FREE OBJECT borders.
ENDFORM.                    " F_BORDE_ARR_NOR
*&---------------------------------------------------------------------*
*&      Form  F_CELL_FONDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cell_fondo .
***** COLOR DE FONDO
  CALL METHOD OF cells 'INTERIOR' = int.
  SET PROPERTY OF int 'ColorIndex' = 30. "Tinto
  SET PROPERTY OF int 'Pattern' = 1.
  FREE OBJECT int.
ENDFORM.                    " F_CELL_FONDO
*&---------------------------------------------------------------------*
*&      Form  F_CELL_FONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cell_font .
***** FORMATO DE CELDA
  CALL METHOD OF cells 'FONT' = font.
  SET PROPERTY OF font 'BOLD' = 2.
  SET PROPERTY OF font 'SIZE' = '12'.
  SET PROPERTY OF font 'colorindex' = 2. "Blanco
  FREE OBJECT font.
ENDFORM.                    " F_CELL_FONT
*&---------------------------------------------------------------------*
*&      Form  F_CELL_FONT_CANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cell_font_cant .
***** FORMATO DE CELDA
  CALL METHOD OF cells 'FONT' = font.
  SET PROPERTY OF font 'BOLD' = 2.
  SET PROPERTY OF font 'SIZE' = '12'.
  FREE OBJECT font.
ENDFORM.                    " F_CELL_FONT_CANT
*&---------------------------------------------------------------------*
*&      Form  F_LLENA_DETALLE_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*      -->P_LV_LINES  text
*      -->P_LV_FLAG  text
*      <--P_LV_CONTADOR  text
*----------------------------------------------------------------------*
FORM f_llena_detalle_excel  USING  plv_tabix    TYPE i
                                   plv_lines    TYPE i
                                   plv_flag     TYPE i
                          CHANGING plv_contador TYPE i.

  PERFORM f_rellena_celda USING 'B' plv_tabix plv_contador
                                    git_detalle-bldat plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'C' plv_tabix plv_contador
                                    git_detalle-xblnr plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'D' plv_tabix plv_contador
                                    git_detalle-srvpos plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'E' plv_tabix plv_contador
                                    git_detalle-asktx plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'F' plv_tabix plv_contador
                                    git_detalle-menge plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'G' plv_tabix plv_contador
                                    git_detalle-g_dmbtr plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'H' plv_tabix plv_contador
                                    git_detalle-r_dmbtr plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'I' plv_tabix plv_contador
                                    git_detalle-a_dmbtr
                                    plv_lines plv_flag.
  PERFORM f_rellena_celda USING 'J' plv_tabix plv_contador
                                    git_detalle-rf_dmbtr
                                    plv_lines plv_flag.
  PERFORM f_rellena_celda USING 'K' plv_tabix plv_contador
                                    git_detalle-vf_belnr plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'L' plv_tabix plv_contador
                                    git_detalle-dc_belnr plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'M' plv_tabix plv_contador
                                    git_detalle-fch_ver plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'N' plv_tabix plv_contador
                                    git_detalle-dmbtr plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'O' plv_tabix plv_contador
                                    git_detalle-mwskz plv_lines
                                    plv_flag.
  PERFORM f_rellena_celda USING 'P' plv_tabix plv_contador
                                    git_detalle-total plv_lines
                                    plv_flag.
  plv_contador = plv_contador + 1.

ENDFORM.                    " F_LLENA_DETALLE_EXCEL
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_FOLDER  text
*      <--P_LV_PATH  text
*----------------------------------------------------------------------*
FORM f_get_filename  CHANGING  plv_folder TYPE string
                               plv_path   TYPE bapi_msg.

  cl_gui_frontend_services=>directory_browse(
    EXPORTING window_title = 'DOWNLOAD DIRECTORY'
    CHANGING selected_folder = plv_folder ).
  IF plv_folder IS NOT INITIAL.
    plv_path = plv_folder.
  ENDIF.

ENDFORM.                    " F_GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  F_CONVER_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_PATH  text
*----------------------------------------------------------------------*
FORM f_conver_path  CHANGING plv_path TYPE bapi_msg.
  IF plv_path CS '\'.
    CONCATENATE plv_path '\' gv_ebeln '_' gv_lifnr '_' sy-datum+6(2)
    '_' sy-datum+4(2) '_' sy-datum+0(4) '-' sy-uzeit+0(2) '-'
    sy-uzeit+2(2) INTO plv_path.
  ELSE.
    CONCATENATE plv_path '/' gv_ebeln '_' gv_lifnr '_' sy-datum+6(2)
     '_' sy-datum+4(2) '_' sy-datum+0(4) '-' sy-uzeit+0(2) '-'
     sy-uzeit+2(2) INTO plv_path.
  ENDIF.
ENDFORM.                    " F_CONVER_PATH
*&---------------------------------------------------------------------*
*&      Form  F_GUARDA_ARCH_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PATH  text
*----------------------------------------------------------------------*
FORM f_guarda_arch_excel  USING plv_path TYPE bapi_msg.
* GUARDA EN EL EXCEL
  CALL METHOD OF sheet 'SaveAs'
  EXPORTING
  #1 = plv_path "PATH A DONDE SERA GUARDADO
  #2 = 1.

* LIBERA EL OBJETO
  FREE OBJECT: application.
ENDFORM.                    " F_GUARDA_ARCH_EXCEL
