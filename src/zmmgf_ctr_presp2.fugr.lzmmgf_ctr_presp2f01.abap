*----------------------------------------------------------------------*
***INCLUDE LZMMGF_CTR_PRESP2F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_CRT_IDRFC
*&---------------------------------------------------------------------*
FORM f_crt_idrfc  CHANGING p_idrfc TYPE zmmde_0030_id
                           p_subrc TYPE subrc.
  DATA: lv_max TYPE zmmde_0030_id,
        lv_id TYPE zmmde_0030_id.

  CLEAR: p_idrfc, p_subrc, lv_max, lv_id.

  CONCATENATE sy-datum sy-uzeit '00' INTO p_idrfc.
  lv_max = p_idrfc + 99.

  SELECT SINGLE idrfc FROM zmmtt_30_hjsrv_h INTO lv_id
  WHERE idrfc = p_idrfc.
  IF sy-subrc EQ 0.
    DO 1000 TIMES.
      ADD 1 TO p_idrfc.
      SELECT SINGLE idrfc FROM zmmtt_30_hjsrv_h INTO lv_id
      WHERE idrfc = p_idrfc.
      IF sy-subrc NE 0.
        EXIT.
      ELSEIF p_idrfc EQ lv_max.
        WAIT UP TO 1 SECONDS.
        CONCATENATE sy-datum sy-uzeit '00' INTO p_idrfc.
        lv_max = p_idrfc + 99.
      ELSEIF sy-index EQ 1000.
        p_subrc = 2. "Erorr al crear ID unico
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.                    " F_CRT_IDRFC

*&---------------------------------------------------------------------*
*&      Form  F_CRT_REGS_TABLES
*&---------------------------------------------------------------------*
FORM f_crt_regs_tables  USING    p_idrfc  TYPE zmmde_0030_id
                                 p_cab    TYPE zmmwa_0030_hjsrv_h
                                 pit_dets TYPE zmmit_0030_hjsrv_d
                                 p_absgr  TYPE absgr
                                 p_waers  TYPE waers              "jane mod tipo de moneda

                        CHANGING pwa_h    TYPE zmmtt_30_hjsrv_h
                                 pit_d    TYPE ty_it_det
                                 p_subrc  TYPE subrc
                                 p_error_absgr  TYPE sy-subrc.

  DATA: lwa_d TYPE zmmtt_30_hjsrv_d,
        lwa_dets LIKE LINE OF pit_dets.

  CLEAR: pwa_h, pit_d, p_subrc, lwa_d, lwa_dets.

* Cabecera
  pwa_h-idrfc     = p_idrfc.
  pwa_h-ebeln     = p_cab-no_pedido.
  pwa_h-lifnr     = p_cab-no_proveedor.
*  pwa_h-bill      = p_cab-factura.         "JANE
*  pwa_h-desc_bill = p_cab-desc_factura.    "JANE
*  pwa_h-date_bill = p_cab-fecha_factura.   "JANE
*  pwa_h-amnt_bill = p_cab-monto_factura.   "JANE
*  pwa_h-waers     = 'MXN'. "Validar si queda en codigo Duro
  pwa_h-waers     = p_waers.               "JANE: TIPO DE MONEDA
  pwa_h-stsh      = '00'. "Nuevo
  pwa_h-date_exec = sy-datum.
  pwa_h-time_exec = sy-uzeit.

* Detalles


  LOOP AT pit_dets INTO lwa_dets.

    lwa_d-idrfc = p_idrfc.              "id rfc
    lwa_d-ebeln = pwa_h-ebeln.          "no de pedido
    lwa_d-ebelp = lwa_dets-no_posicion. "no posicion
    lwa_d-extrow = lwa_dets-no_linea.    "no de linea
    lwa_d-asnum = lwa_dets-id_servicio. "id servicio
    lwa_d-stsd  = '00'.                 "Satus hoja de servicios

    lwa_d-bill  = lwa_dets-no_factura.          "JANE
    lwa_d-prctg = lwa_dets-avance_estima_p.     "JANE: AVANCE EN %
    lwa_d-erfmg = lwa_dets-avance_estima_c.     "JANE:  AVANCE EN CANTIDAD

    "tipo de contrato
    lwa_d-bktxt = lwa_dets-referencia_web.      "JANE:  FOLIO WEB
    lwa_d-xblnr = lwa_dets-rango_estima.        "JANE:  RANGO DE ESTIMACIONES

    lwa_d-msgerr = 'Estimación correcta'.

    IF p_absgr = 10 AND lwa_dets-avance_estima_p IS INITIAL.
      lwa_d-msgerr = 'Estimación incorrecta: esp. precio alzado'.
      p_error_absgr = 1.
    ELSEIF p_absgr = 20 AND lwa_dets-avance_estima_c IS INITIAL.
      lwa_d-msgerr = 'Estimación incorrecta: esp. precio unitario'.
      p_error_absgr = 2.
    ENDIF.

    APPEND lwa_d TO pit_d.
    CLEAR: lwa_dets, lwa_d.
  ENDLOOP.
*** ---> Begin of RPM 21.10.2014
*** Se agrega sort
  SORT pit_d BY ebeln ebelp extrow asnum.
*** <--- End  of RPM 21.10.2014
  "eliminar duplicados
  DELETE ADJACENT DUPLICATES FROM pit_d COMPARING ebeln ebelp extrow asnum.
* Inserta registros en tablas Z
  INSERT zmmtt_30_hjsrv_h FROM pwa_h.
  IF sy-subrc EQ 0.
    INSERT zmmtt_30_hjsrv_d FROM TABLE pit_d[].
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      p_subrc = 3.
    ENDIF.
  ELSE.
    p_subrc = 3.
  ENDIF.

ENDFORM.                    " F_CRT_REGS_TABLES

*&---------------------------------------------------------------------*
*&      Form  F_GET_PO
*&---------------------------------------------------------------------*
FORM f_get_po  USING    p_ebeln  TYPE ebeln
                        p_idrfc  TYPE zmmde_0030_id
               CHANGING pwa_hdr  TYPE bapiekkol
                        pit_item TYPE ty_it_po_item
                        pit_serv TYPE ty_it_po_serv
                        pit_accs TYPE ty_it_po_accs
                        p_subrc  TYPE subrc.

  CLEAR: pwa_hdr, pit_item, pit_serv, pit_accs, p_subrc.

  CALL FUNCTION 'BAPI_PO_GETDETAIL' "#EC CI_USAGE_OK[1803189] "#EC CI_USAGE_OK[2438131]
    EXPORTING
      purchaseorder             = p_ebeln
      items                     = 'X'
      services                  = 'X'
    IMPORTING
      po_header                 = pwa_hdr
    TABLES
      po_items                  = pit_item
      po_item_services          = pit_serv
      po_item_srv_accass_values = pit_accs.

  IF sy-subrc NE 0 OR pwa_hdr IS INITIAL OR pit_item IS INITIAL
    OR pit_serv IS INITIAL OR pit_accs IS INITIAL.
    p_subrc = 6.
    "Actualiza detalles de Z para dejar mensaje de error
    UPDATE zmmtt_30_hjsrv_d
    SET msgerr = 'Pedido no existe'
    WHERE idrfc EQ p_idrfc.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_PO
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_PO
*&---------------------------------------------------------------------*
FORM f_process_po USING pwa_po_hdr  TYPE bapiekkol
                        pit_po_item TYPE ty_it_po_item
                        pit_po_serv TYPE ty_it_po_serv
                        pit_po_accs TYPE ty_it_po_accs
                        p_absgr  TYPE absgr
                        p_hojas TYPE zmmde_lblni
               CHANGING pwa_hdr     TYPE zmmtt_30_hjsrv_h
                        pit_det     TYPE ty_it_det
                        pit_hojas   TYPE ty_it_hojas
                        pit_msg     TYPE ty_it_msg
                        p_subrc     TYPE subrc.

  DATA: lwa_item LIKE LINE OF pit_po_item,
        lwa_det LIKE LINE OF pit_det,
        lv_pckg  LIKE lwa_item-pckg_no,
        lv_spckg LIKE lwa_item-pckg_no,
        lwa_bapihdr TYPE bapiessrc,
        lwa_serv LIKE LINE OF pit_po_serv,
        lwa_serv2 LIKE LINE OF pit_po_serv,
        lit_bapiesll TYPE TABLE OF bapiesllc WITH HEADER LINE,
        lwa_bapiesll TYPE bapiesllc,
        lit_bapiacc TYPE TABLE OF bapiesklc WITH HEADER LINE,
        lwa_poaccs LIKE LINE OF pit_po_accs,
        lv_flag TYPE flag,
        lv_sheet TYPE bapiessr-sheet_no,
        lwa_sheet TYPE bapiessrc,  "BAPIESSRC
        lit_bapiret TYPE TABLE OF bapiret2 WITH HEADER LINE,
        lv_nosheet TYPE i, "Numero de hojas
        lv_noitems TYPE i. "Numero de Posiciones a procesar
  RANGES lrg_pos FOR lwa_item-po_item.
  RANGES lrg_lines FOR lwa_det-extrow. "lines range

  CLEAR: pit_hojas, pit_msg, lwa_item, lwa_det, lrg_pos[], lrg_pos,
         lv_pckg, lv_spckg, lwa_serv, lit_bapiesll, lit_bapiesll[],
         lwa_serv2, lit_bapiacc, lit_bapiacc[], lwa_poaccs, lv_flag,
         p_subrc, lv_sheet, lit_bapiret, lv_nosheet, lwa_bapiesll.

  CLEAR: lwa_sheet. "JANE

* Se extraen las posiciones del pedido que se van a trabajar
* jane mod
  DATA: lwa_bill TYPE c LENGTH 14.  "factura
  DATA: lwa_bktxt TYPE bktxt.       "codigo web
  DATA: lwa_xblnr TYPE xblnr.       "Rango de estimaciones


  DATA: lwa_char TYPE c LENGTH 16.
****

  LOOP AT pit_det INTO lwa_det.
    "range ebelp
    lrg_pos-sign    = 'I'.
    lrg_pos-option  = 'EQ'.
    lrg_pos-low     = lwa_det-ebelp.

    "range lines
    lrg_lines-sign    = 'I'.
    lrg_lines-option  = 'EQ'.
    lrg_lines-low     = lwa_det-extrow.

    "other fields
    lwa_bill       = lwa_det-bill.
    lwa_bktxt      = lwa_det-bktxt.
    lwa_xblnr      = lwa_det-xblnr.

    APPEND lrg_pos.
    APPEND lrg_lines.
    CLEAR: lwa_det, lrg_pos, lrg_lines.
  ENDLOOP.
  SORT lrg_pos BY low.
  SORT lrg_lines BY low.



  DELETE ADJACENT DUPLICATES FROM lrg_pos COMPARING low.

* Se inicializa paquete y subpaquete para creacion de Hojas
  lv_pckg = '0000000001'.
  lv_spckg = lv_pckg + 1.

* Se recorren solo las posiciones del Pedido que envio la WEB
  LOOP AT pit_po_item INTO lwa_item WHERE po_item IN lrg_pos.
    lv_flag = 'X'.
    "Se llena cabecera para Hoja para Pedido / Posición
    lwa_bapihdr-pckg_no    = lv_pckg.
    " lwa_bapihdr-short_text = pwa_hdr-bill.
    "lwa_bapihdr-short_text = lwa_bill. JANE MOD
    lwa_bapihdr-po_number  = lwa_item-po_number.
    lwa_bapihdr-po_item    = lwa_item-po_item.
    lwa_bapihdr-doc_date   = sy-datum.
    lwa_bapihdr-post_date  = sy-datum.
    lwa_bapihdr-accasscat  = 'P'. "Validar si queda en codigo duro
    lwa_bapihdr-acceptance = 'X'. "Validar si queda en codigo duro
    lwa_bapihdr-begdate    = sy-datum.
    lwa_bapihdr-enddate    = sy-datum.
    lwa_bapihdr-person_int = sy-uname.
    lwa_bapihdr-person_ext = sy-uname.
    lwa_bapihdr-ref_date   = sy-datum.
    lwa_bapihdr-ref_doc_no = lwa_item-po_number.

*  *JANE MOD
*    lwa_bapihdr-ext_number = lwa_bill.  "LBLNE
*    lwa_bapihdr-location = lwa_bktxt.    "DLORT
*    lwa_bapihdr-short_text = lwa_xblnr.                     "TXZ01_ESSR
*    lwa_bapihdr-ref_doc_no = 'XBLNR-NOREFDOC'.  "XBLNR

*Asignación de Variable de Control: Referencia, Factura
    CONDENSE lwa_bill NO-GAPS.
    CONDENSE lwa_xblnr NO-GAPS.
    CONCATENATE lwa_bill '-' lwa_xblnr INTO lwa_char. "concatena factura estimacion
    lwa_char = lwa_char+0(16). "Trunca cadena a 16 caracteres
    CONDENSE lwa_char NO-GAPS.  "Elimina espacios en blanco

    lwa_bapihdr-short_text = lwa_bktxt.   "TXZ01_ESSR char(40)
    lwa_bapihdr-ext_number = lwa_xblnr.   "LBLNE char(16)
    lwa_bapihdr-location = lwa_bktxt.     "DLORT char(25)
    lwa_bapihdr-ref_doc_no = lwa_char.    "XBLNR  char(16)
*  *END JANE MOD


*   Se recupera paquete de Posicion
    READ TABLE pit_po_serv INTO lwa_serv
    WITH KEY  pckg_no   = lwa_item-pckg_no.


* si recupero paquetes
    IF sy-subrc EQ 0.
*     Contruye linea para el paquete
      CLEAR lwa_bapiesll.
      lwa_bapiesll-pckg_no    = lwa_bapihdr-pckg_no.
      lwa_bapiesll-line_no    = lwa_serv-line_no.
      lwa_bapiesll-ext_line   = lwa_serv-ext_line.
      lwa_bapiesll-outl_level = lwa_serv-outl_level.
      lwa_bapiesll-outl_no    = lwa_serv-outl_no.
      lwa_bapiesll-outl_ind   = lwa_serv-outl_ind.
      lwa_bapiesll-subpckg_no = lv_spckg.
*      APPEND lit_bapiesll.
*      CLEAR: lit_bapiesll.


      "Se recorren servicios de esa posición/paquete
      LOOP AT pit_po_serv INTO lwa_serv2 WHERE pckg_no EQ lwa_serv-subpckg_no AND ext_line IN lrg_lines. "JANE ext_line
        "Se verifica si el servicio de esta POS fue enviado por WEB
        READ TABLE pit_det INTO lwa_det WITH KEY asnum = lwa_serv2-service
                                                 ebeln = lwa_bapihdr-po_number
                                                 ebelp = lwa_bapihdr-po_item.
        IF sy-subrc EQ 0.
          "Agregar linea para el paquete.
          APPEND lwa_bapiesll TO lit_bapiesll.
          "Se llena Posicion de servicio y su porcentaje
          lit_bapiesll-pckg_no    = lv_spckg.
          lit_bapiesll-line_no    = lwa_serv2-line_no.
          lit_bapiesll-ext_line   = lwa_serv2-ext_line.
          lit_bapiesll-subpckg_no = lwa_serv2-subpckg_no.
          lit_bapiesll-service    = lwa_serv2-service.
          lit_bapiesll-base_uom   = lwa_serv2-base_uom.
          lit_bapiesll-gr_price   = lwa_serv2-gr_price.
          lit_bapiesll-price_unit = lwa_serv2-price_unit.
          lit_bapiesll-pln_pckg   = lwa_serv2-pckg_no.
          lit_bapiesll-pln_line   = lwa_serv2-line_no.
          lit_bapiesll-short_text = lwa_serv2-short_text.
          "cantidad:    ERFMG
          "porcentaje:  PRCTG

          "jane mod
          IF p_absgr = 10.
            lit_bapiesll-quantity   = ( lwa_serv2-quantity / 100 ) * lwa_det-prctg.
            lwa_det-erfmg = 0.
          ELSE.
            lit_bapiesll-quantity = lwa_det-erfmg.
            "lwa_det-prctg = lwa_det-erfmg.
            lwa_det-prctg = 0.
          ENDIF.

          APPEND: lit_bapiesll.

          "Se recupera reg. de sub-paquete y linea para completar ITAB de %
          READ TABLE pit_po_accs INTO lwa_poaccs
          WITH KEY pckg_no = lwa_serv2-pckg_no
                   line_no = lwa_serv2-line_no.

          "Completa itab para la bapi
          IF sy-subrc EQ 0.
            lit_bapiacc-pckg_no    = lwa_serv2-pckg_no.
            lit_bapiacc-line_no    = lwa_serv2-line_no.
            lit_bapiacc-serno_line = lwa_poaccs-serno_line.
            lit_bapiacc-percentage = lwa_det-prctg.   "porcentaje
            lit_bapiacc-quantity = lwa_det-erfmg.   "cantidad
            lit_bapiacc-serial_no  = lwa_poaccs-serial_no.
            APPEND lit_bapiacc.
          ENDIF.

          PERFORM f_prepare_before_call_bapi.
          "Llamada de BAPI
          CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
            EXPORTING
              entrysheetheader          = lwa_bapihdr
            IMPORTING
              entrysheet                = lv_sheet
            TABLES
              entrysheetservices        = lit_bapiesll
              entrysheetsrvaccassvalues = lit_bapiacc
              return                    = lit_bapiret.

          "Se valida si se creo la Hoja de entrada
          IF lv_sheet IS NOT INITIAL.
***       Inicio BAF-Pichardo 11.11.2014
            wa_datos-sheet_no = lv_sheet.
            p_hojas = wa_datos-sheet_no.
*            APPEND wa_datos TO it_datos.
***       Fin BAF Pichardo
            ADD 1 TO lv_nosheet.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.

          "Actualiza registros Detalle de POS/Servicios procesados
          PERFORM f_upd_det_table USING lit_bapiret[] lv_sheet
                                        lwa_bapihdr-po_item
                                        lwa_serv2-service
                               CHANGING pit_det pit_hojas pit_msg.

        ENDIF.

        CLEAR: lwa_serv, lit_bapiesll, lwa_serv2, lwa_det, lit_bapiacc,
        lwa_poaccs, lit_bapiesll[], lit_bapiesll,
        lv_sheet, lit_bapiacc[], lit_bapiacc, lit_bapiret[], lit_bapiret.
        CLEAR wa_datos.
      ENDLOOP.
    ENDIF.
*    "Se espera un segundo y libera bloqueos
*    IF lv_nosheet IS NOT INITIAL.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*      WAIT UP TO 1 SECONDS.
*      CALL FUNCTION 'DEQUEUE_ALL'
*        EXPORTING
*          _synchron = 'X'.
*    ENDIF.
*    "Se llama BAPI para crear Hoja de Entrada
*    CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
*      EXPORTING
*        entrysheetheader          = lwa_bapihdr
*      IMPORTING
*        entrysheet                = lv_sheet
*      TABLES
*        entrysheetservices        = lit_bapiesll
*        entrysheetsrvaccassvalues = lit_bapiacc
*        return                    = lit_bapiret.
*    "Se valida si se creo la Hoja de entrada
*    IF lv_sheet IS NOT INITIAL.
*      ADD 1 TO lv_nosheet.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*      "Se suma 1 al paquete y subpaquete para la sig.
*      "Hoja de entrada a crear
*      lv_pckg  = lv_spckg + 1.
*      lv_spckg = lv_pckg + 1.
*    ENDIF.
*    "Actualiza registros Detalle de POS/Servicios procesados
*    PERFORM f_upd_det_table USING lit_bapiret[] lv_sheet lwa_bapihdr-po_item
*                            CHANGING pit_det pit_hojas pit_msg.

    CLEAR: lwa_item, lwa_serv, lwa_bapihdr, lv_sheet, lit_bapiesll, lit_bapiesll[],
           lit_bapiacc, lit_bapiacc[], lit_bapiret, lit_bapiret[], lwa_bapiesll.
  ENDLOOP.
***---> Inicio 27.10.2014
*** Se agrega funcion para proyecto VIMM.
***  IF it_datos IS NOT INITIAL.
***    CALL FUNCTION 'ZFEFM_CRT_ADD_INV_CTRL_PRES' STARTING NEW TASK 'T'
***      EXPORTING
***        i_proveedor = vl_prov
***        i_pedido    = vl_pedido
***        i_factura   = vl_fact2
***      TABLES
***        it_hjsent   = it_datos.
***  ENDIF.
***<---    Fin 27.10.2014
  "Se valida si al menos se proceso 1 pocision.
  IF lv_flag IS INITIAL.
    p_subrc = 9."No se encontro POS indicadas por WEB en este Pedido
    EXIT.
  ENDIF.
*  " Se recupera el numero de posiciones que se procesaron.
*  DESCRIBE TABLE lrg_pos LINES lv_noitems.
  "Se Actualiza registro cabecera Z.
*  PERFORM f_upd_cab_table USING lv_noitems lv_nosheet CHANGING pwa_hdr pit_msg.
  PERFORM f_upd_cab_table USING lv_nosheet CHANGING pwa_hdr pit_msg.

ENDFORM.                    " F_PROCESS_PO
*&---------------------------------------------------------------------*
*&      Form  F_UPD_DET_TABLE
*&---------------------------------------------------------------------*
FORM f_upd_det_table  USING    pt_ret   TYPE tb_bapiret2
                               pv_sheet TYPE bapiessr-sheet_no
                               p_ebelp  TYPE ebelp
                               p_service
                      CHANGING pt_det   TYPE ty_it_det
                               pt_hojas TYPE ty_it_hojas
                               pt_msg   TYPE ty_it_msg.
  DATA: lt_det     TYPE TABLE OF zmmtt_30_hjsrv_d,
        ls_ret     LIKE LINE OF pt_ret,
        lv_msg     TYPE bapi_msg,
        lv_acepted TYPE flag,
        ls_hoja    TYPE zmmwa_0030_exp_hoja,
        ls_msg     TYPE zmmwa_0030_exp_msg,
        lv_flag    TYPE flag.
  FIELD-SYMBOLS <det> LIKE LINE OF pt_det.

  CLEAR: lt_det, ls_ret, lv_msg, lv_acepted, ls_hoja, ls_msg, lv_flag.

  "Se valida si se acepto la Hoja de entarda.
  LOOP AT pt_ret TRANSPORTING NO FIELDS
  WHERE type = 'I' AND id = 'SE' AND number = '103'.
    lv_acepted = 'X'.
    EXIT.
  ENDLOOP.
  "Se recupera el ultimo de mensaje retorno par agrabar en tabla Z
  " y se alimenta IT EXP de mensajes
  LOOP AT pt_ret INTO ls_ret.
    IF lv_acepted = 'X'.
      AT FIRST.
        lv_msg = ls_ret-message.
      ENDAT.
      ls_msg-no_posicion = p_ebelp.
      ls_msg-mensaje     = ls_ret-message.
      ls_msg-subrc       = '00'.
      APPEND ls_msg TO pt_msg.
    ELSE.
      AT FIRST.
        lv_msg = ls_ret-message.
      ENDAT.
      ls_msg-no_posicion = p_ebelp.
      ls_msg-mensaje     = ls_ret-message.
      ls_msg-subrc       = '10'.
      APPEND ls_msg TO pt_msg.
    ENDIF.
    IF ls_ret-type EQ 'E' AND lv_flag IS INITIAL.
      lv_msg = ls_ret-message.
      lv_flag = 'X'.
    ENDIF.
    CLEAR: ls_ret, ls_msg.
  ENDLOOP.
  "Se agrega Hoja a Itab EXP
  IF pv_sheet IS NOT INITIAL.
    ls_hoja-no_posicion  = p_ebelp.
    ls_hoja-id_servicio  = p_service.
    ls_hoja-hoja_entrada = pv_sheet.
    APPEND ls_hoja TO pt_hojas.
  ENDIF.
  "Se actualizan valores en ITAB y se agregan a IT p/ Update Z Table
  LOOP AT pt_det ASSIGNING <det> WHERE ebelp EQ p_ebelp
                                 AND  asnum EQ p_service.
    IF pv_sheet IS INITIAL.
      <det>-stsd = '02'."Error al crear Hoja de entrada
      <det>-msgerr = lv_msg.
      ls_msg-subrc  = '10'.
    ELSEIF lv_acepted = 'X'.
      <det>-stsd = '04'."Hoja de entarda aceptada
    ELSE.
      <det>-stsd = '03'."Hoja de entrada creada
    ENDIF.
    <det>-lblni = pv_sheet."Hoja de entrada.
    APPEND <det> TO lt_det.
  ENDLOOP.

  UPDATE zmmtt_30_hjsrv_d FROM TABLE lt_det.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    "Se le informa a WEB que no se pudo actualizar Regs de Tabla Z DET
    ls_msg-no_posicion = p_ebelp.
    ls_msg-mensaje     = 'Error al actualizar Tabla Z Detalle'.
    ls_msg-subrc       =  '11'.
    APPEND ls_msg TO pt_msg.
  ENDIF.

ENDFORM.                    " F_UPD_DET_TABLE
*&---------------------------------------------------------------------*
*&      Form  F_UPD_CAB_TABLE
*&---------------------------------------------------------------------*
FORM f_upd_cab_table  USING    "p_noitems TYPE i
                               p_nosheet TYPE i
                      CHANGING pwa_hdr   TYPE zmmtt_30_hjsrv_h
                               pt_msg    TYPE ty_it_msg.
  DATA ls_msg LIKE LINE OF pt_msg.
  DATA lv_idrfc TYPE zmmtt_30_hjsrv_d-idrfc.
  CLEAR: ls_msg, lv_idrfc.

*  IF p_nosheet EQ p_noitems.
*    pwa_hdr-stsh = '03'."Totalmente procesado
*  ELSEIF p_nosheet IS NOT INITIAL.
*    pwa_hdr-stsh = '02'. "Parcialmente procesado
*  ENDIF.
  SELECT SINGLE idrfc FROM zmmtt_30_hjsrv_d INTO lv_idrfc
  WHERE idrfc EQ pwa_hdr-idrfc
  AND   ebeln EQ pwa_hdr-ebeln
  AND   stsd LT '03'.
  IF p_nosheet EQ 0.
    pwa_hdr-stsh = '01'. "Error info invalida
  ELSEIF lv_idrfc IS NOT INITIAL.
    pwa_hdr-stsh = '02'. "Parcialmente procesado
  ELSEIF lv_idrfc IS INITIAL.
    pwa_hdr-stsh = '03'."Totalmente procesado
  ENDIF.

  IF pwa_hdr-stsh IS NOT INITIAL.
    UPDATE zmmtt_30_hjsrv_h FROM pwa_hdr.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ls_msg-mensaje     = 'Error al actualizar Tabla Z Cabecera'.
      ls_msg-subrc   = '12'.
      APPEND ls_msg TO pt_msg.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_UPD_CAB_TABLE
*&---------------------------------------------------------------------*
*&      Form  F_UPD_ZTABLES_INVALID_INFO
*&---------------------------------------------------------------------*
FORM f_upd_ztables_invalid_info  CHANGING pwa_hdr TYPE zmmtt_30_hjsrv_h
                                          pit_det TYPE ty_it_det
                                          pit_msg TYPE ty_it_msg.
  DATA: ls_msg LIKE LINE OF pit_msg.
  FIELD-SYMBOLS <det> LIKE LINE OF pit_det.

  CLEAR: ls_msg.

  pwa_hdr-stsh = '01'."Error Info. invalida
  LOOP AT pit_det ASSIGNING <det>.
    <det>-stsd = '01'."Error Info. invalida
  ENDLOOP.

* Actualiza solo estatus de cabecera y detalles
  UPDATE zmmtt_30_hjsrv_h
  SET stsh = '01'
  WHERE idrfc = pwa_hdr-idrfc.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    UPDATE zmmtt_30_hjsrv_d
    SET stsd = '01'
   WHERE idrfc = pwa_hdr-idrfc.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ls_msg-mensaje = 'Error al act. estatus regs. Detalles'.
      ls_msg-subrc   = '14'.
      APPEND ls_msg TO pit_msg.
    ENDIF.
  ELSE.
    ls_msg-mensaje = 'Error al act. estatus regs. Cabecera'.
    ls_msg-subrc   = '15'.
    APPEND ls_msg TO pit_msg.
  ENDIF.

ENDFORM.                    " F_UPD_ZTABLES_INVALID_INFO
*&---------------------------------------------------------------------*
*&      Form  F_VALID_POS_SERVICE_PO
*&---------------------------------------------------------------------*
FORM f_valid_pos_service_po  USING    pit_poitem TYPE ty_it_po_item
                                      pit_poserv TYPE ty_it_po_serv
                                      pit_det    TYPE ty_it_det
                             CHANGING p_subrc    TYPE subrc.
  DATA: ls_det  LIKE LINE OF pit_det,
        lt_det  TYPE ty_it_det,
        ls_item LIKE LINE OF pit_poitem,
        ls_serv LIKE LINE OF pit_poserv.

  CLEAR: ls_det, lt_det, ls_item, ls_serv, p_subrc..

  LOOP AT pit_det INTO ls_det.
    "Valida que pos en PO exista
    READ TABLE pit_poitem INTO ls_item
    WITH KEY po_item = ls_det-ebelp.
    IF sy-subrc EQ 0.
      "Recupera subpaquete que contiene servicios
      READ TABLE pit_poserv INTO ls_serv
      WITH KEY pckg_no = ls_item-pckg_no.
      "Se valida que sevicio esta en subpaquete
      READ TABLE pit_poserv TRANSPORTING NO FIELDS
      WITH KEY pckg_no = ls_serv-subpckg_no
               service = ls_det-asnum.

      IF sy-subrc NE 0.
        ls_det-msgerr = 'Servicio en Posicion no existe'.
        APPEND ls_det TO lt_det.
      ENDIF.
    ELSE.
      ls_det-msgerr = 'Posicion no existe'.
      APPEND ls_det TO lt_det.
    ENDIF.

    CLEAR: ls_det, ls_item, ls_serv.
  ENDLOOP.

  "Si se detectaron errores se actualiza mensajes en detalles Tabla Z
  "y se regresa un subrc 5
  IF lt_det IS NOT INITIAL.
    p_subrc = 8.
    UPDATE zmmtt_30_hjsrv_d FROM TABLE lt_det.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_VALID_POS_SERVICE_PO
*&---------------------------------------------------------------------*
*&      Form  F_VALID_LIFNR
*&---------------------------------------------------------------------*
FORM f_valid_lifnr  USING    p_hdr_vendor       TYPE lifnr
                             p_cab_no_proveedor TYPE lifnr
                             pwa_hdr            TYPE zmmtt_30_hjsrv_h
                    CHANGING p_subrc            TYPE subrc.

  IF p_hdr_vendor NE p_cab_no_proveedor.
    p_subrc = 7.
    UPDATE zmmtt_30_hjsrv_d
    SET msgerr = 'Proveedor enviado X WEB diferente al del Pedido'
    WHERE idrfc = pwa_hdr-idrfc.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_VALID_LIFNR
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_BEFORE_CALL_BAPI
*&---------------------------------------------------------------------*
*  Acciones a realizar antes de llamar BAPI de creación Hoja de Entrada
*  y evitar que marque que el pedido ya esta bloqueado.
*----------------------------------------------------------------------*
FORM f_prepare_before_call_bapi .
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WAIT UP TO 1 SECONDS.

  CALL FUNCTION 'DEQUEUE_ALL'
    EXPORTING
      _synchron = 'X'.
ENDFORM.                    "f_prepare_before_call_bapi" F_PREPARE_BEFORE_CALL_BAPI
*&---------------------------------------------------------------------*
*&      Form  f_get_adinfo_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EBELN    text
*      -->P_ABSGR    text
*      -->P_WAERS    text
*----------------------------------------------------------------------*
FORM f_get_adinfo_po  USING     p_ebeln      TYPE ebeln
                      CHANGING  p_absgr      TYPE absgr
                                p_waers      TYPE waers.
  DATA: lwa_ekko TYPE ekko.
*** ---> Begin of  RPM 21.20.2014
***  SELECT * FROM ekko
***     INTO lwa_ekko
***     WHERE ebeln = p_ebeln.
***  ENDSELECT.
*** Se elimina select end select.
  SELECT SINGLE * FROM ekko
     INTO lwa_ekko
     WHERE ebeln = p_ebeln.
*** <--- End  of  RPM 21.20.2014

  p_absgr = lwa_ekko-absgr.
  p_waers = lwa_ekko-waers.

ENDFORM.                    "f_get_adinfo_po
*&---------------------------------------------------------------------*
*&      Form  f_valid_contract
*&---------------------------------------------------------------------*
*       Valida tipo de contrato  <> 0
*----------------------------------------------------------------------*
*      -->P_ABSGR    text
*      -->PWA_HDR    text
*      -->P_SUBRC    text
*----------------------------------------------------------------------*
FORM f_valid_contract   USING     p_absgr      TYPE absgr
                                  pwa_hdr      TYPE zmmtt_30_hjsrv_h
                        CHANGING  p_subrc      TYPE subrc.

  DATA: ls_t165r  TYPE t165r.
*** ---> Begin of RPM 21.10.2014
*** Se comenta select end select
***  SELECT * FROM t165r INTO ls_t165r
***    WHERE  absgr = p_absgr.
***  ENDSELECT.
  SELECT SINGLE * FROM t165r INTO ls_t165r
  WHERE  absgr = p_absgr.
*** <--- Enf of RPM 21.10.2014
  IF ls_t165r-absgr <> 10 AND ls_t165r-absgr <> 20.
    p_subrc = 5.
    UPDATE zmmtt_30_hjsrv_d
    SET msgerr = 'Tipo de contrato erroneo.'
    WHERE idrfc = pwa_hdr-idrfc.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_valid_contract
*&---------------------------------------------------------------------*
*&      Form  f_return_msg
*&---------------------------------------------------------------------*
*       Funcion para retornar mensajes a web por medio de tabla_export
*----------------------------------------------------------------------*
*      -->P_POS      text
*      -->P_MSG      text
*      -->PIT_MSG    text
*----------------------------------------------------------------------*
FORM f_return_msg USING       p_pos     TYPE  ebelp
                              p_msg     TYPE  bapi_msg
                              v_jsubrc  TYPE  subrc
                  CHANGING    pit_msg   TYPE  ty_it_msg.

  DATA: ls_msg  LIKE LINE OF pit_msg.
  ls_msg-no_posicion = p_pos.
  ls_msg-mensaje = p_msg.
  ls_msg-subrc = v_jsubrc.
  APPEND ls_msg TO pit_msg.

ENDFORM.                    "f_return_msg

*--->>> RSDK908058 --->>>
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_PO_2
*&---------------------------------------------------------------------*
*& Es una copia de la subrutina F_PROCESS_PO, pero se hacen ajustes
*& para el manejo de LUW, de tal forma que o se generan todas las
*& hojas de entrada o no se genera ninguna.
*&---------------------------------------------------------------------*
FORM f_process_po_2 USING pwa_po_hdr   TYPE bapiekkol
                          pit_po_item  TYPE ty_it_po_item
                          pit_po_serv  TYPE ty_it_po_serv
                          pit_po_accs  TYPE ty_it_po_accs
                          p_absgr      TYPE absgr
                          p_moneda     TYPE waers
                          p_f_fact     TYPE datum
                          p_f_auto     TYPE datum
*                         p_testrun    TYPE testrun     "Se comenta por cambio
                 CHANGING pwa_hdr      TYPE zmmtt_30_hjsrv_h
                          pit_det      TYPE ty_it_det
                          pit_hojas    TYPE ty_it_hojas
                          pit_msg      TYPE ty_it_msg
                          p_hojas      TYPE zmmde_lblni
                          p_subrc      TYPE subrc.

  DATA:
        lwa_item     LIKE LINE OF pit_po_item,
        lwa_det      LIKE LINE OF pit_det,
        lv_pckg      LIKE lwa_item-pckg_no,
        lv_spckg     LIKE lwa_item-pckg_no,
        lwa_bapihdr  TYPE bapiessrc,
        lwa_test     TYPE TESTRUN,
        lwa_serv     LIKE LINE OF pit_po_serv,
        lwa_serv2    LIKE LINE OF pit_po_serv,
        lit_bapiesll TYPE TABLE OF bapiesllc WITH HEADER LINE,
        lwa_bapiesll TYPE bapiesllc,
        lit_bapiacc  TYPE TABLE OF bapiesklc WITH HEADER LINE,
        lwa_poaccs   LIKE LINE OF pit_po_accs,
        lv_flag      TYPE flag,
        lv_sheet     TYPE bapiessr-sheet_no,
        lwa_sheet    TYPE bapiessrc,  "BAPIESSRC
        lit_bapiret  TYPE TABLE OF bapiret2 WITH HEADER LINE,
        lv_nosheet   TYPE i, "Numero de hojas
        lv_noitems   TYPE i, "Numero de Posiciones a procesar
        lwa_bill     TYPE c LENGTH 14,  "factura
        lwa_bktxt    TYPE bktxt,       "codigo web
        lwa_xblnr    TYPE xblnr,       "Rango de estimaciones
        lwa_char     TYPE c LENGTH 16,
        lwa_msg      LIKE LINE OF pit_msg,
        "Itab p/ Lineas externas por Subpakete (Pos en Pedido de Serv)
        BEGIN OF lit_lines OCCURS 0,
          pckg_no  LIKE lwa_item-pckg_no,
          ext_line LIKE lwa_det-extrow,
        END OF lit_lines.

  RANGES:
          lrg_pos FOR lwa_item-po_item.

* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
  DATA: lv_fecautusd TYPE c,  "Bandera Fecha Autorizacion USD
        lv_fecautmxn TYPE c.  "Bandera Fecha Autorizacion MXN
* <- END OF INSERT MCA-20170714 OT: RSDK909520
  DATA: lv_fecauteur TYPE c.  "Bandera FechaAutorizacón EUR   "INSERT MCA-20180822 OT: RSDK910293

  CLEAR:
         p_subrc,
         lwa_item, lwa_det, lv_pckg, lv_spckg, lwa_bapihdr, lwa_serv,
         lwa_serv2, lit_bapiesll[], lit_bapiesll, lwa_bapiesll,
         lit_bapiacc[], lit_bapiacc, lwa_poaccs, lv_flag, lwa_sheet,
         lit_bapiret[], lit_bapiret, lv_nosheet, lv_noitems, lwa_bill,
         lwa_bktxt, lwa_xblnr, lwa_char, lwa_msg, lrg_pos[], lrg_pos,
         lit_lines[], lit_lines, lwa_test.

* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
* Obtener Bandera para aplicar fecha de autorizacion para USD
  CLEAR: lv_fecautusd.
  SELECT SINGLE zflag
    INTO lv_fecautusd
    FROM zutil_parameters
   WHERE zreport EQ 'ZMMFM_0030_CRT_HOJA_SERV'
     AND zfield  EQ 'FECAUT_USD'.

* Obtener Bandera para aplicar fecha de autorizacion para MXN
  CLEAR: lv_fecautmxn.
  SELECT SINGLE zflag
    INTO lv_fecautmxn
    FROM zutil_parameters
   WHERE zreport EQ 'ZMMFM_0030_CRT_HOJA_SERV'
     AND zfield  EQ 'FECAUT_MXN'.  "CHANGE MCA-20170721 OT: RSDK909533
* <- END OF INSERT MCA-20170714 OT: RSDK909520
* -> BEGIN OF INSERT MCA-20180822 OT: RSDK910293
* Obtener Bandera para aplicar fecha de autorizacion para EUR
  CLEAR: lv_fecauteur.
  SELECT SINGLE zflag
    INTO lv_fecauteur
    FROM zutil_parameters
   WHERE zreport EQ 'ZMMFM_0030_CRT_HOJA_SERV'
     AND zfield  EQ 'FECAUT_EUR'.
* <- END OF "INSERT MCA-20180822 OT: RSDK910293
  lwa_msg-mensaje = text-m03.
  lwa_msg-subrc   = '00'.
  APPEND lwa_msg TO pit_msg.

  LOOP AT pit_det INTO lwa_det.
    "range ebelp
    lrg_pos-sign    = 'I'.
    lrg_pos-option  = 'EQ'.
    lrg_pos-low     = lwa_det-ebelp.
    APPEND lrg_pos.

    "lines
    READ TABLE pit_po_item INTO lwa_item
    WITH KEY po_item = lwa_det-ebelp.
    READ TABLE pit_po_serv INTO lwa_serv
    WITH KEY  pckg_no = lwa_item-pckg_no.
    READ TABLE pit_po_serv INTO lwa_serv2
    WITH KEY pckg_no = lwa_serv-subpckg_no
             service = lwa_det-asnum."Servicio

    lit_lines-pckg_no  = lwa_serv2-pckg_no.
    lit_lines-ext_line = lwa_serv2-ext_line.
    APPEND lit_lines.

    lwa_bill       = lwa_det-bill.
    lwa_bktxt      = lwa_det-bktxt.
    lwa_xblnr      = lwa_det-xblnr.

    CLEAR: lwa_det, lrg_pos, lwa_item, lwa_serv, lwa_serv2, lit_lines.
  ENDLOOP.
  SORT lrg_pos BY low.
  DELETE ADJACENT DUPLICATES FROM lrg_pos COMPARING low.

* Se inicializa paquete y subpaquete para creacion de Hojas
  lv_pckg = '0000000001'.
  lv_spckg = lv_pckg + 1.

* Se recorren solo las posiciones del Pedido que envio la WEB
  LOOP AT pit_po_item INTO lwa_item WHERE po_item IN lrg_pos.
    "Se llena cabecera para Hoja para Pedido / Posición
    lwa_bapihdr-pckg_no    = lv_pckg.
    lwa_bapihdr-po_number  = lwa_item-po_number.
    lwa_bapihdr-po_item    = lwa_item-po_item.
***  Inicio RPM 02.2017
    IF p_moneda = 'USD'.
      lwa_bapihdr-doc_date  = p_f_fact.  " Fecha del documento
      lwa_bapihdr-post_date = p_f_fact.  " Fecha contabilización
* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
      IF lv_fecautusd EQ 'X'.  "Usar Fecha autorizacion USD
        lwa_bapihdr-post_date = p_f_auto.  " Fecha contabilización = Fecha autorizacion
      ENDIF.
* <- END OF INSERT MCA-20170714 OT: RSDK909520
    ELSEIF p_moneda = 'MXN'.
      lwa_bapihdr-doc_date  =  p_f_fact.
      lwa_bapihdr-post_date =  p_f_auto.
* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
      IF lv_fecautmxn IS INITIAL.  "NO Usar Fecha Autorizacion MXN
        lwa_bapihdr-post_date = p_f_fact.  " Fecha contabilización = Fecha Factura
      ENDIF.
* <- END OF INSERT MCA-20170714 OT: RSDK909520
    ENDIF.
* -> BEGIN OF INSERT MCA-20180822 OT: RSDK910293
    IF p_moneda = 'EUR'.
      lwa_bapihdr-doc_date  = p_f_fact.  " Fecha del documento
      lwa_bapihdr-post_date = p_f_fact.  " Fecha contabilización
* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
      IF lv_fecauteur EQ 'X'.  "Usar Fecha autorizacion EUR
        lwa_bapihdr-post_date = p_f_auto.  " Fecha contabilización = Fecha autorizacion
      ENDIF.
    ENDIF.
* <- END OF "INSERT MCA-20180822 OT: RSDK910293

***
***    lwa_bapihdr-doc_date   = sy-datum.
***    lwa_bapihdr-post_date  = sy-datum.
***
***  FIn    RPM 02.2017

    lwa_bapihdr-accasscat  = 'P'. "Validar si queda en codigo duro
    lwa_bapihdr-acceptance = 'X'. "Validar si queda en codigo duro
    lwa_bapihdr-begdate    = sy-datum.
    lwa_bapihdr-enddate    = sy-datum.
    lwa_bapihdr-person_int = sy-uname.
    lwa_bapihdr-person_ext = sy-uname.
    lwa_bapihdr-ref_date   = sy-datum.
    lwa_bapihdr-ref_doc_no = lwa_item-po_number.

*Asignación de Variable de Control: Referencia, Factura
    CONDENSE lwa_bill NO-GAPS.
    CONDENSE lwa_xblnr NO-GAPS.
    CONCATENATE lwa_bill '-' lwa_xblnr INTO lwa_char. "concatena factura estimacion
    lwa_char = lwa_char+0(16). "Trunca cadena a 16 caracteres
    CONDENSE lwa_char NO-GAPS.  "Elimina espacios en blanco

    lwa_bapihdr-short_text = lwa_bktxt.   "TXZ01_ESSR char(40)
    lwa_bapihdr-ext_number = lwa_xblnr.   "LBLNE char(16)
    lwa_bapihdr-location = lwa_bktxt.     "DLORT char(25)
    lwa_bapihdr-ref_doc_no = lwa_char.    "XBLNR  char(16)

*   Se recupera paquete de Posicion
    READ TABLE pit_po_serv INTO lwa_serv
    WITH KEY  pckg_no   = lwa_item-pckg_no.

* si recupero paquetes
    IF sy-subrc EQ 0.
*     Contruye linea para el paquete
      CLEAR lwa_bapiesll.
      lwa_bapiesll-pckg_no    = lwa_bapihdr-pckg_no.
      lwa_bapiesll-line_no    = lwa_serv-line_no.
      lwa_bapiesll-ext_line   = lwa_serv-ext_line.
      lwa_bapiesll-outl_level = lwa_serv-outl_level.
      lwa_bapiesll-outl_no    = lwa_serv-outl_no.
      lwa_bapiesll-outl_ind   = lwa_serv-outl_ind.
      lwa_bapiesll-subpckg_no = lv_spckg.


      "Se recorren servicios de esa posición/paquete
      LOOP AT pit_po_serv INTO lwa_serv2 WHERE pckg_no EQ lwa_serv-subpckg_no.
        "Se verifica si el servicio de esta POS fue enviado por WEB
        READ TABLE lit_lines WITH KEY pckg_no  = lwa_serv2-pckg_no
                                      ext_line = lwa_serv2-ext_line.
        IF sy-subrc NE 0.
          CLEAR: lwa_serv2, lit_lines.
          CONTINUE.
        ENDIF.

        READ TABLE pit_det INTO lwa_det WITH KEY asnum = lwa_serv2-service
                                                 ebeln = lwa_bapihdr-po_number
                                                 ebelp = lwa_bapihdr-po_item.
        IF sy-subrc NE 0.
          CLEAR: lwa_serv2, lit_lines, lwa_det.
          CONTINUE.
        ELSE. "Se ejecuta BAPI
          lv_flag = 'X'.
          "Agregar linea para el paquete.
          APPEND lwa_bapiesll TO lit_bapiesll.
          "Se llena Posicion de servicio y su porcentaje
          lit_bapiesll-pckg_no    = lv_spckg.
          lit_bapiesll-line_no    = lwa_serv2-line_no.
          lit_bapiesll-ext_line   = lwa_serv2-ext_line.
          lit_bapiesll-subpckg_no = lwa_serv2-subpckg_no.
          lit_bapiesll-service    = lwa_serv2-service.
          lit_bapiesll-base_uom   = lwa_serv2-base_uom.
          lit_bapiesll-gr_price   = lwa_serv2-gr_price.
          lit_bapiesll-price_unit = lwa_serv2-price_unit.
          lit_bapiesll-pln_pckg   = lwa_serv2-pckg_no.
          lit_bapiesll-pln_line   = lwa_serv2-line_no.
          lit_bapiesll-short_text = lwa_serv2-short_text.

          IF p_absgr = 10.
            lit_bapiesll-quantity   = ( lwa_serv2-quantity / 100 ) * lwa_det-prctg.
            lwa_det-erfmg = 0.
          ELSE.
            lit_bapiesll-quantity = lwa_det-erfmg.
            lwa_det-prctg = 0.
          ENDIF.

          APPEND: lit_bapiesll.

          "Se recupera reg. de sub-paquete y linea para completar ITAB de %
          READ TABLE pit_po_accs INTO lwa_poaccs
          WITH KEY pckg_no = lwa_serv2-pckg_no
                   line_no = lwa_serv2-line_no.

          "Completa itab para la bapi
          IF sy-subrc EQ 0.
            lit_bapiacc-pckg_no    = lwa_serv2-pckg_no.
            lit_bapiacc-line_no    = lwa_serv2-line_no.
            lit_bapiacc-serno_line = lwa_poaccs-serno_line.
            lit_bapiacc-percentage = lwa_det-prctg.   "porcentaje
            lit_bapiacc-quantity = lwa_det-erfmg.   "cantidad
            lit_bapiacc-serial_no  = lwa_poaccs-serial_no.
            APPEND lit_bapiacc.
          ENDIF.

          PERFORM f_vald_lock
                  TABLES   pit_det[] pit_msg[]
                  USING    lwa_item-po_number pwa_po_hdr-doc_cat lwa_det
                  CHANGING p_subrc.
          " Interrumpe Creacion de Hojas de entrada por Error Detectado
*         IF p_subrc NE 0 AND p_testrun EQ ' '.    " INSERT ECN-20180903 OT: RSDK910309
          IF p_subrc NE 0.
            EXIT.
          ENDIF.

         " Se ejecuta BAPI
         CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
           EXPORTING
             entrysheetheader          = lwa_bapihdr
*             testrun                   = lwa_test     " INSERT ECN-20180903 OT: RSDK910309 Se comenta ya que el parametro debe ir directamente en la primer ejecucion.
           IMPORTING
             entrysheet                = lv_sheet
           TABLES
             entrysheetservices        = lit_bapiesll
             entrysheetsrvaccassvalues = lit_bapiacc
             return                    = lit_bapiret.

          "Actualiza registros Detalle de POS/Servicios procesados
          PERFORM f_upd_det_itabs USING lit_bapiret[] lv_sheet
                                        lwa_bapihdr-po_item
                                        lwa_serv2-service
                                        lwa_det
                               CHANGING pit_det pit_hojas pit_msg
                                        p_hojas lv_nosheet p_subrc.

        ENDIF.

        CLEAR: lwa_serv, lit_bapiesll, lwa_serv2, lwa_det, lit_bapiacc,
               lwa_poaccs, lit_bapiesll[], lit_bapiesll, lv_sheet,
               lit_bapiacc[], lit_bapiacc, lit_bapiret[], lit_bapiret,
               wa_datos, lit_lines, lwa_test.

        "Interrumpe Creacion de Hojas de entrada por Error Detectado
*       IF p_subrc NE 0 AND  p_testrun EQ ' '.    " INSERT ECN-20180903 OT: RSDK910309
         IF p_subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "INSERT MCA-20151021 OT: RSDK908688
          DELETE pit_msg                     "INSERT MCA-20151021 OT: RSDK908688
           WHERE subrc EQ '00'.              "INSERT MCA-20151021 OT: RSDK908688
          EXIT.
       ENDIF.

      ENDLOOP.


    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "INSERT MCA-20151021 OT: RSDK908688
      DELETE pit_msg                       "INSERT MCA-20151021 OT: RSDK908688
       WHERE subrc EQ '00'.                "INSERT MCA-20151021 OT: RSDK908688
      CLEAR: lv_flag.
          EXIT.
    ENDIF.

    CLEAR: lwa_item, lwa_serv, lwa_bapihdr, lv_sheet, lit_bapiesll, lit_bapiesll[],
           lit_bapiacc, lit_bapiacc[], lit_bapiret, lit_bapiret[], lwa_bapiesll.

    "Interrumpe Creacion de Hojas de entrada por Error Detectado
*   IF p_subrc NE 0 AND p_testrun EQ ' '.           "Error al crear Hoja de entrada
    IF p_subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "INSERT MCA-20151021 OT: RSDK908688
      DELETE pit_msg                       "INSERT MCA-20151021 OT: RSDK908688
       WHERE subrc EQ '00'.                "INSERT MCA-20151021 OT: RSDK908688
          EXIT.
    ELSEIF lv_flag IS INITIAL."Se detecto una Pos-Serv invalido
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "INSERT MCA-20151021 OT: RSDK908688
      DELETE pit_msg                       "INSERT MCA-20151021 OT: RSDK908688
       WHERE subrc EQ '00'.                "INSERT MCA-20151021 OT: RSDK908688
          EXIT.
    ENDIF.

  ENDLOOP.

  "Se valida que se puedan procesar todas las posiciones y servicios
  IF lv_flag IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "INSERT MCA-20151021 OT: RSDK908688
    DELETE pit_msg                       "INSERT MCA-20151021 OT: RSDK908688
     WHERE subrc EQ '00'.                "INSERT MCA-20151021 OT: RSDK908688
    p_subrc = 9."No se encontro POS indicadas por WEB en este Pedido
          EXIT.
  ENDIF.

  "Se Actualiza registros en tablas Z
  PERFORM f_upd_z_tables TABLES   pit_det pit_msg
                         USING    lv_nosheet
                         CHANGING pwa_hdr pit_hojas p_hojas p_subrc.

ENDFORM.                    " F_PROCESS_PO_2
*&---------------------------------------------------------------------*
*&      Form  F_VALD_LOCK
*&---------------------------------------------------------------------*
*    Valida bloqueo de pedido, para evitar esperar 1 segundo por
*    intento, se promedio que un enqueue fallido tarda
*    200 microsegundos, por lo que se pone 50,000 intentos que
*    equivalen a estar intentando bloquear el pedido por 10 segundos.
*    Esto se hace para asegurar creación de la hoja de entrada
*----------------------------------------------------------------------*
FORM f_vald_lock  TABLES   pit_det     STRUCTURE zmmtt_30_hjsrv_d
                           pit_msg     STRUCTURE zmmwa_0030_exp_msg
                  USING    p_po_number TYPE ebeln
                           p_bstyp     TYPE ebstyp
                           pwa_det     TYPE zmmtt_30_hjsrv_d
                  CHANGING p_subrc     TYPE subrc.
  DATA:
        lv_times TYPE i,
        lv_lock  TYPE flag.
  FIELD-SYMBOLS:
                 <det> TYPE zmmtt_30_hjsrv_d.

  CLEAR: lv_times, lv_lock, p_subrc.
  UNASSIGN <det>.

  lv_times = '50000'(v01)."Intentos para bloquear el pedido

  "Intenta bloquear el pedido
  DO lv_times TIMES.
    CALL FUNCTION 'MM_ENQUEUE_DOCUMENT'
      EXPORTING
        i_ebeln         = p_po_number
        i_bstyp         = p_bstyp
      EXCEPTIONS
        document_locked = 1
        system_failure  = 2
        parameter_error = 3
        OTHERS          = 4.
    IF sy-subrc EQ 0.
      lv_lock = abap_true.
      EXIT.
    ENDIF.
  ENDDO.

  "Si se bloqueo el pedido
  IF lv_lock EQ abap_true.
    CALL FUNCTION 'MM_DEQUEUE_DOCUMENT'
      EXPORTING
        i_ebeln         = p_po_number
        i_bstyp         = p_bstyp
        i_wait          = abap_true
      EXCEPTIONS
        system_failure  = 1
        parameter_error = 2
        OTHERS          = 3.
    IF sy-subrc NE 0.
      CLEAR lv_lock.
    ENDIF.
  ENDIF.

  "No se pudo bloquear el pedido
  IF lv_lock EQ space.
    p_subrc = 20.

    pit_msg-no_posicion = pwa_det-ebelp.
    CONCATENATE pwa_det-asnum  '-' text-m01 INTO pit_msg-mensaje.
    pit_msg-subrc       = '20'.
    APPEND pit_msg. CLEAR pit_msg.

    READ TABLE pit_det ASSIGNING <det>
    WITH KEY idrfc  = pwa_det-idrfc
             ebeln  = pwa_det-ebeln
             ebelp  = pwa_det-ebelp
             extrow = pwa_det-extrow.
    IF sy-subrc EQ 0.
      <det>-stsd    = '02'."Error al crear Hoja de entrada
      <det>-msgerr  = text-m01.
      CLEAR: <det>-lblni.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_VALD_LOCK
*&---------------------------------------------------------------------*
*&      Form  F_UPD_DET_ITABS
*&---------------------------------------------------------------------*
FORM f_upd_det_itabs  USING    pt_ret    TYPE tb_bapiret2
                               pv_sheet  TYPE bapiessr-sheet_no
                               p_ebelp   TYPE ebelp
                               p_service TYPE asnum
                               pwa_det   TYPE zmmtt_30_hjsrv_d
                      CHANGING pt_det    TYPE ty_it_det
                               pt_hojas  TYPE ty_it_hojas
                               pt_msg    TYPE ty_it_msg
                               p_hojas   TYPE zmmde_lblni
                               p_nosheet TYPE i
                               p_subrc   TYPE subrc.
  DATA:
        ls_ret     LIKE LINE OF pt_ret,
        lv_msg     TYPE bapi_msg,
        lv_acepted TYPE flag,
        ls_hoja    TYPE zmmwa_0030_exp_hoja,
        ls_msg     TYPE zmmwa_0030_exp_msg,
        lv_flag    TYPE flag,
        lwa_det  TYPE zmmtt_30_hjsrv_d."Para Borrar Hojas en Error
  FIELD-SYMBOLS <det> LIKE LINE OF pt_det.

  CLEAR: ls_ret, lv_msg, lv_acepted, ls_hoja, ls_msg, lv_flag, lwa_det.
  UNASSIGN <det>.

  "Se valida si se acepto la Hoja de entarda.
  READ TABLE pt_ret TRANSPORTING NO FIELDS
  WITH KEY type   = 'I'
           id     = 'SE'
           number = '103'.
  IF sy-subrc EQ 0.
    lv_acepted = 'X'.
  ELSEIF sy-subrc NE 0.
  p_subrc = 20.

*Se valida si acepto la hoja de entrada en modo TEST.
*   READ TABLE pt_ret TRANSPORTING NO FIELDS
*   WITH KEY type   = 'S'
*            id     = 'SE'
*            number = '534'.
*   IF sy-subrc EQ 0.
*     lv_acepted = 'X'.
*   ELSEIF sy-subrc NE 0.
*     p_subrc = 20.
*   ENDIF.

  ENDIF.

  "Se recupera el ultimo de mensaje retorno par agrabar en tabla Z
  " y se alimenta IT EXP de mensajes
  LOOP AT pt_ret INTO ls_ret.
    IF lv_acepted = 'X'.
      AT FIRST.
        lv_msg = ls_ret-message.
      ENDAT.
      ls_msg-no_posicion = p_ebelp.
      CONCATENATE pwa_det-asnum  '-' ls_ret-message INTO ls_msg-mensaje.
      ls_msg-subrc       = '00'.
      APPEND ls_msg TO pt_msg.
    ELSE.
      AT FIRST.
        lv_msg = ls_ret-message.
      ENDAT.
      ls_msg-no_posicion = p_ebelp.
      CONCATENATE pwa_det-asnum  '-' ls_ret-message INTO ls_msg-mensaje.
      ls_msg-subrc       = '20'.
      APPEND ls_msg TO pt_msg.
    ENDIF.
    "En caso de Error se suple el Msg por el primero con tipo Error.
    IF ls_ret-type EQ 'E' AND lv_flag IS INITIAL.
      lv_msg = ls_ret-message.
      lv_flag = 'X'.
    ENDIF.
    CLEAR: ls_ret, ls_msg.
  ENDLOOP.
  "Se agrega Hoja a Itab EXP
  IF pv_sheet IS NOT INITIAL.
    p_hojas = pv_sheet.
    ADD 1 TO p_nosheet.
    ls_hoja-no_posicion  = p_ebelp.
    ls_hoja-id_servicio  = p_service.
    ls_hoja-hoja_entrada = pv_sheet.
    APPEND ls_hoja TO pt_hojas.
  ENDIF.
  "Se actualizan valores en ITAB DET
  READ TABLE pt_det ASSIGNING <det>
   WITH KEY idrfc  = pwa_det-idrfc
            ebeln  = pwa_det-ebeln
            ebelp  = pwa_det-ebelp
            extrow = pwa_det-extrow.
  CHECK sy-subrc EQ 0.
  IF pv_sheet IS INITIAL.
    <det>-stsd = '02'."Error al crear Hoja de entrada
    <det>-msgerr = lv_msg.
  ELSEIF pv_sheet IS NOT INITIAL AND lv_acepted NE 'X'.
    <det>-stsd = '03'."Hoja de entrada creada
    <det>-msgerr = lv_msg.
    <det>-lblni = pv_sheet."Hoja de entrada.
  ELSEIF lv_acepted EQ 'X'.
    <det>-stsd  = '04'."Hoja de entarda aceptada
    <det>-lblni = pv_sheet."Hoja de entrada.
  ENDIF.

ENDFORM.                    " F_UPD_DET_ITABS
*&---------------------------------------------------------------------*
*&      Form  f_upd_z_tables
*&---------------------------------------------------------------------*
*       Se Actualiza registros detalle en tabla Z
*----------------------------------------------------------------------*
FORM f_upd_z_tables TABLES   pit_det   STRUCTURE zmmtt_30_hjsrv_d
                             pit_msg   STRUCTURE zmmwa_0030_exp_msg
                    USING    p_nosheet TYPE i
                    CHANGING pwa_hdr   TYPE zmmtt_30_hjsrv_h
                             pit_hojas TYPE ty_it_hojas
                             p_hojas   TYPE zmmde_lblni
                             p_subrc   TYPE subrc.
  DATA: lwa_det TYPE zmmtt_30_hjsrv_d,
        ls_msg  TYPE zmmwa_0030_exp_msg.
  CLEAR: lwa_det, ls_msg.

  LOOP AT pit_det INTO lwa_det WHERE stsd LT '04'."Hoja Aceptada
    EXIT.
  ENDLOOP.

  IF p_nosheet EQ 0.
    pwa_hdr-stsh = '01'. "Error info invalida
  ELSEIF lwa_det IS NOT INITIAL.
    pwa_hdr-stsh = '02'. "Parcialmente procesado
  ELSEIF lwa_det IS INITIAL.
    pwa_hdr-stsh = '03'."Totalmente procesado
  ENDIF.

  UPDATE zmmtt_30_hjsrv_d FROM TABLE pit_det.
  IF sy-subrc EQ 0.
    UPDATE zmmtt_30_hjsrv_h FROM pwa_hdr.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF sy-subrc NE 0.
    p_subrc = 20.
    ls_msg-mensaje = text-m02.
    ls_msg-subrc   = '12'.
    APPEND ls_msg TO pit_msg.
  ENDIF.

ENDFORM.                    " f_upd_z_tables
*<<<--- RSDK908058 <<<---

*--->>> RSDK910309 --->>>
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_PO_1
*&---------------------------------------------------------------------*
*& Es una copia de la subrutina F_PROCESS_PO_02  se hacen ajustes
*& para el manejo del modo Test en la generacion de hojas de entrada
*&
*& 21-09-2018 - Se comenta llamado a rutina por peticion del usuario
*& derivado del doble de tiempo que tardaria en procesar una autorizacion
*&---------------------------------------------------------------------*
FORM f_process_po_1 USING pwa_po_hdr   TYPE bapiekkol
                          pit_po_item  TYPE ty_it_po_item
                          pit_po_serv  TYPE ty_it_po_serv
                          pit_po_accs  TYPE ty_it_po_accs
                          p_absgr      TYPE absgr
                          p_moneda     TYPE waers
                          p_f_fact     TYPE datum
                          p_f_auto     TYPE datum
                 CHANGING pwa_hdr      TYPE zmmtt_30_hjsrv_h
                          pit_det      TYPE ty_it_det
                          pit_hojas    TYPE ty_it_hojas
                          pit_msg      TYPE ty_it_msg
                          p_hojas      TYPE zmmde_lblni
                          p_subrc      TYPE subrc.

  DATA:
        lwa_item     LIKE LINE OF pit_po_item,
        lwa_det      LIKE LINE OF pit_det,
        lv_pckg      LIKE lwa_item-pckg_no,
        lv_spckg     LIKE lwa_item-pckg_no,
        lwa_bapihdr  TYPE bapiessrc,
        lwa_test     TYPE TESTRUN,
        lwa_serv     LIKE LINE OF pit_po_serv,
        lwa_serv2    LIKE LINE OF pit_po_serv,
        lit_bapiesll TYPE TABLE OF bapiesllc WITH HEADER LINE,
        lwa_bapiesll TYPE bapiesllc,
        lit_bapiacc  TYPE TABLE OF bapiesklc WITH HEADER LINE,
        lwa_poaccs   LIKE LINE OF pit_po_accs,
        lv_flag      TYPE flag,
        lv_sheet     TYPE bapiessr-sheet_no,
        lwa_sheet    TYPE bapiessrc,  "BAPIESSRC
        lit_bapiret  TYPE TABLE OF bapiret2 WITH HEADER LINE,
        lv_nosheet   TYPE i, "Numero de hojas
        lv_noitems   TYPE i, "Numero de Posiciones a procesar
        lwa_bill     TYPE c LENGTH 14,  "factura
        lwa_bktxt    TYPE bktxt,       "codigo web
        lwa_xblnr    TYPE xblnr,       "Rango de estimaciones
        lwa_char     TYPE c LENGTH 16,
        lwa_msg      LIKE LINE OF pit_msg,
        "Itab p/ Lineas externas por Subpakete (Pos en Pedido de Serv)
        BEGIN OF lit_lines OCCURS 0,
          pckg_no  LIKE lwa_item-pckg_no,
          ext_line LIKE lwa_det-extrow,
        END OF lit_lines.

  RANGES:
          lrg_pos FOR lwa_item-po_item.

* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
  DATA: lv_fecautusd TYPE c,  "Bandera Fecha Autorizacion USD
        lv_fecautmxn TYPE c.  "Bandera Fecha Autorizacion MXN
* <- END OF INSERT MCA-20170714 OT: RSDK909520
  DATA: lv_fecauteur TYPE c.  "Bandera FechaAutorizacón EUR   "INSERT MCA-20180822 OT: RSDK910293

  CLEAR:
         p_subrc,
         lwa_item, lwa_det, lv_pckg, lv_spckg, lwa_bapihdr, lwa_serv,
         lwa_serv2, lit_bapiesll[], lit_bapiesll, lwa_bapiesll,
         lit_bapiacc[], lit_bapiacc, lwa_poaccs, lv_flag, lwa_sheet,
         lit_bapiret[], lit_bapiret, lv_nosheet, lv_noitems, lwa_bill,
         lwa_bktxt, lwa_xblnr, lwa_char, lwa_msg, lrg_pos[], lrg_pos,
         lit_lines[], lit_lines, lwa_test.

* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
* Obtener Bandera para aplicar fecha de autorizacion para USD
  CLEAR: lv_fecautusd.
  SELECT SINGLE zflag
    INTO lv_fecautusd
    FROM zutil_parameters
   WHERE zreport EQ 'ZMMFM_0030_CRT_HOJA_SERV'
     AND zfield  EQ 'FECAUT_USD'.

* Obtener Bandera para aplicar fecha de autorizacion para MXN
  CLEAR: lv_fecautmxn.
  SELECT SINGLE zflag
    INTO lv_fecautmxn
    FROM zutil_parameters
   WHERE zreport EQ 'ZMMFM_0030_CRT_HOJA_SERV'
     AND zfield  EQ 'FECAUT_MXN'.  "CHANGE MCA-20170721 OT: RSDK909533
* <- END OF INSERT MCA-20170714 OT: RSDK909520
* -> BEGIN OF INSERT MCA-20180822 OT: RSDK910293
* Obtener Bandera para aplicar fecha de autorizacion para EUR
  CLEAR: lv_fecauteur.
  SELECT SINGLE zflag
    INTO lv_fecauteur
    FROM zutil_parameters
   WHERE zreport EQ 'ZMMFM_0030_CRT_HOJA_SERV'
     AND zfield  EQ 'FECAUT_EUR'.
* <- END OF "INSERT MCA-20180822 OT: RSDK910293
  lwa_msg-mensaje = text-m04.
  lwa_msg-subrc   = '00'.
  APPEND lwa_msg TO pit_msg.

  LOOP AT pit_det INTO lwa_det.
    "range ebelp
    lrg_pos-sign    = 'I'.
    lrg_pos-option  = 'EQ'.
    lrg_pos-low     = lwa_det-ebelp.
    APPEND lrg_pos.

    "lines
    READ TABLE pit_po_item INTO lwa_item
    WITH KEY po_item = lwa_det-ebelp.
    READ TABLE pit_po_serv INTO lwa_serv
    WITH KEY  pckg_no = lwa_item-pckg_no.
    READ TABLE pit_po_serv INTO lwa_serv2
    WITH KEY pckg_no = lwa_serv-subpckg_no
             service = lwa_det-asnum."Servicio

    lit_lines-pckg_no  = lwa_serv2-pckg_no.
    lit_lines-ext_line = lwa_serv2-ext_line.
    APPEND lit_lines.

    lwa_bill       = lwa_det-bill.
    lwa_bktxt      = lwa_det-bktxt.
    lwa_xblnr      = lwa_det-xblnr.

    CLEAR: lwa_det, lrg_pos, lwa_item, lwa_serv, lwa_serv2, lit_lines.
  ENDLOOP.
  SORT lrg_pos BY low.
  DELETE ADJACENT DUPLICATES FROM lrg_pos COMPARING low.

* Se inicializa paquete y subpaquete para creacion de Hojas
  lv_pckg = '0000000001'.
  lv_spckg = lv_pckg + 1.

* Se recorren solo las posiciones del Pedido que envio la WEB
  LOOP AT pit_po_item INTO lwa_item WHERE po_item IN lrg_pos.
    "Se llena cabecera para Hoja para Pedido / Posición
    lwa_bapihdr-pckg_no    = lv_pckg.
    lwa_bapihdr-po_number  = lwa_item-po_number.
    lwa_bapihdr-po_item    = lwa_item-po_item.
***  Inicio RPM 02.2017
    IF p_moneda = 'USD'.
      lwa_bapihdr-doc_date  = p_f_fact.  " Fecha del documento
      lwa_bapihdr-post_date = p_f_fact.  " Fecha contabilización
* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
      IF lv_fecautusd EQ 'X'.  "Usar Fecha autorizacion USD
        lwa_bapihdr-post_date = p_f_auto.  " Fecha contabilización = Fecha autorizacion
      ENDIF.
* <- END OF INSERT MCA-20170714 OT: RSDK909520
    ELSEIF p_moneda = 'MXN'.
      lwa_bapihdr-doc_date  =  p_f_fact.
      lwa_bapihdr-post_date =  p_f_auto.
* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
      IF lv_fecautmxn IS INITIAL.  "NO Usar Fecha Autorizacion MXN
        lwa_bapihdr-post_date = p_f_fact.  " Fecha contabilización = Fecha Factura
      ENDIF.
* <- END OF INSERT MCA-20170714 OT: RSDK909520
    ENDIF.
* -> BEGIN OF INSERT MCA-20180822 OT: RSDK910293
    IF p_moneda = 'EUR'.
      lwa_bapihdr-doc_date  = p_f_fact.  " Fecha del documento
      lwa_bapihdr-post_date = p_f_fact.  " Fecha contabilización
* -> BEGIN OF INSERT MCA-20170714 OT: RSDK909520
      IF lv_fecauteur EQ 'X'.  "Usar Fecha autorizacion EUR
        lwa_bapihdr-post_date = p_f_auto.  " Fecha contabilización = Fecha autorizacion
      ENDIF.
    ENDIF.
* <- END OF "INSERT MCA-20180822 OT: RSDK910293

    lwa_bapihdr-accasscat  = 'P'. "Validar si queda en codigo duro
    lwa_bapihdr-acceptance = 'X'. "Validar si queda en codigo duro
    lwa_bapihdr-begdate    = sy-datum.
    lwa_bapihdr-enddate    = sy-datum.
    lwa_bapihdr-person_int = sy-uname.
    lwa_bapihdr-person_ext = sy-uname.
    lwa_bapihdr-ref_date   = sy-datum.
    lwa_bapihdr-ref_doc_no = lwa_item-po_number.

*Asignación de Variable de Control: Referencia, Factura
    CONDENSE lwa_bill NO-GAPS.
    CONDENSE lwa_xblnr NO-GAPS.
    CONCATENATE lwa_bill '-' lwa_xblnr INTO lwa_char. "concatena factura estimacion
    lwa_char = lwa_char+0(16). "Trunca cadena a 16 caracteres
    CONDENSE lwa_char NO-GAPS.  "Elimina espacios en blanco

    lwa_bapihdr-short_text = lwa_bktxt.   "TXZ01_ESSR char(40)
    lwa_bapihdr-ext_number = lwa_xblnr.   "LBLNE char(16)
    lwa_bapihdr-location = lwa_bktxt.     "DLORT char(25)
    lwa_bapihdr-ref_doc_no = lwa_char.    "XBLNR  char(16)

*   Se recupera paquete de Posicion
    READ TABLE pit_po_serv INTO lwa_serv
    WITH KEY  pckg_no   = lwa_item-pckg_no.

* si recupero paquetes
    IF sy-subrc EQ 0.
*     Contruye linea para el paquete
      CLEAR lwa_bapiesll.
      lwa_bapiesll-pckg_no    = lwa_bapihdr-pckg_no.
      lwa_bapiesll-line_no    = lwa_serv-line_no.
      lwa_bapiesll-ext_line   = lwa_serv-ext_line.
      lwa_bapiesll-outl_level = lwa_serv-outl_level.
      lwa_bapiesll-outl_no    = lwa_serv-outl_no.
      lwa_bapiesll-outl_ind   = lwa_serv-outl_ind.
      lwa_bapiesll-subpckg_no = lv_spckg.


      "Se recorren servicios de esa posición/paquete
      LOOP AT pit_po_serv INTO lwa_serv2 WHERE pckg_no EQ lwa_serv-subpckg_no.
        "Se verifica si el servicio de esta POS fue enviado por WEB
        READ TABLE lit_lines WITH KEY pckg_no  = lwa_serv2-pckg_no
                                      ext_line = lwa_serv2-ext_line.
        IF sy-subrc NE 0.
          CLEAR: lwa_serv2, lit_lines.
          CONTINUE.
        ENDIF.

        READ TABLE pit_det INTO lwa_det WITH KEY asnum = lwa_serv2-service
                                                 ebeln = lwa_bapihdr-po_number
                                                 ebelp = lwa_bapihdr-po_item.
        IF sy-subrc NE 0.
          CLEAR: lwa_serv2, lit_lines, lwa_det.
          CONTINUE.
        ELSE. "Se ejecuta BAPI
          lv_flag = 'X'.
          "Agregar linea para el paquete.
          APPEND lwa_bapiesll TO lit_bapiesll.
          "Se llena Posicion de servicio y su porcentaje
          lit_bapiesll-pckg_no    = lv_spckg.
          lit_bapiesll-line_no    = lwa_serv2-line_no.
          lit_bapiesll-ext_line   = lwa_serv2-ext_line.
          lit_bapiesll-subpckg_no = lwa_serv2-subpckg_no.
          lit_bapiesll-service    = lwa_serv2-service.
          lit_bapiesll-base_uom   = lwa_serv2-base_uom.
          lit_bapiesll-gr_price   = lwa_serv2-gr_price.
          lit_bapiesll-price_unit = lwa_serv2-price_unit.
          lit_bapiesll-pln_pckg   = lwa_serv2-pckg_no.
          lit_bapiesll-pln_line   = lwa_serv2-line_no.
          lit_bapiesll-short_text = lwa_serv2-short_text.

          IF p_absgr = 10.
            lit_bapiesll-quantity   = ( lwa_serv2-quantity / 100 ) * lwa_det-prctg.
            lwa_det-erfmg = 0.
          ELSE.
            lit_bapiesll-quantity = lwa_det-erfmg.
            lwa_det-prctg = 0.
          ENDIF.

          APPEND: lit_bapiesll.

          "Se recupera reg. de sub-paquete y linea para completar ITAB de %
          READ TABLE pit_po_accs INTO lwa_poaccs
          WITH KEY pckg_no = lwa_serv2-pckg_no
                   line_no = lwa_serv2-line_no.

          "Completa itab para la bapi
          IF sy-subrc EQ 0.
            lit_bapiacc-pckg_no    = lwa_serv2-pckg_no.
            lit_bapiacc-line_no    = lwa_serv2-line_no.
            lit_bapiacc-serno_line = lwa_poaccs-serno_line.
            lit_bapiacc-percentage = lwa_det-prctg.   "porcentaje
            lit_bapiacc-quantity = lwa_det-erfmg.   "cantidad
            lit_bapiacc-serial_no  = lwa_poaccs-serial_no.
            APPEND lit_bapiacc.
          ENDIF.

          PERFORM f_vald_lock
                  TABLES   pit_det[] pit_msg[]
                  USING    lwa_item-po_number pwa_po_hdr-doc_cat lwa_det
                  CHANGING p_subrc.
          " Interrumpe Creacion de Hojas de entrada por Error Detectado
          IF p_subrc NE 0.
            EXIT.
          ENDIF.

* Se realiza la creacion por posicion del pedido y sus servicios la hoja de entrada en modo Test.
*<<<---*INSERT ECN-20180912 OT: RSDK910309 <<<---
*         " Pasa el parametro de ejecucion en Test
*         lwa_test = p_testrun.                 " INSERT ECN-20180903 OT: RSDK910309 Se comenta ya que el parametro debe ir directamente en la primer ejecucion.
          lwa_test = 'X'.

           " Primer llamado a la BAPI en modo TEST
          CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
            EXPORTING
              entrysheetheader          = lwa_bapihdr
              testrun                   = lwa_test
              no_commit                 = lwa_test
            IMPORTING
              entrysheet                = lv_sheet
            TABLES
              entrysheetservices        = lit_bapiesll
              entrysheetsrvaccassvalues = lit_bapiacc
              return                    = lit_bapiret.

          "Actualiza registros Detalle de POS/Servicios procesados
          PERFORM f_upd_det_itabs USING lit_bapiret[] lv_sheet
                                        lwa_bapihdr-po_item
                                        lwa_serv2-service
                                        lwa_det
                               CHANGING pit_det pit_hojas pit_msg
                                        p_hojas lv_nosheet p_subrc.

        ENDIF.

        CLEAR: lwa_serv, lit_bapiesll, lwa_serv2, lwa_det, lit_bapiacc,
               lwa_poaccs, lit_bapiesll[], lit_bapiesll, lv_sheet,
               lit_bapiacc[], lit_bapiacc, lit_bapiret[], lit_bapiret,
               wa_datos, lit_lines, lwa_test.

      ENDLOOP.

    ELSE.

     ENDIF.

  ENDLOOP.

  "Se Actualiza registros en tablas Z
  PERFORM f_upd_z_tables TABLES   pit_det pit_msg
                         USING    lv_nosheet
                         CHANGING pwa_hdr pit_hojas p_hojas p_subrc.


ENDFORM.                    " f_process_po_1
*--->>> RSDK910309 --->>>

*--->>> RSDK910309 --->>>
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_FACTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_IDRFC  text
*----------------------------------------------------------------------*
FORM F_VALIDA_FACTURA  USING p_cab type ZMMWA_0030_HJSRV_H
                             p_imp_dets type ZMMIT_0030_HJSRV_D
                             p_v_idrfc
                             p_moneda type WAERS
                      CHANGING p_msg  TYPE ty_it_msg
                          p_subrc      TYPE subrc.

TABLES:zmmtt_30_hjsrv_h,
       zmmtt_30_hjsrv_d.

DATA: BEGIN OF gs_hjsrv,
        idrfc   type  zmmtt_30_hjsrv_h-idrfc,
        ebeln   type  zmmtt_30_hjsrv_h-ebeln,
        lifnr   TYPE  zmmtt_30_hjsrv_h-lifnr,
        bill    TYPE  zmmtt_30_hjsrv_d-bill,
      END OF gs_hjsrv.

DATA:  wa_msgerr type zmmtt_30_hjsrv_d-MSGERR,
       pwa_h     type zmmtt_30_hjsrv_h,
       lwa_d     type zmmtt_30_hjsrv_d.

* Valida que no exista registro en las tablas zmmtt_30_hjsrv_h y zmmtt_30_hjsrv_d

READ TABLE p_imp_dets INTO wa_det INDEX 1.

if sy-subrc eq 0.

    SELECT SINGLE a~idrfc a~ebeln a~lifnr b~bill
     INTO  gs_hjsrv
      FROM zmmtt_30_hjsrv_h AS a
      INNER JOIN zmmtt_30_hjsrv_d AS b
        ON a~idrfc = b~idrfc
      WHERE a~ebeln = p_cab-no_pedido
       AND a~lifnr  = p_cab-no_proveedor
       AND b~bill   = wa_det-no_factura.

   IF sy-subrc eq 0.

          CONCATENATE 'Factura' wa_det-no_factura 'procesada incompleta reportar a Sistemas'
                INTO wa_msgerr SEPARATED BY space.

*              Cabecera
                pwa_h-idrfc     = p_v_idrfc.
                pwa_h-ebeln     = p_cab-no_pedido.
                pwa_h-lifnr     = p_cab-no_proveedor.
                pwa_h-waers     = p_moneda.
                pwa_h-stsh      = '01'. "Informacion Erronea
                pwa_h-date_exec = sy-datum.
                pwa_h-time_exec = sy-uzeit.

*              Detalle
                lwa_d-idrfc = p_v_idrfc.
                lwa_d-ebeln = p_cab-no_pedido.
                lwa_d-ebelp = wa_det-no_posicion.
                lwa_d-extrow = wa_det-no_linea.
                lwa_d-asnum = wa_det-id_servicio.
                lwa_d-stsd  = '01'.
                lwa_d-bill  = wa_det-no_factura.
                lwa_d-prctg = wa_det-avance_estima_p.
                lwa_d-erfmg = wa_det-avance_estima_c.
                lwa_d-msgerr = wa_msgerr.

*               * Inserta registros en tablas Z
                INSERT zmmtt_30_hjsrv_h FROM pwa_h.
                IF sy-subrc EQ 0.
                  INSERT zmmtt_30_hjsrv_d FROM lwa_d.
                    IF sy-subrc EQ 0.
                      COMMIT WORK AND WAIT.
                    ENDIF.
                ENDIF.

            p_subrc = 10.

            PERFORM f_return_msg  USING 0  wa_msgerr p_subrc
                                  CHANGING   p_msg[].


          CLEAR: lwa_d, pwa_h.


   ENDIF.

 ENDIF.

ENDFORM.                    " F_VALIDA_FACTURA
