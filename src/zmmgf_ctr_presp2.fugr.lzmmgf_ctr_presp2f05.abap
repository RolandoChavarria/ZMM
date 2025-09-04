*----------------------------------------------------------------------*
***INCLUDE LZMMGF_CTR_PRESP2F05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_EXE_BAPI_PO_GETITTEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_HEADERS_LIST  text
*      -->P_LIT_ITEMS_LIST  text
*      -->P_LIT_DOC_TYPE  text
*      -->P_I_EBELN  text
*      -->P_LV_PURCH_ORG  text
*      -->P_LV_ITEM_CAT  text
*      -->P_LV_ACCTASSCAT  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_exe_bapi_po_getittems  TABLES   pit_headers_list STRUCTURE bapiekkol
                                         "Insertar nombre correcto para <...>
                                       pit_items_list STRUCTURE bapiekpoc
                              USING    lit_doc_type TYPE wrf_pbas_esart_rtty "#EC CI_USAGE_OK[2368913]
                                       i_ebeln
                                       pv_purch_org
                                       pv_item_cat
                                       pv_acctasscat
                              CHANGING pv_subrc.

  DATA: lv_whith_po_headers TYPE bapimmpara-selection VALUE 'X'.
  DATA: lv_deleted_items TYPE bapimmpara-selection VALUE 'X'.


* Se ejecuta BAPI para conocer todos los pedidos que se encuentren en la fecha seleccionada
  CALL FUNCTION 'ZBAPI_PO_GETITEMS'
    EXPORTING
      purchaseorder                = i_ebeln
*     doc_type                     = pv_doc_type
*     doc_date                     = pv_doc_date
*     PUR_GROUP                    =
      purch_org                    = pv_purch_org
*     VENDOR                       =
*     SUPPL_PLANT                  =
*     MATERIAL                     =
*     MAT_GRP                      =
      item_cat                     = pv_item_cat
      acctasscat                   = pv_acctasscat
*     PLANT                        =
*     TRACKINGNO                   =
*     SHORT_TEXT                   =
*     CREATED_BY                   =
*     PREQ_NAME                    = ' '
      with_po_headers              = lv_whith_po_headers
      deleted_items                = lv_deleted_items
*     ITEMS_OPEN_FOR_RECEIPT       = ' '
*     PUR_MAT                      = ' '
*     MATERIAL_EVG                 =
*     PUR_MAT_EVG                  =
  TABLES
    po_headers                   = pit_headers_list[]
    po_items                     = pit_items_list[]
*     RETURN                       =
   imp_doc_types                = lit_doc_type.

ENDFORM.                    " F_EXE_BAPI_PO_GETITTEMS
*&---------------------------------------------------------------------*
*&      Form  F_LOG_ID_WEB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ZFLAG  text
*      <--P_V_ID_WEB  text
*      <--P_E_SUBRC  text
*      <--P_E_BAPI_MSG  text
*----------------------------------------------------------------------*
FORM f_log_id_web  CHANGING  lv_zflag    TYPE zutil_parameters-zchar
                            p_v_id_web  TYPE zid_referencia
                            p_v_subrc   TYPE subrc
                            p_v_msgbapi TYPE bapi_msg.

  DATA lit_det TYPE TABLE OF zmmtt_detail_log.
  DATA: lv_max TYPE zid_referencia,
        lv_id TYPE zid_referencia.

  CLEAR: p_v_id_web, p_v_subrc, p_v_msgbapi, lv_max, lv_id.

  SELECT SINGLE zflag FROM zutil_parameters INTO lv_zflag
   WHERE zreport EQ 'ZMMFM_0050_CONF_OC'
   AND   zfield  EQ 'ACTIVATE_LOG_WEB'.

  IF lv_zflag = ' '.
    p_v_id_web = '999999999X'.
    EXIT.
  ELSEIF lv_zflag = 'X'.
    CONCATENATE sy-datum sy-uzeit '00' INTO p_v_id_web.
    lv_max = p_v_id_web + 99.
  ENDIF.


  SELECT SINGLE id_control FROM zmmtt_header_log INTO lv_id
  WHERE id_control = p_v_id_web.
  IF sy-subrc EQ 0.
    DO 1000 TIMES.
      ADD 1 TO p_v_id_web.
      SELECT SINGLE id_control FROM zmmtt_header_log INTO lv_id
      WHERE id_control = p_v_id_web.
      IF sy-subrc NE 0.
        EXIT.
      ELSEIF p_v_id_web EQ lv_max.
        WAIT UP TO 1 SECONDS.
        CONCATENATE sy-datum sy-uzeit '00' INTO p_v_id_web.
        lv_max = p_v_id_web + 99.
      ELSEIF sy-index EQ 1000.
        p_v_subrc = 05. "Error al crear ID de referencia
        p_v_msgbapi = 'Error al calcular ID para comunicar a WEB'.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.                    " F_LOG_ID_WEB
*&---------------------------------------------------------------------*
*&      Form  F_AUTO_LIMP_LOG_WEB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_auto_limp_log_web .

  DATA: lv_zchar TYPE zutil_parameters-zchar,
            lv_fch TYPE sy-datum.

  SELECT SINGLE zchar FROM zutil_parameters INTO lv_zchar
  WHERE zreport EQ 'ZMMFM_0050_CONF_OC'
  AND   zfield  EQ 'DIAS_BORRAR_LOG_WEB'.
  IF sy-subrc NE 0.
    lv_zchar = 730.
  ENDIF.

  lv_fch = sy-datum - lv_zchar.

  SELECT SINGLE aedat FROM zmmtt_header_log INTO lv_fch
  WHERE aedat LE lv_fch.
  CHECK sy-subrc EQ 0.

  DELETE FROM zmmtt_header_log WHERE aedat LE lv_fch.
  IF sy-subrc EQ 0.
    DELETE FROM zmmtt_detail_log WHERE aedat LE lv_fch.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_AUTO_LIMP_LOG_WEB
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0430   text
*      -->P_I_EBELN  text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion2   USING     p_pgrm           TYPE rs38l_fnam
                                    i_ebeln          TYPE zmmde_ebeln
                           CHANGING p_id_rfc         TYPE zid_referencia
                                    p_conse          TYPE zmmde_conse
                                    p_v_subrc        TYPE subrc.

  DATA:it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: lv_zflag TYPE zutil_parameters-zchar.

  CLEAR: wa_params, it_params, p_conse, p_v_subrc.

  SELECT SINGLE zflag FROM zutil_parameters INTO lv_zflag
   WHERE zreport EQ 'ZMMFM_0000_LOG'
   AND   zfield  EQ 'ACTIVATE_LOG'.

  CHECK lv_zflag IS NOT INITIAL.



* Agregar Parametro import
  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_EBELN'. "Nombre del parametro en el RFC
  wa_params-val_param = i_ebeln. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  CALL FUNCTION 'ZMMFM_0000_LOG'
    EXPORTING
      namepgr   = p_pgrm
    IMPORTING
      e_subrc   = p_v_subrc
      e_id_ref  = p_id_rfc
    TABLES
      it_params = it_params.


ENDFORM.                    " F_REG_LOG_CONEXION2
*&---------------------------------------------------------------------*
*&      Form  F_REG_EXEP_RFC2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_ID_RFC  text
*      -->P_E_SUBRC  text
*      -->P_E_BAPI_MSG  text
*      <--P_V_CONSE  text
*----------------------------------------------------------------------*
FORM f_reg_exep_rfc2  USING    p_id_rfc    TYPE zid_referencia
                              p_subrc     TYPE subrc
                              p_msg       TYPE bapi_msg
                     CHANGING p_conse     TYPE zmmde_conse.
  DATA: lwa_det TYPE zmmlog_det_rfc,
        lit_det TYPE TABLE OF zmmlog_det_rfc.

  CLEAR: lwa_det, lit_det.

  CHECK p_id_rfc IS NOT INITIAL.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_SUBRC' p_subrc.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_BAPI_MSG' p_msg.

  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_id_rfc
    TABLES
      it_params = lit_det.

ENDFORM.                    " F_REG_EXEP_RFC2
*&---------------------------------------------------------------------*
*&      Form  F_GET_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_ZUTIL  text
*      -->P_0500   text
*      <--P_LV_PURCH_ORG  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_get_zutil_param  TABLES   pit_zutil STRUCTURE zutil_parameters
                                   "Insertar nombre correcto para <...>
                        USING    p_field
                        CHANGING p_value
                                 p_subrc.

  DATA: lwa_zutil TYPE zutil_parameters.
  CLEAR p_subrc.

  READ TABLE pit_zutil INTO lwa_zutil WITH KEY zfield = p_field.
  IF lwa_zutil-zchar = ' '.
    p_subrc = 02.
  ELSEIF lwa_zutil-zchar IS NOT INITIAL.
    p_value = lwa_zutil-zchar.
  ELSE.
  ENDIF.

ENDFORM.                    " F_GET_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*&      Form  F_VAL_PROYECTO_PEDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LIT_PEDIDOS[]  text
*----------------------------------------------------------------------*
FORM f_val_proyecto_peds  CHANGING lit_pedidos TYPE gty_it_pedidos
                                   e_subrc TYPE subrc.
  DATA: BEGIN OF lt_ekko OCCURS 0,
        ebeln TYPE ekko-ebeln,
        posid TYPE prps-posid,
        END OF lt_ekko.

  DATA: BEGIN OF lt_prps OCCURS 0,
        posid TYPE prps-posid,
        usr10 TYPE prps-usr10,
        END OF lt_prps.

  CLEAR: lt_ekko[], lt_ekko, lt_prps[], lt_prps.

  CHECK lit_pedidos IS NOT INITIAL.

* Se recuperan proyectos de los pedidos
  SELECT ebeln verkf FROM ekko INTO TABLE lt_ekko
  FOR ALL ENTRIES IN lit_pedidos
  WHERE ebeln EQ lit_pedidos-ebeln.

  CHECK sy-subrc EQ 0.

* Se recuperan Proyectos que se pueden enviar a WEB
  SELECT posid  usr10 FROM prps INTO TABLE lt_prps
  FOR ALL ENTRIES IN lt_ekko
  WHERE posid EQ lt_ekko-posid
  AND   usr10 EQ 'X'.

  IF sy-subrc <> 0.
    e_subrc = 03.
  ENDIF.
* Se eliminan aquellos pedidos que no se deben enviar a web
* segun el proyecto
  LOOP AT lt_ekko.
    READ TABLE lt_prps WITH KEY posid = lt_ekko-posid.
    IF sy-subrc NE 0.
      DELETE lit_pedidos WHERE ebeln = lt_ekko-ebeln.
    ENDIF.
    CLEAR: lt_ekko, lt_prps.
  ENDLOOP.


ENDFORM.                    " F_VAL_PROYECTO_PEDS
*&---------------------------------------------------------------------*
*&      Form  F_EXE_BAPI_PO_GETDETAIL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_DETAIL_PO  text
*      -->P_LIT_ACCOUNT_PO  text
*      -->P_LIT_SERVICES_PO  text
*      -->P_LIT_TEXTHEADER_PO  text
*      -->P_LIT_HISTORY_PO  text
*      -->P_LIT_POCOND  text
*      -->P_LIT_PEDIDOS_EBELN  text
*      <--P_LWA_HEADER_PO  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_exe_bapi_po_getdetail1  TABLES   pit_detail_po STRUCTURE bapimepoitem
                                        pit_account_po STRUCTURE bapimepoaccount
                                        pit_services_po STRUCTURE bapiesllc
                                        pit_textheader_po STRUCTURE bapimepotextheader
                                        pit_history_po STRUCTURE bapiekbe
                                        pit_pocond STRUCTURE bapimepocond
                               USING    p_po_number
                               CHANGING pwa_header_po TYPE bapimepoheader
                                        pv_subrc.

  DATA: lindex TYPE sy-tabix.

  REFRESH: pit_detail_po[].
  REFRESH: pit_account_po[].
  REFRESH: pit_services_po[].
  REFRESH: pit_textheader_po[].
  REFRESH: pit_history_po[].
  REFRESH: pit_pocond[].
  CLEAR: pwa_header_po.



  CALL FUNCTION 'BAPI_PO_GETDETAIL1' "#EC CI_USAGE_OK[2438131]
    EXPORTING
       purchaseorder            = p_po_number
       account_assignment       = 'X'
       item_text                = 'X'
       header_text              = 'X'
       delivery_address         = 'X'
       version                  = 'X'
       services                 = 'X'
   IMPORTING
     poheader                 = pwa_header_po
*   POEXPIMPHEADER           =
   TABLES
*   RETURN                   =
     poitem                   = pit_detail_po[]
*   POADDRDELIVERY           =
*   POSCHEDULE               =
     poaccount                = pit_account_po[]
*   POCONDHEADER             =
    pocond                   = pit_pocond[]
*   POLIMITS                 =
*   POCONTRACTLIMITS         =
     poservices               = pit_services_po[]
*   POSRVACCESSVALUES        =
     potextheader             = pit_textheader_po[]
*   POTEXTITEM               =
*   POEXPIMPITEM             =
*   POCOMPONENTS             =
*   POSHIPPINGEXP            =
     pohistory                = pit_history_po[]
*   POHISTORY_TOTALS         =
*   POCONFIRMATION           =
*   ALLVERSIONS              =
*   POPARTNER                =
*   EXTENSIONOUT             =
            .

* Borrrar marcadas para borrar
***  LOOP AT pit_detail_po WHERE delete_ind NE space.
***    lindex = sy-tabix.
***    DELETE pit_account_po WHERE po_item = pit_detail_po-po_item.
****--> Inicia Modificación RSDK907067 - 1.
**** IJOF : Se cimenta codigo para no eliminar las partidas
***         " con marca de borrado.
****    DELETE pit_detail_po INDEX lindex.
****<-- Fin Modificación RSDK907067 - 1.
***  ENDLOOP.

  IF pit_detail_po[] IS INITIAL.
    pv_subrc = 4.
    EXIT.
  ENDIF.

  IF pit_account_po[] IS INITIAL.
    pv_subrc = 5.
    EXIT.
  ENDIF.

  IF pit_services_po[] IS INITIAL.
    pv_subrc = 6.
    EXIT.
  ENDIF.

  IF pit_textheader_po[] IS INITIAL.
    pv_subrc = 7.
    EXIT.
  ENDIF.

  IF pit_history_po[] IS INITIAL.
    pv_subrc = 8.
    EXIT.
  ENDIF.

  IF pit_pocond[] IS INITIAL.
    pv_subrc = 9.
    EXIT.
  ENDIF.

ENDFORM.                    " F_EXE_BAPI_PO_GETDETAIL1
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_POCOND  text
*      -->P_LWA_HEADER_PO  text
*      -->P_LWA_DETAIL_PO  text
*      <--P_LWA_DETAIL  text
*----------------------------------------------------------------------*
FORM f_build_detail  TABLES   pit_pocond STRUCTURE bapimepocond
                     USING    pwa_header_po TYPE bapimepoheader
                              pwa_detail_po TYPE bapimepoitem
                     CHANGING pwa_detail    TYPE zmmwa_0030_detail_po.

  DATA lv_pos TYPE n LENGTH 6.
  DATA wa_pocond TYPE bapimepocond.
  CLEAR: pwa_detail, lv_pos.

  pwa_detail-no_pedido       = pwa_header_po-po_number.
  pwa_detail-no_posicion     = pwa_detail_po-po_item.
  lv_pos = pwa_detail-no_posicion.
  pwa_detail-descrip_pos     = pwa_detail_po-short_text.
  pwa_detail-ind_borrado_pos = pwa_detail_po-delete_ind.
  pwa_detail-ind_entrega_fin = pwa_detail_po-no_more_gr.
* pwa_detail-id_contrato  = pwa_detail_po-agreement.
  pwa_detail-cantidad_pos    = pwa_detail_po-quantity.
  pwa_detail-precio_neto     = pwa_detail_po-net_price.
*--> Inicia Modificación RSDK907067 - 2.
* IJOF : Se asigna contenido al campo agregado.
  pwa_detail-gpo_art        = pwa_detail_po-matl_group.
*<-- Fin Modificación RSDK907067 - 2.

  READ TABLE pit_pocond INTO wa_pocond
  WITH KEY cond_type = 'PBXX'
           itm_number = lv_pos.
  pwa_detail-valor_bruto = wa_pocond-cond_value.
  CLEAR wa_pocond.

  READ TABLE pit_pocond INTO wa_pocond
  WITH KEY cond_type = 'ZRET'
           itm_number = lv_pos.
  pwa_detail-porc_retencion = wa_pocond-cond_value.
  CLEAR wa_pocond.

  READ TABLE pit_pocond INTO wa_pocond
  WITH KEY cond_type = 'ZAAN'
           itm_number = lv_pos.
  pwa_detail-porc_amortiza = wa_pocond-cond_value.
  CLEAR wa_pocond.

  pwa_detail-importe_reten = ( pwa_detail-valor_bruto * pwa_detail-porc_retencion ) / 100.


  pwa_detail-importe_amortiza = ( pwa_detail-valor_bruto * pwa_detail-porc_amortiza ) / 100.

ENDFORM.                    " F_BUILD_DETAIL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_SERVICES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_ACCOUNT_PO  text
*      -->P_LIT_HISTORY_PO  text
*      -->P_LIT_EKBE  text
*      -->P_LWA_DETAIL_PO  text
*      -->P_LWA_SERVICES_PO  text
*      -->P_LV_PREFIJO  text
*      <--P_LWA_HEADER_PO  text
*      <--P_LWA_SERVICES  text
*      <--P_LWA_HEADER  text
*----------------------------------------------------------------------*
FORM f_build_services  TABLES   pit_account_po STRUCTURE bapimepoaccount
                                pit_history_po STRUCTURE bapiekbe
                                pit_ekbe STRUCTURE ekbe
                       USING    pwa_detail_po TYPE bapimepoitem
                                pwa_services_po TYPE bapiesllc
*->Begin of Custom Code Migration S/4HANA POSTCONVERSION_HANA_DRCS 14.07.2024
                                pv_prefijo TYPE char2
*                                pv_prefijo TYPE char02
*->End of Custom Code Migration S/4HANA POSTCONVERSION_HANA_DRCS 14.07.2024
                       CHANGING pwa_header_po TYPE bapimepoheader
                                pwa_services TYPE zmmwa_0040_services_po
                                pwa_header TYPE zmmwa_0020_header_po.


  DATA: pwa_account_po LIKE LINE OF pit_account_po.
  DATA: pwa_history_po LIKE LINE OF pit_history_po.

  DATA: lv_sum TYPE bapiekbe-cl_val_loc.
* DATA: lv_objnr TYPE proj-objnr.

  DATA: lwa_prps TYPE prps.
  DATA: lwa_proj TYPE proj.
  FIELD-SYMBOLS: <fs_ekbe> LIKE LINE OF pit_ekbe.


  CLEAR pwa_services.
  pwa_services-no_pedido        = pwa_header_po-po_number.
  pwa_services-no_posicion      = pwa_detail_po-po_item.
*  pwa_services-ind_borrado_ser  = pwa_services_po-delete_ind.
  pwa_services-id_servicio      = pwa_services_po-service.
  pwa_services-cantidad         = pwa_services_po-quantity.

  pwa_services-unidad_medida    = pwa_services_po-base_uom.
  pwa_services-monto_importe    = pwa_services_po-gr_price.
  pwa_services-monto_neto       = pwa_services_po-gr_price * pwa_services_po-quantity.
  pwa_services-ind_borrado_ser  = pwa_services_po-delete_ind.

  pwa_services-no_linea         = pwa_services_po-ext_line.
  pwa_services-desc_servicio    = pwa_services_po-short_text.
  LOOP AT pit_account_po INTO pwa_account_po WHERE po_item = pwa_detail_po-po_item.
    pwa_services-elemento_pep   =  pwa_account_po-wbs_element.
  ENDLOOP.

  CLEAR lv_sum.

* Se sustituye código, se cambia el método de cálculo
  LOOP AT pit_ekbe ASSIGNING <fs_ekbe> WHERE ebeln  = pwa_services-no_pedido   AND
                                             ebelp  = pwa_services-no_posicion AND
                                          (  bewtp  = 'Q' OR bewtp = 'E' )     AND
                                             srvpos = pwa_services-id_servicio.
    ADD <fs_ekbe>-wrbtr TO lv_sum.
  ENDLOOP.

  IF pwa_services-elemento_pep IS INITIAL .
    EXIT.
  ENDIF.

*  BREAK DEVLPEXT.
  DATA: lv_aux TYPE bapi_msg.
  DATA: lv_aux2 TYPE bapi_msg.
  DO.
    IF pwa_services-elemento_pep CS '-'.
      SPLIT pwa_services-elemento_pep AT '-' INTO  lv_aux pwa_services-elemento_pep.
      CONCATENATE lv_aux2 lv_aux INTO lv_aux2.
    ELSE.
*      CONCATENATE LV_AUX2 LV_AUX INTO LV_AUX2.
      CONCATENATE lv_aux2 pwa_services-elemento_pep INTO pwa_services-elemento_pep .
      EXIT.
    ENDIF.

  ENDDO.

  CLEAR lwa_proj.
  SELECT SINGLE post1 INTO  CORRESPONDING FIELDS OF lwa_proj
  FROM proj
  WHERE pspid = pwa_header-id_proyecto.
  IF sy-subrc EQ 0.
    pwa_header-desc_proyecto = lwa_proj-post1.
  ENDIF.

ENDFORM.                    " F_BUILD_SERVICES
*&---------------------------------------------------------------------*
*&      Form  F_GET_IMPORTE_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_DETAIL_PO  text
*      <--P_LWA_HEADER_IMPORTE_PEDIDO  text
*----------------------------------------------------------------------*
FORM f_get_importe_pedido   TABLES   lit_account_po STRUCTURE bapimepoaccount
                           CHANGING p_importe_pedido.

  FIELD-SYMBOLS: <fs_account_po> LIKE LINE OF lit_account_po.
  CLEAR p_importe_pedido.
*  LOOP AT lit_account_po ASSIGNING <fs_account_po>. "WHERE delete_ind IS INITIAL.  "COMMENT MCA-20160506 OT:RSDK908972
  LOOP AT lit_account_po ASSIGNING <fs_account_po> WHERE delete_ind IS INITIAL.     "INSERT MCA-20160506 OT:RSDK908972
    ADD <fs_account_po>-net_value TO p_importe_pedido.
  ENDLOOP.
ENDFORM.                    " F_GET_IMPORTE_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_RETURN2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HEADER  text
*      -->P_IT_DETAIL  text
*      -->P_IT_SERVICES  text
*      -->P_IT_HISTORIAL  text
*      -->P_IT_TOTALES  text
*      -->P_E_BAPI_MSG  text
*      -->P_V_ID_RFC  text
*      <--P_V_CONSE  text
*----------------------------------------------------------------------*
FORM f_reg_log_return2 TABLES   p_it_header    STRUCTURE zmmwa_0020_header_po
                                p_it_detail    STRUCTURE zmmwa_0030_detail_po
                                p_it_services  STRUCTURE zmmwa_0040_services_po
                                p_it_historial STRUCTURE zmmwa_0040_hist
                                p_it_totales   STRUCTURE zmmwa_0040_total
                       USING    p_e_bapi_msg   TYPE bapi_msg
                                p_v_id_rfc     TYPE zid_referencia
                                p_v_id_web     TYPE zid_referencia
                                p_e_subrc      TYPE subrc
                       CHANGING p_v_conse      TYPE zmmde_conse.

  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: wa_it_header   LIKE LINE OF  p_it_header.
  DATA: wa_it_detail   LIKE LINE OF  p_it_detail.
  DATA: wa_it_services LIKE LINE OF  p_it_services.
  DATA: wa_it_historial LIKE LINE OF p_it_historial.
  DATA: wa_it_totales   LIKE LINE OF p_it_totales.

  DATA:  lv_string        TYPE string.
  CLEAR: wa_params, it_params, wa_it_header, wa_it_detail, wa_it_services, wa_it_historial, wa_it_totales, p_it_header,
         p_it_detail, p_it_services.

  CHECK p_v_id_rfc IS NOT INITIAL.


  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING  p_v_conse 'E' 'E_SUBRC' p_e_subrc.

  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING  p_v_conse 'E' 'E_BAPI_MSG' p_e_bapi_msg.


  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING  p_v_conse 'E' 'E_ID_ENVIO' p_v_id_web.



  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING p_v_conse 'T' 'IT_HEADER' ''.

  LOOP AT p_it_header INTO wa_it_header.
    PERFORM f_string_log USING 'ZMMWA_0020_HEADER_PO' wa_it_header
                         CHANGING lv_string.
    ADD 1 TO p_v_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_v_conse 'T' 'IT_HEADER' lv_string.


    CLEAR: wa_it_header, lv_string.
  ENDLOOP.



  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING p_v_conse 'T' 'IT_DETAIL' ''.

  LOOP AT p_it_detail INTO wa_it_detail.
    PERFORM f_string_log USING 'ZMMWA_0030_DETAIL_PO' wa_it_detail
                         CHANGING lv_string.
    ADD 1 TO p_v_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_v_conse 'T' 'IT_DETAIL' lv_string.
    CLEAR: wa_it_detail, lv_string.
  ENDLOOP.


  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING p_v_conse 'T' 'IT_SERVICES' ''.

  LOOP AT p_it_services INTO wa_it_services.
    PERFORM f_string_log USING 'ZMMWA_0040_SERVICES_PO' wa_it_services
                         CHANGING lv_string.
    ADD 1 TO p_v_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_v_conse 'T' 'IT_SERVICES' lv_string.
    CLEAR: wa_it_services, lv_string.
  ENDLOOP.


  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING p_v_conse 'T' 'IT_HISTORIAL' ''.

  LOOP AT p_it_historial INTO wa_it_historial.
    PERFORM f_string_log USING 'ZMMWA_0040_HIST' wa_it_historial
                         CHANGING lv_string.
    ADD 1 TO p_v_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_v_conse 'T' 'IT_HISTORIAL' lv_string.
    CLEAR: wa_it_historial, lv_string.
  ENDLOOP.

  ADD 1 TO p_v_conse.
  PERFORM f_add_t_params TABLES it_params USING p_v_conse 'T' 'IT_TOTALES' ''.

  LOOP AT p_it_totales INTO wa_it_totales.
    PERFORM f_string_log USING 'ZMMWA_0040_TOTAL' wa_it_totales
                         CHANGING lv_string.
    ADD 1 TO p_v_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_v_conse 'T' 'IT_TOTALES' lv_string.
    CLEAR: wa_it_totales, lv_string.
  ENDLOOP.

  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_v_id_rfc
    TABLES
      it_params = it_params.


ENDFORM.                    " F_REG_LOG_RETURN2
*&---------------------------------------------------------------------*
*&      Form  F_LOG_WEB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HEADER  text
*      -->P_IT_DETAIL  text
*      -->P_IT_SERVICES  text
*      -->P_IT_HEADER_LOG  text
*      -->P_IT_DETAIL_LOG  text
*      -->P_IT_SERV_LOG  text
*      -->P_V_ID_WEB  text
*      <--P_E_ID_ENVIO  text
*      <--P_E_BAPI_MSG  text
*      <--P_E_SUBRC  text
*----------------------------------------------------------------------*
FORM f_log_web  TABLES   p_it_header STRUCTURE zmmwa_0020_header_po
                         p_it_detail STRUCTURE zmmwa_0030_detail_po
                         p_it_services STRUCTURE zmmwa_0040_services_po
                         p_it_header_log STRUCTURE zmmtt_header_log
                         p_it_detail_log STRUCTURE zmmtt_detail_log
                         p_it_serv_log STRUCTURE zmmtt_serv_log
                USING    p_v_id_web TYPE zid_referencia
                CHANGING p_e_id_envio TYPE zid_referencia
                         p_e_bapi_msg TYPE bapi_msg
                         p_e_subrc  TYPE subrc.

  DATA: wa_header_log LIKE LINE OF p_it_header_log.
  DATA: wa_detail_log LIKE LINE OF p_it_detail_log.
  DATA: wa_services_log LIKE LINE OF p_it_serv_log.

  IF p_v_id_web = '999999999X'.
    p_e_id_envio = p_v_id_web.
  ENDIF.

  CHECK p_v_id_web <> '999999999X'.

  wa_header_log-id_control = p_v_id_web.
  wa_header_log-estatus = '01'.
  wa_header_log-aedat = sy-datum.
  wa_header_log-cputm = sy-uzeit.
  wa_header_log-ernam = sy-uname.

  LOOP AT p_it_header.
    ADD 1 TO wa_header_log-no_pedidos.
    wa_detail_log-id_control = p_v_id_web.
    wa_detail_log-pedido = p_it_header-no_pedido.
    wa_detail_log-estatus = '01'.
    wa_detail_log-aedat = sy-datum.
    wa_detail_log-cputm = sy-uzeit.
    wa_detail_log-ernam = sy-uname.

    LOOP AT p_it_detail WHERE no_pedido = p_it_header-no_pedido.
      ADD 1 TO wa_detail_log-no_posiciones. "Esto hace la suma por pedido
      ADD 1 TO wa_header_log-no_posiciones.  "Esta va haciendo la suma total de el HDR
      CLEAR p_it_detail.
    ENDLOOP.

    LOOP AT p_it_services WHERE no_pedido = p_it_header-no_pedido.
      ADD 1 TO wa_detail_log-no_servicios. "Esto hace la suma por pedido
      ADD 1 TO wa_header_log-no_servicios. "Esta va haciendo la suma total de el HDR
      CLEAR p_it_services.
    ENDLOOP.

    APPEND wa_detail_log TO  p_it_detail_log.
    CLEAR: wa_detail_log.
  ENDLOOP.


  INSERT zmmtt_header_log FROM wa_header_log.
  IF sy-subrc EQ 0.
    p_e_id_envio = p_v_id_web.
    INSERT zmmtt_detail_log FROM TABLE p_it_detail_log.
    IF sy-subrc EQ 0.
      p_e_id_envio = p_v_id_web.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      p_e_id_envio = ' '.
      p_e_subrc = 06.
      p_e_bapi_msg   = 'Error al grabar Log de Ordenes de Compra WEB / SAP'.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    p_e_id_envio = ' '.
    p_e_subrc = 06.
    p_e_bapi_msg   = 'Error al grabar Log de Ordenes de Compra WEB / SAP'.
  ENDIF.


  CLEAR: wa_header_log.


ENDFORM.                    " F_LOG_WEB
*&---------------------------------------------------------------------*
*&      Form  F_ADD_T_PARAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_DET  text
*      -->P_P_CONSE  text
*      -->P_0450   text
*      -->P_0451   text
*      -->P_P_SUBRC  text
*----------------------------------------------------------------------*
FORM f_add_t_params  TABLES   pt_params STRUCTURE zmmlog_det_rfc
                     USING    p_conse
                              p_type
                              p_param
                              p_value.
  DATA ls_params TYPE zmmlog_det_rfc.

  CLEAR ls_params.

  ls_params-conse = p_conse.
  ls_params-type_param = p_type.
  ls_params-param = p_param.
  ls_params-val_param = p_value.
  APPEND ls_params TO pt_params.

ENDFORM.                    " F_ADD_T_PARAMS
*&---------------------------------------------------------------------*
*&      Form  F_STRING_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1277   text
*      -->P_WA_IT_HEADER  text
*      <--P_LV_STRING  text
*----------------------------------------------------------------------*
FORM f_string_log  USING    p_struc  TYPE dd02l-tabname
                            p_val    TYPE any
                   CHANGING p_string TYPE string.
  DATA: fcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        lv_fld TYPE string,
        lv_s TYPE string.
  FIELD-SYMBOLS <fld> TYPE ANY.

  CLEAR: fcat[], fcat, lv_fld, lv_s.
  UNASSIGN <fld>.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_struc
      i_client_never_display = ''
    CHANGING
      ct_fieldcat            = fcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT fcat.
    CONCATENATE 'P_VAL-' fcat-fieldname INTO lv_fld.
    ASSIGN (lv_fld) TO <fld>.
    IF <fld> IS ASSIGNED.
      IF <fld> EQ ' '.
        lv_s = 'Vacio'.
      ELSE.
        lv_s = <fld>.
      ENDIF.
      CONCATENATE p_string fcat-fieldname '=' lv_s '|' INTO p_string.
    ENDIF.
    CLEAR: fcat, lv_fld, lv_s.
    UNASSIGN <fld>.
  ENDLOOP.


ENDFORM.                    " F_STRING_LOG
