
FUNCTION zmmfm_0030_crt_hoja_serv.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_CAB) TYPE  ZMMWA_0030_HJSRV_H OPTIONAL
*"     VALUE(I_FECHA_FACTURA) TYPE  DATUM
*"     VALUE(I_FECHA_AUTORIZACION) TYPE  DATUM
*"     VALUE(I_MONEDA) TYPE  WAERS
*"  EXPORTING
*"     VALUE(I_HOJAS) TYPE  ZMMDE_LBLNI
*"  TABLES
*"      IT_IMP_DETS TYPE  ZMMIT_0030_HJSRV_D
*"      IT_EXP_HOJAS STRUCTURE  ZMMWA_0030_EXP_HOJA OPTIONAL
*"      IT_EXP_MSG STRUCTURE  ZMMWA_0030_EXP_MSG OPTIONAL
*"  EXCEPTIONS
*"      ERROR_CAB_Y_POS_INCOMPLETE
*"      ERROR_CRT_IDRFC
*"      ERROR_CRT_REG_TABLES
*"      ERROR_TO_GET_DETAIL_PO
*"      ERROR_PROVEEDOR_NE_PO
*"      ERROR_POS_SERVICE_NOT_FOUND
*"      ERROR_POS_ON_PO_NOT_FOUND
*"      ERROR_REG_LOG
*"      ERROR_TYPE_CONTRACT
*"      ERROR_ESTIMATE
*"      ERROR_VIM
*"----------------------------------------------------------------------
* 0. Declaración de variables
  DATA: v_subrc    TYPE subrc,
        v_jsubrc   TYPE subrc VALUE 0,  "jane
        v_idrfc    TYPE zmmde_0030_id,
        wa_hdr     TYPE zmmtt_30_hjsrv_h,
        it_det     TYPE TABLE OF zmmtt_30_hjsrv_d,
        wa_po_hdr  TYPE bapiekkol,
        it_po_item TYPE TABLE OF bapiekpo,
        it_po_serv TYPE TABLE OF bapiesll,
        it_po_accs TYPE TABLE OF bapieskl,
        iv_subrc   TYPE  subrc,   "Variable de subrc para LOG
        v_id_rfc   TYPE zid_referencia,
        v_conse    TYPE zmmde_conse, "Para llevar el consecutivo de cada Param
        wa_hojas   TYPE zwa_lblni.   "

* 1. Limpiar variables: subrc, id rfc y consecutivo
  CLEAR: v_subrc, v_id_rfc, v_conse.
* 1.1 Se agrega rutina para el control en nuevo proyecto.
* ---> Inicio 27.10.2014
***  READ TABLE it_imp_dets INTO wa_det INDEX 1.
***  IF sy-subrc EQ 0.
***    vl_prov    = i_cab-no_proveedor.
***    vl_pedido  = i_cab-no_pedido.
***    vl_fact2 = wa_det-no_factura.
***    CALL FUNCTION 'ZFEFM_CHECK_FE_CTRL_PRES'
***      EXPORTING
***        i_proveedor = i_cab-no_proveedor
***        i_pedido    = i_cab-no_pedido
***        i_factura   = vl_fact2
***      IMPORTING
***        e_rfc       = vl_rfc
***        e_subrc     = vl_subrc
***        e_msgerr    = vl_msgerr.
***    IF vl_subrc NE 0.
***      vl_msg_aux = vl_msgerr.
***      CONDENSE vl_msg_aux.
***      PERFORM f_reg_exep_rfc  USING     v_id_rfc vl_msg_aux
***                              CHANGING  v_conse.
***      wa_msg_error-no_posicion = i_cab-no_pedido.
***      wa_msg_error-mensaje     = vl_msg_aux.
***      wa_msg_error-subrc       = '11'.
***      APPEND wa_msg_error TO it_exp_msg.
***      CHECK vl_subrc = 0.
***    ENDIF.
***  ENDIF.
* <--- FIn    27.10.2014
* 2. Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_conexion   TABLES it_imp_dets
                               USING 'ZMMFM_0030_CRT_HOJA_SERV' i_cab
                               CHANGING v_id_rfc v_conse iv_subrc.
  "no se guarda en tablas de main log
  v_jsubrc = sy-subrc.
  "IF sy-subrc NE 0.
  "  RAISE error_reg_log.
  "ENDIF.
  CHECK v_jsubrc = 0.



* 3. Válida que hayan enviado cabecera y posiciones con servicios (no imports vacíos)
  IF i_cab IS INITIAL OR it_imp_dets[] IS INITIAL.
    PERFORM f_reg_exep_rfc  USING     v_id_rfc 'Error Tablas I_CAB y IT_IMP_DETS incompletas'
                            CHANGING  v_conse.
    "no se guarda en tablas de locales de log
    "RAISE error_cab_y_pos_incomplete.
    v_jsubrc = 1.

    "mensaje a regresar
    PERFORM f_return_msg  USING 0  'Error Tablas I_CAB y IT_IMP_DETS incompletas' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


* 4. Se crea el IDRFC para registros en tabla Z de esta llamada RFC (guarda en tabla local de log)
  PERFORM f_crt_idrfc CHANGING v_idrfc v_subrc.
  "si se ejecuta error al guardar en tabla local de log
  IF v_subrc NE 0.
    "se guarda info en main log
    PERFORM f_reg_exep_rfc  USING     v_id_rfc 'Error al crear IDRFC'
                            CHANGING  v_conse.
    v_jsubrc = 2.
    PERFORM f_return_msg  USING 0  'Error al crear IDRFC' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


*--->>>INICIO RSDK910309 --->>>
* Se realiza una validacion para evitar reprocesar una factura
* con esto se evitara el incidente de duplicidad de posiciones.
IF sy-tcode NE  'SE37'. " Se inserta validacion ya que si el RFC es ejecutado desde SE37 esta validacion no aplica.
  PERFORM f_valida_factura USING  i_cab
                                it_imp_dets[]
                                v_idrfc
                                i_moneda
                           CHANGING it_exp_msg[] v_jsubrc.

  CHECK v_jsubrc = 0.
ENDIF.

*--->>>FIN RSDK910309 --->>>


* START JANE MOD:
* 5. GET ADITIONAL INFO: TIPO DE CONTRATO Y TIPO DE MONEDA
  DATA: wa_absgr TYPE ekko-absgr VALUE 0.     "Tipo de contrato
  DATA: wa_waers TYPE ekko-waers  VALUE 'MXN'.     "Tipo de moneda
  DATA: wa_error_absgr TYPE sy-subrc. "Error en tipo estimación dentro de rango de estimaciones
  PERFORM f_get_adinfo_po     USING     i_cab-no_pedido
                              CHANGING  wa_absgr wa_waers.
* END JANE MOD

* 6. Se crean los registros en tabla Z de esta llamada RFC
  PERFORM f_crt_regs_tables USING     v_idrfc i_cab it_imp_dets[] wa_absgr wa_waers
                            CHANGING  wa_hdr it_det v_subrc wa_error_absgr.
  "si no se guarda info en tablas locales de log
  IF v_subrc NE 0.
    "se guarda info en main log
    PERFORM f_reg_exep_rfc USING      v_id_rfc 'Error al crear registros en tablas Z'
                           CHANGING   v_conse.
    "RAISE error_crt_reg_tables.
    v_jsubrc = 3.
    PERFORM f_return_msg  USING 0  'Error al crear registros en tablas Z' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


* START JANE MOD:
* 7. Se verifica error en tipo de estimación
  IF wa_error_absgr NE 0.
    "update estatus hoja de entrada
    PERFORM f_upd_ztables_invalid_info CHANGING wa_hdr it_det it_exp_msg[].
    "se guarda info en main log
    PERFORM f_reg_exep_rfc USING      v_id_rfc 'Error en estimacion'
                           CHANGING   v_conse.
    "RAISE error_estimate.
    v_jsubrc = 4.
    PERFORM f_return_msg  USING 0  'Error en estimacion' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


* 8. Validar tipo de contrato
  PERFORM f_valid_contract  USING     wa_absgr wa_hdr
                            CHANGING  v_subrc.
  IF v_subrc NE 0.
    "update estatus hoja de entrada
    PERFORM f_upd_ztables_invalid_info CHANGING wa_hdr it_det it_exp_msg[].
    "se guarda info en main log
    PERFORM f_reg_exep_rfc  USING v_id_rfc 'Tipo de contrato erroneo'
                            CHANGING v_conse.
    "RAISE error_type_contract.
    v_jsubrc = 5.
    PERFORM f_return_msg  USING 0  'Tipo de contrato erroneo' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.
* END JANE MOD

* 9. Se recupera Detalle de pedido con BAPI
  PERFORM f_get_po USING    wa_hdr-ebeln wa_hdr-idrfc
                   CHANGING wa_po_hdr it_po_item it_po_serv
                            it_po_accs v_subrc.
  IF v_subrc NE 0.
    "update estatus hoja de entrada
    PERFORM f_upd_ztables_invalid_info CHANGING wa_hdr it_det it_exp_msg[].
    "se guarda info en main log
    PERFORM f_reg_exep_rfc USING v_id_rfc 'Error al recuperar el detalle del pedido'
                          CHANGING v_conse.
    v_jsubrc = 6.
    PERFORM f_return_msg  USING 0  'Error al recuperar el detalle del pedido' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


* 10. Se valida que proveedor sea igual al del pedido
  PERFORM f_valid_lifnr USING wa_po_hdr-vendor i_cab-no_proveedor wa_hdr
                        CHANGING v_subrc.
  IF v_subrc NE 0.
    "update estatus hoja de entrada
    PERFORM f_upd_ztables_invalid_info CHANGING wa_hdr it_det it_exp_msg[].
    "se guarda info en main log
    PERFORM f_reg_exep_rfc USING v_id_rfc 'Proveedor enviado X WEB diferente al del Pedido'
                          CHANGING v_conse.
    "RAISE error_proveedor_ne_po.
    v_jsubrc = 7.
    PERFORM f_return_msg  USING 0  'Proveedor enviado X WEB diferente al del Pedido' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


* 11. Se valida si posiciones y servicios enviados por WEB existen en el PO.
  PERFORM f_valid_pos_service_po USING it_po_item it_po_serv it_det
                                 CHANGING v_subrc.
  IF v_subrc NE 0.
    "update estatus hoja de entrada
    PERFORM f_upd_ztables_invalid_info CHANGING wa_hdr it_det it_exp_msg[].
    "se guarda info en main log
    PERFORM f_reg_exep_rfc USING v_id_rfc 'Servicio no encontrado'
                         CHANGING v_conse.
    "RAISE error_pos_service_not_found.
    v_jsubrc = 8.
    PERFORM f_return_msg  USING 0  'Servicio no encontrado' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.
  CHECK v_jsubrc = 0.


* 12. Se crea por posicion del pedido y sus servicios la hoja de entrada
*--->>> RSDK908058 --->>>
*  perform f_process_po using wa_po_hdr it_po_item
*                             it_po_serv it_po_accs
*                             wa_absgr "jane mod
*                             i_hojas
*                       changing wa_hdr it_det it_exp_hojas[]
*                                it_exp_msg[] v_subrc.
*  if v_subrc ne 0.

*--->>>INICIO   RSDK910309 --->>>
*& 21-09-2018 - Se comenta llamado a rutina por peticion del usuario
*& derivado del doble de tiempo que tardaria en procesar una autorizacion
*--------------------------------------------------------------------------------
* Se realiza una ejecucion previa a la creacion de la hoja de entrada en modo TEST
* PERFORM f_process_po_1 USING  wa_po_hdr it_po_item
*                               it_po_serv it_po_accs
*                               wa_absgr
*                               i_moneda
*                               i_fecha_factura
*                               i_fecha_autorizacion
*                      CHANGING wa_hdr it_det it_exp_hojas[]
*                               it_exp_msg[] i_hojas v_subrc.

*Si la ejecucion en modo Test manda un error ya no procede con la generacion de la entrada de hoja.
* IF v_subrc NE 20.
*--->>> FIN RSDK910309 --->>>
 PERFORM f_process_po_2 USING  wa_po_hdr it_po_item
                               it_po_serv it_po_accs
                               wa_absgr
                               i_moneda
                               i_fecha_factura
                               i_fecha_autorizacion
*                               i_testrun             " INSERT ECN-20180903 OT: RSDK910309  Se quita de los parametros de entrada a peticion del usuario.
                      CHANGING wa_hdr it_det it_exp_hojas[]
                               it_exp_msg[] i_hojas v_subrc.
* ENDIF.

  IF v_subrc NE 0 AND v_subrc NE 20.
*<<<--- RSDK908058 <<<---
    "update estatus hoja de entrada
    PERFORM f_upd_ztables_invalid_info CHANGING wa_hdr it_det it_exp_msg[].
    "se guarda info en main log
    PERFORM f_reg_exep_rfc USING v_id_rfc 'No se encontro posicion indicada por WEB en este Pedido'
                         CHANGING v_conse.
    "RAISE error_pos_on_po_not_found.
    v_jsubrc = 9.
    PERFORM f_return_msg  USING 0  'No se encontro posicion indicada por WEB en este Pedido' v_jsubrc
                          CHANGING   it_exp_msg[].
  ENDIF.

  CHECK v_jsubrc = 0.

  "guardar info en hoja de entrada
  " perform f_upd_sheet_entry USING it_exp_hojas[].

  PERFORM f_reg_log_return   TABLES it_exp_hojas it_exp_msg
                             USING   v_id_rfc v_subrc
                             CHANGING v_conse.

ENDFUNCTION.
