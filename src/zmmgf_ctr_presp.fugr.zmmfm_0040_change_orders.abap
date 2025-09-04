FUNCTION zmmfm_0040_change_orders.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      IT_CHANGE_ORDERS TYPE  ZMMTT_0040_CHOR
*"  EXCEPTIONS
*"      ERROR_NO_CHANGE_ORDRS_FOUND
*"      RECORDS_ARE_INCOMPLETE
*"      DATA_NO_VALID
*"      ERROR_TO_SAVE_ON_TABLE_SAP
*"      ERROR_REG_LOG
*"----------------------------------------------------------------------
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Angélica González
*& Fecha            : 20-FEBRERO-2014
*& Objetivo         : Se requiere modificar el siguiente RFC
*&                    ZMMFM_0040_CHANGE_ORDERS para enviar de WEB a SAP
*&                    el texto  del objeto  del pedido en ordenes de cambio
*&                    incrementales ya que antes el programa colocaba como
*&                    texto la primera partida azul del pedido incremental.
*& Transporte       : RSDK907015.
*&---------------------------------------------------------------------*
  DATA:  wa_change LIKE LINE OF it_change_orders,
         lv_subrc   TYPE sy-subrc,
         v_subrc    TYPE  subrc,   "Variable de subrc para LOG
         v_bapi_msg TYPE bapi_msg,
         v_id_rfc   TYPE zid_referencia,
         v_conse    TYPE zmmde_conse. "Para llevar el consecutivo de cada Parametro
  CLEAR: lv_subrc, v_subrc, v_id_rfc, v_conse.

* Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_con TABLES it_change_orders
                        USING 'ZMMFM_0040_CHANGE_ORDERS'
                        CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc NE 0.
    wa_change-bapi_msg = 'Error al crear registro en tabla REG_LOG'.
    wa_change-subrc = 01.
    MODIFY it_change_orders FROM wa_change TRANSPORTING bapi_msg subrc
    WHERE subrc EQ '00'
       OR subrc NE '00'.
  ENDIF.
  CHECK v_subrc = 0.

  IF it_change_orders[] IS INITIAL.
    v_subrc = 02.
    wa_change-subrc = 02.
    wa_change-bapi_msg = 'No se encontraron ordenes de cambio por grabar.'.
    MODIFY it_change_orders FROM wa_change TRANSPORTING bapi_msg subrc
    WHERE subrc EQ '00'
       OR subrc NE '00'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc wa_change-subrc
                                  wa_change-bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK v_subrc EQ 0.

  PERFORM f_val_orders        TABLES  it_change_orders
                              CHANGING v_subrc.
  IF v_subrc NE 0.
    v_bapi_msg = 'Error(es) es validaciones de tabla'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc v_subrc v_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK v_subrc = 0.

  PERFORM f_val_values TABLES it_change_orders
                              CHANGING v_subrc.
  IF v_subrc NE 0.
    v_bapi_msg = 'Error(es) en validaciones de valores'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc v_subrc v_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK v_subrc = 0.

  PERFORM f_insrt_on_table TABLES it_change_orders
                           using   v_id_rfc
                           CHANGING v_subrc
                                    v_conse.

  PERFORM f_reg_log_return3  TABLES it_change_orders
                             USING v_id_rfc
                             CHANGING v_conse.

   IF v_subrc NE 0.
    v_bapi_msg = 'Error al grabar en SAP'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc v_subrc v_bapi_msg
                                CHANGING v_conse.
  ENDIF.
  CHECK v_subrc = 0.

ENDFUNCTION.
