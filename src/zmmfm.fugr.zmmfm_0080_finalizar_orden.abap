FUNCTION zmmfm_0080_finalizar_orden.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_EBELN) TYPE  EBELN
*"     VALUE(I_LIFNR) TYPE  LIFNR
*"     VALUE(I_FOLIO) TYPE  IHREZ
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"  TABLES
*"      IT_IMP_POSITIONS STRUCTURE  ZMMWA_0080_POSITIONS
*"      IT_EXP_MSG STRUCTURE  ZMMWA_0030_EXP_MSG
*"      IT_RETURN STRUCTURE  ZRETURN
*"----------------------------------------------------------------------
*Finaliza Orden de Servicios

  DATA: it_update_positions TYPE STANDARD TABLE OF zmmwa_0080_positions.
  REFRESH:it_update_positions.

* 1. Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_conexion3 TABLES it_imp_positions
                              USING 'ZMMFM_0080_FINALIZAR_ORDEN' i_ebeln i_lifnr i_folio
                              CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK v_subrc = 0.


* 2. Valida Proveedor - Pedido
  PERFORM validate_lifnr USING i_ebeln i_lifnr CHANGING e_subrc e_bapi_msg.
  CHECK e_subrc IS INITIAL.


* 3. Valida posiciones
  PERFORM validate_pos USING i_ebeln CHANGING it_imp_positions[] it_update_positions[] it_exp_msg[].
  CHECK it_update_positions[] IS NOT INITIAL.


  "4. Actualiza posiciones
  PERFORM update_pos USING i_ebeln i_folio
                     CHANGING it_imp_positions[]
                              it_update_positions[]
                              it_exp_msg[]
                              it_return[]
                              e_subrc
                              e_bapi_msg.
  IF it_return[] IS NOT INITIAL.
    SORT it_return[] BY zsy_subrc zposicion.
    DELETE ADJACENT DUPLICATES FROM it_return COMPARING ALL FIELDS.
  ENDIF.

ENDFUNCTION.
