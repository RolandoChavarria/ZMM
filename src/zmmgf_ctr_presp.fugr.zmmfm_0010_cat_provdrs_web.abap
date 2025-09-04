FUNCTION zmmfm_0010_cat_provdrs_web.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_LIFNR) TYPE  LFA1-LIFNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_IT_CATPROV) TYPE  ZMMIT_0010_CAT_PROV
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"----------------------------------------------------------------------
* Desarrollador: Omar Romero - BAF
* Fecha: 04-Sep-2013
* Funcional Yuridia Flores
* Objetivo: Debido a que se estan manejando proveedores numericos y
*   Alfanumericos, se requiere poder controlar la excepcion generada
*   al momento de rellenar de ceros el proveedor numerico.¨
* Transporte:  RSDK906676
*"----------------------------------------------------------------------
  DATA: wa_lfa1 TYPE lfa1, "Estructura
        wa_knvk TYPE knvk, "Estructura
        wa_prov TYPE zmmwa_0010_cat_prov, "Estructura
        wa_adr6 TYPE adr6,
        it_lfa1 TYPE TABLE OF lfa1,"Internal table
        it_knvk TYPE TABLE OF knvk,"Internal table
        it_adr6 TYPE TABLE OF adr6,
        v_ekorg TYPE ekorg, "Variable para Org. de compras
        v_err_lfm1 TYPE flag,  "Variable para detectar error en LFM1
        v_subrc TYPE  subrc,   "Variable de subrc para LOG
        v_id_rfc TYPE zid_referencia.
  DATA  v_conse TYPE zmmde_conse. "Para llevar el consecutivo de cada Param

  CLEAR: wa_lfa1, wa_knvk, wa_prov, wa_adr6, it_lfa1, it_knvk, it_adr6,
         v_ekorg, v_err_lfm1, v_id_rfc, v_conse.
* --->>> RSDK906676 --->>> 1
*  UNPACK i_lifnr TO i_lifnr.
  TRY.
      UNPACK i_lifnr TO i_lifnr.
    CATCH cx_sy_conversion_no_number.
      "No hacer nada p/manejar el String alfanumerica como lo envia WEB
  ENDTRY.
* <<<--- RSDK906676 <<<--- 1
* Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_conexion3 USING 'ZMMFM_0010_CAT_PROVDRS_WEB' i_lifnr
                             CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.

* Se recupera la organizacion de compras para los proveedores a recuperar
  PERFORM f_get_ekorg CHANGING v_ekorg.
  IF sy-subrc <> 0.
    e_subrc = '02'.
    e_bapi_msg = 'Config. ekorg no existe'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.


  IF i_lifnr IS INITIAL OR i_lifnr EQ '0000000000'. "Si no envia proveedor
    PERFORM f_get_all_lifnr USING v_ekorg
                            CHANGING it_lfa1 it_knvk it_adr6.

    IF it_lfa1 IS INITIAL.
      e_subrc = '03'.
      e_bapi_msg = 'No se encontraron Proveedores'.
      PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
    ENDIF.
    CHECK e_subrc = 0.

    PERFORM f_fill_prov_all USING it_lfa1 it_knvk it_adr6
                            CHANGING e_it_catprov.

  ELSE. " Si envio proveedor
    PERFORM f_get_single_lifnr USING i_lifnr v_ekorg
                               CHANGING wa_lfa1 wa_knvk wa_adr6 v_err_lfm1.
    IF wa_lfa1 IS INITIAL.
      e_subrc = '04'.
      e_bapi_msg = 'No se encontraron Proveedores'.
      PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
      CHECK e_subrc = 0.
    ELSEIF v_err_lfm1 EQ 'X'.
      e_subrc = '05'.
      e_bapi_msg = 'Informacion de Proveedor en LFM1 Incompleta'.
      PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
    ENDIF.
    CHECK e_subrc = 0.

    PERFORM f_fill_prov_single USING wa_lfa1 wa_knvk wa_adr6
                               CHANGING wa_prov.
    APPEND wa_prov TO e_it_catprov.
  ENDIF.
  IF sy-subrc = 0.
    e_subrc = '00'.
    e_bapi_msg = 'Registros Entrados'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.

  PERFORM f_reg_log_return USING e_it_catprov v_id_rfc
                           CHANGING v_conse.


ENDFUNCTION.
