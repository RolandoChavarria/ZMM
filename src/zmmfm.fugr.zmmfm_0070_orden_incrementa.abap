FUNCTION zmmfm_0070_orden_incrementa.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_VENDOR) TYPE  LIFNR
*"     VALUE(I_PUR_GRP) TYPE  BKGRP
*"     VALUE(I_PROYECT) TYPE  EVERK
*"     VALUE(I_FOLIO) TYPE  IHREZ
*"     VALUE(I_TEST) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"     VALUE(I_OBJETO1) TYPE  ZMMDE_OB_PED
*"     VALUE(I_OBJETO2) TYPE  ZMMDE_OB_PED
*"     VALUE(I_ID) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"  TABLES
*"      T_RETURN TYPE  BAPIRET2_T
*"  CHANGING
*"     VALUE(T_AZULES) TYPE  ZMMFM_0070_AZUL_TT
*"     VALUE(T_ROJAS) TYPE  ZMMFM_0070_ROJA_TT
*"----------------------------------------------------------------------
*1.	Solicitud  de requerimiento Se requiere desarrollar RFC que genere una Orden de Cambio Incremental de
* servicios de tipo ZOCI; este documento de compra será utilizado como un nuevo pedido de servicios para
* extender servicios con un proveedor.
*2.	Especificación Funcional Se requiere generar una interface de WEB a SAP interactúe en relación a la
* información de Ordenes de Compra de Servicios. El origen es WEB GGI y el destino es SAP RE
*Se conceptualiza que los envíos de información será mediante el evento de autorización para incremental un
* servicio en WEB, por ello el programa a desarrollar debe considerar los siguientes parámetro: no. proveedor,
*   centro, monto del servicio, grupo de  compras y proyecto
*&---------------------------------------------------------------------*
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Angélica González
*& Fecha            : 20-FEBRERO-2014
*& Objetivo         : Se requiere modificar el siguiente
*&                    ZMMFM_0070_ORDEN_INCREMENTA para enviar de WEB a
*&                    SAP el texto  del objeto  del pedido en ordenes de
*&                    cambio incrementales ya que antes el programa colocaba
*&                    como texto la primera partida azul del pedido
*&                    incremental.
*& Transporte       : RSDK907015 y RSDK907051.
*&---------------------------------------------------------------------*
  g_id = i_id.
* Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_conexion USING 'ZMMFM_0070_ORDEN_INCREMENTA' i_vendor
                                      i_pur_grp i_proyect i_folio
                                      t_azules t_rojas i_test
                             CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.


  PERFORM main USING i_vendor i_pur_grp i_proyect i_folio
                     t_azules t_rojas i_test
*--> Inicia Modificación RSDK907015 - 1.
*& IJOF : Se agregan parametros import.
                     i_objeto1 i_objeto2
*<-- Fin Modificación RSDK907015 - 1.
            CHANGING e_subrc  e_bapi_msg .

  t_return[] = it_return[].
ENDFUNCTION.
                                                            " MAIN1
