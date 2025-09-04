FUNCTION ZMMFM_0020_PENALIZACION_PO1.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_VENDOR) TYPE  LIFNR
*"     VALUE(I_PUR_GRP) TYPE  BKGRP
*"     VALUE(I_PROYECT) TYPE  EVERK
*"     VALUE(I_FOLIO) TYPE  IHREZ
*"     VALUE(I_TEST) TYPE  BAPIFLAG-BAPIFLAG OPTIONAL
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
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

* Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_conexion USING 'ZMMFM_0020_PENALIZACION_PO' i_vendor
                                      i_pur_grp i_proyect i_folio
                                      t_azules t_rojas i_test
                             CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.


  PERFORM main_pen USING i_vendor i_pur_grp i_proyect i_folio
                     t_azules t_rojas i_test
            CHANGING e_subrc  e_bapi_msg .


ENDFUNCTION.
                                                            " MAIN1
