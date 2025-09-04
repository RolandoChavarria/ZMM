FUNCTION ZMMFM_0080_ORDEN_DECREMENTA.
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
*"----------------------------------------------------------------------
* Generear orde decrmenta

  PERFORM f_reg_log_conexion2 USING 'ZMMFM_0080_ORDEN_DECREMENTA' i_vendor
                                      i_pur_grp i_proyect i_folio
                                      t_azules i_test
                             CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.

  PERFORM main_decrementa USING i_vendor i_pur_grp i_proyect i_folio
                     T_AZULES I_TEST
            CHANGING e_subrc  e_bapi_msg .


ENDFUNCTION.
                                                            " MAIN1
