FUNCTION zmmfm_0090_disponible.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_PROYECT) TYPE  EVERK
*"     VALUE(I_AZUL) TYPE  MATKL
*"  EXPORTING
*"     VALUE(E_AZUL) TYPE  MATKL
*"     VALUE(E_PRICE) TYPE  NETWR
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  PERFORM f_reg_log_conexion_disp USING 'ZMMFM_0090_DISPONIBLE' i_proyect i_azul
                             CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.



  PERFORM main_disponible USING i_proyect i_azul
                          CHANGING e_azul e_price e_subrc  e_bapi_msg .


ENDFUNCTION.
                                                            " MAIN1
