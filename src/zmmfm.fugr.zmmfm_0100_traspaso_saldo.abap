FUNCTION zmmfm_0100_traspaso_saldo.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_PROYECT) TYPE  EVERK
*"     VALUE(I_FOLIO) TYPE  IHREZ
*"     VALUE(ORIGEN) TYPE  MATKL
*"     VALUE(RECEPT) TYPE  MATKL
*"     VALUE(MONTO) TYPE  BPAK-WERT
*"     VALUE(MONEDA) TYPE  WAERS
*"     VALUE(ANIO) TYPE  GJAHR
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  PERFORM f_reg_log_conexion_disp USING 'ZMMFM_0100_TRASPASO_SALDO'
        i_proyect origen CHANGING v_id_rfc v_conse v_subrc.

  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.
* tomar valores para no modificar mas codifo
  DATA: lsa TYPE zmmfm_0070_azul,
        t_azul_r TYPE zmmfm_0070_azul_tt,
        i_azul_e TYPE  zmmfm_0070_azul.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
    EXPORTING
      input  = i_proyect
    IMPORTING
      output = i_proyect.

  REFRESH t_azul_r.
  lsa-matkl = recept.
  lsa-netwr = monto.
  lsa-werks = i_proyect+6(4).
  SHIFT  lsa-werks LEFT DELETING LEADING '0'.
  APPEND lsa TO  t_azul_r.

  i_azul_e-matkl = origen.
  i_azul_e-netwr = monto.
  i_azul_e-werks = i_proyect+6(4).
  SHIFT  i_azul_e-werks LEFT DELETING LEADING '0'.
  PERFORM main_traspaso_saldo USING i_proyect i_azul_e t_azul_r monto
                                    anio moneda i_folio
                          CHANGING  e_subrc e_bapi_msg .


ENDFUNCTION.
                                                            " MAIN1
