FUNCTION zmmfm_0050_conf_oc .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_ID_ENV) TYPE  ZID_REFERENCIA
*"  EXPORTING
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"  TABLES
*"      IT_PEDIDOS STRUCTURE  ZMMWA_0040_RET_PED
*"----------------------------------------------------------------------

  DATA:  wa_header_log TYPE zmmtt_header_log."Estructura
  DATA:  it_detail_log TYPE TABLE OF zmmtt_detail_log."iTab

  PERFORM f_comp_header USING i_id_env
                        CHANGING wa_header_log
                                 e_bapi_msg
                                 e_subrc.
  CHECK e_subrc eq 0.  "Terminamos ejec de RFC en caso de SUBRC NE o0

  PERFORM f_rec_details TABLES   it_detail_log
                                 it_pedidos
                        USING    i_id_env
                        CHANGING e_bapi_msg
                                 e_subrc.
  CHECK e_subrc eq 0.

  PERFORM f_upd_log TABLES it_detail_log
                    CHANGING wa_header_log
                             e_bapi_msg
                             e_subrc.



ENDFUNCTION.
