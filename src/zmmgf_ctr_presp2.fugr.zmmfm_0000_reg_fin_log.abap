FUNCTION zmmfm_0000_reg_fin_log.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_ID_REF) TYPE  ZID_REFERENCIA
*"  TABLES
*"      IT_PARAMS STRUCTURE  ZMMLOG_DET_RFC
*"----------------------------------------------------------------------

  CHECK it_params[] IS NOT INITIAL.

  PERFORM f_reg_params_fin_log TABLES it_params USING i_id_ref.

ENDFUNCTION.
