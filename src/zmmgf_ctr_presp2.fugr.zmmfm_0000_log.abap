FUNCTION zmmfm_0000_log.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(NAMEPGR) TYPE  RS38L_FNAM
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_MSGBAPI) TYPE  BAPI_MSG
*"     VALUE(E_ID_REF) TYPE  ZID_REFERENCIA
*"  TABLES
*"      IT_PARAMS STRUCTURE  ZMMLOG_DET_RFC
*"----------------------------------------------------------------------

  DATA: v_id_ref    TYPE zid_referencia.

* Valida que se envien por lo menos 1 parametro
  IF it_params[] IS INITIAL.
    e_subrc = 01.
    e_msgbapi = 'Obligatorio especificar al menos 1 parametro'.
  ENDIF.
  CHECK e_subrc EQ 0.

*  Se crea el ID_REF
  PERFORM f_crt_id_ref CHANGING v_id_ref e_subrc e_msgbapi.
  CHECK e_subrc EQ 0.

  PERFORM f_crte_regs_tables TABLES it_params
                             USING v_id_ref namepgr
                             CHANGING e_id_ref e_subrc e_msgbapi.
*** --> Inicio BAF_RPM 23.10.2014 se comenta form
***  PERFORM f_auto_borrado_log.
*** --> Fin  BAF_RPM 23.10.2014 se comenta form

ENDFUNCTION.
