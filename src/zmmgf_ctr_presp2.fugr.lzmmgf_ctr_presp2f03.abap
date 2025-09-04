*----------------------------------------------------------------------*
***INCLUDE LZMMGF_CTR_PRESP2F03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CRTE_REGS_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_ID_REF  text
*      <--P_NAMEPGR  text
*      <--P_IT_PARAMS  text
*----------------------------------------------------------------------*
FORM f_crte_regs_tables  TABLES   pit_params STRUCTURE zmmlog_det_rfc
                         USING    p_id_ref   TYPE zid_referencia
                                  p_namepgr  TYPE rs38l_fnam
                         CHANGING p_id_ref2  TYPE zid_referencia
                                  p_subrc    TYPE subrc
                                  p_msgbapi  TYPE bapi_msg.
  DATA: lwa_log TYPE zreg_log,
        lit_det TYPE TABLE OF zlog_det WITH HEADER LINE.

  CLEAR: p_id_ref2, lwa_log, lit_det[], lit_det, p_subrc, p_msgbapi.



* Se prepara Reg. de Tabla LOG
  lwa_log-id_referencia = p_id_ref.
  lwa_log-namepgr       = p_namepgr.
  lwa_log-ernam         = sy-uname. "Usuario firmado en SAP
  lwa_log-aedat         = sy-datum. "Fecha actual en servidor SAP
  lwa_log-cputm         = sy-uzeit. "Hora actual del servidor SAP

* Se preparan regs. de tabla detalle
  LOOP AT pit_params.
    lit_det-id_referencia = p_id_ref.
    lit_det-conse         = pit_params-conse.
    lit_det-aedat         = lwa_log-aedat.
    lit_det-momnt         = 'I'. "Inicio de Ejecuci{on del RFC
    lit_det-type_param    = pit_params-type_param.
    lit_det-param         = pit_params-param.
    lit_det-val_param      = pit_params-val_param.
    lit_det-int2 = STRLEN( lit_det-val_param ).
    APPEND lit_det.

    CLEAR: pit_params, lit_det.
  ENDLOOP.

* Grabar en tablas Z
  INSERT zreg_log FROM lwa_log.
  IF sy-subrc EQ 0.
    INSERT zlog_det FROM TABLE lit_det.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
      p_id_ref2 = p_id_ref.
    ELSE.
      ROLLBACK WORK.
      p_subrc = 3.
      p_msgbapi = 'Error al grabar en tablas Z'.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    p_subrc = 3.
    p_msgbapi = 'Error al grabar en tablas Z'.
  ENDIF.

ENDFORM.                    " F_CRTE_REGS_TABLES
*&---------------------------------------------------------------------*
*&      Form  F_AUTO_BORRADO_LOG
*&---------------------------------------------------------------------*
***FORM f_auto_borrado_log .
***  DATA: lv_zchar TYPE zutil_parameters-zchar,
***        lv_fch TYPE sy-datum.
***
***  SELECT SINGLE zchar FROM zutil_parameters INTO lv_zchar
***  WHERE zreport EQ 'ZMMFM_0000_LOG'
***  AND   zfield  EQ 'DIAS_BORRAR_LOG'.
***  IF sy-subrc NE 0.
***    lv_zchar = 35.
***  ENDIF.
***
***  lv_fch = sy-datum - lv_zchar.
***
***  SELECT SINGLE aedat FROM zreg_log INTO lv_fch
***  WHERE aedat LE lv_fch.
***  CHECK sy-subrc EQ 0.
***
***  DELETE FROM zreg_log WHERE aedat LE lv_fch.
***  IF sy-subrc EQ 0.
***    DELETE FROM zlog_det WHERE aedat LE lv_fch.
***    IF sy-subrc EQ 0.
***      COMMIT WORK AND WAIT.
***    ELSE.
***      ROLLBACK WORK.
***    ENDIF.
***  ENDIF.
***ENDFORM.                    " F_AUTO_BORRADO_LOG
*&---------------------------------------------------------------------*
*&      Form  F_REG_PARAMS_FIN_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PARAMS  text
*      -->P_I_ID_REF  text
*----------------------------------------------------------------------*
FORM f_reg_params_fin_log  TABLES   pit_params STRUCTURE zmmlog_det_rfc
                           USING    p_ref TYPE zid_referencia.
  DATA lit_det TYPE TABLE OF zlog_det WITH HEADER LINE.

  CLEAR: pit_params, lit_det[], lit_det.



* Se preparan regs. de tabla detalle
  LOOP AT pit_params.
    lit_det-id_referencia = p_ref.
    lit_det-conse         = pit_params-conse.
    lit_det-aedat         = sy-datum.
    lit_det-momnt         = 'F'. "FIN de Ejecuci{on del RFC
    lit_det-type_param    = pit_params-type_param.
    lit_det-param         = pit_params-param.
    lit_det-val_param      = pit_params-val_param.
    lit_det-int2 = STRLEN( lit_det-val_param ).
    APPEND lit_det.

    CLEAR: pit_params, lit_det.
  ENDLOOP.

  INSERT zlog_det FROM TABLE lit_det.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " F_REG_PARAMS_FIN_LOG
