*----------------------------------------------------------------------*
***INCLUDE LZMMGF_CTR_PRESP2F04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IMP_DETS  text
*      -->P_0066   text
*      -->P_I_CAB  text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion  TABLES   p_it_imp_dets TYPE    zmmit_0030_hjsrv_d
                         USING    p_pgrm        TYPE    rs38l_fnam
                                  p_i_cab       TYPE    zmmwa_0030_hjsrv_h
                         CHANGING p_id_rfc      TYPE    zid_referencia
                                  p_conse       TYPE    zmmde_conse
                                  p_v_subrc     TYPE    subrc.


  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: lv_zflag TYPE zutil_parameters-zchar.
  DATA: wa_it_imp_dets LIKE LINE OF p_it_imp_dets.
*  DATA: v_monto_fac(15) TYPE c.    "JANE
  DATA: v_avance_p(10) TYPE c VALUE ''.                     "1234.5
  DATA: v_avance_c(20) TYPE c VALUE ''.   "1,123,123,123.000


  CLEAR: wa_params, it_params, p_conse, p_v_subrc, p_it_imp_dets.

  SELECT SINGLE zflag FROM zutil_parameters INTO lv_zflag
   WHERE zreport EQ 'ZMMFM_0000_LOG'
   AND   zfield  EQ 'ACTIVATE_LOG'.

  CHECK lv_zflag IS NOT INITIAL.

* Agregar Parametro
  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_CAB'. "Nombre del parametro en el RFC
*  WRITE: p_i_cab-monto_factura TO v_monto_fac.   "JANE
  CONCATENATE '|' p_i_cab-no_pedido '|' p_i_cab-no_proveedor
*              '|' p_i_cab-factura '|' p_i_cab-desc_factura "JANE
*              '|' p_i_cab-fecha_factura '|' v_monto_fac    "JANE
              INTO wa_params-val_param.
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
*  CLEAR: wa_params, v_monto_fac.   "JANE
  CLEAR:  wa_params.


  IF p_it_imp_dets IS INITIAL.
    ADD 1 TO p_conse.
    wa_params-conse = p_conse.
    wa_params-type_param = 'I'.
    wa_params-param = 'IT_IMP_DETS'.
    wa_params-val_param = ''.
    APPEND wa_params TO it_params.

    LOOP AT p_it_imp_dets INTO wa_it_imp_dets.
      ADD 1 TO p_conse.
      wa_params-conse = p_conse.
      wa_params-type_param = 'I'.
      wa_params-param = 'IT_IMP_DETS'.

      WRITE: wa_it_imp_dets-avance_estima_p TO v_avance_p.
      WRITE: wa_it_imp_dets-avance_estima_c TO v_avance_c.

      CONCATENATE '|' wa_it_imp_dets-no_posicion '|' wa_it_imp_dets-id_servicio
                  '|' v_avance_p '|' v_avance_c                                     "jane
                  '|' wa_it_imp_dets-referencia_web '|' wa_it_imp_dets-no_factura   "jane
                  '|' wa_it_imp_dets-rango_estima '|' wa_it_imp_dets-no_linea
                  INTO wa_params-val_param.         "jane

      APPEND wa_params TO it_params.
      CLEAR: wa_params,  wa_it_imp_dets.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'ZMMFM_0000_LOG'
    EXPORTING
      namepgr   = p_pgrm
    IMPORTING
      e_subrc   = p_v_subrc
      e_id_ref  = p_id_rfc
    TABLES
      it_params = it_params.
ENDFORM.                    " F_REG_LOG_CONEXION
*&---------------------------------------------------------------------*
*&      Form  F_REG_EXEP_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_ID_RFC  text
*      -->P_0091   text
*      <--P_V_CONSE  text
*----------------------------------------------------------------------*

FORM f_reg_exep_rfc  USING  p_id_rfc    TYPE zid_referencia
                            p_val_param   TYPE string
                   CHANGING p_conse       TYPE zmmde_conse.

  DATA: lwa_det TYPE zmmlog_det_rfc,
        lit_det TYPE TABLE OF zmmlog_det_rfc.

  CLEAR: lwa_det, lit_det.

  CHECK p_id_rfc IS NOT INITIAL.

  ADD 1 TO p_conse.
  lwa_det-conse      = p_conse.
  lwa_det-type_param = 'E'.
  lwa_det-param      = 'EXEPCION'.
  lwa_det-val_param  = p_val_param.
  APPEND lwa_det TO lit_det.

  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_id_rfc
    TABLES
      it_params = lit_det.


ENDFORM.                    " F_REG_EXEP_RFC
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_RETURN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXP_HOJAS  text
*      -->P_ERROR_REG_LOG  text
*----------------------------------------------------------------------*
FORM f_reg_log_return  TABLES   p_it_exp_hojas  STRUCTURE zmmwa_0030_exp_hoja
                                p_it_exp_msg    STRUCTURE zmmwa_0030_exp_msg
                       USING    p_v_id_rfc         TYPE   zid_referencia
                                v_subrc            type   subrc
                       CHANGING p_v_conse          TYPE   zmmde_conse.

  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: wa_it_exp_hojas LIKE LINE OF p_it_exp_hojas.
  DATA: wa_it_exp_msg   LIKE LINE OF p_it_exp_msg.

  CLEAR: wa_params, it_params, wa_it_exp_hojas, wa_it_exp_msg.

  CHECK p_v_id_rfc IS NOT INITIAL.

  IF p_it_exp_hojas IS INITIAL.
    ADD 1 TO p_v_conse.
    wa_params-conse = p_v_conse.
    wa_params-type_param = 'E'.
    wa_params-param = 'IT_EXP_HOJAS'.
    wa_params-val_param = ''.
    APPEND wa_params TO it_params.

    LOOP AT p_it_exp_hojas INTO wa_it_exp_hojas.
      ADD 1 TO p_v_conse.
      wa_params-conse = p_v_conse.
      wa_params-type_param = 'E'.
      wa_params-param = 'IT_EXP_HOJAS'.
      CONCATENATE '|' wa_it_exp_hojas-no_posicion '|' wa_it_exp_hojas-id_servicio
      '|' wa_it_exp_hojas-hoja_entrada '|' INTO wa_params-val_param.
      APPEND wa_params TO it_params.
      CLEAR: wa_params, wa_it_exp_hojas.
    ENDLOOP.
  ENDIF.

  IF p_it_exp_msg IS INITIAL.
    ADD 1 TO p_v_conse.
    wa_params-conse = p_v_conse.
    wa_params-type_param = 'E'.
    wa_params-param = 'IT_EXP_MSG'.
    wa_params-val_param = ''.
    APPEND wa_params TO it_params.

    LOOP AT p_it_exp_msg INTO wa_it_exp_msg.
      ADD 1 TO p_v_conse.
      wa_params-conse = p_v_conse.
      wa_params-type_param = 'E'.
      wa_params-param = 'IT_EXP_MSG'.
      CONCATENATE '|' wa_it_exp_msg-no_posicion '|' wa_it_exp_msg-mensaje
       '|' wa_it_exp_msg-subrc '|' INTO wa_params-val_param.
      APPEND wa_params TO it_params.
      CLEAR: wa_params, wa_it_exp_msg.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_v_id_rfc
    TABLES
      it_params = it_params.
ENDFORM.                    " F_REG_LOG_RETURN
