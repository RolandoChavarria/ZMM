*----------------------------------------------------------------------*
***INCLUDE LZMMGF_CTR_PRESP2F02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CRT_ID_REF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_ID_REF  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_crt_id_ref  CHANGING p_v_id_ref  type zid_referencia
                            p_v_subrc   type subrc
                            p_v_msgbapi TYPE bapi_msg.

  DATA: lv_max TYPE zid_referencia,
        lv_id TYPE zid_referencia.

  CLEAR: p_v_id_ref, p_v_subrc, p_v_msgbapi, lv_max, lv_id.

  CONCATENATE sy-datum sy-uzeit '00' INTO p_v_id_ref.
  lv_max = p_v_id_ref + 99.

**
* >>>INICIO OT 201405212044525  Mejora Perf RFC de Vivienda
*  data lit_det TYPE TABLE OF zlog_det.
*
*  select * from zlog_det into TABLE lit_det.
* >>>FIN OT 201405212044525   Mejora Perf RFC de Vivienda
**
  SELECT SINGLE id_referencia FROM zreg_log INTO lv_id
  WHERE id_referencia = p_v_id_ref.
  IF sy-subrc EQ 0.
    DO 1000 TIMES.
      ADD 1 TO p_v_id_ref.
      SELECT SINGLE id_referencia FROM zreg_log INTO lv_id
      WHERE id_referencia = p_v_id_ref.
      IF sy-subrc NE 0.
        EXIT.
      ELSEIF p_v_id_ref EQ lv_max.
        WAIT UP TO 1 SECONDS.
        CONCATENATE sy-datum sy-uzeit '00' INTO p_v_id_ref.
        lv_max = p_v_id_ref + 99.
      ELSEIF sy-index EQ 1000.
        p_v_subrc = 2. "Error al crear ID de referencia
        p_v_msgbapi = 'Error al calcular ID referencia'.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.                  " F_CRT_ID_REF
