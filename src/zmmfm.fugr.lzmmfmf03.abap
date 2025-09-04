*----------------------------------------------------------------------*
***INCLUDE LZMMFMF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Rurinas para Log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0006   text
*      -->P_I_VENDOR  text
*      -->P_I_PUR_GRP  text
*      -->P_I_PROYECT  text
*      -->P_I_FOLIO  text
*      -->P_T_AZULES  text
*      -->P_T_ROJAS  text
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0006   text
*      -->P_I_VENDOR  text
*      -->P_I_PUR_GRP  text
*      -->P_I_PROYECT  text
*      -->P_I_FOLIO  text
*      -->P_T_AZULES  text
*      -->P_T_ROJAS  text
*      -->P_I_TEST  text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion  USING    p_pgrm     TYPE rs38l_fnam
                                  p_lifnr    TYPE lifnr
                                  p_ekgrp    TYPE bkgrp
                                  p_proye    TYPE everk
                                  p_folio    TYPE ihrez
                                  "Tablas de posi y servic
                                  t_items    TYPE zmmfm_0070_azul_tt
                                  t_servi    TYPE zmmfm_0070_roja_tt
                                  p_test     TYPE bapiflag-bapiflag
                         CHANGING p_id_rfc   TYPE zid_referencia
                                  p_conse    TYPE zmmde_conse
                                  p_v_subrc  TYPE subrc.

  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: lv_zflag TYPE zutil_parameters-zchar.
  DATA: wa_par_azul   LIKE LINE OF  t_items.
  DATA: wa_par_roja   LIKE LINE OF  t_servi.
  DATA: lv_string     TYPE string.
  CLEAR: wa_params, it_params, p_conse, p_v_subrc.

  SELECT SINGLE zflag FROM zutil_parameters INTO lv_zflag
   WHERE zreport EQ 'ZMMFM_0000_LOG'
   AND   zfield  EQ 'ACTIVATE_LOG'.

  CHECK lv_zflag IS NOT INITIAL.


* Agregar Parametro import
  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_VENDOR'. "Nombre del parametro en el RFC
  wa_params-val_param = p_lifnr. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_PUR_GRP'. "Nombre del parametro en el RFC
  wa_params-val_param = p_ekgrp. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_PROYECT'. "Nombre del parametro en el RFC
  wa_params-val_param = p_proye. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_FOLIO'. "Nombre del parametro en el RFC
  wa_params-val_param = p_folio. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_TEST'. "Nombre del parametro en el RFC
  wa_params-val_param = p_test. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

* t_azules t_rojas i_test


  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES it_params USING p_conse 'M' 'T_AZULES' ''.

  LOOP AT t_items INTO wa_par_azul.
    PERFORM f_string_log USING 'ZMMFM_0070_AZUL' wa_par_azul
                         CHANGING lv_string.
    ADD 1 TO p_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_conse 'M' 'T_AZULES' lv_string.

    CLEAR: wa_par_azul, lv_string.
  ENDLOOP.



  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES it_params USING p_conse 'M' 'T_ROJAS' ''.

  LOOP AT t_servi INTO wa_par_roja.
    PERFORM f_string_log USING 'ZMMFM_0070_ROJA' wa_par_roja
                         CHANGING lv_string.
    ADD 1 TO p_conse.
    PERFORM f_add_t_params TABLES it_params
                          USING p_conse 'M' 'T_ROJAS' lv_string.


    CLEAR: wa_par_azul, lv_string.
  ENDLOOP.


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
*&      Form  F_ADD_T_PARAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PARAMS  text
*      -->P_P_V_CONSE  text
*      -->P_0202   text
*      -->P_0203   text
*      -->P_0204   text
*----------------------------------------------------------------------*
FORM f_add_t_params  TABLES   pt_params STRUCTURE zmmlog_det_rfc
                     USING    p_conse
                              p_type
                              p_param
                              p_value.
  DATA ls_params TYPE zmmlog_det_rfc.
  DATA v_string TYPE string.

  CLEAR: ls_params, v_string.

  v_string = p_value.
  CONDENSE v_string.

  ls_params-conse = p_conse.
  ls_params-type_param = p_type.
  ls_params-param = p_param.
  ls_params-val_param = v_string.
  APPEND ls_params TO pt_params.
ENDFORM.                    " F_ADD_T_PARAMS
*&---------------------------------------------------------------------*
*&      Form  F_STRING_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0272   text
*      -->P_WA_PAR_ROJA  text
*      <--P_LV_STRING  text
*----------------------------------------------------------------------*
FORM f_string_log  USING    p_struc  TYPE dd02l-tabname
                            p_val    TYPE any
                   CHANGING p_string TYPE string.
  DATA: fcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        lv_fld TYPE string,
        lv_s TYPE string.
  FIELD-SYMBOLS <fld> TYPE ANY.

  CLEAR: fcat[], fcat, lv_fld, lv_s.
  UNASSIGN <fld>.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_struc
      i_client_never_display = ''
    CHANGING
      ct_fieldcat            = fcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT fcat.
    CONCATENATE 'P_VAL-' fcat-fieldname INTO lv_fld.
    ASSIGN (lv_fld) TO <fld>.
    IF <fld> IS ASSIGNED.
      IF <fld> EQ ' '.
        lv_s = 'Vacio'.
      ELSE.
        lv_s = <fld>.
      ENDIF.
      CONCATENATE p_string fcat-fieldname '=' lv_s '|' INTO p_string.
    ENDIF.
    CLEAR: fcat, lv_fld, lv_s.
    UNASSIGN <fld>.
  ENDLOOP.


ENDFORM.                    " F_STRING_LOG
*&---------------------------------------------------------------------*
*&      Form  F_REG_EXEP_RFC_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_ID_RFC  text
*      -->P_E_SUBRC  text
*      -->P_E_BAPI_MSG  text
*      <--P_V_CONSE  text
*----------------------------------------------------------------------*
FORM f_reg_exep_rfc_msg  USING p_id_rfc    TYPE zid_referencia
                               p_subrc     TYPE subrc
                               p_msg       TYPE bapi_msg
                     CHANGING  p_conse     TYPE zmmde_conse.
  DATA: lwa_det TYPE zmmlog_det_rfc,
        lit_det TYPE TABLE OF zmmlog_det_rfc.

  CLEAR: lwa_det, lit_det.

  CHECK p_id_rfc IS NOT INITIAL.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_SUBRC' p_subrc.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_BAPI_MSG' p_msg.

  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_id_rfc
    TABLES
      it_params = lit_det.
ENDFORM.                    " F_REG_EXEP_RFC_MSG
*&---------------------------------------------------------------------*
*&      Form  F_DISPONIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_RPSCO  text
*----------------------------------------------------------------------*
FORM f_disponible  TABLES t_rpsco STRUCTURE rpsco
                   using wlp01
                         wlp02.
  data: wa_aux type rpsco.

* Get real
    LOOP AT t_rpsco INTO wa_aux WHERE vorga EQ 'COIN' OR vorga EQ 'RMBE' OR vorga EQ 'KCOM'.
      IF wa_aux-vorga EQ 'COIN'.
        ADD: wa_aux-wlp01 TO wlp01,
             wa_aux-wlp02 TO wlp01,
             wa_aux-wlp03 TO wlp01,
             wa_aux-wlp04 TO wlp01,
             wa_aux-wlp05 TO wlp01,
             wa_aux-wlp06 TO wlp01,
             wa_aux-wlp07 TO wlp01,
             wa_aux-wlp08 TO wlp01,
             wa_aux-wlp09 TO wlp01,
             wa_aux-wlp10 TO wlp01,
             wa_aux-wlp11 TO wlp01,
             wa_aux-wlp12 TO wlp01,
             wa_aux-wlp13 TO wlp01,
             wa_aux-wlp14 TO wlp01,
             wa_aux-wlp15 TO wlp01,
             wa_aux-wlp16 TO wlp01.
* Get comminment
      ELSEIF wa_aux-vorga EQ 'RMBE' OR wa_aux-vorga EQ 'KCOM'.
        ADD: wa_aux-wlp01 TO wlp02,
             wa_aux-wlp02 TO wlp02,
             wa_aux-wlp03 TO wlp02,
             wa_aux-wlp04 TO wlp02,
             wa_aux-wlp05 TO wlp02,
             wa_aux-wlp06 TO wlp02,
             wa_aux-wlp07 TO wlp02,
             wa_aux-wlp08 TO wlp02,
             wa_aux-wlp09 TO wlp02,
             wa_aux-wlp10 TO wlp02,
             wa_aux-wlp11 TO wlp02,
             wa_aux-wlp12 TO wlp02,
             wa_aux-wlp13 TO wlp02,
             wa_aux-wlp14 TO wlp02,
             wa_aux-wlp15 TO wlp02,
             wa_aux-wlp16 TO wlp02.
      ENDIF.
    ENDLOOP.

ENDFORM.                    " F_DISPONIBLE
