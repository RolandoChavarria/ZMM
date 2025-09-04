*----------------------------------------------------------------------*
***INCLUDE LZMMFMF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0006   text
*      -->P_I_VENDOR  text
*      -->P_I_PUR_GRP  text
*      -->P_I_PROYECT  text
*      -->P_I_FOLIO  text
*      -->P_T_AZULES  text
*      -->P_I_TEST  text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion2 USING    p_pgrm     TYPE rs38l_fnam
                                  p_lifnr    TYPE lifnr
                                  p_ekgrp    TYPE bkgrp
                                  p_proye    TYPE everk
                                  p_folio    TYPE ihrez
                                  "Tablas de posi y servic
                                  t_items    TYPE zmmfm_0070_azul_tt
                                  p_test     TYPE bapiflag-bapiflag
                         CHANGING p_id_rfc   TYPE zid_referencia
                                  p_conse    TYPE zmmde_conse
                                  p_v_subrc  TYPE subrc.

  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: lv_zflag TYPE zutil_parameters-zchar.
  DATA: wa_par_azul   LIKE LINE OF  t_items.
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

  CALL FUNCTION 'ZMMFM_0000_LOG'
    EXPORTING
      namepgr   = p_pgrm
    IMPORTING
      e_subrc   = p_v_subrc
      e_id_ref  = p_id_rfc
    TABLES
      it_params = it_params.


ENDFORM.                    " F_REG_LOG_CONEXION2
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0006   text
*      -->P_I_PROYECT  text
*      -->P_I_AZUL  text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion_disp  USING    p_pgrm TYPE rs38l_fnam
                                       p_i_proyect  TYPE  everk
                                       p_i_azul   TYPE  matkl
                              CHANGING p_id_rfc   TYPE zid_referencia
                                       p_conse    TYPE zmmde_conse
                                       p_v_subrc  TYPE subrc.

  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: lv_zflag TYPE zutil_parameters-zchar.
  CLEAR:wa_params, it_params, p_conse, p_v_subrc.

  SELECT SINGLE zflag FROM zutil_parameters INTO lv_zflag
   WHERE zreport EQ 'ZMMFM_0000_LOG'
   AND   zfield  EQ 'ACTIVATE_LOG'.

  CHECK lv_zflag IS NOT INITIAL.


* Agregar Parametro import
  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_PROYECT'. "Nombre del parametro en el RFC
  wa_params-val_param = p_i_proyect. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_AZUL'. "Nombre del parametro en el RFC
  wa_params-val_param = p_i_azul. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.


  CALL FUNCTION 'ZMMFM_0000_LOG'
    EXPORTING
      namepgr   = p_pgrm
    IMPORTING
      e_subrc   = p_v_subrc
      e_id_ref  = p_id_rfc
    TABLES
      it_params = it_params.

ENDFORM.                    " F_REG_LOG_CONEXION_DISP
*&---------------------------------------------------------------------*
*&      Form  F_REG_EXEP_RFC_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_ID_RFC  text
*      -->P_E_SUBRC  text
*      -->P_E_BAPI_MSG  text
*      -->P_E_PRICE  text
*      -->P_E_MATKL  text
*      <--P_V_CONSE  text
*----------------------------------------------------------------------*
FORM f_reg_exep_rfc_disp  USING p_id_rfc    TYPE zid_referencia
                               p_subrc     TYPE subrc
                               p_msg       TYPE bapi_msg
                               e_price     TYPE  netwr
                               e_matkl     TYPE matkl
                     CHANGING  p_conse     TYPE zmmde_conse.
  DATA: lwa_det TYPE zmmlog_det_rfc,
        lit_det TYPE TABLE OF zmmlog_det_rfc.

  CLEAR: lwa_det, lit_det.

  CHECK p_id_rfc IS NOT INITIAL.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_SUBRC' p_subrc.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_BAPI_MSG' p_msg.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_AZUL' e_matkl.

  ADD 1 TO p_conse.
  PERFORM f_add_t_params TABLES lit_det USING  p_conse 'E' 'E_PRICE' e_price.

  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_id_rfc
    TABLES
      it_params = lit_det.

ENDFORM.                    " F_REG_EXEP_RFC_DISP
