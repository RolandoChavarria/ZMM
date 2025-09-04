*----------------------------------------------------------------------*
***INCLUDE LZMMFMF07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LIFNR  text
*----------------------------------------------------------------------*
FORM validate_lifnr  USING    p_ebeln   TYPE ebeln
                              p_lifnr   TYPE lifnr
                     CHANGING e_subrc   TYPE subrc
                              e_bapi_msg TYPE bapi_msg.

  DATA: ls_lfb1 TYPE lfb1.
  DATA: lwa_msg TYPE zmmwa_0030_exp_msg.

  SELECT SINGLE lifnr FROM ekko INTO ls_lfb1-lifnr
    WHERE ebeln = p_ebeln AND lifnr = p_lifnr.

  IF sy-subrc IS NOT INITIAL.
    e_subrc = '02'.

    CONCATENATE p_lifnr  'Proveedor no existe en el pedido'
    INTO e_bapi_msg SEPARATED BY space.

    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.

ENDFORM.                    " VALIDATE_LIFNR
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_EBELN  text
*      <--P_E_SUBRC  text
*      <--P_E_BAPI_MSG  text
*----------------------------------------------------------------------*
FORM validate_pos  USING    p_ebeln             TYPE ebeln
                   CHANGING pt_positions        TYPE ty_positions
                            pt_update_positions TYPE ty_positions
                            pt_msg              TYPE ty_msg.

  DATA: lwa_positions TYPE zmmwa_0080_positions,
        lt_ekpo LIKE STANDARD TABLE OF ekpo,
        lwa_ekpo TYPE ekpo,
        lwa_msg TYPE zmmwa_0030_exp_msg.

* Selecciona posiciones
  SELECT * FROM ekpo INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
    WHERE ebeln = p_ebeln.

* Recorre posiciones web
  LOOP AT pt_positions INTO lwa_positions.
    lwa_msg-no_posicion  = lwa_positions-no_posicion.

    TRANSLATE lwa_positions-ind_entrega_fin TO UPPER CASE.
    READ TABLE lt_ekpo INTO lwa_ekpo WITH KEY ebeln = p_ebeln
                                              ebelp = lwa_positions-no_posicion.
    "Valida posicion
    IF sy-subrc <> 0.
      lwa_msg-subrc  = '03'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion no existe'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.

      "Valida indicador de borrado o fin de entrega
    ELSEIF lwa_ekpo-loekz = 'L' OR lwa_ekpo-elikz = 'X'.

      lwa_msg-subrc = '04'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion invalida'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.

    ELSEIF lwa_positions-ind_entrega_fin <> 'X'.

      lwa_msg-subrc = '05'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion no actualizada'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.

    ELSE.
      APPEND lwa_positions TO pt_update_positions.
    ENDIF.

  ENDLOOP.

  "Registra posiciones de error en log
  PERFORM f_reg_log_conexion4 TABLES pt_msg
                              USING 'ZMMFM_0080_FINALIZAR_ORDEN'
                              CHANGING v_id_rfc v_conse v_subrc.

ENDFORM.                    " VALIDATE_POS

*&---------------------------------------------------------------------*
*&      Form  validate_cancel_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EBELN              text
*      -->PT_POSITIONS         text
*      -->PT_UPDATE_POSITIONS  text
*      -->PT_MSG               text
*----------------------------------------------------------------------*
FORM validate_cancel_pos  USING    p_ebeln             TYPE ebeln
                          CHANGING pt_positions        TYPE ty_positions
                          pt_update_positions          TYPE ty_positions
                          pt_msg                       TYPE ty_msg.

  DATA: lwa_positions TYPE zmmwa_0080_positions,
        lt_ekpo LIKE STANDARD TABLE OF ekpo,
        lwa_ekpo TYPE ekpo,
        lwa_msg TYPE zmmwa_0030_exp_msg.

* Selecciona posiciones
  SELECT * FROM ekpo INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
    WHERE ebeln = p_ebeln.

* Recorre posiciones web
  LOOP AT pt_positions INTO lwa_positions.
    lwa_msg-no_posicion  = lwa_positions-no_posicion.

    READ TABLE lt_ekpo INTO lwa_ekpo WITH KEY ebeln = p_ebeln
                                              ebelp = lwa_positions-no_posicion.
    "Valida posicion
    IF sy-subrc <> 0.
      lwa_msg-subrc  = '03'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion no existe'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.

      "Valida indicador de borrado
    ELSEIF lwa_ekpo-loekz = 'L'.

      lwa_msg-subrc = '04'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion invalida'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.

    ELSEIF lwa_positions-ind_entrega_fin <> ' ' AND lwa_positions-ind_entrega_fin <> ''.

      lwa_msg-subrc = '05'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion no cancelada'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.

    ELSE.
      APPEND lwa_positions TO pt_update_positions.
    ENDIF.
  ENDLOOP.

  "Registra posiciones de error en log
  PERFORM f_reg_log_conexion4 TABLES pt_msg
                              USING 'ZMMFM_0110_CANCELAR_ORDEN'
                              CHANGING v_id_rfc v_conse v_subrc.

ENDFORM.                    " VALIDATE_POS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_EBELN  text
*      -->P_I_FOLIO  text
*      <--P_IT_IMP_POSITIONS[]  text
*      <--P_E_SUBRC  text
*      <--P_E_BAPI_MSG  text
*----------------------------------------------------------------------*
FORM update_pos  USING    p_ebeln             TYPE ebeln
                          p_folio             TYPE ihrez
                 CHANGING pt_positions        TYPE ty_positions
                          pt_update_positions TYPE ty_positions
                          pt_msg              TYPE ty_msg
                          pt_return           TYPE ty_return
                          e_subrc             TYPE subrc
                          e_bapi_msg          TYPE bapi_msg.

  DATA: lt_hdr     TYPE bapimepoheader,
        lt_hdrx    TYPE bapimepoheaderx,
        lt_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        lt_poitem  LIKE bapimepoitem  OCCURS 0 WITH HEADER LINE,
        lt_poitemx LIKE bapimepoitemx OCCURS 0 WITH HEADER LINE,
        lwa_positions TYPE zmmwa_0080_positions,
        lwa_msg TYPE zmmwa_0030_exp_msg,
        lt_msg LIKE STANDARD TABLE OF zmmwa_0030_exp_msg.


* Llena header bapi
  lt_hdr-ref_1 = p_folio.
  lt_hdrx-ref_1 = 'X'.


* Recorre partidas a actualizar
  LOOP AT pt_update_positions INTO lwa_positions.

    lt_poitem-po_item = lwa_positions-no_posicion.
    lt_poitem-no_more_gr = 'X'.
    "lt_poitem-no_more_gr = ' '.
    APPEND lt_poitem.

    lt_poitemx-po_item = lwa_positions-no_posicion.
    lt_poitemx-no_more_gr = 'X'.
    APPEND lt_poitemx.

    CLEAR: lt_poitem, lt_poitemx.

  ENDLOOP.


* Actualiza
  CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      purchaseorder = p_ebeln
      poheader      = lt_hdr
      poheaderx     = lt_hdrx
    TABLES
      return        = lt_return
      poitem        = lt_poitem
      poitemx       = lt_poitemx.

* Valida errores bapi
  READ TABLE lt_return WITH KEY type = 'E'.

  "Actualiza posiciones
  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR lwa_positions.
**********************************************************************
*** Rutina que modifica los documentos de comprometido ligados al pedido
*** add 14.03.2016
    PERFORM f_modif_comp USING p_ebeln
                               pt_update_positions
                               pt_return.
**********************************************************************
    LOOP AT pt_update_positions INTO lwa_positions.
      lwa_msg-subrc = '06'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion finalizada'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.
      APPEND lwa_msg TO lt_msg.
      CLEAR lwa_msg.
    ENDLOOP.

    "Error de llenado de la BAPI
  ELSE.
    e_subrc = '07'.
    e_bapi_msg = 'Error en el llenado de la BAPI'.

    CLEAR: pt_msg.
    REFRESH: pt_msg.

    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.


* Registra en log
  PERFORM f_reg_log_conexion4 TABLES lt_msg
                              USING 'ZMMFM_0080_FINALIZAR_ORDEN'
                              CHANGING v_id_rfc v_conse v_subrc.

ENDFORM.                    " UPDATE_POS

*&---------------------------------------------------------------------*
*&      Form  update_cancel_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EBELN              text
*      -->P_FOLIO              text
*      -->PT_POSITIONS         text
*      -->PT_UPDATE_POSITIONS  text
*      -->PT_MSG               text
*      -->E_SUBRC              text
*      -->E_BAPI_MSG           text
*----------------------------------------------------------------------*
FORM update_cancel_pos  USING    p_ebeln             TYPE ebeln
                                 p_folio             TYPE ihrez
                        CHANGING pt_positions        TYPE ty_positions
                                 pt_update_positions TYPE ty_positions
                                 pt_msg              TYPE ty_msg
                                 e_subrc             TYPE subrc
                                 e_bapi_msg          TYPE bapi_msg.

  DATA: lt_hdr     TYPE bapimepoheader,
        lt_hdrx    TYPE bapimepoheaderx,
        lt_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        lt_poitem  LIKE bapimepoitem  OCCURS 0 WITH HEADER LINE,
        lt_poitemx LIKE bapimepoitemx OCCURS 0 WITH HEADER LINE,
        lwa_positions TYPE zmmwa_0080_positions,
        lwa_msg TYPE zmmwa_0030_exp_msg,
        lt_msg LIKE STANDARD TABLE OF zmmwa_0030_exp_msg.


* Llena header bapi
  lt_hdr-ref_1 = p_folio.
  lt_hdrx-ref_1 = 'X'.


* Recorre partidas a cancelar
  LOOP AT pt_update_positions INTO lwa_positions.

    lt_poitem-po_item = lwa_positions-no_posicion.
    lt_poitem-no_more_gr = ' '.
    APPEND lt_poitem.

    lt_poitemx-po_item = lwa_positions-no_posicion.
    lt_poitemx-no_more_gr = 'X'.

    APPEND lt_poitemx.
    CLEAR: lt_poitem, lt_poitemx.

  ENDLOOP.

* Actualiza
  CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      purchaseorder = p_ebeln
      poheader      = lt_hdr
      poheaderx     = lt_hdrx
    TABLES
      return        = lt_return
      poitem        = lt_poitem
      poitemx       = lt_poitemx.

* Valida errores bapi
  READ TABLE lt_return WITH KEY type = 'E'.

  "Actualiza posiciones
  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR lwa_positions.

    LOOP AT pt_update_positions INTO lwa_positions.
      lwa_msg-subrc = '06'.
      lwa_msg-no_posicion = lwa_positions-no_posicion.

      CONCATENATE lwa_positions-no_posicion  'Posicion cancelada'
      INTO lwa_msg-mensaje SEPARATED BY space.

      APPEND lwa_msg TO pt_msg.
      APPEND lwa_msg TO lt_msg.
      CLEAR lwa_msg.
    ENDLOOP.

    "Error de llenado de la BAPI
  ELSE.
    e_subrc = '07'.
    e_bapi_msg = 'Error en el llenado de la BAPI'.

    CLEAR: pt_msg.
    REFRESH: pt_msg.

    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.


* Registra en log
  PERFORM f_reg_log_conexion4 TABLES lt_msg
                              USING 'ZMMFM_0080_DESMARCA_FIN_ORDEN'
                              CHANGING v_id_rfc v_conse v_subrc.

ENDFORM.                    " CANCELAR_POS
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0006   text
*      -->P_I_EBELN  text
*      -->P_I_LIFNR  text
*      -->P_I_FOLIO  text
*      -->P_IT_EXP_SERVICES  text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion3  TABLES   p_positions TYPE ty_positions
                          USING    p_pgrm     TYPE rs38l_fnam
                                   p_ebeln    TYPE ebeln
                                   p_lifnr    TYPE lifnr
                                   p_folio    TYPE ihrez

                          CHANGING p_id_rfc TYPE zid_referencia
                                   p_conse  TYPE zmmde_conse
                                   p_subrc  TYPE subrc.


  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: lv_zflag TYPE zutil_parameters-zchar.
  DATA: lv_string     TYPE string.
  CLEAR: wa_params, it_params, p_conse, p_subrc.
  DATA: lwa_positions TYPE zmmwa_0080_positions.

  CLEAR: wa_params, it_params, p_conse, p_subrc.


  SELECT SINGLE zflag FROM zutil_parameters INTO lv_zflag
   WHERE zreport EQ 'ZMMFM_0000_LOG'
   AND   zfield  EQ 'ACTIVATE_LOG'.

  CHECK lv_zflag IS NOT INITIAL.


* Agregar Parametro import
  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_EBELN'. "Nombre del parametro en el RFC
  wa_params-val_param = p_ebeln. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_LIFNR'. "Nombre del parametro en el RFC
  wa_params-val_param = p_lifnr. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

  ADD 1 TO p_conse."Se agrega 1 para ir enumerando cada parametro
  wa_params-conse = p_conse. "Consecutivo
  wa_params-type_param = 'I'. "Import
  wa_params-param = 'I_FOLIO'. "Nombre del parametro en el RFC
  wa_params-val_param = p_folio. "Valor del parametro
  APPEND wa_params TO it_params. "Agrega reg. a itab que se enviara en FM de LOG
  CLEAR: wa_params.

* t_azules
  "IF p_positions IS NOT INITIAL.
  LOOP AT p_positions INTO lwa_positions.
    ADD 1 TO p_conse.
    wa_params-conse = p_conse.
    wa_params-type_param = 'I'.
    wa_params-param = 'IT_IMP_POSITIONS'.

    CONCATENATE '|' lwa_positions-no_pedido '|' lwa_positions-no_posicion                                   "jane
                '|' lwa_positions-descrip_pos '|' lwa_positions-ind_entrega_fin
                INTO wa_params-val_param.

    APPEND wa_params TO it_params.
    CLEAR: wa_params,  lwa_positions.
  ENDLOOP.
  "ENDIF.


* Parametros
  CALL FUNCTION 'ZMMFM_0000_LOG'
    EXPORTING
      namepgr   = p_pgrm
    IMPORTING
      e_subrc   = p_subrc
      e_id_ref  = p_id_rfc
    TABLES
      it_params = it_params.


ENDFORM.                    " F_REG_LOG_CONEXION3
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG_CONEXION4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_UPDATE_POSITIONS  text
*      -->P_0188   text
*      <--P_V_ID_RFC  text
*      <--P_V_CONSE  text
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_reg_log_conexion4  TABLES   p_msg TYPE ty_msg
                          USING    p_pgrm     TYPE rs38l_fnam
                          CHANGING p_id_rfc TYPE zid_referencia
                                   p_conse  TYPE zmmde_conse
                                   p_subrc  TYPE subrc.

  DATA: it_params TYPE TABLE OF zmmlog_det_rfc.
  DATA: wa_params LIKE LINE OF it_params.
  DATA: wa_msg LIKE LINE OF p_msg.
  CLEAR: wa_params, it_params, wa_msg.

  CHECK p_id_rfc IS NOT INITIAL.



  LOOP AT p_msg INTO wa_msg.
    ADD 1 TO p_conse.
    wa_params-conse = p_conse.
    wa_params-type_param = 'E'.
    wa_params-param = 'IT_EXP_MSG'.
    CONCATENATE '|' wa_msg-no_posicion '|' wa_msg-mensaje
     '|' wa_msg-subrc '|' INTO wa_params-val_param.
    APPEND wa_params TO it_params.
    CLEAR: wa_params, wa_msg.
  ENDLOOP.



  CALL FUNCTION 'ZMMFM_0000_REG_FIN_LOG'
    EXPORTING
      i_id_ref  = p_id_rfc
    TABLES
      it_params = it_params.


ENDFORM.                    " F_REG_LOG_CONEXION4
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_COMP
*&---------------------------------------------------------------------*
*& Rutina creada para la modificacion de documentos de comprometido de
*& manera automatica.
*----------------------------------------------------------------------*
FORM f_modif_comp USING pedido
                        t_pos TYPE ty_positions
                        t_ret TYPE ty_return.

  DATA:  it_det  TYPE TABLE OF zrscsdetail,
         wa_det  TYPE zrscsdetail,
         indice  TYPE sy-tabix,
         indice2 TYPE sy-tabix,
         vl_ind  TYPE c LENGTH 1.

  DATA: it_kblp  TYPE TABLE OF kblp,
        wa_kblp  TYPE kblp,
        it_ekpo  TYPE TABLE OF ekpo,
        wa_ekpo  TYPE ekpo,
        wa_data  TYPE fmr_interface_det,
        wa_header TYPE zrscsheader,
        wa_ret    TYPE zreturn,
        wa_pos    LIKE LINE OF t_pos.

  SELECT * FROM zrscsdetail INTO TABLE it_det FOR ALL ENTRIES IN t_pos
    WHERE ebeln = t_pos-no_pedido
      AND ebelp = t_pos-no_posicion.
  IF sy-subrc EQ 0.
***
    SELECT * FROM ekpo INTO TABLE it_ekpo FOR ALL ENTRIES IN t_pos
        WHERE ebeln = t_pos-no_pedido
          AND ebelp = t_pos-no_posicion.

    SELECT * INTO TABLE it_kblp FROM kblp
      FOR ALL ENTRIES IN it_det
      WHERE belnr = it_det-zdocm
        AND pspnr = it_det-zpep
        AND erlkz NE 'X'.

    LOOP AT it_ekpo INTO wa_ekpo.
      LOOP AT it_det INTO wa_det WHERE ebelp = wa_ekpo-ebelp.
        indice = sy-tabix.
        IF wa_ekpo-loekz IS NOT INITIAL.
          wa_det-zdele = wa_ekpo-loekz.
          wa_det-zst   = 'X'.
          READ TABLE it_kblp INTO wa_kblp WITH KEY belnr =  wa_det-zdocm
                                                   pspnr =  wa_det-zpep.
          IF sy-subrc EQ 0.
            indice2  = sy-tabix.
            wa_kblp-erlkz = 'X'.
            MODIFY it_kblp FROM wa_kblp INDEX indice2.
          ENDIF.
        ELSEIF wa_ekpo-elikz IS NOT INITIAL.
          wa_det-zefinal = wa_ekpo-elikz.
          wa_det-zst   = 'X'.
          READ TABLE it_kblp INTO wa_kblp WITH KEY belnr =  wa_det-zdocm
                                                   pspnr =  wa_det-zpep.
          IF sy-subrc EQ 0.
            indice2  = sy-tabix.
            wa_kblp-erlkz = 'X'.
            MODIFY it_kblp FROM wa_kblp INDEX indice2.
          ENDIF.
        ELSE.
          CLEAR: wa_det-zdele,
                 wa_det-zst,
                 wa_det-zefinal.
          READ TABLE it_kblp INTO wa_kblp WITH KEY belnr =  wa_det-zdocm
                                                   pspnr =  wa_det-zpep.
          IF sy-subrc EQ 0.
            indice2  = sy-tabix.
            CLEAR wa_kblp-erlkz.
            MODIFY it_kblp FROM wa_kblp INDEX indice2.
          ENDIF.
        ENDIF.
        MODIFY it_det FROM wa_det INDEX indice.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  MODIFY zrscsdetail FROM TABLE it_det.
  CLEAR: wa_det,
         it_det.
  REFRESH it_det.
***   Se modifica el documento de comprometido
  SORT it_kblp BY belnr.
  CLEAR wa_data.
  DATA: it_data  TYPE TABLE OF fmr_interface_det.

  LOOP AT it_kblp INTO wa_kblp.
    wa_data-belnr       = wa_kblp-belnr.
    wa_data-blpos       = wa_kblp-blpos.
    wa_data-wrbtr       = wa_kblp-wtges.
    wa_data-dmbtr       = wa_kblp-wtges.
    wa_data-ptext       = wa_kblp-ptext.
    wa_data-lifnr       = wa_kblp-lifnr.
    wa_data-ps_psp_pnr  = wa_kblp-pspnr.
    wa_data-saknr       = wa_kblp-saknr.
    wa_data-erlkz       = wa_kblp-erlkz.
    APPEND wa_data TO it_data.
    CALL FUNCTION 'FMFR_CHANGE_FROM_DATA'
      EXPORTING
        i_belnr       = wa_kblp-belnr
        i_flg_commit  = 'X'
      TABLES
        t_posdata     = it_data
      EXCEPTIONS
        error_occured = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      wa_ret-zsy_subrc = 04.
      wa_ret-zposicion = wa_data-blpos.
      wa_ret-zpedido   = pedido.
      wa_ret-ztexto    = wa_data-ptext.
      APPEND wa_ret TO t_ret.
    ELSE.
***  Llenamos la tabla de mensajes de retorno a WEB
***  para que se muestren despues de hacer correctamente la
***  actualizacion
      wa_ret-zsy_subrc = 00.
      wa_ret-zposicion = wa_data-blpos.
      wa_ret-zpedido   = pedido.
      wa_ret-ztexto    = wa_data-ptext.
      APPEND wa_ret TO t_ret.
    ENDIF.
    CLEAR: wa_data,
           it_data,
           wa_ret.
    REFRESH it_data.
  ENDLOOP.
  IF sy-subrc NE 0.
***  No se modificaron doc de comp pero si hay datos que enviar
***  a la tabla return
    CLEAR wa_ret.
    LOOP AT t_pos INTO wa_pos.
      wa_ret-zsy_subrc = 00.
      wa_ret-zposicion = wa_pos-no_posicion.
      wa_ret-zpedido   = pedido.
      wa_ret-ztexto    = wa_pos-descrip_pos.
      APPEND wa_ret TO t_ret.
    ENDLOOP.
  ENDIF.
  SELECT * FROM zrscsdetail INTO TABLE it_det WHERE ebeln = pedido.
  IF sy-subrc EQ 0.
    LOOP AT it_det INTO wa_det.
      IF wa_det-zconc   IS NOT INITIAL OR
         wa_det-zefinal IS NOT INITIAL OR
         wa_det-zdele   IS NOT INITIAL OR
         wa_det-zst     IS NOT INITIAL.
        vl_ind = 'X'.
      ELSE.
        CLEAR vl_ind.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF vl_ind = 'X'.
    SELECT SINGLE * FROM zrscsheader INTO wa_header WHERE ebeln = pedido.
    IF sy-subrc EQ 0.
      wa_header-zconc = 'X'.
      MODIFY zrscsheader FROM wa_header.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_MODIF_COMP
