*----------------------------------------------------------------------*
***INCLUDE LZMMFMF01 .
*----------------------------------------------------------------------*
* Funciones para verificacion de disponible
*&---------------------------------------------------------------------*
*&      Form  MAIN_DISPONIBLE
*&---------------------------------------------------------------------*
*      -->P_i_project  Nombre del projecto
*      -->P_I_AZUL  Id de la partida azul
*      <--P_E_AZUL  Devolucion de la partida azul
*      <--P_E_PRICE  Importe de preuspuesto disppoinible
*      <--P_E_SUBRC  Mane error
*      <--P_E_BAPI_MSG  mes de eror
*----------------------------------------------------------------------*
FORM main_disponible  USING    i_project  TYPE  everk
                               i_matkl    TYPE  matkl
                      CHANGING e_matkl    TYPE  matkl
                               e_price     TYPE  netwr
                               e_subrc     TYPE  subrc
                               e_bapi_msg  TYPE  bapi_msg.

  DATA: l_pep TYPE ps_posid.
  e_matkl = i_matkl.
* Armado del pep
  PERFORM main_disponible_pep  USING i_project i_matkl
                            CHANGING e_subrc e_bapi_msg l_pep.
  CHECK e_subrc = 0.
  IF e_subrc IS INITIAL.
* Get the budget available.
    PERFORM main_disponible_price USING i_project e_matkl CHANGING e_subrc e_bapi_msg l_pep e_price.

  ENDIF.

ENDFORM.                    " MAIN_DISPONIBLE

*&---------------------------------------------------------------------*
*&      Form  main_disponible_price
*&---------------------------------------------------------------------*
FORM main_disponible_price USING i_project   TYPE  everk
                                 e_azul      TYPE matkl
                        CHANGING e_subrc     TYPE  subrc
                                 e_bapi_msg  TYPE  bapi_msg
                                 c_pep       TYPE  ps_posid
                                 e_price     TYPE  netwr.
  DATA: lday(2) TYPE c,
        lmon(2) TYPE c,
        lyea(4) TYPE c,
        lyea2(4) TYPE c,
        ls_prps TYPE prps,
        ls_rpsco TYPE rpsco,
        ls_budet TYPE rpsco, " get diffents budgets PR-IN-0001-RM-031 PR-IN-0001-RM-030
        lt_rpsco TYPE SORTED TABLE OF rpsco WITH UNIQUE DEFAULT KEY,
*** Inicio Ricardo Pichardo
        it_rpsco_aux  TYPE TABLE OF rpsco,
        it_rpsco_aux2 TYPE TABLE OF rpsco,
        e_price_aux      TYPE netwr,
        e_price_aux2      TYPE netwr.
*** Fin Ricardo Pichardo
  TABLES rpsco.
  RANGES r_year FOR rpsco-gjahr.
  DATA: wa_year LIKE LINE OF r_year.

  PERFORM conversion_exit_abpsp_input USING c_pep CHANGING  ls_prps-pspnr.
  SELECT SINGLE objnr FROM prps INTO ls_rpsco-objnr WHERE  pspnr EQ ls_prps-pspnr
           AND  pkokr = 'GIG'.
  IF sy-subrc IS INITIAL.
    PERFORM hr_in_get_date_components CHANGING lday lmon lyea.
    TYPES: BEGIN OF ty_params,
            zreport TYPE syrepid,
            zfield  TYPE awkey,
            zchar   TYPE chardata_d,
           END OF ty_params.
    DATA: it_param TYPE TABLE OF ty_params,
          wa_param  TYPE ty_params,
          v_year1 TYPE c LENGTH 4,
          v_year2 TYPE c LENGTH 4.


    SELECT zreport zfield zchar INTO TABLE it_param FROM zutil_parameters
                                WHERE zreport = 'ZMMFM_0090_DISPONIBLE_DATE'.
    IF sy-subrc NE 0.
      e_subrc = '04'.
      CONCATENATE 'Project:' i_project  'falta conf. ZUTIL_PARAMETERS' INTO e_bapi_msg SEPARATED BY space.
      PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
      CHECK e_subrc = 0.
    ENDIF.

    READ TABLE it_param INTO wa_param INDEX 1.
    lyea2 = lyea + 5.
    wa_year-sign   = 'I'.
    wa_year-option = 'BT'.
    wa_year-low    = wa_param-zchar.
    wa_year-high   = lyea2.
    APPEND wa_year TO r_year.
***    SELECT * FROM rpsco INTO TABLE lt_rpsco WHERE objnr EQ ls_rpsco-objnr AND gjahr EQ lyea.
***    SELECT * FROM rpsco INTO TABLE lt_rpsco WHERE objnr EQ ls_rpsco-objnr AND gjahr IN r_year.
    SELECT * FROM rpsco INTO TABLE it_rpsco_aux WHERE objnr EQ ls_rpsco-objnr AND gjahr IN r_year.
    MOVE it_rpsco_aux TO it_rpsco_aux2.
    DELETE it_rpsco_aux2 WHERE gjahr NE lyea.

  ENDIF.
***  IF lt_rpsco[] IS INITIAL.
  IF it_rpsco_aux IS INITIAL AND it_rpsco_aux2 IS INITIAL.
    e_subrc = '04'.
    CONCATENATE 'Project:' i_project  'NO presupuesto' INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
    CHECK e_subrc = 0.
  ELSE.
* Get the budget
    CLEAR: ls_budet.
    LOOP AT it_rpsco_aux INTO ls_rpsco WHERE vorga EQ 'KBFR'.
      ADD ls_rpsco-wlp00 TO ls_budet-wlp00.
    ENDLOOP.

    PERFORM f_disponible TABLES it_rpsco_aux
                         USING ls_budet-wlp01
                               ls_budet-wlp02.


* Presupuesto - real - comprometido
    e_price_aux = ls_budet-wlp00 - ls_budet-wlp01 - ls_budet-wlp02.
*********************
*& E_PRICE2
*********************
    CLEAR: ls_budet,
           ls_rpsco.


    LOOP AT it_rpsco_aux2 INTO ls_rpsco WHERE vorga EQ 'KBFR'.
      ADD ls_rpsco-wlp00 TO ls_budet-wlp00.
    ENDLOOP.

    PERFORM f_disponible TABLES it_rpsco_aux2
                         USING ls_budet-wlp01
                               ls_budet-wlp02.

    e_price_aux2 = ls_budet-wlp00 - ls_budet-wlp01 - ls_budet-wlp02.
    e_price = e_price_aux.
***    IF e_price_aux2 < e_price_aux.
***      e_price = e_price_aux2.
***    ELSEIF e_price_aux2 > e_price_aux.
***      e_price = e_price_aux.
******    ELSE.
******      e_price = e_price_aux.
***    ENDIF.
    e_subrc = '00'.
    CONCATENATE 'Project:' i_project  'Presupuesto disponible' INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_disp USING v_id_rfc e_subrc e_bapi_msg e_price e_azul
                            CHANGING v_conse.
    CHECK e_subrc = 0.
  ENDIF.

ENDFORM.                    "main_disponible_price
*----------------------------------------------------------------------*
* Armar el pep
*----------------------------------------------------------------------*
FORM main_disponible_pep  USING i_project   TYPE  everk
                                i_matkl     TYPE  matkl
                      CHANGING  e_subrc     TYPE  subrc
                                e_bapi_msg  TYPE  bapi_msg
                                c_pep       TYPE  ps_posid.
  DATA: l_t001w TYPE t001w,
        l_t001k TYPE t001k.
  DATA: lv_proyecto TYPE everk.  "Numero de proyecto
  CLEAR: e_subrc, e_bapi_msg, c_pep.

*-* Busca Centro en T001W
  l_t001w-werks = i_project+4(4).

* -> BEGIN OF INSERT MCA-20220628 OT: RSDK912341
  DATA: lv_bukrs TYPE bukrs,
        lv_werks TYPE werks_d.
* Localizar la sociedad en base al proyecto
  SELECT SINGLE vbukr
    INTO lv_bukrs
    FROM proj
   WHERE pspid EQ i_project.
  IF sy-subrc EQ 0. "Encontró la sociedad
* Se valida exista la sociedad-centro original en la tabla ZMMTT_WERKS_SUST
* Para tomar el valor del centro sustituto
    CLEAR: lv_proyecto.
    CLEAR: lv_werks.
    SELECT SINGLE werks_sust
      INTO lv_werks
      FROM zmmtt_werks_sust
     WHERE bukrs EQ lv_bukrs
       AND werks_ori EQ l_t001w-werks.
    IF sy-subrc EQ 0.
      MOVE lv_werks TO l_t001w-werks.
      lv_proyecto = 'X'.
    ELSE.
* Si el proyecto existe en la tabla ZMMTT_FUSION_SOC
* se deben de quedar los ceros en L_T001W-WERKS, en caso contrario
* Se ejecutará la rutina CONVERSION_EXIT_ALPHA_OUTPUT
* El cual elimina los ceros de L_T001W-WERKS
      CLEAR: lv_proyecto.
      SELECT SINGLE proyecto
        INTO lv_proyecto
        FROM zmmtt_fusion_soc
       WHERE proyecto EQ i_project.
      IF lv_proyecto IS INITIAL. "No encontró el proyecto
        PERFORM conversion_exit_alpha_output USING l_t001w-werks CHANGING l_t001w-werks.
      ENDIF.
    ENDIF.
  ELSE.
* <- END OF INSERT MCA-20220628 OT: RSDK912341
* -> BEGIN OF INSERT MCA-20220124 OT: RSDK912041
* Si el proyecto existe en la tabla ZMMTT_FUSION_SOC
* se deben de quedar los ceros en L_T001W-WERKS, en caso contrario
* Se ejecutará la rutina CONVERSION_EXIT_ALPHA_OUTPUT
* El cual elimina los ceros de L_T001W-WERKS
    CLEAR: lv_proyecto.
    SELECT SINGLE proyecto
      INTO lv_proyecto
      FROM zmmtt_fusion_soc
     WHERE proyecto EQ i_project.
    IF lv_proyecto IS INITIAL. "No encontró el proyecto
* <- END OF INSERT MCA-20220124 OT: RSDK912041
      PERFORM conversion_exit_alpha_output USING l_t001w-werks CHANGING l_t001w-werks.
    ENDIF.  "INSERT MCA-20220124 OT: RSDK912041
  ENDIF.  "INSERT MCA-20220628 OT: RSDK912341

  SELECT SINGLE bwkey FROM t001w INTO l_t001w-bwkey WHERE werks EQ l_t001w-werks.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '02'.
    CONCATENATE 'Centro:' l_t001w-bwkey 'NO EXISTE' INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.
* Sociedad
  SELECT SINGLE bukrs FROM t001k INTO l_t001k-bukrs WHERE bwkey EQ l_t001w-bwkey.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '03'.
    CONCATENATE 'Sociedad:' l_t001k-bukrs 'NO EXISTE' INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.

*-* Genera elemento pep del proyecto g_elpep se reusa en ambas funciones
  CONCATENATE 'PE' i_project+4(4) l_t001k-bukrs i_project+10 i_matkl(2) i_matkl+2(2) INTO c_pep SEPARATED BY '-'.
  CONDENSE c_pep NO-GAPS.

ENDFORM.                    "MAIN_DISPONIBLE_pep

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_output USING in CHANGING out.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = in
    IMPORTING
      output = out.
ENDFORM.                    "conversion_exit_alpha_output

*&---------------------------------------------------------------------*
FORM conversion_exit_abpsp_input USING in CHANGING out.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
    EXPORTING
      input     = in
    IMPORTING
      output    = out
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
ENDFORM.                    "conversion_exit_alpha_output

*&---------------------------------------------------------------------*
*&      Form  VALIDA_BUKRS
*&---------------------------------------------------------------------*
FORM valida_bukrs  USING    p_lifnr    TYPE lifnr
                            p_bukrs    TYPE lfb1-bukrs
                   CHANGING e_subrc    TYPE subrc
                            e_bapi_msg TYPE bapi_msg.

  DATA: ls_lfb1 TYPE lfb1.

  SELECT SINGLE lifnr FROM lfb1 INTO ls_lfb1-lifnr WHERE
    lifnr EQ p_lifnr AND bukrs EQ p_bukrs.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '02'.
    CONCATENATE p_lifnr  'Proveedor no existe en la sociedad'
    p_bukrs INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.

ENDFORM.                    " VALIDA_BUKRS
