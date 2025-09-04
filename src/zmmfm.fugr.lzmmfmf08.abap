*----------------------------------------------------------------------*
***INCLUDE LZMMFMF08 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MAIN_TRASPASO_SALDO
*&---------------------------------------------------------------------*
FORM main_traspaso_saldo  USING i_proyect TYPE  everk
                                i_azul_e  TYPE zmmfm_0070_azul
                                i_azul_r  TYPE zmmfm_0070_azul_tt
                                i_amount TYPE bp_wert3
                                i_gjahr TYPE bp_u_sjahr
                                i_waers TYPE twaer
                                i_folio TYPE  ihrez
                       CHANGING e_subrc TYPE subrc
                                e_bapi_msg TYPE  bapi_msg.
  DATA: e_pep TYPE  prps-posid,
        r_pep TYPE  prps-posid,
        r_proj TYPE ps_posid,
        l_proj TYPE proj,
        l_pro  TYPE everk,
        l_id_rfc TYPE zid_referencia.
* Validar si hay suficiente saldo
  PERFORM get_pep_transp USING i_azul_e i_azul_r i_proyect
              CHANGING r_pep e_subrc e_bapi_msg.

  CHECK e_subrc IS INITIAL.
  PERFORM conversion_exit_projn_input USING i_proyect
                                      CHANGING r_proj.
* Disponiblel_pro
  PERFORM conversion_exit_projn_input USING i_proyect
                                    CHANGING l_pro.
  l_id_rfc = v_id_rfc.
  PERFORM zmmfm_0090_disponible_tr
    USING l_pro i_azul_e CHANGING e_subrc e_bapi_msg.
  v_id_rfc = l_id_rfc.
  CHECK e_subrc IS INITIAL.
* Valida prep
  PERFORM conversion_exit_projn_input USING i_proyect
                                      CHANGING r_proj.
* Project
  PERFORM cjdw_proj_select_single USING r_proj  ""i_proyect
                                  CHANGING l_proj.
* Liberar presupuesto
  e_pep = g_elpep.
  PERFORM libera_presupuesto USING i_gjahr i_amount l_proj
          i_waers e_pep i_folio i_azul_e.
  CHECK e_subrc IS INITIAL.
*Aplicar transpaso
  PERFORM transpaso_saldo USING i_gjahr i_gjahr i_amount i_waers
      e_pep r_pep e_bapi_msg i_folio l_proj.

ENDFORM.                    " MAIN_TRASPASO_SALDO

*&---------------------------------------------------------------------*
*&      Form  Get_werks
*&---------------------------------------------------------------------*
FORM get_pep_transp USING i_azul_e TYPE  zmmfm_0070_azul
                          i_azul_r TYPE zmmfm_0070_azul_tt
                          p_proye TYPE everk
            CHANGING r_pep TYPE  prps-posid
                      e_subrc TYPE subrc
                     e_bapi_msg  TYPE bapi_msg.

  DATA: l_t001w TYPE ty_t001w,
        l_t001k TYPE ty_t001k,
        l_proye TYPE everk,
        l_proj TYPE proj.

  DATA: ls_rece TYPE  zmmfm_0070_azul.

  PERFORM conversion_exit_abpsn_input USING p_proye CHANGING l_proye.

  SELECT SINGLE * FROM proj INTO l_proj WHERE pspid EQ l_proye.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '02'.
    e_bapi_msg = 'Projecto invalido'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.

* Get constants to the program
  SELECT zfield zchar INTO TABLE t_prmtrs  FROM zutil_parameters
         WHERE zreport EQ 'ZMMFM_0100_TRASPASO_SALDO'.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '02'.
    e_bapi_msg = 'ZUTIL_PARAMETERS - REQUERIDOS'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc IS INITIAL.
* Valida emisor
  IF i_azul_e-matkl IS INITIAL.
    e_subrc = '02'.
    e_bapi_msg = 'Error: falta partida emisor.'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ELSE.
* Denominaciones para grupos de artículos
    SELECT * FROM t023t INTO TABLE it_t023t
        WHERE spras EQ sy-langu AND matkl EQ i_azul_e-matkl.
* Centro
    SELECT vkorg werks INTO TABLE it_t001w FROM t001w
          WHERE werks EQ i_azul_e-werks.

    SELECT bwkey bukrs INTO TABLE it_t001k FROM t001k
            WHERE bwkey EQ i_azul_e-werks.
    IF it_t001k[] IS NOT INITIAL.
      READ TABLE it_t001k INTO l_t001k INDEX 1.
      g_bukrs = l_t001k-bukrs.
    ENDIF.
  ENDIF.
* Valida receptor
  IF i_azul_r[] IS INITIAL.
    e_subrc = '02'.
    e_bapi_msg = 'Error: falta partida receptor.'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.


  ENDIF.
  CHECK e_subrc IS INITIAL.
*-* Busca Centro en T001W
  IF it_t001w[] IS NOT INITIAL.
    READ TABLE it_t001w INTO l_t001w INDEX 1. "" WITH KEY vkorg = p_item-werks.
  ENDIF.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '03'.
    CONCATENATE 'Centro:' i_azul_e-werks 'NO EXISTE' INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc IS INITIAL.
* Sociedad
  IF it_t001k[] IS NOT INITIAL.
    READ TABLE it_t001k INTO l_t001k WITH KEY bwkey = l_t001w-vkorg. ""werks.
  ENDIF.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '04'.
    CONCATENATE 'Sociedad:' l_t001w-vkorg 'NO EXISTE'
    INTO e_bapi_msg SEPARATED BY space.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc IS INITIAL.
  g_bus_area = 0 + l_t001w-vkorg.
*-* Genera elemento pep del proyecto
  CONCATENATE 'PE' g_bus_area l_t001k-bukrs p_proye+14
    i_azul_e-matkl(2) i_azul_e-matkl+2(2)
    INTO g_elpep SEPARATED BY '-'.
  CONDENSE g_elpep NO-GAPS.
* GEt pep receptor
  IF i_azul_r[] IS NOT INITIAL.
    READ TABLE i_azul_r INTO ls_rece INDEX 1.
    IF sy-subrc IS INITIAL.
      CONCATENATE 'PE' g_bus_area l_t001k-bukrs p_proye+14
        ls_rece-matkl(2) ls_rece-matkl+2(2)
      INTO r_pep SEPARATED BY '-'.
      CONDENSE r_pep NO-GAPS.
    ENDIF.
  ENDIF.
ENDFORM.                    "validaciones

*&---------------------------------------------------------------------*
*&      Form  ZMMFM_0090_DISPONIBLE
*&---------------------------------------------------------------------*
FORM zmmfm_0090_disponible_tr USING i_proj TYPE everk
      i_azul  TYPE zmmfm_0070_azul CHANGING
      i_subrc TYPE subrc
      i_msg   TYPE  bapi_msg.

  DATA: lmatkl TYPE matkl.
  lmatkl = i_azul-matkl.

  CALL FUNCTION 'ZMMFM_0090_DISPONIBLE'
    EXPORTING
      i_proyect        = i_proj
      i_azul           = lmatkl
   IMPORTING
*     E_AZUL           = i_azul
*     E_PRICE          =
     e_subrc          = i_subrc
     e_bapi_msg       = i_msg.

ENDFORM.                    "ZMMFM_0090_DISPONIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM libera_presupuesto USING i_gjahr  TYPE bp_u_sjahr
                              i_amount TYPE bp_wert3
                              l_proj   TYPE proj
                              i_waers  TYPE twaer
                              r_pep TYPE  prps-posid
                              i_folio TYPE  ihrez
                              i_azul_e  TYPE zmmfm_0070_azul.

  DATA: e_bapi_msg  TYPE  bapi_msg.
  DATA: lt_bpak TYPE STANDARD TABLE OF bpak WITH HEADER LINE,
        e_err   TYPE oax,
        e_subrc TYPE subrc,
        ls_prps TYPE prps,
        e_azul     TYPE matkl,
        lt_ret  TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE.
* Prerar datos
  DATA:   l_pep TYPE  prps-posid.
  PERFORM conversion_exit_abpsn_input USING r_pep CHANGING l_pep.
  SELECT SINGLE * FROM prps INTO ls_prps WHERE posid EQ l_pep.
  ""S_OBJNR
  i_amount = ABS( i_amount ).
  i_amount = i_amount * -1.
  lt_bpak-e_objnr = ls_prps-objnr.                          "PR00000343
  lt_bpak-s_gjahr = i_gjahr.
  lt_bpak-e_gjahr = i_gjahr.
  lt_bpak-s_gnjhr = i_gjahr.
  "" E_GNJHR
  lt_bpak-s_ges = 'X'.
  lt_bpak-e_ges = 'X'.
  lt_bpak-s_profil = l_proj-bprof.                          "ZPS001
  lt_bpak-e_profil = l_proj-bprof.                          "ZPS001
  lt_bpak-s_wrttp = '45'.
  lt_bpak-e_wrttp = '45'.
  lt_bpak-s_vorga = 'KBFR'.
  lt_bpak-e_vorga = 'KBFR'.
  lt_bpak-s_buzei = '000'.
  lt_bpak-e_buzei = '000'.
  lt_bpak-s_perio = '001'.
  lt_bpak-e_perio = '001'.
  lt_bpak-vadat = sy-datum.
  lt_bpak-bldat = sy-datum.
  lt_bpak-wert = i_amount.                                  "1.00-
  lt_bpak-sgtext  = i_folio.   "DEMO BTO
  lt_bpak-twaer = i_waers. "MXN

  APPEND lt_bpak.

  CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
   EXPORTING
     i_budget_activity            = 'KBFR'                  ""'KBN0'
*   I_BUDGET_ACTIV_SUP_RET       = ' '
*   I_COMMIT_DATA                = ' '
     i_delta_amounts              = 'X'
     i_rollup_data                = 'X'
     i_check_plan_data            = 'X'
     i_application                = 'P'
     i_commit_all                 = 'X'
   IMPORTING
     e_errors_found               = e_err
    TABLES
      it_bpak                      = lt_bpak
     it_return                    = lt_ret
   EXCEPTIONS
     no_update                    = 1
     OTHERS                       = 2.

  IF sy-subrc IS NOT INITIAL.
    v_subrc = sy-subrc.
    CONCATENATE 'Project:' l_proj-PSPNR  'Error Liberacion Saldo' INTO e_bapi_msg SEPARATED BY space.
    e_subrc = '04'.
  ELSE.
    e_subrc = '00'.
    CONCATENATE 'Project:' l_proj-PSPNR  'Liberacion Saldo' INTO e_bapi_msg SEPARATED BY space.
  ENDIF.
  e_azul = i_azul_e-matkl.
  PERFORM f_reg_exep_rfc_disp USING v_id_rfc e_subrc e_bapi_msg i_amount e_azul
                             CHANGING v_conse.
  CLEAR e_bapi_msg.

ENDFORM.                    "libera_presupuesto

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cjdw_proj_select_single USING p_pro  TYPE proj-pspid
                          CHANGING p_proj TYPE proj.

  CALL FUNCTION 'CJDW_PROJ_SELECT_SINGLE'
   EXPORTING
     pspid                   = p_pro
*     PSPNR                   = ' '
*     VSNMR                   = ' '
*     MEMORY_ONLY             =
   IMPORTING
     e_proj                  = p_proj
   EXCEPTIONS
     missing_parameter       = 1
     not_found               = 2
          OTHERS                  = 3.

ENDFORM.                    "CJDW_PROJ_SELECT_SINGLE

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM conversion_exit_projn_input USING in CHANGING out.
  CALL FUNCTION 'CONVERSION_EXIT_PROJN_INPUT'
    EXPORTING
      input  = in
    IMPORTING
      output = out.

ENDFORM.                    "CONVERSION_EXIT_PROJN_INPUT

*&---------------------------------------------------------------------*
*   *Aplicar transpaso
*  PERFORM transpaso_saldo USING i_waers i_waers  i_amount i_waers
*      p_pep r_pep e_bapi_msg.
*----------------------------------------------------------------------*
FORM transpaso_saldo USING e_year   TYPE bpdy-u_sjahr
                           r_year   TYPE bpdy-u_sjahr
                           p_amount TYPE bpak-wert
                           p_moneda TYPE bpak-twaer
                           e_pep       TYPE prps-posid
                           r_pep       TYPE prps-posid
                           e_bapi_msg  TYPE  bapi_msg
                           i_folio TYPE  ihrez
                           p_proj TYPE proj.

  DATA:   messtab   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
          e_subrc   TYPE subrc,
          lproj     TYPE proj,
          ctumode   LIKE ctu_params-dismode VALUE 'N',
          cupdate   LIKE ctu_params-updmode VALUE 'L',
          e_azul    TYPE matkl,
          l_bapi_msg  TYPE  bapi_msg,
          lcad(21).

  PERFORM bdc_dynpro      USING 'SAPMKBUA' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=ERFA'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'BPDY-U_SJAHR'.
  lcad = sy-datum+2(2).
  lcad+2 = sy-datum+4(2).
  lcad+4 = sy-datum(4).
  PERFORM bdc_field       USING 'BPDY-BLDAT' lcad.
  PERFORM bdc_field       USING 'BPDY-U_SJAHR' e_year. ""sy-datum(4).
  PERFORM bdc_field       USING 'BPDY-U_EJAHR' r_year.  ""sy-datum(4).
  PERFORM bdc_field       USING 'BPDY-SGTXT' i_folio.
* Next
  PERFORM bdc_dynpro      USING 'SAPMKBUA' '0201'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=BUCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'BPAK-S_GES(01)'.
  PERFORM bdc_field       USING 'PRPS-POSID(01)' e_pep.  ""'PE-0076-5030-001-04-09'.
  PERFORM bdc_field       USING 'PRPSR-POSID(01)' r_pep.  ""'PE-0076-5030-001-04-10'.
  lcad = ABS( p_amount ).
  PERFORM bdc_field       USING 'BPAK-WERT(01)' lcad.       "'1'.
  PERFORM bdc_field       USING 'BPDY-WAER1(01)' p_moneda. ""'MXN'.
  PERFORM bdc_field       USING 'BPAK-S_GES(01)' 'X'.

  CALL TRANSACTION 'CJ34' USING bdcdata
                    MODE   ctumode
                    UPDATE cupdate
                    MESSAGES INTO messtab.

  IF messtab[] IS NOT INITIAL.
    READ TABLE messtab INDEX 1.

    e_subrc = '00'.
    CONCATENATE 'Project:' p_proj-PSPNR  'Traspaso Saldo' INTO e_bapi_msg SEPARATED BY space.
    CONCATENATE  r_pep+17(2) r_pep+20(2) INTO e_azul.
    PERFORM f_reg_exep_rfc_disp USING v_id_rfc e_subrc e_bapi_msg p_amount e_azul
                               CHANGING v_conse.
    e_bapi_msg = messtab-msgv1.
  ENDIF.

* Ultimo paso  dejar ya diponible el monto transpasado
  REFRESH: bdcdata, messtab.
  CLEAR: bdcdata, messtab.
* Start

  GET PARAMETER ID 'CAC' FIELD lproj-pspid.
  IF lproj-pspid IS INITIAL.
    SET PARAMETER ID 'CAC' FIELD p_proj-vkokr.
    PERFORM bdc_dynpro      USING 'SAPLSPO4'   '0300'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'SVALD-VALUE(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=FURT'.
    PERFORM bdc_field       USING 'SVALD-VALUE(01)' 'GIG'.
  ENDIF.

* Start save
  PERFORM bdc_dynpro      USING 'SAPMKBUD' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'PROJ-PSPID'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field       USING 'PROJ-PSPID' p_proj-pspid.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR'  'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=DROT'.
  PERFORM bdc_field       USING 'DROPT-PTIME'  e_year.      "'2013'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=MRKA'.
  PERFORM bdc_field       USING 'DROPT-PTIME' e_year.       "'2013'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=KOPS'.
  PERFORM bdc_field       USING 'DROPT-PTIME'  '2013'.
  PERFORM bdc_dynpro      USING 'SAPLSPO5' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=OK'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'SPOPLI-SELFLAG(08)'.
  PERFORM bdc_field       USING 'SPOPLI-SELFLAG(01)' ''.
  PERFORM bdc_field       USING 'SPOPLI-SELFLAG(08)' 'X'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0706'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'BPDY-COPY_PERC'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTE'.
  PERFORM bdc_field       USING 'BPDY-COPY_PERC' '100.00'.
  PERFORM bdc_field       USING 'BPDY-COPY_REPL' 'X'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=DROT'.
  PERFORM bdc_field       USING 'DROPT-PTIME'  '0'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR'  'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=MRKA'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=KOPS'.
  PERFORM bdc_dynpro      USING 'SAPLSPO5' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=OK'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'SPOPLI-SELFLAG(03)'.
  PERFORM bdc_field       USING 'SPOPLI-SELFLAG(01)' ''.
  PERFORM bdc_field       USING 'SPOPLI-SELFLAG(03)' 'X'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0706'.
  PERFORM bdc_field       USING 'BDC_CURSOR'  'BPDY-COPY_PERC'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTE'.
  PERFORM bdc_field       USING 'BPDY-COPY_PERC' '100.00'.
  PERFORM bdc_field       USING 'BPDY-COPY_REPL' 'X'.
  PERFORM bdc_dynpro      USING 'SAPLKBPP' '0320'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'DROPT-PTIME'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=POST'.

  CALL TRANSACTION 'CJ32' USING bdcdata
                    MODE   ctumode
                    UPDATE cupdate
                    MESSAGES INTO messtab.

  IF messtab[] IS NOT INITIAL.
    MOVE e_bapi_msg TO l_bapi_msg.
    READ TABLE messtab WITH KEY msgid = 'BP'.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE messtab INDEX 1.
    ENDIF.
    IF sy-subrc IS INITIAL.
      l_bapi_msg = messtab-msgv1.
    ENDIF.
    e_subrc = '00'.
    CONCATENATE 'Project:' p_proj-PSPNR  'Traspaso Saldo Disponible' INTO e_bapi_msg SEPARATED BY space.
    CONCATENATE  r_pep+17(2) r_pep+20(2) INTO e_azul.
    PERFORM f_reg_exep_rfc_disp USING v_id_rfc e_subrc e_bapi_msg p_amount e_azul
                               CHANGING v_conse.
    e_bapi_msg = l_bapi_msg.
  ENDIF.


ENDFORM.                    "transpaso_saldo
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ABPSn_INPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_abpsn_input USING p_proye CHANGING l_proye.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING
      input  = p_proye
    IMPORTING
      output = l_proye.
ENDFORM.                   "CONVERSION_EXIT_ABPSn_INPUT
