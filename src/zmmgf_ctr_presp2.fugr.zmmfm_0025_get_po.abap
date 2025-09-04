FUNCTION zmmfm_0025_get_po.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_EBELN) TYPE  ZMMDE_EBELN
*"  EXPORTING
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_ID_ENVIO) TYPE  ZID_REFERENCIA
*"  TABLES
*"      IT_HEADER STRUCTURE  ZMMWA_0020_HEADER_PO
*"      IT_DETAIL STRUCTURE  ZMMWA_0030_DETAIL_PO
*"      IT_SERVICES STRUCTURE  ZMMWA_0040_SERVICES_PO
*"      IT_HISTORIAL STRUCTURE  ZMMWA_0040_HIST
*"      IT_TOTALES STRUCTURE  ZMMWA_0040_TOTAL
*"  EXCEPTIONS
*"      CONFIG_INFO_NOT_FOUND
*"      NO_DATA_HEADER_BAPI_GETITEMS
*"      NO_DATA_DETAIL_BAPI_GETITEMS
*"      PEDS_DESCRT_FOR_DELIND
*"      ERROR_REG_LOG
*"----------------------------------------------------------------------
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Angélica González
*& Fecha            : 05-MARZO-2014
*& Objetivo         : Se requiere modificar el siguiente RFC
*&                    ZMMFM_0025_GET_PO para agregar un campo Grupo de
*&                    Artículos (GPO_ART) para estructurar la información
*&                    en WEB.
*& Transporte       : RSDK907067.
*&---------------------------------------------------------------------*
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Angélica González
*& Fecha            : 10-MARZO-2014
*& Objetivo         : Se requiere para este RFC (ZMMFM_0025_GET_PO)
*&                    mostrar los campos de texto de cabecera del pedido.
*& Transporte       : RSDK907089.
*&---------------------------------------------------------------------*

  DATA: lit_zutil TYPE STANDARD TABLE OF zutil_parameters.
  DATA: lit_headers_list TYPE STANDARD TABLE OF bapiekkol.
  DATA: lit_items_list TYPE TABLE OF bapiekpoc.
  DATA: lit_detail_po TYPE STANDARD TABLE OF bapimepoitem.
  DATA: lit_account_po TYPE STANDARD TABLE OF bapimepoaccount.
  DATA: lit_services_po TYPE TABLE OF bapiesllc.
  DATA: lit_it_historial_po TYPE TABLE OF bapiesllc.
  DATA: lit_textheader_po TYPE STANDARD TABLE OF bapimepotextheader.
  DATA: lit_history_po TYPE TABLE OF bapiekbe.
  DATA: lit_pocond TYPE TABLE OF bapimepocond.
  DATA: lit_ekbe TYPE STANDARD TABLE OF ekbe.
  DATA:  it_ekbe2 TYPE TABLE OF ekbe.
  DATA:  it_ekpo TYPE TABLE OF ekpo.
  DATA:  it_ekbe3 TYPE TABLE OF ekbe.
  DATA:  wa_ekbe3 LIKE LINE OF it_ekbe3.
*  DATA:  it_bkpf TYPE bkpf OCCURS 0.
*** Itab para recuperar pedidos de estatus 2<zzz
  DATA  lit_ekko2 TYPE TABLE OF ekko WITH HEADER LINE.
*** Itab para hacer la union de las 2 tablas con pedidos de estatus 1 y 2.
  "Esta Itab tiene header line por defaul (Otra forma de declarar itabs)
  DATA: BEGIN OF lit_pedidos OCCURS 0,
        ebeln TYPE ebeln,
        verkf TYPE verkf,
        END OF lit_pedidos.

  DATA:  BEGIN OF it_ekko OCCURS 0,
        ebeln TYPE ebeln,
        verkf TYPE verkf,
        usr10 TYPE usr10,
        END OF it_ekko.

  DATA: BEGIN OF lit_prps OCCURS 0,
       posid TYPE posid,
       usr10 TYPE usr10,
       END OF lit_prps.

  DATA: BEGIN OF it_posid OCCURS 0,
      verkf TYPE posid,
     usr10 TYPE usr10,
       END OF it_posid.

  DATA: BEGIN OF it_bseg OCCURS 0,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        shkzg TYPE shkzg,
        dmbtr TYPE dmbtr,
        pswsl TYPE pswsl,
        ktosl TYPE ktosl,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        srvpos TYPE srvpos,
        belnr_aw  TYPE belnr_d,
        gjahr_aw  TYPE gjahr,
        END OF it_bseg.

  DATA: BEGIN OF it_awkey OCCURS 0,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        srvpos TYPE srvpos,
        awkey  TYPE awkey,
        belnr  TYPE belnr_d,
        gjahr  TYPE gjahr,
        END OF it_awkey.

  DATA: BEGIN OF it_bkpf OCCURS 0,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        awkey TYPE awkey,
        END OF it_bkpf.


  DATA:  it_header_log TYPE TABLE OF zmmtt_header_log.
  DATA:  it_detail_log TYPE TABLE OF zmmtt_detail_log.
  DATA:  it_serv_log TYPE TABLE OF zmmtt_serv_log.

  FIELD-SYMBOLS: <fs_ekbe> LIKE LINE OF lit_ekbe.

  DATA: wa_ekbe2 LIKE LINE OF it_ekbe2.
  DATA: lwa_zutil TYPE zutil_parameters.
  DATA: lwa_headers_list LIKE LINE OF lit_headers_list.
  DATA: lwa_ekbe3 LIKE LINE OF it_ekbe3.
  DATA: lwa_header LIKE LINE OF it_header.
  DATA: lwa_detail LIKE LINE OF it_detail.
  DATA: lwa_pedidos LIKE LINE OF lit_pedidos.
  DATA: lwa_services LIKE LINE OF it_services.
  DATA: lwa_historial LIKE LINE OF it_historial.
  DATA: lwa_totales LIKE LINE OF it_totales.
  DATA: lwa_textheader_po LIKE LINE OF lit_textheader_po.
  DATA: lwa_header_po TYPE bapimepoheader.
  DATA: lwa_detail_po TYPE bapimepoitem.
  DATA: lwa_history_po LIKE LINE OF lit_history_po.
  DATA: lwa_historial_po LIKE LINE OF lit_it_historial_po.
  DATA: lwa_services_po LIKE LINE OF lit_services_po,
        lwa_services_po2 LIKE LINE OF lit_services_po.
  DATA: lwa_pocond LIKE LINE OF lit_pocond.
  DATA: lwa_prps LIKE LINE OF lit_prps.
  DATA: wa_ekpo LIKE LINE OF it_ekpo.
  DATA: wa_ekko LIKE LINE OF it_ekko.
  DATA: lit_doc_type TYPE wrf_pbas_esart_rtty. "#EC CI_USAGE_OK[2368913]
  DATA: lwa_doc_type LIKE LINE OF lit_doc_type.

  DATA: lwa_account_po LIKE LINE OF lit_account_po.
  DATA: wa_header_log LIKE LINE OF it_header_log.
  DATA: wa_detail_log LIKE LINE OF it_detail_log.

  DATA: wa_serv_log LIKE LINE OF it_serv_log.
  DATA: v_id_web    TYPE zid_referencia.
  DATA: lv_purch_org TYPE bapiekko-purch_org.
  DATA: v_curr  TYPE p LENGTH 13 DECIMALS 2.


  DATA: lv_item_cat TYPE bapiekpo-item_cat.
  DATA: lv_acctasscat TYPE bapiekpo-acctasscat.
  DATA: lv_prefijo TYPE c LENGTH 2.
  DATA: lv_netpr TYPE netpr.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lv_zflag TYPE zutil_parameters-zchar.
  DATA: v_subrc  TYPE  subrc.   "Variable de subrc para LOG
  DATA: v_id_rfc TYPE zid_referencia.
  DATA  v_conse TYPE zmmde_conse. "Para llevar el consecutivo de cada Param
  DATA: v_c_ped TYPE int4.
  DATA: v_c_pos TYPE int4.
  DATA: v_c_ser TYPE int4.

  DATA: lv_kbetr   TYPE kbetr,  "Pctje Condicion   "MODIFY MCA-20200625 OT: RSDK911349
        lv_taxcode TYPE mwskz.  "Indicador de IVA  "INSERT MCA-20200625 OT: RSDK911349

  FIELD-SYMBOLS: <lfs_account_po> TYPE bapimepoaccount.  "INSERT MCA-20160506 OT:RSDK908972

* Se crea ID para Log de confirmacion de consistencia con WEB
  PERFORM f_log_id_web CHANGING lv_zflag v_id_web e_subrc e_bapi_msg.
  CHECK e_subrc EQ 0.

*  Subrutina para limpieza de tablas de Log de SAP a WEB.
  PERFORM f_auto_limp_log_web.

* Se crea subrutina para LOG de control presupuestal
  PERFORM f_reg_log_conexion2 USING 'ZMMFM_0025_GET_PO' i_ebeln
                             CHANGING v_id_rfc v_conse v_subrc.
  IF v_subrc <> 0.
    e_subrc = '01'.
    e_bapi_msg = 'Error al crear registro en tabla REG_LOG'.
  ENDIF.
  CHECK e_subrc = 0.

  IF i_ebeln IS INITIAL.
    e_subrc = '07'.
    e_bapi_msg = 'Obligatorio especificar un numero de pedido'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK i_ebeln IS NOT INITIAL.

* Obtenemos configuración de tabla zutil_parameters
  SELECT * INTO TABLE lit_zutil
  FROM zutil_parameters
  WHERE zreport = 'ZMMFM_0020_GET_PO'.
  IF sy-subrc <> 0.
    e_subrc = '02'.
    e_bapi_msg = 'Config. incompleta ZUTIL_PARAMETERS'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.


  PERFORM f_get_zutil_param  TABLES lit_zutil
                           USING 'PURCH_ORG'
                         CHANGING lv_purch_org
                                  lv_subrc.

  IF lv_subrc NE 0.
    e_subrc = '02'.
    e_bapi_msg = 'Config. incompleta ZUTIL_PARAMETERS'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
  ENDIF.

  CHECK e_subrc = 0.

  LOOP AT lit_zutil INTO lwa_zutil  WHERE zfield CS 'DOC_TYPE'.
    lwa_doc_type-sign = 'I'.
    lwa_doc_type-option = 'EQ'.
    lwa_doc_type-low = lwa_zutil-zchar.
    APPEND lwa_doc_type TO lit_doc_type.
    CLEAR: lwa_doc_type, lwa_zutil.
  ENDLOOP.
  IF sy-subrc <> 0.
    e_subrc = 02.
    e_bapi_msg = 'Config. incompleta ZUTIL_PARAMETERS'.

    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
  ENDIF.

  CHECK e_subrc = 0.



  PERFORM f_get_zutil_param TABLES lit_zutil
                            USING 'ITEM_CAT'
                          CHANGING lv_item_cat
                                   lv_subrc.
  IF lv_subrc = 02.
    e_subrc = 02.
    e_bapi_msg = 'Config. incompleta ZUTIL_PARAMETERS'.

    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.



  PERFORM f_get_zutil_param TABLES lit_zutil
                          USING 'ACCTASSCAT'
                        CHANGING lv_acctasscat
                                 lv_subrc.
  IF lv_subrc = 02.
    e_subrc = 02.
    e_bapi_msg = 'Config. incompleta ZUTIL_PARAMETERS'.

    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.

  CHECK e_subrc = 0.


  PERFORM f_get_zutil_param TABLES lit_zutil
                          USING  'PREFIJO_PROJ'
                         CHANGING lv_prefijo
                                  lv_subrc.
  IF lv_subrc NE 0.
    e_subrc = 02.
    e_bapi_msg = 'Config. incompleta ZUTIL_PARAMETERS'.

    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
  ENDIF.

  CHECK e_subrc = 0.

  PERFORM f_exe_bapi_po_getittems TABLES lit_headers_list
                                         lit_items_list
                                   USING lit_doc_type
                                         "lv_doc_type
                                         i_ebeln
                                         lv_purch_org
                                         lv_item_cat
                                         lv_acctasscat
                                CHANGING lv_subrc.

  CLEAR: it_ekbe2[], lit_ekko2, lit_ekko2[], lit_pedidos, lit_pedidos[].

  IF lit_headers_list[] IS INITIAL.
    SELECT ebeln cpudt bewtp FROM ekbe
    INTO CORRESPONDING FIELDS OF TABLE it_ekbe2
    WHERE ebeln EQ i_ebeln
    AND  ( bewtp = 'A' OR bewtp = 'E' OR bewtp = 'Q' ).
  ELSE.
    SELECT ebeln cpudt bewtp FROM ekbe
    INTO CORRESPONDING FIELDS OF TABLE it_ekbe2
    FOR ALL ENTRIES IN lit_headers_list
    WHERE ebeln NE lit_headers_list-po_number
    AND ebeln EQ i_ebeln
*    AND   cpudt EQ lv_i_date
    AND  ( bewtp = 'A' OR bewtp = 'E' OR bewtp = 'Q' ).
  ENDIF.
  SORT it_ekbe2 BY ebeln.
  "Como la EKBE es por pos. se eliminan los pedidos duplicados de la LIT_EKBE2
  DELETE ADJACENT DUPLICATES FROM it_ekbe2 COMPARING ebeln.
  "Si se recuperaron pedidos de la EKBE 2 se hace ahora el filtro en la EKKO
  "Usando los ZUTIL_PARAMETERS
  IF it_ekbe2 IS NOT INITIAL.
    SELECT ebeln ekorg bstyp verkf FROM ekko
    INTO CORRESPONDING FIELDS OF TABLE lit_ekko2
    FOR ALL ENTRIES IN it_ekbe2
    WHERE ebeln EQ it_ekbe2-ebeln
    AND   ekorg EQ '1000'
    AND bsart IN lit_doc_type.
  ENDIF.

* En los sig. 2 LOOPs se va a hacer la union de las 2 itabs.
* No se deben de borrar las 2 itabs LIT_HEADER_LIST Y LIT_EKKO2 para sabe el status de cada pedido
  LOOP AT lit_headers_list INTO lwa_headers_list.
    lit_pedidos-ebeln = lwa_headers_list-po_number.
    APPEND lit_pedidos.
    CLEAR: lwa_headers_list, lit_pedidos."Se limpian estructura y header line de itab
  ENDLOOP.
  LOOP AT lit_ekko2.
    lit_pedidos-ebeln = lit_ekko2-ebeln.

    APPEND lit_pedidos.

    CLEAR: lit_ekko2, it_ekko, lit_pedidos. "Se limpian header lines de itabs.
  ENDLOOP.
*** Si valida que se tengan pedidos en la itab de unificacion de pedidos
*** En caso contrario se provoca exepción
  IF lit_pedidos[] IS INITIAL.
    e_subrc = '03'.
    e_bapi_msg = 'No se encontraron registros con el pedido indicado'.

    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.

  CHECK e_subrc = 0.


* Se recorren todos los headers de los pedidos
* Se modifica la itab que se reccorre con los pedidos
*  LOOP AT lit_headers_list INTO lwa_headers_list.
  SORT lit_pedidos BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lit_pedidos COMPARING ebeln.


  PERFORM f_val_proyecto_peds CHANGING lit_pedidos[]
                                       e_subrc.
  IF e_subrc <> 0.
    e_subrc = 03.
    e_bapi_msg = 'El proyecto del pedido no esta marcado para ser comunicado a WEB'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
  ENDIF.

  CHECK e_subrc = 0.

  IF lit_pedidos[] IS NOT INITIAL.
    SELECT * FROM ekbe INTO TABLE it_ekbe3
    FOR ALL ENTRIES IN lit_pedidos
    WHERE ebeln EQ lit_pedidos-ebeln.
  ENDIF.

  LOOP AT it_ekbe3 INTO wa_ekbe3.
    it_awkey-ebeln  = wa_ekbe3-ebeln.
    it_awkey-ebelp  = wa_ekbe3-ebelp.
    it_awkey-srvpos = wa_ekbe3-srvpos.
    it_awkey-belnr  = wa_ekbe3-belnr.
    it_awkey-gjahr  = wa_ekbe3-gjahr.

    CONCATENATE wa_ekbe3-belnr wa_ekbe3-gjahr INTO it_awkey-awkey.
    APPEND it_awkey.
    CLEAR: it_awkey, wa_ekbe3.
  ENDLOOP.

  IF sy-subrc = 0.
    SELECT bukrs belnr gjahr awkey FROM bkpf INTO CORRESPONDING FIELDS OF TABLE it_bkpf
      FOR ALL ENTRIES IN it_awkey
      WHERE awkey = it_awkey-awkey.
  ENDIF.

  IF sy-subrc = 0.

* Quick Fix Replace SELECT from table BSEG by API Call
* 02.07.2025 17:07:44 LTORRES
* Transport RESK900081 ATC - 2
* Replaced Code:
*    SELECT bukrs belnr gjahr shkzg dmbtr pswsl ktosl FROM bseg INTO CORRESPONDING FIELDS OF TABLE it_bseg
*      FOR ALL ENTRIES IN it_bkpf
*      WHERE bukrs EQ it_bkpf-bukrs
*      AND   belnr EQ it_bkpf-belnr
*      AND   gjahr EQ it_bkpf-gjahr
*      AND (  ktosl EQ  'KBS'
*      OR     ktosl EQ  'ZE2'
*      OR     ktosl EQ  'ZE1'
*       OR    ktosl EQ  'WRX' ).

DATA ETL395C4R8814 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L395C4R3385 TYPE FAGL_T_FIELD.
LT_FIELDS_L395C4R3385 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBTR' )
 ( LINE = 'PSWSL' )
 ( LINE = 'KTOSL' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BKPF[]
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND KTOSL EQ 'KBS' OR KTOSL EQ 'ZE2' OR KTOSL EQ 'ZE1' OR KTOSL EQ 'WRX'|
              IT_FIELDLIST = LT_FIELDS_L395C4R3385
    IMPORTING ET_BSEG = ETL395C4R8814
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL395C4R8814 ) > 0.
  CLEAR IT_BSEG.
  TYPES: BEGIN OF TYL395C4R9180,
    BUKRS TYPE BSEG-BUKRS,
    BELNR TYPE BSEG-BELNR,
    GJAHR TYPE BSEG-GJAHR,
    SHKZG TYPE BSEG-SHKZG,
    DMBTR TYPE BSEG-DMBTR,
    PSWSL TYPE BSEG-PSWSL,
    KTOSL TYPE BSEG-KTOSL,
  END OF TYL395C4R9180.
  DATA: LML395C4R8215 TYPE TYL395C4R9180,
        LWL395C4R4336 LIKE LINE OF IT_BSEG.
  LOOP AT ETL395C4R8814 REFERENCE INTO DATA(LDRL395C4R5909).
    LML395C4R8215-BUKRS = LDRL395C4R5909->BUKRS.
    LML395C4R8215-BELNR = LDRL395C4R5909->BELNR.
    LML395C4R8215-GJAHR = LDRL395C4R5909->GJAHR.
    LML395C4R8215-SHKZG = LDRL395C4R5909->SHKZG.
    LML395C4R8215-DMBTR = LDRL395C4R5909->DMBTR.
    LML395C4R8215-PSWSL = LDRL395C4R5909->PSWSL.
    LML395C4R8215-KTOSL = LDRL395C4R5909->KTOSL.
    MOVE-CORRESPONDING LML395C4R8215 TO LWL395C4R4336.
    APPEND LWL395C4R4336 TO IT_BSEG.
  ENDLOOP.
  SY-DBCNT = LINES( ETL395C4R8814 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.

* End of Quick Fix

    SORT it_bseg BY belnr gjahr ktosl.
    FIELD-SYMBOLS <bseg> LIKE LINE OF it_bseg.
    LOOP AT it_bseg ASSIGNING <bseg>.
      READ TABLE it_bkpf WITH KEY belnr = <bseg>-belnr
                                  gjahr = <bseg>-gjahr
                                  bukrs = <bseg>-bukrs.
      READ TABLE it_awkey WITH KEY awkey = it_bkpf-awkey.
      <bseg>-ebeln = it_awkey-ebeln.
      <bseg>-ebelp = it_awkey-ebelp.
      <bseg>-srvpos = it_awkey-srvpos.
      <bseg>-belnr_aw  = it_awkey-belnr.
      <bseg>-gjahr_aw  = it_awkey-gjahr.
    ENDLOOP.
  ENDIF.

  LOOP AT lit_pedidos.
    CLEAR lwa_header.
    CLEAR lwa_textheader_po.

* Se obtiene el historial del pedido
    REFRESH lit_ekbe.

    SELECT * INTO TABLE lit_ekbe
    FROM ekbe
    WHERE ebeln EQ lit_pedidos-ebeln.

* Se ejecuta la BAPI get detail1
    PERFORM f_exe_bapi_po_getdetail1 TABLES lit_detail_po
                                            lit_account_po
                                            lit_services_po
                                            lit_textheader_po
                                            lit_history_po
                                            lit_pocond
*                                      USING lwa_headers_list-po_number
                                      USING lit_pedidos-ebeln
                                   CHANGING lwa_header_po
                                            lv_subrc.

    CASE lv_subrc.
      WHEN 4.
        CONTINUE.
      WHEN 5.
        CONTINUE.
      WHEN 6.
        CONTINUE.
      WHEN 7.
*        CONTINUE.
*      WHEN 8.
*        CONTINUE.
    ENDCASE.

* -> BEGIN OF INSERT MCA-20200625 OT: RSDK911349
* Obtener porcentaje de IVA
    CLEAR: lv_taxcode.
    LOOP AT lit_services_po INTO lwa_services_po2.
      IF lv_taxcode IS INITIAL AND
         lwa_services_po2-tax_code IS NOT INITIAL.
        CLEAR: lv_kbetr.
        SELECT SINGLE kbetr INTO lv_kbetr
          FROM konp
         WHERE kappl = 'MS'
           AND kschl = 'ZNAV'
           AND mwsk1 = lwa_services_po2-tax_code.

        lwa_header-iva  = lv_kbetr / 10.
        lv_taxcode = lwa_services_po2-tax_code.
        EXIT.
      ENDIF.
    ENDLOOP.
* <- END OF INSERT MCA-20200625 OT: RSDK911349

    lwa_header-no_pedido       = lwa_header_po-po_number.
    lwa_header-clase_pedido    = lwa_header_po-doc_type.
    lwa_header-no_proveedor    = lwa_header_po-vendor.
    lwa_header-tipo_contrato   = lwa_header_po-reason_cancel.
    lwa_header-sociedad_fi     = lwa_header_po-comp_code.
    lwa_header-id_proyecto     = lwa_header_po-sales_pers.
    lwa_header-gpo_compras     = lwa_header_po-pur_group.
    lwa_header-fecha_pedido    = lwa_header_po-doc_date.
    lwa_header-in_periodo_val  = lwa_header_po-vper_start.
    lwa_header-fin_periodo_val = lwa_header_po-vper_end.
***
    lwa_header-tipo_cambio = lwa_header_po-exch_rate.
***
* Ajuste incluis campo Referencia
*    lwa_header-referencia = lwa_header_po-our_ref.   "COMMENT MCA-20200520 OT: RSDK911311
    lwa_header-referencia = lwa_header_po-telephone.  "INSERT MCA-20200520 OT: RSDK911311
***
    SELECT SINGLE butxt FROM t001
    INTO lwa_header-desc_sociedad
    WHERE bukrs = lwa_header-sociedad_fi.
* Se determina el estatus = 1 mediante los pedidos contenidos en la lit_headers_list
* LOOP AT lit_headers_list[] TRANSPORTING NO FIELDS WHERE po_number EQ lwa_header-no_pedido.
* Se determina si el pedido debe de ser considerado con estatus 1 {o 2.
    READ TABLE lit_headers_list TRANSPORTING NO FIELDS
    WITH KEY po_number = lwa_header-no_pedido.
    IF sy-subrc EQ 0.
      lwa_header-estatus = 1.
    ELSE.
      READ TABLE lit_ekko2 TRANSPORTING NO FIELDS
      WITH KEY ebeln = lwa_header-no_pedido.
      IF sy-subrc EQ 0.
        lwa_header-estatus = 2.
      ENDIF.
    ENDIF.

*--> Inicia Modificación RSDK907089 - 1.
* IJOF : Se comenta código y se agrega multiple lectura de tabla
*        LIT_TEXTHEADER_PO para los textos de cabecera.

    LOOP AT lit_textheader_po INTO lwa_textheader_po
                             WHERE po_number EQ lwa_header_po-po_number
                               AND text_id   EQ 'F01'.

      CONCATENATE lwa_header-desc_pedido lwa_textheader_po-text_line
             INTO lwa_header-desc_pedido
             SEPARATED BY space.

      IF sy-tabix EQ 1.
        SHIFT lwa_header-desc_pedido BY 1 PLACES LEFT.
      ENDIF.

    ENDLOOP.
*    READ TABLE lit_textheader_po INTO lwa_textheader_po WITH KEY po_number = lwa_header_po-po_number.
*    IF sy-subrc = 0.
*      lwa_header-desc_pedido = lwa_textheader_po-text_line.
*    ENDIF.
*<-- Fin Modificación RSDK907089 - 1.

    lwa_header-moneda_c = lwa_header_po-currency.


    LOOP AT it_ekpo INTO wa_ekpo " WHERE loekz <>'L'
    WHERE ebeln EQ lwa_header-no_pedido.
      lv_netpr = lv_netpr + wa_ekpo-brtwr.
    ENDLOOP.

    lwa_header-importe_pedido   = lv_netpr.

* Se recorre la lista de items
*   Se agrega ajuste apra solo proporcionar las posiciones que no esten marcadas como borradas ***
    LOOP AT lit_detail_po INTO lwa_detail_po. "WHERE delete_ind = ' '.

      PERFORM f_build_detail TABLES lit_pocond
                              USING lwa_header_po
                                    lwa_detail_po
                           CHANGING lwa_detail.

* Se recuppera Subpaquete que contiene servicios de posicion.
      CLEAR lwa_services_po2.
      READ TABLE lit_services_po INTO lwa_services_po2
      WITH KEY pckg_no = lwa_detail_po-pckg_no.
*      CLEAR: lwa_header-iva. "COMMENT MCA-20200625 OT: RSDK911349
* Se obtienen los servicios del subpaquete recuperado
* correspondientes a la POS del Pedido
* Se agrega validacion de no enviar servicios borrados
      LOOP AT  lit_services_po INTO  lwa_services_po
      WHERE pckg_no = lwa_services_po2-subpckg_no.

* Se construye la tabla de servicios
        PERFORM f_build_services TABLES lit_account_po
                                        lit_history_po
                                        lit_ekbe
                                  USING lwa_detail_po
                                        lwa_services_po
                                        lv_prefijo
                               CHANGING lwa_header_po
                                        lwa_services
                                        lwa_header.


        APPEND lwa_services TO it_services.

      ENDLOOP.

      "Se limpian variables de WA a trabajar en el LOOP.
      "Se hace LOOP para determinar si existen Pos de HIST de tipo Q

      LOOP AT lit_history_po INTO lwa_history_po
              WHERE po_item EQ lwa_detail-no_posicion AND hist_type = 'A'.
        IF lwa_detail-anticipo IS INITIAL OR lwa_detail-moneda_anticipo IS INITIAL.
          lwa_detail-anticipo = lwa_history_po-hist_type.
          lwa_detail-moneda_anticipo = lwa_history_po-currency.
        ENDIF.

        IF lwa_history_po-db_cr_ind EQ 'H'.
          v_curr = lwa_history_po-val_loccur * -1.
          lwa_detail-saldo_anticipo = lwa_detail-saldo_anticipo + v_curr.
        ELSE.
          lwa_detail-saldo_anticipo = lwa_detail-saldo_anticipo + lwa_history_po-val_loccur.
        ENDIF.
        CLEAR: v_curr.
        IF lwa_history_po-mat_doc BETWEEN '2000000000' AND '2499999999'.
          IF lwa_history_po-mat_doc BETWEEN '2000000000' AND '2099999999'.
            v_curr = lwa_history_po-val_loccur * -1.
            lwa_detail-monto_anticipo = lwa_detail-monto_anticipo + v_curr.
          ENDIF.
          IF lwa_history_po-mat_doc BETWEEN '2400000000' AND '2499999999'.
            lwa_detail-monto_anticipo = lwa_detail-monto_anticipo + lwa_history_po-val_loccur.
          ENDIF.
        ENDIF.
        CLEAR: v_curr, lwa_history_po.
      ENDLOOP.

      IF lwa_detail-anticipo IS NOT INITIAL.
        COLLECT lwa_detail INTO it_detail.
      ENDIF.

      CLEAR: lwa_detail-anticipo, lwa_detail-monto_anticipo, lwa_detail-moneda_anticipo, v_curr.
      "Se hace LOOP para determinar si existen Pos de HIST de tipo A
      "Se verifica si ya se tiene agregado un registro para este Pedido-Pos
      "En caso de no existir se agrega 1
      READ TABLE it_detail TRANSPORTING NO FIELDS
      WITH KEY no_pedido   = lwa_detail-no_pedido
               no_posicion = lwa_detail-no_posicion.
      IF sy-subrc NE 0."Si no se lozalizo ningun reg. con ese pedido-posicion
        COLLECT lwa_detail INTO it_detail."Se agrega regs. con 3 campos vacios
      ENDIF.

      LOOP AT it_ekbe3 INTO lwa_ekbe3
           WHERE ebeln  EQ lwa_services-no_pedido
            AND  ebelp  EQ lwa_services-no_posicion
            AND  srvpos <> ' '
*            AND (  bewtp  EQ 'Q' OR bewtp  EQ 'E' ).
            AND bewtp  EQ 'Q'.

        lwa_historial-no_pedido        =   lwa_ekbe3-ebeln.
        lwa_historial-no_posicion      =   lwa_ekbe3-ebelp.
        lwa_historial-docto_material   =   lwa_ekbe3-belnr.
        lwa_historial-fecha_contable   =   lwa_ekbe3-budat.
        lwa_historial-tipo_historial   =   lwa_ekbe3-bewtp.
        lwa_historial-cantidad_hist    =   lwa_ekbe3-menge.
        lwa_historial-importe_hist     =   lwa_ekbe3-dmbtr.
        lwa_historial-moneda_hist      =   lwa_ekbe3-waers.
        lwa_historial-debe_haber       =   lwa_ekbe3-shkzg.
        lwa_historial-referencia_web   =   lwa_ekbe3-xblnr.
        lwa_historial-referencia_sap   =   lwa_ekbe3-lfbnr.
        lwa_historial-id_servicio_hist =   lwa_ekbe3-srvpos.
        COLLECT lwa_historial  INTO  it_historial.

        CLEAR:lwa_historial,lwa_ekbe3.
      ENDLOOP.

      LOOP AT it_ekbe3 INTO lwa_ekbe3
          WHERE ebeln  EQ lwa_services-no_pedido
           AND  ebelp  EQ lwa_services-no_posicion
           AND  srvpos <> ' '
           AND  bewtp EQ 'E'.
        lwa_historial-no_pedido        =   lwa_ekbe3-ebeln.
        lwa_historial-no_posicion      =   lwa_ekbe3-ebelp.
        lwa_historial-docto_material   =   lwa_ekbe3-belnr.
        lwa_historial-fecha_contable   =   lwa_ekbe3-budat.
        lwa_historial-referencia_web   =   lwa_ekbe3-xblnr.
        lwa_historial-referencia_sap   =   lwa_ekbe3-lfbnr.
        lwa_historial-id_servicio_hist =   lwa_ekbe3-srvpos.
        LOOP AT it_bseg WHERE ebeln = lwa_ekbe3-ebeln
                        AND   ebelp = lwa_ekbe3-ebelp
                        AND   srvpos = lwa_ekbe3-srvpos
                        AND   belnr_aw = lwa_ekbe3-belnr
                        AND   gjahr    = lwa_ekbe3-gjahr.

          CASE it_bseg-ktosl.
            WHEN 'KBS'.
              lwa_historial-tipo_historial = 'K'.
            WHEN 'ZE1'.
              lwa_historial-tipo_historial = 'F'.
            WHEN 'ZE2'.
              lwa_historial-tipo_historial = 'M'.
            WHEN 'WRX'.
              lwa_historial-tipo_historial = 'E'.
          ENDCASE.

          IF it_bseg-ktosl = 'WRX' .
            lwa_historial-cantidad_hist = lwa_ekbe3-menge.
          ELSE.
            lwa_historial-cantidad_hist = '0'.
          ENDIF.

          lwa_historial-importe_hist  = it_bseg-dmbtr.
          lwa_historial-moneda_hist   = it_bseg-pswsl.
          lwa_historial-debe_haber    = it_bseg-shkzg.
          APPEND  lwa_historial  TO  it_historial.
          CLEAR: lwa_historial-importe_hist, lwa_historial-cantidad_hist,
                 lwa_historial-moneda_hist, lwa_historial-debe_haber,
                 lwa_historial-tipo_historial.

        ENDLOOP.

        CLEAR:lwa_historial,lwa_ekbe3.
        SORT it_historial BY no_pedido no_posicion docto_material tipo_historial.

      ENDLOOP.

      CLEAR lwa_totales.
      LOOP AT it_ekbe3 INTO lwa_ekbe3
        WHERE ebeln  = lwa_header-no_pedido
         AND  ebelp  = lwa_detail-no_posicion
         AND  srvpos <> ' '
         AND  bewtp = 'E'.

        lwa_totales-no_pedido_t   = lwa_ekbe3-ebeln.
        lwa_totales-no_posicion_t = lwa_ekbe3-ebelp.
        lwa_totales-id_servicio_t = lwa_ekbe3-srvpos.
        lwa_totales-total_cantidad = '0'.

        LOOP AT it_bseg WHERE ebeln = lwa_ekbe3-ebeln
                        AND   ebelp = lwa_ekbe3-ebelp
                        AND   srvpos = lwa_ekbe3-srvpos
                        AND   belnr_aw = lwa_ekbe3-belnr
                        AND   gjahr    = lwa_ekbe3-gjahr.
*
          CASE it_bseg-ktosl.
            WHEN 'KBS'.
              lwa_totales-tipo_historial_t = 'K'.
            WHEN 'ZE1'.
              lwa_totales-tipo_historial_t = 'F'.
            WHEN 'ZE2'.
              lwa_totales-tipo_historial_t = 'M'.
            WHEN 'WRX'.
              lwa_totales-tipo_historial_t = 'E'.
          ENDCASE.

          IF it_bseg-ktosl = 'WRX' .
            IF lwa_ekbe3-shkzg EQ 'H'.
              lwa_totales-total_cantidad = lwa_ekbe3-menge * -1.
            ELSE.
              lwa_totales-total_cantidad = lwa_ekbe3-menge.
            ENDIF.
          ENDIF.

          IF it_bseg-shkzg EQ 'H'.
            lwa_totales-importe_estima_t = it_bseg-dmbtr * -1.
          ELSE.
            lwa_totales-importe_estima_t = it_bseg-dmbtr.
          ENDIF.

          COLLECT lwa_totales INTO it_totales.
          CLEAR: lwa_totales-tipo_historial_t, lwa_totales-total_cantidad.

        ENDLOOP.

        CLEAR:lwa_totales,lwa_ekbe3.
        SORT it_totales BY no_pedido_t no_posicion_t id_servicio_t tipo_historial_t.

      ENDLOOP.

    ENDLOOP.
* ->BEGIN OF INSERT MCA-20160506 OT:RSDK908972
* Mueve el indicador de borrado a los precios brutos para que no se consideren
    LOOP AT lit_detail_po INTO lwa_detail_po WHERE delete_ind IS NOT INITIAL.
      READ TABLE lit_account_po ASSIGNING <lfs_account_po>
        WITH KEY po_item = lwa_detail_po-po_item.
      MOVE lwa_detail_po-delete_ind TO <lfs_account_po>-delete_ind.
    ENDLOOP.
* <- END OF INSERT MCA-20160506 OT:RSDK908972
    PERFORM f_get_importe_pedido     TABLES  lit_account_po
                                   CHANGING lwa_header-importe_pedido.


* Se agrega validacion para revisar que al menos se tenga 1 posicion para
* un pedido, esto como complicacion de validacion de posiciones marcadas como borradas
    READ TABLE it_detail TRANSPORTING NO FIELDS
    WITH KEY no_pedido = lwa_header-no_pedido.
    IF sy-subrc EQ 0. "Si se encontro al menos 1 posicion si se agrega cabecera
      APPEND lwa_header TO it_header.
    ENDIF.
    CLEAR: lwa_header_po, lit_account_po[], lit_services_po[], lit_history_po[].
    CLEAR: lit_pedidos.
  ENDLOOP.


  " Se valida si la tabla de cabeceras esta vacia, si es asi se manda mensaje de regreso
  IF it_header[] IS INITIAL.
    e_subrc = '04'.
    e_bapi_msg = 'Pedido descartado por Indicador de borrado a nivel POS'.
    PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.

  IF e_subrc = 0.
    PERFORM f_log_web TABLES it_header it_detail it_services it_header_log it_detail_log it_serv_log
                      USING v_id_web
                      CHANGING e_id_envio e_bapi_msg e_subrc.
    IF e_subrc <> 0.
      PERFORM f_reg_exep_rfc2 USING v_id_rfc e_subrc e_bapi_msg
                              CHANGING v_conse.
    ENDIF.
  ENDIF.
  CHECK e_subrc = 0.

  e_bapi_msg = 'Ejecucion exitosa'.

  PERFORM f_reg_log_return2  TABLES it_header it_detail it_services it_historial it_totales
                             USING e_bapi_msg v_id_rfc v_id_web e_subrc
                             CHANGING v_conse.



ENDFUNCTION.
