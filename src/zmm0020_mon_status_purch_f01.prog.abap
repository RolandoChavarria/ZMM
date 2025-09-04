*&---------------------------------------------------------------------*
*&  Include           ZMM0020_MON_STATUS_PURCH_F01
*&---------------------------------------------------------------------*
*&      Form  F_GET_EBELN_FROM_PSPID
*&---------------------------------------------------------------------*
*       Se obtienen los pedidos ligados al proyecto
*----------------------------------------------------------------------*
*      -->P_LIT_PSPIDEBELN  text
*      -->P_P_PSPID  text
*----------------------------------------------------------------------*
FORM f_get_ebeln_from_pspid  TABLES   pit_pspidebeln
                                              STRUCTURE zmmwa_pspidebeln
                                      pit_bedat
                                        "Insertar nombre correcto para

                             USING    p_pspid.

  SELECT verkf ebeln INTO TABLE pit_pspidebeln
  FROM ekko
  WHERE verkf = p_pspid
    AND bedat IN pit_bedat
    .


  IF sy-subrc NE 0.
    MESSAGE i000(zggi) WITH
    'No existen pedidos para el proyecto y fecha indicados' DISPLAY LIKE 'E'.

  ENDIF.
ENDFORM.                    " F_GET_EBELN_FROM_PSPID
*&---------------------------------------------------------------------*
*&      Form  F_GET_INFO_HEADER_MONITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_PSPID  text
*      -->P_LIT_HEADER_MONITOR  text
*----------------------------------------------------------------------*
FORM f_get_info_monitor
        TABLES   pit_pspidebeln STRUCTURE zmmwa_pspidebeln
                 pit_header_monitor STRUCTURE zmmwa_0020_header_monitor
                 pit_detail_monitor STRUCTURE
                                         zmmwa_0020_detail_services_mon.


  DATA: lit_detail_po TYPE STANDARD TABLE OF bapimepoitem.
  DATA: lit_account_po TYPE STANDARD TABLE OF bapimepoaccount.
  data: lit_SCHEDULE_po type STANDARD TABLE OF  BAPIMEPOSCHEDULE.
  DATA: lit_services_po TYPE TABLE OF bapiesllc.
  DATA: lit_textheader_po TYPE STANDARD TABLE OF bapimepotextheader.
  DATA: lit_zutil TYPE STANDARD TABLE OF zutil_parameters.

  DATA: lit_history_po TYPE TABLE OF bapiekbe.
  DATA: lwa_header_po TYPE bapimepoheader.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lv_werks_name1 TYPE name1.
  DATA: lv_lifnr_name1 TYPE name1.
  DATA: lv_subpkg TYPE packno.
  DATA: lv_delete TYPE c LENGTH 1.
  DATA: lv_delete2 TYPE c LENGTH 1.
  DATA: lwa_header_monitor LIKE LINE OF pit_header_monitor.
  DATA: lwa_detail_monitor LIKE LINE OF pit_detail_monitor.

  DATA: lit_ekbe TYPE STANDARD TABLE OF ekbe.

  FIELD-SYMBOLS: <fs_pspidebeln> LIKE LINE OF pit_pspidebeln.
  FIELD-SYMBOLS: <fs_detail_po> LIKE LINE OF lit_detail_po.
  FIELD-SYMBOLS: <fs_services_po> LIKE LINE OF lit_services_po.
  FIELD-SYMBOLS: <fs_schedule_po> LIKE LINE OF lit_schedule_po.
  FIELD-SYMBOLS: <fs_ekbe> LIKE LINE OF lit_ekbe.
  FIELD-SYMBOLS: <fs_zutil> LIKE LINE OF lit_zutil.
  FIELD-SYMBOLS: <fs_account_po> LIKE LINE OF lit_account_po.

* Se obtiene el historial de los pedidos
  IF pit_pspidebeln[] IS NOT INITIAL.
    SELECT * INTO TABLE lit_ekbe
    FROM ekbe FOR ALL ENTRIES IN pit_pspidebeln
    WHERE ebeln = pit_pspidebeln-ebeln.
  ENDIF.
*  break devlpext.

  SELECT * INTO TABLE lit_zutil
  FROM zutil_parameters
  WHERE zreport     = 'ZMM0020_MON_STATUS_PURCH'
    AND zfield  LIKE  '%TIPO_ENTRADA%'.


  LOOP AT pit_pspidebeln ASSIGNING <fs_pspidebeln>.
    CLEAR lwa_header_monitor .
    CLEAR lv_werks_name1.
    CLEAR lv_lifnr_name1.
    CLEAR lv_subrc.

    PERFORM f_exe_bapi_po_getdetail1 TABLES lit_detail_po
                                             lit_account_po
                                             lit_services_po
                                             lit_textheader_po
                                             lit_history_po
                                             lit_SCHEDULE_po
                                       USING <fs_pspidebeln>-ebeln
                                    CHANGING lwa_header_po
                                             lv_subrc.

    CASE lv_subrc.
      WHEN 4.
        CONTINUE.
      WHEN 5.
        CONTINUE.
      WHEN 6.
        CONTINUE.
    ENDCASE.
*    break devlpext.

    LOOP AT lit_detail_po ASSIGNING <fs_detail_po> . "WHERE delete_ind NE 'L'.<<<<<<<.

      CLEAR lwa_header_monitor.
      IF lv_lifnr_name1 IS INITIAL AND lv_werks_name1 IS INITIAL.
        PERFORM f_get_field_value USING 'T001W'
                                        'WERKS'
                                        'NAME1'
                                        <fs_detail_po>-plant
                               CHANGING lv_werks_name1.

        PERFORM f_get_field_value USING 'LFA1'
                                        'LIFNR'
                                        'NAME1'
                                        lwa_header_po-vendor
                               CHANGING lv_lifnr_name1.

      ENDIF.
      loop at lit_schedule_po assigning <fs_schedule_po>.
      lwa_header_monitor-ebeln         = lwa_header_po-po_number.
      lwa_header_monitor-aedat         = lwa_header_po-creat_date.
      lwa_header_monitor-lifnr         = lwa_header_po-vendor.
      lwa_header_monitor-clase         = lwa_header_po-doc_type.
      lwa_header_monitor-tipo_contrato = lwa_header_po-reason_cancel.

      lwa_header_monitor-aedat_entreg  = <fs_schedule_po>-STAT_DATE.
      lwa_header_monitor-ebelp         = <fs_detail_po>-po_item.
      lwa_header_monitor-txz01         = <fs_detail_po>-short_text.
      lwa_header_monitor-lifnr_name1   = lv_lifnr_name1.
      endloop.

* Se obtiene el numero de paquete de la tabla de servicios
      PERFORM f_get_pckg_service TABLES lit_services_po
                                 USING  <fs_detail_po>-pckg_no
                              CHANGING lv_subpkg.

* Se arman los servicios
      LOOP AT lit_services_po ASSIGNING <fs_services_po>
                                  WHERE pckg_no = lv_subpkg.
        " AND delete_ind IS INITIAL.<<<<<
*        CLEAR lwa_detail_monitor.
*        CLEAR lwa_header_monitor.

        CLEAR lwa_header_monitor-id_servicio.
        CLEAR lwa_header_monitor-desc_servicio.
        CLEAR lwa_header_monitor-monto_neto.
        CLEAR lwa_header_monitor-waers.
        CLEAR lwa_header_monitor-elemento_pep.
        CLEAR lwa_header_monitor-sum_ctd.
        CLEAR lwa_header_monitor-status_atraso.
        CLEAR lwa_header_monitor-semaf.
        CLEAR lwa_header_monitor-cantidad.

        lwa_header_monitor-id_servicio    = <fs_services_po>-service.
        lwa_header_monitor-desc_servicio  = <fs_services_po>-short_text.
        lwa_header_monitor-cantidad       = <fs_services_po>-quantity.
        lwa_header_monitor-monto_neto     = <fs_services_po>-gr_price * <fs_services_po>-quantity.
        lwa_header_monitor-waers          = lwa_header_po-currency.

        READ TABLE lit_account_po ASSIGNING <fs_account_po> WITH KEY po_item = <fs_detail_po>-po_item.
        IF sy-subrc EQ 0.
          lwa_header_monitor-elemento_pep   = <fs_account_po>-wbs_element.
        ENDIF.
* Se arman los detalles de las posiciones de EM
        LOOP AT lit_ekbe ASSIGNING <fs_ekbe> WHERE ebeln   = lwa_header_po-po_number
                                               AND ebelp   = <fs_detail_po>-po_item
                                               AND srvpos  = <fs_services_po>-service
                                               AND bewtp   = 'E'.


*          lwa_detail_monitor-no_hoja_entrada = <fs_ekbe>-belnr.
          lwa_detail_monitor-fecha_entrada   = <fs_ekbe>-budat.

          PERFORM f_calculate_ctds TABLES lit_zutil
                                   USING <fs_ekbe>-shkzg
                                         <fs_ekbe>-menge
                                         'E'
                                CHANGING lwa_detail_monitor-ctd
                                         lwa_header_monitor-sum_ctd
                                         lwa_detail_monitor-tipo_entrada
                                         lwa_header_monitor-porce
                                      .

          lwa_detail_monitor-belnr           = <fs_ekbe>-belnr.
          lwa_detail_monitor-gjahr           = <fs_ekbe>-gjahr.
          lwa_detail_monitor-rmwwr           = <fs_ekbe>-dmbtr.
          lwa_detail_monitor-waers           = <fs_ekbe>-waers.

          lwa_detail_monitor-ebeln       = lwa_header_po-po_number.
          lwa_detail_monitor-ebelp       = <fs_detail_po>-po_item.
          lwa_detail_monitor-id_servicio = <fs_services_po>-service.


          IF <fs_ekbe>-shkzg = 'H'.
            lwa_detail_monitor-rmwwr = lwa_detail_monitor-rmwwr * -1.
          ENDIF.
          lwa_detail_monitor-bewtp = 'E'.
          APPEND lwa_detail_monitor TO pit_detail_monitor.

          CLEAR lwa_detail_monitor-fecha_entrada.
          CLEAR lwa_detail_monitor-ctd.
*          CLEAR lwa_header_monitor-sum_ctd.
          CLEAR lwa_detail_monitor-tipo_entrada.
*          CLEAR lwa_header_monitor-porce.
          CLEAR lwa_detail_monitor-belnr .
          CLEAR lwa_detail_monitor-gjahr .
          CLEAR lwa_detail_monitor-rmwwr .
          CLEAR lwa_detail_monitor-waers .
          CLEAR lwa_detail_monitor-ebeln .
          CLEAR lwa_detail_monitor-ebelp .

        ENDLOOP.



* Se calcula el porcentaje real de avance del servicio
        IF <fs_services_po>-quantity NE 0.
          lwa_header_monitor-sum_ctd = ( lwa_header_monitor-sum_ctd / <fs_services_po>-quantity ) * 100.
        ENDIF.

        IF lwa_header_monitor-sum_ctd EQ 100.
          lwa_header_monitor-status_atraso = 'COMPLETADO'.
          lwa_header_monitor-semaf = icon_green_light.
        ELSE.
          IF sy-datum GT <fs_detail_po>-price_date.
            lwa_header_monitor-status_atraso = 'ATRASADO'.
          ELSE.
            lwa_header_monitor-status_atraso = 'EN TIEMPO'.
          ENDIF.

          IF <fs_detail_po>-delete_ind IS NOT INITIAL
           OR <fs_services_po>-delete_ind IS NOT INITIAL.
            lwa_header_monitor-status_atraso = 'BORRADO'.
          ENDIF.

          IF lwa_header_monitor-sum_ctd EQ 0..
            lwa_header_monitor-semaf = icon_red_light.
          ELSE.
            lwa_header_monitor-semaf = icon_yellow_light.
          ENDIF.

        ENDIF.


        APPEND lwa_header_monitor TO pit_header_monitor.


        LOOP AT lit_ekbe ASSIGNING <fs_ekbe> WHERE
                                        ebeln = lwa_header_po-po_number
                                    AND ebelp = <fs_detail_po>-po_item
                                  AND srvpos  = <fs_services_po>-service
                                  AND bewtp   = 'Q'.

          PERFORM f_calculate_ctds TABLES lit_zutil
                                  USING <fs_ekbe>-shkzg
                                        <fs_ekbe>-menge
                                        'Q'
                               CHANGING lwa_detail_monitor-ctd
                                        lwa_header_monitor-sum_ctd
                                        lwa_detail_monitor-tipo_entrada
                                        lwa_header_monitor-porce.
          lwa_detail_monitor-fecha_entrada   = <fs_ekbe>-budat.
          lwa_detail_monitor-belnr           = <fs_ekbe>-belnr.
          lwa_detail_monitor-gjahr           = <fs_ekbe>-gjahr.
          lwa_detail_monitor-rmwwr           = <fs_ekbe>-dmbtr.
          lwa_detail_monitor-waers           = <fs_ekbe>-waers.
          lwa_detail_monitor-id_servicio = <fs_services_po>-service.
          lwa_detail_monitor-ebeln       = lwa_header_po-po_number.
          lwa_detail_monitor-ebelp       = <fs_detail_po>-po_item.
          IF <fs_ekbe>-shkzg = 'H'.
            lwa_detail_monitor-rmwwr = lwa_detail_monitor-rmwwr * -1.
          ENDIF.
          lwa_detail_monitor-bewtp = 'Q'.

          APPEND lwa_detail_monitor TO pit_detail_monitor.



          CLEAR lwa_detail_monitor-fecha_entrada.
          CLEAR lwa_detail_monitor-ctd.
          CLEAR lwa_detail_monitor-tipo_entrada.
          CLEAR lwa_detail_monitor-belnr .
          CLEAR lwa_detail_monitor-gjahr .
          CLEAR lwa_detail_monitor-rmwwr .
          CLEAR lwa_detail_monitor-waers .
          CLEAR lwa_detail_monitor-ebeln .
          CLEAR lwa_detail_monitor-ebelp .

        ENDLOOP.

        CLEAR lwa_detail_monitor .
      ENDLOOP.

    endloop.
  ENDLOOP.



ENDFORM.                    " F_GET_INFO_HEADER_MONITOR
*&---------------------------------------------------------------------*
*&      Form  F_EXE_BAPI_PO_GETDETAIL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_DETAIL_PO  text
*      -->P_LIT_ACCOUNT_PO  text
*      -->P_LIT_SERVICES_PO  text
*      -->P_LIT_TEXTHEADER_PO  text
*      -->P_LIT_HISTORY_PO  text
*      -->P_<FS_PSPIDEBELN>_EBELN  text
*      <--P_LWA_HEADER_PO  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_exe_bapi_po_getdetail1
                TABLES   pit_detail_po STRUCTURE bapimepoitem
                         pit_account_po STRUCTURE bapimepoaccount
                         pit_services_po STRUCTURE bapiesllc
                         pit_textheader_po STRUCTURE bapimepotextheader
                         pit_history_po STRUCTURE bapiekbe
                         pit_SCHEDULE_po STRUCTURE  BAPIMEPOSCHEDULE
                USING    p_po_number
                CHANGING pwa_header_po TYPE bapimepoheader
                         pv_subrc TYPE sy-subrc.

  REFRESH: pit_detail_po[].
  REFRESH: pit_account_po[].
  REFRESH: pit_services_po[].
  REFRESH: pit_textheader_po[].
  REFRESH: pit_history_po[].
  refresh: pit_schedule_po[].
  CLEAR: pwa_header_po.



  CALL FUNCTION 'BAPI_PO_GETDETAIL1'  "#EC CI_USAGE_OK[2438131]
     EXPORTING
        purchaseorder            = p_po_number
        account_assignment       = 'X'
        item_text                = 'X'
        header_text              = 'X'
        delivery_address         = 'X'
        version                  = 'X'
        services                 = 'X'
    IMPORTING
      poheader               = pwa_header_po
*   POEXPIMPHEADER           =
    TABLES
*   RETURN                   =
      poitem                 = pit_detail_po[]
*   POADDRDELIVERY           =
    POSCHEDULE               = pit_schedule_po[]
      poaccount                = pit_account_po[]
*   POCONDHEADER             =
*   POCOND                   =
*   POLIMITS                 =
*   POCONTRACTLIMITS         =
      poservices               = pit_services_po[]
*   POSRVACCESSVALUES        =
      potextheader             = pit_textheader_po[]
*   POTEXTITEM               =
*   POEXPIMPITEM             =
*   POCOMPONENTS             =
*   POSHIPPINGEXP            =
      pohistory                = pit_history_po[]
*   POHISTORY_TOTALS         =
*   POCONFIRMATION           =
*   ALLVERSIONS              =
*   POPARTNER                =
*   EXTENSIONOUT             =
             .

  IF pit_detail_po[] IS INITIAL.
    pv_subrc = 4.
    EXIT.
  ENDIF.

  IF pit_account_po[] IS INITIAL.
    pv_subrc = 5.
    EXIT.
  ENDIF.

  IF pit_services_po[] IS INITIAL.
    pv_subrc = 6.
    EXIT.
  ENDIF.

  IF pit_textheader_po[] IS INITIAL.
    pv_subrc = 7.
    EXIT.
  ENDIF.

  IF pit_history_po[] IS INITIAL.
    pv_subrc = 8.
    EXIT.
  ENDIF.

  IF pit_schedule_po[] IS INITIAL.
    pv_subrc = 9.
    EXIT.
  ENDIF.

ENDFORM.                    " F_EXE_BAPI_PO_GETDETAIL1
*&---------------------------------------------------------------------*
*&      Form  F_GET_FIELD_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0198   text
*      -->P_0199   text
*      <--P_LV_WERKS_NAME1  text
*----------------------------------------------------------------------*
FORM f_get_field_value  USING    p_tabname
                                 p_colwher
                                 p_colname
                                 p_valwher
                        CHANGING pv_value.
  DATA: lv_where TYPE string.
  CONCATENATE: p_colwher ' = ''' p_valwher '''' INTO lv_where.

  SELECT SINGLE (p_colname) INTO pv_value
  FROM (p_tabname)
  WHERE (lv_where).
ENDFORM.                    " F_GET_FIELD_VALUE
*&---------------------------------------------------------------------*
*&      Form  F_GET_PCKG_SERVICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_SERVICES_PO  text
*      -->P_<FS_DETAIL_PO>_PCKG_NO  text
*      <--P_LV_SUBPCKG  text
*----------------------------------------------------------------------*
FORM f_get_pckg_service  TABLES   pit_services STRUCTURE bapiesllc
                                    "Insertar nombre correcto para <...>
                         USING    pv_pckg_no
                         CHANGING pv_subpckg.

  FIELD-SYMBOLS: <fs_services> LIKE LINE OF pit_services.
  CLEAR:  pv_subpckg.
  READ TABLE pit_services ASSIGNING <fs_services>
  WITH KEY pckg_no = pv_pckg_no.
  IF sy-subrc EQ 0.
    pv_subpckg = <fs_services>-subpckg_no.
  ENDIF.
ENDFORM.                    " F_GET_PCKG_SERVICE
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_HEADER_MONITOR  text
*      -->P_LIT_DETAIL_MONITOR  text
*----------------------------------------------------------------------*
FORM f_show_alv  TABLES
            pit_header_monitor STRUCTURE zmmwa_0020_header_monitor
            pit_detail_monitor STRUCTURE zmmwa_0020_detail_services_mon.
  DATA: lwa_layout TYPE slis_layout_alv.
  lwa_layout-colwidth_optimize = 'X'.
  lwa_layout-colwidth_optimize = 'X'.

  PERFORM f_build_fcat  USING 'GIT_HEADER_MONITOR'
                              'ZMMWA_0020_HEADER_MONITOR'
                     CHANGING gv_repid.

  PERFORM f_build_fcat  USING 'GIT_DETAIL_MONITOR'
                              'ZMMWA_0020_DETAIL_SERVICES_MON'
                     CHANGING gv_repid.

  PERFORM f_build_hierarchy_alv.


  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
*   I_INTERFACE_CHECK              = ' '
     i_callback_program             = gv_repid
*   I_CALLBACK_PF_STATUS_SET       = ' '
     i_callback_user_command        = slis_ev_user_command
   is_layout                      = lwa_layout
     it_fieldcat                    = git_fieldcat[]
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        =
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
*   I_DEFAULT                      = 'X'
     i_save                         = 'A'
*   IS_VARIANT                     =
     it_events                      = git_events[]
*   IT_EVENT_EXIT                  =
      i_tabname_header               = gv_tabname_header
      i_tabname_item                 = gv_tabname_item
*   I_STRUCTURE_NAME_HEADER        =
*   I_STRUCTURE_NAME_ITEM          =
      is_keyinfo                     = gwa_keyinfo
     is_print                       =  gd_prntparams
*   IS_REPREP_ID                   =
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                =
*   IR_SALV_HIERSEQ_ADAPTER        =
*   IT_EXCEPT_QINFO                =
*   I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab_header                = git_header_monitor[]
      t_outtab_item                  = git_detail_monitor[]
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


*ZMMWA_0020_HEADER_MONITOR
*ZMMWA_0020_services_monitor
*ZMMWA_0020_DETAIL_SERVICES_MON


ENDFORM.                    " F_SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_FIELDCAT  text
*      <--P_GV_REPID  text
*      -->P_0635   text
*----------------------------------------------------------------------*
FORM f_build_fcat
                   USING    pv_it_name
                            pv_strname
                   CHANGING pv_repid.

  FIELD-SYMBOLS: <fs_fieldcat> LIKE LINE OF git_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
     i_program_name               = pv_repid
     i_internal_tabname           = pv_it_name
     i_structure_name             = pv_strname
*     I_CLIENT_NEVER_DISPLAY       = 'X'
     i_inclname                   = pv_repid
*     I_BYPASSING_BUFFER           =
*     I_BUFFER_ACTIVE              =
    CHANGING
      ct_fieldcat                  = git_fieldcat
   EXCEPTIONS
     inconsistent_interface       = 1
     program_error                = 2
     OTHERS                       = 3


            .



  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*


  LOOP AT git_fieldcat ASSIGNING <fs_fieldcat>.

    IF <fs_fieldcat>-tabname = 'GIT_HEADER_MONITOR'.

      IF <fs_fieldcat>-fieldname = 'TIPO_CONTRATO'.
        <fs_fieldcat>-seltext_s = 'T.Contrato'.
        <fs_fieldcat>-seltext_l = 'T.Contrato'.
        <fs_fieldcat>-seltext_m = 'T.Contrato'.
        <fs_fieldcat>-reptext_ddic = 'T.Contrato'.


      ENDIF.

      IF <fs_fieldcat>-fieldname = 'TXZ01'.
        <fs_fieldcat>-seltext_s = '    Desc. Posicion'.
        <fs_fieldcat>-seltext_l = '    Descripcion de posicion'.
        <fs_fieldcat>-seltext_m = '    Descripcion de posicion'.
        <fs_fieldcat>-reptext_ddic = '    Descripcion de posicion'.
      ENDIF.

      IF <fs_fieldcat>-fieldname = 'DESC_SERVICIO'.
        <fs_fieldcat>-seltext_s = 'Desc. servicio'.
        <fs_fieldcat>-seltext_l = 'Descripcion de servicio'.
        <fs_fieldcat>-seltext_m = 'Descripcion de servicio'.
        <fs_fieldcat>-reptext_ddic = 'Descripcion de servicio'.
      ENDIF.

      IF <fs_fieldcat>-fieldname = 'SUM_CTD'.
        <fs_fieldcat>-seltext_s = 'Avance'.
        <fs_fieldcat>-seltext_l = 'Avance'.
        <fs_fieldcat>-seltext_m = 'Avance'.
        <fs_fieldcat>-reptext_ddic = 'Avance'.
      ENDIF.

      IF <fs_fieldcat>-fieldname = 'STATUS_ATRASO'.
        <fs_fieldcat>-seltext_s = 'Status'.
        <fs_fieldcat>-seltext_l = 'Status'.
        <fs_fieldcat>-seltext_m = 'Status'.
        <fs_fieldcat>-reptext_ddic = 'Status'.

      ENDIF.

      IF <fs_fieldcat>-fieldname = 'MONTO_NETO'.
         <fs_fieldcat>-seltext_s = 'Imp. Bruto'.
         <fs_fieldcat>-seltext_l = 'Importe Bruto'.
         <fs_fieldcat>-seltext_m = 'Importe Bruto'.
         <fs_fieldcat>-reptext_ddic = 'Importe Bruto'.
      ENDIF.

      IF <fs_fieldcat>-fieldname = 'LIFNR'.
        <fs_fieldcat>-seltext_s = '#Prov'.
        <fs_fieldcat>-seltext_l = '#Provdr'.
        <fs_fieldcat>-seltext_m = '#Proveedor'.
        <fs_fieldcat>-reptext_ddic = '#Proveedor'.
      ENDIF.


      IF <fs_fieldcat>-fieldname = 'LIFNR_NAME1'.
        <fs_fieldcat>-seltext_s = ' Nom. de Proveedor'.
        <fs_fieldcat>-seltext_l = '      Nombre de Proveedor'.
        <fs_fieldcat>-seltext_m = '      Nombre de Proveedor'.
        <fs_fieldcat>-reptext_ddic = '      Nombre de Proveedor'.
      ENDIF.

      CASE <fs_fieldcat>-fieldname.
        WHEN 'EBELN'.
          <fs_fieldcat>-intlen = 15.
          <fs_fieldcat>-ddic_outputlen = 15.
          <fs_fieldcat>-hotspot = 'X'.

*          <fs_fieldcat>-EMPHASIZE = 'X'.
      ENDCASE.

    ELSEIF <fs_fieldcat>-tabname = 'GIT_DETAIL_MONITOR'.

      IF <fs_fieldcat>-fieldname = 'RMWWR'.
        <fs_fieldcat>-seltext_s = 'Imp. Neto'.
        <fs_fieldcat>-seltext_l = 'Importe Neto'.
        <fs_fieldcat>-seltext_m = 'Importe Neto'.
        <fs_fieldcat>-reptext_ddic = 'Importe Neto'.
      ENDIF.

      IF <fs_fieldcat>-fieldname = 'EBELN'.
        <fs_fieldcat>-no_out = 'X'."Ocultar campo de ALV
      ENDIF.
      IF <fs_fieldcat>-fieldname = 'EBELP'.
        <fs_fieldcat>-no_out = 'X'."Ocultar campo de ALV
      ENDIF.
      IF <fs_fieldcat>-fieldname = 'ID_SERVICIO'.
        <fs_fieldcat>-no_out = 'X'."Ocultar campo de ALV
      ENDIF.
      IF <fs_fieldcat>-fieldname = 'GJAHR'.
        <fs_fieldcat>-no_out = 'X'."Ocultar campo de ALV
      ENDIF.

      CASE <fs_fieldcat>-fieldname.
        WHEN 'BEWTP'.
          <fs_fieldcat>-no_out = 'X'.
        WHEN 'BELNR' OR 'EBELN'.
          <fs_fieldcat>-hotspot = 'X'.

      ENDCASE.

    ENDIF.
  ENDLOOP.


ENDFORM.                    " F_BUILD_FCAT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_HIERARCHY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_hierarchy_alv .


  gwa_keyinfo-header01 = 'EBELN'.
  gwa_keyinfo-item01 = 'EBELN'.

  gwa_keyinfo-header02 = 'EBELP'.
  gwa_keyinfo-item02 = 'EBELP'.

  gwa_keyinfo-header03 = 'ID_SERVICIO'.
  gwa_keyinfo-item03 = 'ID_SERVICIO'.


  gv_tabname_header = 'GIT_HEADER_MONITOR'.
  gv_tabname_item = 'GIT_DETAIL_MONITOR'.

  PERFORM f_build_events.
  PERFORM build_print_params.


ENDFORM.                    " F_BUILD_HIERARCHY_ALV
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_events .
  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = git_events[].


  READ TABLE git_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'TOP_OF_PAGE' TO ls_event-form.
    MOVE 'TOP_OF_PAGE' TO ls_event-name.
    APPEND ls_event TO git_events.
  ENDIF.

  READ TABLE git_events WITH KEY name =  slis_ev_end_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'END_OF_PAGE' TO ls_event-form.
    APPEND ls_event TO git_events.
  ENDIF.

  READ TABLE git_events WITH KEY name =  slis_ev_end_of_list
                         INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'END_OF_LIST' TO ls_event-form.
    APPEND ls_event TO git_events.
  ENDIF.

ENDFORM.                    " F_BUILD_EVENTS

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: listwidth TYPE i,
        ld_pagepos(10) TYPE c,
        ld_page(10)    TYPE c.


  CASE r_ucomm.
    WHEN '&IC1'.
      PERFORM f_go_transaction USING rs_selfield.

  ENDCASE.
*  WRITE: sy-uline(50).
*  SKIP.
*  WRITE:/ 'Page:', sy-pagno .
ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  build_print_params
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_print_params.
  gd_prntparams-reserve_lines = '3'.   "Lines reserved for footer
  gd_prntparams-no_coverpage = 'X'.
ENDFORM.                    "build_print_params


*&---------------------------------------------------------------------*
*& Form VALIDATE_BUKRS
*&---------------------------------------------------------------------*
* Para validar si existe el proyecto
*----------------------------------------------------------------------*
FORM validate_pspid.

  DATA: lwa_proj TYPE proj.
  IF NOT p_pspid IS INITIAL.

    SELECT SINGLE * INTO lwa_proj
    FROM proj
      WHERE pspid = p_pspid.

    IF sy-subrc NE 0.
      MESSAGE e000(zggi) WITH
       'No se encuentra el proyecto, favor de validar' DISPLAY LIKE 'E'.

    ENDIF.


  ENDIF.
ENDFORM.                    "validate_pspid


*---------------------------------------------------------------------*
* FORM TOP_OF_PAGE *
*---------------------------------------------------------------------*
* TOP OF PAGE *
*---------------------------------------------------------------------*
FORM top_of_page.

  DATA: v_count(05) TYPE n.
  DATA: l_count(05) TYPE c.
  DATA: lv_header(100) TYPE c.
  DATA: lv_date1(10) TYPE c.
  DATA: lv_date2(10) TYPE c.
  DATA: lwa_bedat LIKE LINE OF pr_bedat.


*--- Get the number of Documents to be displayed
*  DESCRIBE TABLE Git_headER_MONITOR LINES v_count.
*  v_count = v_count.
*  l_count = v_count.
*  CONCATENATE ' Profit center wise documents'
*  'Number of Documents'
*  v_count
*  INTO v_header SEPARATED BY ':'.

  SELECT SINGLE post1 INTO lv_header
  FROM proj
  WHERE pspid = p_pspid.
*  break devlpext.
  CONCATENATE p_pspid lv_header INTO lv_header SEPARATED BY space.

  REFRESH git_head.
  git_head-typ = 'H'.
  git_head-info = lv_header.
  APPEND git_head.

  IF pr_bedat[] IS NOT INITIAL.
    git_head-typ = 'H'.
    READ TABLE pr_bedat INTO lwa_bedat INDEX 1.
    IF sy-subrc EQ 0.
      WRITE lwa_bedat-low TO lv_date1.
      WRITE lwa_bedat-high TO lv_date2.

      CONCATENATE 'Periodo:' lv_date1 'al' lv_date2 INTO lv_header SEPARATED BY space.
      git_head-info = lv_header.
      APPEND git_head.
    ENDIF.


  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
  EXPORTING
  it_list_commentary = git_head[]
* I_LOGO =
* I_END_OF_LIST_GRID =
  .

ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CALCULATE_CTDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_ZUTIL  text
*      <--P_LWA_DETAIL_MONITOR_CTD  text
*      <--P_LWA_HEADER_MONITOR_SUM_CTD  text
*      <--P_LWA_DETAIL_MONITOR_TIPO_ENTRAD  text
*      -->P_<FS_EKBE>_SHKZG  text
*      -->P_<FS_EKBE>_MENGE  text
*----------------------------------------------------------------------*
FORM f_calculate_ctds  TABLES   pit_zutil STRUCTURE zutil_parameters
                                  "Insertar nombre correcto para <...>
                       USING    pv_ekbe_shkzg
                                pv_ekbe_menge
                                pv_type
                       CHANGING pv_ctd
                                pv_sum_ctd
                                pv_tipo_entrada
                                pv_porce.

  FIELD-SYMBOLS: <fs_zutil> LIKE LINE OF pit_zutil.
  DATA: lv_string TYPE string.


  pv_porce = '%'.
  IF pv_ekbe_shkzg EQ 'S'. " Se toma valor positivo
    CONCATENATE pv_type '_' 'TIPO_ENTRADA_S' INTO lv_string.
    READ TABLE pit_zutil ASSIGNING <fs_zutil> WITH KEY zfield = lv_string
                                                        zflag = pv_type.
    IF sy-subrc EQ 0.
      ADD pv_ekbe_menge TO pv_ctd.
      ADD pv_ekbe_menge TO pv_sum_ctd.
      pv_tipo_entrada = <fs_zutil>-zchar.

    ENDIF.
  ELSE.
    CONCATENATE pv_type '_' 'TIPO_ENTRADA_H' INTO lv_string.
    READ TABLE pit_zutil ASSIGNING <fs_zutil> WITH KEY zfield = lv_string
                                                    zflag = pv_type.
    IF sy-subrc EQ 0.
      pv_ctd     = pv_ctd - pv_ekbe_menge.
      pv_sum_ctd = pv_sum_ctd - pv_ekbe_menge .
      pv_tipo_entrada = <fs_zutil>-zchar.
    ENDIF.
  ENDIF.


ENDFORM.                    " F_CALCULATE_CTDS
*&---------------------------------------------------------------------*
*&      Form  F_GO_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM f_go_transaction    USING rs_selfield TYPE slis_selfield.
  DATA: lwa_header_monitor LIKE LINE OF git_header_monitor.
  DATA: lwa_detail_monitor LIKE LINE OF git_detail_monitor.

  IF rs_selfield-tabname = 'GIT_HEADER_MONITOR'.  " Header
    READ TABLE git_header_monitor INTO lwa_header_monitor INDEX rs_selfield-tabindex.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CASE rs_selfield-fieldname .
      WHEN 'EBELN' .
        PERFORM f_go_me23 USING lwa_header_monitor-ebeln.
      WHEN 'LIFNR'.
      WHEN 'WERKS'.
      WHEN 'ELEMENTO_PEP'.
    ENDCASE.
  ELSEIF rs_selfield-tabname = 'GIT_DETAIL_MONITOR'.  " Detail
    READ TABLE git_detail_monitor INTO lwa_detail_monitor INDEX rs_selfield-tabindex.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CASE rs_selfield-fieldname .
      WHEN 'EBELN' .
        PERFORM f_go_me23 USING lwa_header_monitor-ebeln.
      WHEN 'BELNR'.
        IF lwa_detail_monitor-bewtp = 'E'.
          PERFORM f_go_mb03 USING lwa_detail_monitor-belnr lwa_detail_monitor-gjahr.
        ELSEIF lwa_detail_monitor-bewtp = 'Q'.
          PERFORM f_go_mir4 USING lwa_detail_monitor-belnr lwa_detail_monitor-gjahr.
        ENDIF.
      WHEN 'EBELN'.

    ENDCASE.

  ENDIF.
ENDFORM.                    " F_GO_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  F_GO_MB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_DETAIL_MONITOR_BELNR  text
*      -->P_LWA_DETAIL_MONITOR_GJAHR  text
*----------------------------------------------------------------------*
FORM f_go_mb03  USING    pv_belnr
                         pv_gjahr.

  l_options-dismode = 'E'.

  REFRESH bdcdata.
  PERFORM bdc_dynpro      USING 'SAPMM07M' '0460'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RM07M-MBLNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
  PERFORM bdc_field       USING 'RM07M-MBLNR' pv_belnr.
  PERFORM bdc_field       USING 'RM07M-MJAHR' pv_gjahr.
  PERFORM bdc_field       USING 'XFULL' 'X'.

  CALL TRANSACTION 'MB03' USING bdcdata OPTIONS FROM l_options MESSAGES "#EC CI_USAGE_OK[1804812]
              INTO it_messtab.
ENDFORM.                                                    " F_GO_MB03

*&---------------------------------------------------------------------*
*&      Form  F_GO_MIR4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_DETAIL_MONITOR_BELNR  text
*      -->P_LWA_DETAIL_MONITOR_GJAHR  text
*----------------------------------------------------------------------*
FORM f_go_mir4  USING    pv_belnr
                         pv_gjahr.

  l_options-dismode = 'E'.

  REFRESH bdcdata.
  PERFORM bdc_dynpro      USING 'SAPLMR1M' '6150'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RBKP-GJAHR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
  PERFORM bdc_field       USING 'RBKP-BELNR' pv_belnr.
  PERFORM bdc_field       USING 'RBKP-GJAHR' pv_gjahr.

  CALL TRANSACTION 'MIR4' USING bdcdata OPTIONS FROM l_options MESSAGES
              INTO it_messtab.
ENDFORM.                                                    " F_GO_MIR4

*---------------------------------------------------------------------*
*&      Form  F_GO_MIR4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_DETAIL_MONITOR_BELNR  text
*      -->P_LWA_DETAIL_MONITOR_GJAHR  text
*----------------------------------------------------------------------*
FORM f_go_me23  USING    pv_ebeln.

  l_options-dismode = 'E'.

  REFRESH bdcdata.
  PERFORM bdc_dynpro      USING 'SAPMM06E' '0105'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RM06E-BSTNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
  PERFORM bdc_field       USING 'RM06E-BSTNR' pv_ebeln.


  CALL TRANSACTION 'ME23' USING bdcdata OPTIONS FROM l_options MESSAGES "#EC CI_USAGE_OK[1804812]   "#EC CI_USAGE_OK[1803189]
              INTO it_messtab.
ENDFORM.                                                    " F_GO_MIR4

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       PROGRAM DYNPRO
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM FVAL  text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> nodata.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.                    "bdc_field
