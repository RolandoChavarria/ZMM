*----------------------------------------------------------------------*
***INCLUDE LZMMFMF05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MAIN_DECREMENTA
*&---------------------------------------------------------------------*
FORM  main_decrementa_serv USING p_lifnr    TYPE lifnr
                 p_ekgrp    TYPE bkgrp
                 p_proye    TYPE everk
                 p_folio    TYPE ihrez
* Tablas de posi y servic
                 t_items    TYPE zmmfm_0070_azul_tt
                 p_test     TYPE bapiflag-bapiflag
       CHANGING  e_subrc    TYPE subrc
                 e_bapi_msg TYPE bapi_msg.
* Validar datos
  PERFORM prereq_decre_serv USING p_test p_lifnr p_proye
                       CHANGING e_subrc e_bapi_msg.

  CHECK e_subrc = 0.
* Lleando de tables aux
  PERFORM fill_aux_tables_decre_serv USING t_items
        CHANGING e_subrc e_bapi_msg.
  CHECK e_subrc = 0.
*  Validar el proveedor en la sociedad correspodiente
  PERFORM valida_bukrs USING p_lifnr g_bukrs
                       CHANGING e_subrc e_bapi_msg.
  CHECK e_subrc = 0.

* Llenado de datos a tabals structuras
  PERFORM fill_tables_decre_serv USING p_lifnr p_ekgrp p_proye
                                  p_folio t_items
                            CHANGING e_subrc e_bapi_msg.
* Running the bapi to create POs, reusar para incremetal y decremental, al modf considera abas func
  PERFORM bapi_po_create CHANGING e_subrc e_bapi_msg.
* eleiminar condiciones

ENDFORM.                    "main

*&---------------------------------------------------------------------*
*&      Form  llenado_datos
*&---------------------------------------------------------------------*
FORM fill_tables_decre_serv USING p_lifnr TYPE lifnr
                       p_ekgrp TYPE bkgrp
                       p_proye TYPE everk
                       p_folio TYPE ihrez
                       t_items TYPE zmmfm_0070_azul_tt
              CHANGING e_subrc TYPE subrc
                       e_bapi_msg  TYPE bapi_msg.
* Filling header structures
  PERFORM fill_header_decre_serv USING p_lifnr p_ekgrp p_folio.

  PERFORM fill_items_decre_serv USING p_proye t_items
                           CHANGING e_subrc e_bapi_msg.

ENDFORM.                    "llenado_datos
*&---------------------------------------------------------------------*
*&      Form  llenado_cabecera
*&---------------------------------------------------------.------------*
FORM fill_header_decre_serv USING p_lifnr TYPE lifnr
                       p_ekgrp TYPE bkgrp
                       p_folio TYPE ihrez.
* Header structure
  gs_poheader-comp_code      = g_bukrs.                     "5030
  gs_poheader-doc_type       = g_bsart. "ZOCD
  gs_poheader-creat_date     = sy-datum.
  gs_poheader-created_by     = sy-uname.
  gs_poheader-item_intvl     = '00010'.
  gs_poheader-vendor         = p_lifnr.                     "30242'
  gs_poheaderx-langu         = sy-langu.
  gs_poheaderx-langu_iso     = sy-langu.
  gs_poheader-pmnttrms       = '0001'.
  gs_poheader-purch_org      = g_ekorg.                     "1000
  gs_poheader-pur_group      = p_ekgrp.                     ""102
  gs_poheader-currency       = 'MXN'.
  gs_poheader-exch_rate      = '1.00000'.
  gs_poheader-doc_date       = sy-datum.
  gs_poheader-vper_start     = sy-datum.
  gs_poheader-vper_end       = sy-datum + 365. "30.11.2012 ""Should be at least 10 dias ahead o a week, Ask tavo about it
  gs_poheader-subitemint     = '00001'.
  gs_poheader-reason_cancel  = '10'.  ""Hard code default 10, Tipo contrato
  gs_poheader-our_ref        = p_folio.
* X structure
  gs_poheaderx-comp_code     = 'X'.
  gs_poheaderx-doc_type      = 'X'.
  gs_poheaderx-creat_date    = 'X'.
  gs_poheaderx-created_by    = 'X'.
  gs_poheaderx-item_intvl    = 'X'.
  gs_poheaderx-vendor        = 'X'.
  gs_poheaderx-langu         = 'X'.
  gs_poheaderx-langu_iso     = 'X'.
  gs_poheaderx-pmnttrms      = 'X'.
  gs_poheaderx-purch_org     = 'X'.
  gs_poheaderx-pur_group     = 'X'.
  gs_poheaderx-currency      = 'X'.
  gs_poheaderx-exch_rate     = 'X'.
  gs_poheaderx-doc_date      = 'X'.
  gs_poheaderx-vper_start    = 'X'.
  gs_poheaderx-vper_end      = 'X'.
  gs_poheaderx-subitemint    = 'X'.
  gs_poheaderx-reason_cancel = 'X'.
  gs_poheaderx-our_ref       = 'X'.

ENDFORM.                    "llenado_cabecera

*&---------------------------------------------------------------------*
*&      Form  llenado_position
*&---------------------------------------------------------------------*
FORM fill_items_decre_serv USING p_proye TYPE everk
                      t_items TYPE zmmfm_0070_azul_tt
             CHANGING e_subrc TYPE subrc
                      e_bapi_msg  TYPE bapi_msg.

  DATA: l_item TYPE ty_item,
        e_error TYPE sy-tabix.
* loop for items or partidas azules
  gs_bapiesllc-line_no         = '0000000001'.
  LOOP AT t_items INTO l_item.
    ADD '0000000001' TO gs_bapiesllc-pckg_no.
    l_item-ebelp = sy-tabix * 10.
    PERFORM get_werks USING l_item p_proye
                      CHANGING e_error e_subrc e_bapi_msg.
    IF e_error EQ 4.
      EXIT.
    ENDIF.
* Structures
    PERFORM item_blue_decre_serv USING l_item.
    PERFORM item_sche_decre_serv USING l_item.
    PERFORM item_acco_decre_serv USING l_item.
    PERFORM item_cond_decre_serv USING l_item.
* For each services no need for decremental
* For each services si se usara futuro
***    PERFORM item_serv_decre_serv USING l_item t_rojas.
* No need partners as well
  ENDLOOP.

ENDFORM.                    "fill_items
*&---------------------------------------------------------------------*
*&      Form  item_acco
*&---------------------------------------------------------------------*
FORM item_acco_decre_serv USING u_item TYPE ty_item.
  DATA: l_asmd   TYPE ty_asmdt,
        l_prmtrs TYPE ty_prmtrs,
        l_roja   TYPE zmmfm_0070_roja.

* Se busca la partida azul, pero en realidad se debe buscar la partida Roja, se converva si cambian de opinion
  READ TABLE it_asmdt INTO l_asmd WITH KEY matkl = u_item-matkl(4).

* account
  it_poaccount-po_item      = u_item-ebelp.                 "00010
  it_poaccount-serial_no    = '01'.
  it_poaccount-creat_date   = sy-datum.
  it_poaccount-quantity     = '1.000'.
*  it_poaccount-gl_account   = l_asmd-bklas. " 60021402,
  CLEAR: l_prmtrs.
  READ TABLE t_prmtrs INTO l_prmtrs WITH KEY zfield = l_asmd-bklas .
  PERFORM conversion_exit_alpha_input USING l_prmtrs-zchar
                                      CHANGING it_poaccount-gl_account.
  it_poaccount-bus_area     = g_bus_area.                   "0076
  it_poaccount-co_area      = 'GIG'.
  it_poaccount-profit_ctr   = 'CEBEDUMMY'.                  "5030007649
  it_poaccount-wbs_element  = g_elpep.               "pe-0076-5030-001-04-01
  it_poaccount-net_value    = u_item-netwr. ""'3,000.000000000'.
  it_poaccount-tax_code     = 'F2'. ""Si se necesitara
  APPEND it_poaccount.

* Account X
  it_poaccountx-po_item     = u_item-ebelp.                 "00010
  it_poaccountx-serial_no   = '01'.
  it_poaccountx-po_itemx    = 'X'.
  it_poaccountx-creat_date  = 'X'.
  it_poaccountx-quantity    = 'X'.
  it_poaccountx-gl_account  = 'X'.
  it_poaccountx-bus_area    = 'X'.
  it_poaccountx-co_area     = 'X'.
  it_poaccountx-profit_ctr  = 'X'.
  it_poaccountx-wbs_element = 'X'.
  it_poaccountx-net_value   = 'X'.
  it_poaccountx-tax_code    = 'X'.
  APPEND it_poaccountx.

ENDFORM.                    "item_acco
*&---------------------------------------------------------------------*
*&      Form  item_cond
*&---------------------------------------------------------------------*
FORM item_cond_decre_serv USING u_item TYPE ty_item.
* Conditions
  CLEAR: it_pocond, it_pocondx.
***it_pocond-condition_no                   1000000356
  it_pocond-itm_number       = u_item-ebelp. ""             000010
  it_pocond-cond_st_no       = '001'.
  it_pocond-cond_count       = '01'.
  it_pocond-cond_type        = 'PBXX'.
  it_pocond-cond_value       = u_item-netwr.  ""3,000.000000
  it_pocond-currency         = 'MXN'.
  it_pocond-currency_iso     = 'MXN'.
  it_pocond-cond_unit        = 'PTO'.
*  it_pocond-cond_unit_iso    = 'C62'.
  it_pocond-cond_p_unt       = '1'.
  it_pocond-applicatio       = 'M'.
  it_pocond-conpricdat       = sy-datum.
  it_pocond-calctypcon       = 'C'.
  it_pocond-conbaseval       = '1.000000'.
  it_pocond-conexchrat       = '1.00000'.
  it_pocond-numconvert       = '1'.
  it_pocond-denominato       = '1'.
  it_pocond-condtype         = 'H'.
  it_pocond-condorigin       = 'A'.
  it_pocond-access_seq       = '00'.
  it_pocond-condcount        = '00'.
  it_pocond-condcntrl        = 'C'.
  it_pocond-condclass        = 'B'.
  it_pocond-condchaman       = 'X'.
*  it_pocond-change_id        = 'U'.
  APPEND it_pocond.

* Conditins X
***it_pocondx-condition_no    =               1000000356
  it_pocondx-itm_number      = u_item-ebelp.                "000010
  it_pocondx-cond_st_no      = '001'.
  it_pocondx-condition_nox   = 'X'.
  it_pocondx-itm_numberx     = 'X'.
  it_pocondx-cond_st_nox     = 'X'.
  it_pocondx-cond_count      = 'X'.
  it_pocondx-cond_type       = 'X'.
  it_pocondx-cond_value      = 'X'.
  it_pocondx-currency        = 'X'.
  it_pocondx-currency_iso    = 'X'.
  it_pocondx-cond_unit       = 'X'.
  it_pocondx-cond_unit_iso   = 'X'.
  it_pocondx-cond_p_unt      = 'X'.
  it_pocondx-applicatio      = 'X'.
  it_pocondx-conpricdat      = 'X'.
  it_pocondx-calctypcon      = 'X'.
  it_pocondx-conbaseval      = 'X'.
  it_pocondx-conexchrat      = 'X'.
  it_pocondx-numconvert      = 'X'.
  it_pocondx-denominato      = 'X'.
  it_pocondx-condtype        = 'X'.
  it_pocondx-condorigin      = 'X'.
  it_pocondx-access_seq      = 'X'.
  it_pocondx-condcount       = 'X'.
  it_pocondx-condcntrl       = 'X'.
  it_pocondx-condclass       = 'X'.
  it_pocondx-condconfig      = 'X'.
  it_pocondx-change_id       = 'I'.
  APPEND it_pocondx.

* Try to  clear condiciones
  CLEAR: it_pocond, it_pocondx.
  it_pocond-itm_number = u_item-ebelp.
  it_pocond-cond_type  = 'ZRET'.
  it_pocond-change_id = 'D'.
  APPEND it_pocond.
  it_pocond-cond_type  = 'ZAAN'.
  APPEND it_pocond.
*-* Genera t_pocondx
  it_pocondx-itm_number  = u_item-ebelp.
  it_pocondx-cond_st_no  = '001'.
  it_pocondx-itm_numberx = 'X'.
  it_pocondx-cond_st_nox = 'X'.
  it_pocondx-cond_type   = 'X'.
  it_pocondx-cond_value  = 'X'.
  it_pocondx-currency    = 'X'.
  it_pocondx-change_id   = 'X'.
  APPEND it_pocondx.
  it_pocondx-itm_number = u_item-ebelp.
  it_pocondx-cond_st_no = '010'.
  it_pocondx-change_id  = 'X'.
  APPEND it_pocondx.

ENDFORM.                    "item_cond
*&---------------------------------------------------------------------*
*&      Form  item_blue
*&---------------------------------------------------------------------*
FORM item_sche_decre_serv USING u_item TYPE ty_item.
  DATA: lday(2) TYPE c,
        lmon(2) TYPE c,
        lyea(4) TYPE c.

  PERFORM hr_in_get_date_components CHANGING lday lmon lyea.
* Schedule
  it_poschedule-po_item         = u_item-ebelp.             "  00010
  it_poschedule-sched_line      = '0001'.
  it_poschedule-del_datcat_ext  = 'T'.
*  it_poschedule-delivery_date   = sy-datum. ""20.11.2012
  CONCATENATE lday lmon lyea INTO it_poschedule-delivery_date
                             SEPARATED BY '.'.
  it_poschedule-quantity        = g_etmen. ""                         1.000
  it_poschedule-stat_date       =  sy-datum.                "20.11.2012
  APPEND it_poschedule.

* Schdeule X
  it_poschedulex-po_item        = u_item-ebelp.             ""00010
  it_poschedulex-sched_line     = '0001'.
  it_poschedulex-po_itemx       = 'X'.
  it_poschedulex-sched_linex    = 'X'.
  it_poschedulex-del_datcat_ext = 'X'.
  it_poschedulex-delivery_date  = 'X'.
  it_poschedulex-quantity       = 'X'.
  it_poschedulex-stat_date      = 'X'.
  APPEND it_poschedulex.

ENDFORM.                    "item_blue

*&---------------------------------------------------------------------*
*&      Form  item_blue
*&---------------------------------------------------------------------*
FORM item_blue_decre_serv USING u_item TYPE ty_item.
  DATA: l_asmd TYPE ty_asmdt.
*  Stop to read from material group, now read from services
*  READ TABLE it_t023t INTO l_t023t WITH KEY matkl = u_item-matkl.
*  CONCATENATE u_item-matkl l_t023t-wgbez60 INTO l_t023t-wgbez60
*                                           SEPARATED BY space.
  PERFORM conversion_exit_alpha_input USING u_item-matkl
      CHANGING l_asmd-asnum.
  READ TABLE it_asmdt INTO l_asmd WITH KEY asnum = l_asmd-asnum.
  CONCATENATE u_item-matkl l_asmd-asktx INTO l_asmd-asktx
                                           SEPARATED BY space.

  g_matkl = u_item-matkl(4).
* Positions
  it_poitem-po_item                     = u_item-ebelp.     ""    00010
  it_poitem-short_text                  = l_asmd-asktx ."  0401 seguros de obra y responsabilid
  it_poitem-plant                       = u_item-werks.     ""   76
  it_poitem-matl_group                  = g_matkl.          "   0401
  it_poitem-quantity                    = '1.000'.
  it_poitem-po_unit                     = 'PTO'.
  it_poitem-po_unit_iso                 = 'PTO'.
*  it_poitem-orderpr_un                  = 'PTO'.
*  it_poitem-orderpr_un_iso              = 'C62'.
  it_poitem-conv_num1                   = '1'.
  it_poitem-conv_den1                   = '1'.
  it_poitem-net_price                   = u_item-netwr.       "" 1,950.00000000
  it_poitem-price_unit                  = '1'.
  it_poitem-prnt_price                  = 'X'.
  it_poitem-unlimited_dlv               = 'X'.
*  it_poitem-item_cat                    = '0'.
  it_poitem-acctasscat                  = 'P'.
*it_poitem-SETT_ITEM                    = 'X'.
  it_poitem-gr_ind                      = 'X'.
  it_poitem-ir_ind                      = 'X'.
  it_poitem-gr_basediv                  = 'X'.
  it_poitem-ackn_reqd                   = 'X'.
  it_poitem-pricedate                   = '1'.
  it_poitem-price_date                  = sy-datum.
  it_poitem-ret_item                   = 'X'.
  it_poitem-sett_item                   = 'X'.
  it_poitem-period_ind_expiration_date  = 'D'.
  it_poitem-pckg_no                     = gs_bapiesllc-pckg_no. ""'0000000001'.
  it_poitem-tax_code                    = g_tax.
  APPEND it_poitem.

* itesm X
  it_poitemx-po_item                    = u_item-ebelp.
  it_poitemx-po_itemx                   = 'X'.
  it_poitemx-short_text                 = 'X'.
  it_poitemx-plant                      = 'X'.
  it_poitemx-matl_group                 = 'X'.
  it_poitemx-quantity                   = 'X'.
  it_poitemx-po_unit                    = 'X'.
  it_poitemx-po_unit_iso                = 'X'.
*  it_poitemx-orderpr_un                 = 'X'.
*  it_poitemx-orderpr_un_iso             = 'X'.
  it_poitemx-conv_num1                  = 'X'.
  it_poitemx-conv_den1                  = 'X'.
  it_poitemx-net_price                  = 'X'.
  it_poitemx-price_unit                 = 'X'.
  it_poitemx-gr_pr_time                 = 'X'.
  it_poitemx-prnt_price                 = 'X'.
  it_poitemx-unlimited_dlv              = 'X'.
  it_poitemx-item_cat                   = 'X'.
  it_poitemx-acctasscat                 = 'X'.
  it_poitemx-gr_ind                     = 'X'.
*it_poitemx-SETT_ITEM                    = 'X'.
  it_poitemx-ir_ind                     = 'X'.
  it_poitemx-gr_basediv                 = 'X'.
  it_poitemx-ackn_reqd                  = 'X'.
  it_poitemx-pricedate                  = 'X'.
  it_poitemx-price_date                 = 'X'.
  it_poitemx-tax_code                   = 'X'.
  it_poitemx-ret_item                   = 'X'.
  it_poitemx-period_ind_expiration_date = 'X'.
  it_poitemx-pckg_no                    = 'X'.
  APPEND it_poitemx.

ENDFORM.                    "item_blue

*&---------------------------------------------------------------------*
*&      Form  fill_aux_tables
*&---------------------------------------------------------------------*
FORM fill_aux_tables_decre_serv USING t_items TYPE zmmfm_0070_azul_tt
                      CHANGING e_subrc    TYPE subrc
                               e_bapi_msg TYPE bapi_msg.

  DATA: lt_asmdt TYPE STANDARD TABLE OF asmdt WITH HEADER LINE,
        lt_asmd TYPE STANDARD TABLE OF asmd WITH HEADER LINE,
        ls_itm TYPE zmmfm_0070_azul,
        ls_ser   TYPE zmmfm_0070_roja,
        ls_t001k TYPE ty_t001k.

  IF t_items[] IS INITIAL.
    e_subrc = '02'.
    e_bapi_msg = 'Error: falta partida azul.'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ELSE.
* Denominaciones para grupos de artículos
    SELECT * FROM t023t INTO TABLE it_t023t FOR ALL ENTRIES IN t_items
        WHERE spras EQ sy-langu AND matkl EQ t_items-matkl.
* Centro
    SELECT vkorg werks INTO TABLE it_t001w
          FROM t001w FOR ALL ENTRIES IN t_items
          WHERE werks EQ t_items-werks.

    SELECT bwkey bukrs INTO TABLE it_t001k
            FROM t001k FOR ALL ENTRIES IN t_items
            WHERE bwkey EQ t_items-werks.
    IF it_t001k[] IS NOT INITIAL.
      READ TABLE it_t001k INTO ls_t001k INDEX 1.
      g_bukrs = ls_t001k-bukrs.
    ENDIF.
  ENDIF.
* get details for services it_asmdt
  LOOP AT t_items INTO ls_itm.
    lt_asmd-matkl = ls_itm-matkl(4).
    APPEND lt_asmd.
  ENDLOOP.
  IF lt_asmd[] IS NOT INITIAL.
    SELECT f~asnum f~asktx p~matkl p~bklas
      INTO TABLE it_asmdt FROM asmdt AS f
       INNER JOIN asmd AS p ON f~asnum = p~asnum
          FOR ALL ENTRIES IN lt_asmd
                  WHERE p~matkl EQ lt_asmd-matkl AND
                f~spras EQ sy-langu.
  ENDIF.
ENDFORM.                    "fill_aux_tables

*&---------------------------------------------------------------------*
*&      Form  validaciones
*&---------------------------------------------------------------------*
FORM prereq_decre_serv USING  p_test     TYPE bapiflag-bapiflag
                         p_lifnr    TYPE lifnr
                         p_proye TYPE everk
                CHANGING e_subrc    TYPE subrc
                         e_bapi_msg TYPE bapi_msg.

  DATA: l_lfa1 TYPE lfa1,
        l_proj TYPE proj.

  g_test = p_test.
  FIELD-SYMBOLS: <fp> TYPE ty_prmtrs.
* Clear varaibles
  CLEAR: g_bukrs, g_bsart, g_elpep, vnum18, g_ekorg,g_etmen,
        g_meins, g_matkl,g_matnr, g_bus_area, gs_bapiesllc, t_prmtrs.
  CLEAR: it_return, it_poitem, it_poitemx, it_poschedule,
          it_poschedulex, it_poaccount, it_poaccountx, it_pocond,
          it_pocondx, it_poservices, it_posrvaccessvalues, it_popartner.
  REFRESH: it_return, it_poitem, it_poitemx, it_poschedule,
           it_poschedulex, it_poaccount, it_poaccountx, it_pocond,
           it_pocondx, it_poservices, it_posrvaccessvalues,
           it_popartner, t_prmtrs.

* validar projecto
  SELECT SINGLE * FROM proj INTO l_proj WHERE pspid EQ p_proye.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '02'.
    e_bapi_msg = 'Projecto invalido'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.
* VALIDAR VENDOR
  SELECT SINGLE lifnr FROM lfa1 INTO l_lfa1-lifnr
         WHERE lifnr EQ p_lifnr.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '03'.
    e_bapi_msg = 'Proveedor invalido'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.
* Get constants to the program
  SELECT zfield zchar INTO TABLE t_prmtrs  FROM zutil_parameters
         WHERE zreport EQ 'ZMMFM_0080_ORDEN_DECREMENTA'.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '04'.
    e_bapi_msg = 'ZUTIL_PARAMETERS - REQUERIDOS'.
    PERFORM f_reg_exep_rfc_msg USING v_id_rfc e_subrc e_bapi_msg
                            CHANGING v_conse.
  ENDIF.
  CHECK e_subrc = 0.
*-* Inicializa Variables
  READ TABLE t_prmtrs ASSIGNING <fp> WITH KEY zfield = 'DOC_TYPE'.  "ZOCI
  IF sy-subrc IS INITIAL.
    g_bsart = <fp>-zchar.
  ENDIF.
  READ TABLE t_prmtrs ASSIGNING <fp> WITH KEY zfield = 'ELMNT_PEP'.
  IF sy-subrc IS INITIAL.
    g_elpep = <fp>-zchar.
  ENDIF.
  READ TABLE t_prmtrs ASSIGNING <fp> WITH KEY zfield = 'MATNR'.
  IF sy-subrc IS INITIAL.
    vnum18 = 0 + <fp>-zchar.
    MOVE vnum18 TO g_matnr.
  ENDIF.
  READ TABLE t_prmtrs ASSIGNING <fp> WITH KEY zfield = 'PURC_ORG'.
  IF sy-subrc IS INITIAL.
    g_ekorg = <fp>-zchar.
  ENDIF.
  READ TABLE t_prmtrs ASSIGNING <fp> WITH KEY zfield = 'QUANTITY'.
  IF sy-subrc IS INITIAL.
    g_etmen = <fp>-zchar.
  ELSE.
    g_etmen = '1.000'.
  ENDIF.
  READ TABLE t_prmtrs ASSIGNING <fp> WITH KEY zfield = 'TAX_CODE'.
  IF sy-subrc IS INITIAL.
    g_tax = <fp>-zchar.
  ENDIF.

ENDFORM.                    "prerequest

*&---------------------------------------------------------------------*
*&      Form  item_serv
*&---------------------------------------------------------------------*
FORM item_serv_decre_serv USING u_item TYPE ty_item
                     t_rojas TYPE zmmfm_0070_roja_tt.
*gs_bapiesllc-pckg_no.
  DATA: l_asmdt TYPE ty_asmdt.
  FIELD-SYMBOLS <fp> TYPE zmmfm_0070_roja.

  gs_bapiesllc-subpckg_no = gs_bapiesllc-pckg_no.
  ADD 1 TO gs_bapiesllc-subpckg_no.

  gs_bapiesllc-ext_line        = '0000000000'.

* Services o partidas rojas
  it_poservices-pckg_no        = gs_bapiesllc-pckg_no.      "0000000001
  it_poservices-line_no        = gs_bapiesllc-line_no.      "0000000001
  it_poservices-ext_line       = gs_bapiesllc-ext_line.
  it_poservices-outl_ind       = 'X'.
  it_poservices-subpckg_no     = gs_bapiesllc-subpckg_no.   "0000000002
  it_poservices-from_line      = '1'.
  APPEND it_poservices.
  CLEAR it_poservices.
  gs_bapiesllc-pckg_no = gs_bapiesllc-subpckg_no.
  it_poservices-line_no = gs_bapiesllc-line_no.
  LOOP AT t_rojas ASSIGNING <fp> WHERE matkl(4) = u_item-matkl.
    READ TABLE it_asmdt INTO l_asmdt WITH KEY asnum = <fp>-matkl.
    CONCATENATE l_asmdt-asnum l_asmdt-asktx INTO l_asmdt-asktx
    SEPARATED BY space.
    ADD  1 TO it_poservices-line_no.
    ADD 10 TO gs_bapiesllc-ext_line.
* Servicse o partida Rojas many positions after hedar position
    it_poservices-pckg_no      = gs_bapiesllc-subpckg_no.   "0000000002
    it_poservices-line_no      = it_poservices-line_no.   "0000000002 "" Incerement for eact service
    it_poservices-ext_line     = gs_bapiesllc-ext_line.   "0000000010 "" Incerement for eact service
    it_poservices-subpckg_no   = '0000000000'.
*    it_poservices-service      = <fp>-matkl. "40101 "" different  service
    PERFORM conversion_exit_alpha_input USING <fp>-matkl
          CHANGING it_poservices-service.
    it_poservices-quantity     = '1.000'.
    it_poservices-base_uom     = 'PTO'.
    it_poservices-price_unit   = '1'.
    it_poservices-gr_price     = <fp>-netwr.                "1,000.0000
    it_poservices-short_text   = l_asmdt-asktx . "ingresos o egresos inducidos
    it_poservices-tax_code     = 'F2'.
    it_poservices-matl_group   = u_item-matkl.              "0401

    APPEND it_poservices.

* Services Values POSRVACCESSVALUES
    it_posrvaccessvalues-pckg_no     = gs_bapiesllc-subpckg_no. "0000000002
    it_posrvaccessvalues-line_no     = it_poservices-line_no.   "0000000002  "" Increment depend POSERVICES just items nof ro first line head
    it_posrvaccessvalues-serno_line  = '01'.
    it_posrvaccessvalues-percentage  = '100.00'.
    it_posrvaccessvalues-serial_no   = '01'.
    it_posrvaccessvalues-quantity    = '1.000'.
    it_posrvaccessvalues-net_value   = '1.00'.
    APPEND it_posrvaccessvalues.
    ADD 1 TO it_poservices-line_no.
  ENDLOOP.

ENDFORM.                    "llenado_position
