*&---------------------------------------------------------------------*
*& Report  ZMM0030_INFODETALLADO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmm0030_infodetallado.
INCLUDE <icon>.
* Variables, check EKBE
TYPES: BEGIN OF ty_items,
         nivel(15) TYPE c,           "1
         bsart     TYPE ekko-bsart,  "2
         ebeln     TYPE ekko-ebeln,
         kdatb     TYPE ekko-kdatb,
         kdate     TYPE ekko-kdate,
         lifnr     TYPE ekko-lifnr,
         name1     TYPE lfa1-name1,  ""7
* Postition
         ebelp     TYPE ekpo-ebelp,  ""8
         werks     TYPE ekpo-werks,
         matkl     TYPE ekpo-matkl,
         wgbez     TYPE t023t-wgbez, "Description
         menge     TYPE ekpo-menge,
         meins     TYPE ekpo-meins,
         brtwr     TYPE ekpo-brtwr,
         amor1(21) TYPE c, ""konp-kbetr, "Amortizacion %
         amor2     TYPE ekpo-netwr,  "AMORTIZACION VALUE
         gara1(21) TYPE c, ""konp-kbetr, "Garantia %
         gara2     TYPE ekpo-netwr,  "Garantia VALUE
         netwr     TYPE ekpo-netwr, ""19
* services
         budat     TYPE ekbe-budat, ""20
         dmbtr     TYPE ekbe-dmbtr,
         meng1     TYPE ekbe-menge, "Cant estimada
         estim     TYPE ekbe-dmbtr,
         amor3     TYPE ekbe-dmbtr,
         gara3     TYPE ekbe-dmbtr,
         sumat     TYPE ekbe-dmbtr, "Sumatoria
         belnr     TYPE ekbe-belnr, "FACTURA
         buda1     TYPE ekbe-budat, "REGISTRO FACTURA
         pasiv     TYPE ekbe-dmbtr,
         waers     TYPE ekbe-waers, ""30
       END OF ty_items.


DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm,
      g_top_key TYPE lvc_nkey,
      gs_proj TYPE proj,
      gs_prps TYPE prps,
      it_lfa1 TYPE SORTED TABLE OF lfa1 WITH UNIQUE DEFAULT KEY,
      it_ekkn TYPE SORTED TABLE OF ekkn WITH UNIQUE DEFAULT KEY,
      it_ekko TYPE SORTED TABLE OF ekko WITH UNIQUE DEFAULT KEY,
      it_ekpo TYPE SORTED TABLE OF ekpo WITH UNIQUE DEFAULT KEY,
      it_ekbe TYPE STANDARD TABLE OF ekbe WITH HEADER LINE,
      it_t023t TYPE SORTED TABLE OF t023t WITH NON-UNIQUE DEFAULT KEY,
      it_asmdt TYPE SORTED TABLE OF asmdt WITH NON-UNIQUE DEFAULT KEY,
      it_cond TYPE STANDARD TABLE OF bapimepocond WITH HEADER LINE,
      it_esll TYPE SORTED TABLE OF ml_esll WITH NON-UNIQUE DEFAULT KEY,
      it_konv TYPE SORTED TABLE OF konv WITH UNIQUE DEFAULT KEY,
      it_bkpf TYPE SORTED TABLE OF bkpf WITH UNIQUE DEFAULT KEY,
      it_bseg TYPE SORTED TABLE OF bseg WITH UNIQUE DEFAULT KEY,
      gi_bsart TYPE edm_bsart_range_tt.
DATA: g_alv_tree          TYPE REF TO cl_gui_alv_tree,
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      gt_fieldcatalog     TYPE lvc_t_fcat,
      gt_item             TYPE STANDARD TABLE OF ty_items,
      gs_item             TYPE ty_items.
DATA: p_logo             TYPE sdydo_value.
DATA: t_prmtrs TYPE SORTED TABLE OF zutil_parameters
      WITH UNIQUE DEFAULT KEY.

* Selection de parametros
PARAMETERS: p_proye           TYPE proj-pspid OBLIGATORY,  ""everk
            p_posid           TYPE prps-pspnr .""OBLIGATORY. ""ps_posid


* Selections
START-OF-SELECTION.
  PERFORM process.



*&---------------------------------------------------------------------*
*&      Form  process
*&---------------------------------------------------------------------*
FORM process.
  DATA: e_subrc TYPE subrc.
  PERFORM valida_projecto CHANGING e_subrc.
*  IF e_subrc IS INITIAL.
  PERFORM valida_pep.
  PERFORM read_data.
  PERFORM show.
*  ENDIF.
ENDFORM.                    "process

*&---------------------------------------------------------------------*
*&      Form  show
*&---------------------------------------------------------------------*
FORM show.
  CALL SCREEN 1100.
ENDFORM.                    "show


*&---------------------------------------------------------------------*
*&      Form  read_ekkn
*&---------------------------------------------------------------------*
FORM read_data.
  DATA: lt_esll TYPE SORTED TABLE OF ml_esll WITH NON-UNIQUE DEFAULT KEY,
        lt_bkpf TYPE STANDARD TABLE OF bkpf WITH HEADER LINE,
        ls_ekbe TYPE ekbe.
*Imputación en el documento de compras
  SELECT * FROM ekkn INTO TABLE it_ekkn
           WHERE ps_psp_pnr EQ p_posid.
  IF it_ekkn[] IS NOT INITIAL.
* Purchase order
    SELECT * FROM ekko INTO TABLE it_ekko
             FOR ALL ENTRIES IN it_ekkn
             WHERE ebeln EQ it_ekkn-ebeln
             AND bsart IN gi_bsart.
* Postions
    IF it_ekko[] IS NOT INITIAL.
      SELECT * FROM ekpo INTO TABLE it_ekpo
               FOR ALL ENTRIES IN it_ekko
               WHERE ebeln EQ it_ekko-ebeln.
* Líneas del paquete de serv.
      SELECT * FROM ml_esll INTO TABLE it_esll
               FOR ALL ENTRIES IN it_ekko
               WHERE ebeln EQ it_ekko-ebeln.
* EKBE
      SELECT * FROM ekbe INTO TABLE it_ekbe
               FOR ALL ENTRIES IN it_ekko
               WHERE ebeln EQ it_ekko-ebeln.
* Get customers
      SELECT * FROM lfa1 INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_ekko
        WHERE lifnr EQ it_ekko-lifnr.
* DESCRIP
      IF it_ekpo[] IS NOT INITIAL.
        SELECT * FROM t023t INTO TABLE it_t023t
          FOR ALL ENTRIES IN it_ekpo
        WHERE spras EQ sy-langu AND matkl EQ it_ekpo-matkl.

        SELECT * FROM t023t INTO TABLE it_t023t
          FOR ALL ENTRIES IN it_ekpo
        WHERE spras EQ sy-langu AND matkl EQ it_ekpo-matkl.
      ENDIF.
* Descr serv
      lt_esll[] = it_esll[].
      DELETE lt_esll WHERE srvpos = space.
      IF lt_esll[] IS NOT INITIAL.
        SELECT * FROM asmdt INTO TABLE it_asmdt
          FOR ALL ENTRIES IN lt_esll
        WHERE spras EQ sy-langu AND asnum EQ lt_esll-srvpos.
      ENDIF.
* looking for conditions
* Quick Fix Replace KONV table access with the access of compatibility view V_KONV
* 30.06.2025 14:56:21 LTORRES
* Transport RESK900081 ATC - 2
* Replaced Code:
*      SELECT * FROM konv INTO TABLE it_konv
*        FOR ALL ENTRIES IN it_ekko
*        WHERE knumv EQ it_ekko-knumv.

SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @IT_EKKO WHERE KNUMV EQ @IT_EKKO-KNUMV INTO CORRESPONDING FIELDS OF TABLE @IT_KONV .
* End of Quick Fix

* amortizavcion
      LOOP AT it_ekbe INTO ls_ekbe.
        CONCATENATE ls_ekbe-belnr ls_ekbe-gjahr INTO
        lt_bkpf-awkey.
        APPEND lt_bkpf.
      ENDLOOP.
      IF lt_bkpf[] IS NOT INITIAL.
        SELECT * FROM bkpf INTO TABLE it_bkpf
        FOR ALL ENTRIES IN lt_bkpf
        WHERE awkey EQ lt_bkpf-awkey.
      ENDIF.
      IF it_bkpf IS NOT INITIAL.
* Quick Fix Replace SELECT from table BSEG by API Call
* 30.06.2025 14:58:25 LTORRES
* Transport RESK900081 ATC - 2
* Replaced Code:
*        SELECT * FROM bseg INTO TABLE it_bseg
*          FOR ALL ENTRIES IN it_bkpf
*          WHERE bukrs EQ it_bkpf-bukrs
*          AND   belnr EQ it_bkpf-belnr
*          AND   gjahr EQ it_bkpf-gjahr.

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BKPF
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR|
    IMPORTING ET_BSEG = IT_BSEG
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC <> 0 OR LINES( IT_BSEG ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( IT_BSEG ).
ENDIF.

* End of Quick Fix
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "read_ekkn

*&---------------------------------------------------------------------*
*&      Form  valida_projecto
*&---------------------------------------------------------------------*
FORM valida_pep.
* validar Element pep PSPNR
  SELECT SINGLE * FROM prps INTO gs_prps WHERE pspnr EQ p_posid.
  IF sy-subrc IS NOT INITIAL.
    WRITE:/ 'Elemento PEP invalido'.
    STOP.
  ENDIF.
  p_logo = 'ZGIG'. ""ENJOYSAP_LOGO
ENDFORM.                    "valida_projecto

*&---------------------------------------------------------------------*
*&      Form  valida_projecto
*&---------------------------------------------------------------------*
FORM valida_projecto CHANGING e_subrc TYPE subrc.
  DATA: lpara TYPE zutil_parameters.
  DATA: l_bsart TYPE edm_bsart_range.

* validar projecto
  SELECT SINGLE * FROM proj INTO gs_proj WHERE pspid EQ p_proye. "#EC *
  IF sy-subrc IS NOT INITIAL.
    WRITE:/ 'Projecto invalido'.
    STOP.
  ENDIF.
* Utilidades
  SELECT * INTO TABLE t_prmtrs
       FROM zutil_parameters
       WHERE zreport EQ 'ZMM0030_INFODETALLADO'.
  IF sy-subrc IS NOT INITIAL.
    e_subrc = '04'.
  ENDIF.
  l_bsart-option = 'EQ'.
  l_bsart-sign = 'I'.
  LOOP AT t_prmtrs INTO lpara.
    l_bsart-low = lpara-zchar.
    APPEND l_bsart TO gi_bsart.
  ENDLOOP.


ENDFORM.                    "valida_projecto
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_1100 OUTPUT.
  PERFORM pbo_1100.
ENDMODULE.                 " STATUS_1100  OUTPUT

*&---------------------------------------------------------------------*
FORM pbo_1100 .
  SET PF-STATUS 'MAIN1000'.
  SET TITLEBAR 'MAINTITLE'.

  IF g_alv_tree IS INITIAL.
    PERFORM init_tree.
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    IF sy-subrc NE 0.
      PERFORM popup_to_inform USING text-100 text-101 text-102 text-103.
    ENDIF.
  ENDIF.
ENDFORM.                                                    " PBO_1000
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_INFORM
*&---------------------------------------------------------------------*
FORM popup_to_inform USING title t1 t2 t3.
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = title
      txt1  = t1
      txt2  = t2
      txt3  = t3.
ENDFORM.                    "POPUP_TO_INFORM
*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
FORM init_tree.
  DATA: l_tree_container_name(30) TYPE c,
        l_hierarchy_header TYPE treev_hhdr,
        lt_list_commentary TYPE slis_t_listheader.
  DATA: ls_variant      TYPE disvariant.

  ls_variant-report = sy-repid.

  l_tree_container_name = 'CCONTAINER1'.
  PERFORM build_fieldcatalog.

  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = l_tree_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'(100).
  ENDIF.

* create tree control
  CREATE OBJECT g_alv_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = 'X'
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
* Hide columns and mark complete column for icon display
  PERFORM build_comment USING lt_list_commentary.

  CALL METHOD g_alv_tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = l_hierarchy_header
      it_list_commentary  = lt_list_commentary
      i_logo              = p_logo
      i_save              = 'A'
      is_variant          = ls_variant
    CHANGING
      it_fieldcatalog     = gt_fieldcatalog
      it_outtab           = gt_item.          "gt_vbap. "table must be empty !

  PERFORM register_events.

* Send data to frontend.
  CALL METHOD g_alv_tree->frontend_update.

* Armar la salida
  PERFORM create_hierarchy.

ENDFORM.                               " init_tree

*----------------------------------------------------------------------*
*      -->PT_LIST_COMMENTARY  text
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader.

  DATA: ls_line TYPE slis_listheader.
*
* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  NOT USED FOR THIS TYPE
  ls_line-info = 'Informe presupuestal detallado'.          "#EC NOTEXT
  APPEND ls_line TO pt_list_commentary.
* STATUS LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'ID Proyecto'.                             "#EC NOTEXT
  CONCATENATE gs_proj-pspid gs_proj-post1 INTO
  ls_line-info SEPARATED BY space.                          "#EC NOTEXT
  APPEND ls_line TO pt_list_commentary.
  ls_line-key  = 'ELEMENTO PEP:'.
  CONCATENATE gs_prps-posid gs_prps-post1 INTO
  ls_line-info SEPARATED BY space.                          "#EC NOTEXT
  APPEND ls_line TO pt_list_commentary.

ENDFORM.                    "build_comment
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING p_hierarchy_header TYPE treev_hhdr.
  p_hierarchy_header-heading = ''.
  p_hierarchy_header-tooltip = 'Presupuesto Detallado'.
  p_hierarchy_header-width = 1.
  p_hierarchy_header-width_pix = ''.
ENDFORM.                               " build_hierarchy_header
*--------------------------------------------------------------------
FORM build_fieldcatalog.
  DATA: ls_fieldcatalog  TYPE lvc_s_fcat,
        lt_fieldcatalog  TYPE lvc_t_fcat.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-fieldname = 'NIVEL'.
  ls_fieldcatalog-coltext   = 'Nivel'.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-no_out = 'X'.
  ls_fieldcatalog-icon      = 'X'.
  ls_fieldcatalog-col_pos   = 1.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
  CLEAR ls_fieldcatalog-no_out.
  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-fieldname = 'BSART'.
  ls_fieldcatalog-coltext   = 'Cl. Doc.'.
  ls_fieldcatalog-col_pos   = 2.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  PERFORM lvc_fieldcatalog_merge USING 'EKPO'
                                 CHANGING lt_fieldcatalog.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'EBELN'.
  IF sy-subrc IS INITIAL.
    ls_fieldcatalog-col_pos   = 3.
    ls_fieldcatalog-no_out = 'X'.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.
  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'AEDAT'.
  IF sy-subrc IS INITIAL.
    ls_fieldcatalog-fieldname = 'KDATB'.
    ls_fieldcatalog-col_pos   = 4.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
    ls_fieldcatalog-coltext   = 'Inicio Vigencia'.

    ls_fieldcatalog-fieldname = 'KDATE'.
    ls_fieldcatalog-col_pos   = 5.
    ls_fieldcatalog-coltext   = 'Fin Vigencia'.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.

  ls_fieldcatalog-fieldname = 'LIFNR'.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-coltext   = 'Proveedor'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

* Button col.
  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-fieldname = 'NAME1'.
  ls_fieldcatalog-coltext   = 'Nombre proveedor'.
  ls_fieldcatalog-col_pos   = 7.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

* positions
  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
  WITH KEY fieldname = 'EBELP'.
  CLEAR ls_fieldcatalog-no_out.
  IF sy-subrc IS INITIAL.
    ls_fieldcatalog-col_pos   = 8.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
  WITH KEY fieldname = 'WERKS'.
  CLEAR ls_fieldcatalog-no_out.
  IF sy-subrc IS INITIAL.
    ls_fieldcatalog-col_pos   = 9.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
  WITH KEY fieldname = 'MATKL'.
  CLEAR ls_fieldcatalog-no_out.
  IF sy-subrc IS INITIAL.
    ls_fieldcatalog-col_pos   = 10.
    ls_fieldcatalog-coltext   = 'Id'.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-fieldname = 'WGBEZ'.
  ls_fieldcatalog-coltext   = 'Descripcion nivel'.
  ls_fieldcatalog-col_pos = 11.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
  WITH KEY fieldname = 'MENGE'.
  IF sy-subrc IS INITIAL.
    CLEAR ls_fieldcatalog-no_out.
    ls_fieldcatalog-col_pos   = 12.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
  WITH KEY fieldname = 'MEINS'.
  IF sy-subrc IS INITIAL.
    CLEAR ls_fieldcatalog-no_out.
    ls_fieldcatalog-col_pos   = 13.
    ls_fieldcatalog-coltext   = 'UM'.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.
  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
  WITH KEY fieldname = 'BRTWR'.
  IF sy-subrc IS INITIAL.
    CLEAR ls_fieldcatalog-no_out.
    ls_fieldcatalog-col_pos   = 14.
    ls_fieldcatalog-coltext   = 'Importe bruto'.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.
*
  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'AMOR1'.
  ls_fieldcatalog-coltext   = 'Amortiza %'.
  ls_fieldcatalog-col_pos = 15.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-decimals_o = 0.
  ls_fieldcatalog-no_zero = 'X'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ls_fieldcatalog-fieldname = 'AMOR2'.
  ls_fieldcatalog-coltext   = 'Amortiza valor'.
  ls_fieldcatalog-col_pos = 16.
  ls_fieldcatalog-decimals_o = 2.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
  CLEAR ls_fieldcatalog-no_zero.
  ls_fieldcatalog-fieldname = 'GARA1'.
  ls_fieldcatalog-coltext   = 'Garantia %'.
  ls_fieldcatalog-col_pos = 17.
  ls_fieldcatalog-decimals_o = 0.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ls_fieldcatalog-fieldname = 'GARA2'.
  ls_fieldcatalog-coltext   = 'Garantia valor'.
  ls_fieldcatalog-col_pos = 18.
  ls_fieldcatalog-decimals_o = 2.
  ls_fieldcatalog-no_zero = 'X'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'NETWR'.
  IF sy-subrc IS INITIAL.
    CLEAR ls_fieldcatalog-no_out.
    ls_fieldcatalog-col_pos   = 19.
    ls_fieldcatalog-coltext   = 'Importe Neto'.
    ls_fieldcatalog-no_zero = 'X'.
    APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ENDIF.

* SERVICES....
  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'AEDAT'.   ""AEDAT   BUDAT
*  IF sy-subrc IS INITIAL.
  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'BUDAT'.
  ls_fieldcatalog-col_pos   = 20.
  ls_fieldcatalog-coltext   = 'Fecha'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
*  ENDIF.
  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'NETWR'.   ""NETWR  DMBTR
*  IF sy-subrc IS INITIAL.
  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'DMBTR'.
  ls_fieldcatalog-col_pos   = 21.
  ls_fieldcatalog-coltext   = 'Anticipo'.
  ls_fieldcatalog-no_zero = 'X'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
*  ENDIF.

  CLEAR ls_fieldcatalog.
  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'MENGE'. ""MENGE
  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'MENG1'.
  ls_fieldcatalog-coltext   = 'Cantidad'.
  ls_fieldcatalog-col_pos = 22.
  ls_fieldcatalog-no_zero = 'X'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'NETWR'.   "  DMBTR

  ls_fieldcatalog-fieldname = 'SUMAT'.
  ls_fieldcatalog-coltext   = 'Importe'.
  ls_fieldcatalog-col_pos = 23.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'ESTIM'.
  ls_fieldcatalog-coltext   = '$ Proveedor'.
  ls_fieldcatalog-col_pos = 24.
  ls_fieldcatalog-no_zero = 'X'.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ls_fieldcatalog-fieldname = 'AMOR3'.
  ls_fieldcatalog-coltext   = '$ Amortizado'.
  ls_fieldcatalog-col_pos = 25.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.
  ls_fieldcatalog-fieldname = 'GARA3'.
  ls_fieldcatalog-coltext   = '$ Garantia'.
  ls_fieldcatalog-outputlen = 26.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.


  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'EBELN'.  ""EBELN   BELNR

  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'BELNR'.
  ls_fieldcatalog-coltext   = 'Factura'.
  ls_fieldcatalog-col_pos = 27.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'AEDAT'. "AEDAT BUDAT
  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'BUDA1'.
  ls_fieldcatalog-coltext   = 'Reg Factura'.
  ls_fieldcatalog-col_pos = 28.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  READ TABLE lt_fieldcatalog INTO ls_fieldcatalog
       WITH KEY fieldname = 'NETWR'.   "NETWR DMBTR
  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'PASIV'.
  ls_fieldcatalog-coltext   = 'Pasivo'.
  ls_fieldcatalog-col_pos = 29.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

  CLEAR ls_fieldcatalog-no_out.
  ls_fieldcatalog-fieldname = 'WAERS'.
  ls_fieldcatalog-col_pos   = 30.
  APPEND ls_fieldcatalog TO gt_fieldcatalog.

ENDFORM.                    "build_fieldcatalog

*--------------------------------------------------------------------
* calrogo
*--------------------------------------------------------------------
FORM lvc_fieldcatalog_merge USING p_name TYPE dd02l-tabname
                            CHANGING p_cat TYPE lvc_t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = p_name    "'VBAP'
    CHANGING
      ct_fieldcat      = p_cat.   "gt_fieldcatalog.
ENDFORM.                    "LVC_FIELDCATALOG_MERGE

*-----------------------------------------------------------------------
FORM register_events.
  DATA: lt_events TYPE cntl_simple_events.
  ""        l_event TYPE cntl_simple_event.

  CALL METHOD g_alv_tree->get_registered_events
    IMPORTING
      events = lt_events.

* Register additional events for your own purposes:
  PERFORM set_event_handler TABLES lt_events.

* register events on frontend
  CALL METHOD g_alv_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

ENDFORM.                               " register_events
*----------------------------------------------------------------------*
FORM set_event_handler TABLES lt_events TYPE cntl_simple_events.
  DATA l_event TYPE cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.

ENDFORM.                    "set_event_handler

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1100  INPUT
MODULE user_command_1100 INPUT.
  PERFORM user_command_1100.
ENDMODULE.                 " USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*&      Form  PAI
*&---------------------------------------------------------------------*
FORM user_command_1100.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " PAI
*&---------------------------------------------------------------------*
*       free object and leave program
*----------------------------------------------------------------------*
FORM exit_program.
  CALL METHOD g_custom_container->free.
  LEAVE PROGRAM.
*  set SCREEN 1000.
ENDFORM.                             " exit_program

*----------------------------------------------------------------------
FORM add_a_folder USING  p_relat_key TYPE lvc_nkey
                         p_node_text TYPE lvc_value
                CHANGING p_new_key.

  DATA: lindex        TYPE sy-tabix,
        l_layout_node TYPE lvc_s_layn.

  l_layout_node-isfolder = ' '.
  l_layout_node-style = cl_gui_column_tree=>style_intensified.
  CALL METHOD g_alv_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = p_node_text
      is_node_layout   = l_layout_node
* psoible
      is_outtab_line   = gs_item          "p_nodo
    IMPORTING
      e_new_node_key   = p_new_key.

  DESCRIBE TABLE gt_item LINES lindex.
  IF lindex GE 1.
    READ TABLE gt_item INTO gs_item INDEX lindex.
    IF sy-subrc IS INITIAL.
      gs_item-nivel = p_new_key.
      MODIFY gt_item FROM gs_item INDEX lindex TRANSPORTING nivel.
    ENDIF.
  ENDIF.

ENDFORM.                    "add_a_folder

*-----------------------------------------------------------------------
FORM add_a_node USING  p_nodo       TYPE ekpo               "#EC NEEDED
                       p_relat_key  TYPE lvc_nkey
                       p_node_text  TYPE lvc_value
                       p_level      TYPE sy-tabix
              CHANGING p_new_key.

  DATA: l_layout_node  TYPE lvc_s_layn,
        lt_layout_item TYPE lvc_t_layi.

*1.Use the layout structure for nodes to add an icon when you
  CASE p_level.
    WHEN 1. " Characte
*      gs_item-ebelp = p_nodo-ebelp.
*      l_layout_node-n_image = '@KB@'.
* Items proper
    WHEN OTHERS.
  ENDCASE.

* Add node
  l_layout_node-style = cl_gui_column_tree=>STYLE_EMPHASIZED_NEGATIVE. ""style_emphasized_c. ""style_intensifd_critical.
  gs_item-nivel = p_new_key.
  CALL METHOD g_alv_tree->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = p_node_text
      is_outtab_line   = gs_item          "p_nodo
      is_node_layout   = l_layout_node
      it_item_layout   = lt_layout_item
    IMPORTING
      e_new_node_key   = p_new_key.

  CLEAR gs_item.

ENDFORM.                    "add_a_node

*&---------------------------------------------------------------------*
*&      Form  create_hierarchy
*&---------------------------------------------------------------------*
FORM create_hierarchy.
  DATA: ls_ekko     TYPE ekko,
        ls_esll     TYPE ml_esll,
        ls_lfa1     TYPE lfa1,
        ls_t023t    TYPE t023t,
        ls_ekbe     TYPE ekbe,
        ls_ekbe1     TYPE ekbe,
        ls_asmdt    TYPE asmdt,
        ls_bseg     TYPE bseg,
        l_node_text TYPE lvc_value,
        ls_bkpf     TYPE bkpf,
        ls_ekpo     TYPE ekpo,
        ls_konv     TYPE konv,
        l_item_key  TYPE lvc_nkey,
        lbal1       TYPE sy-tabix,
        lbal2       TYPE sy-tabix.

*********** Pedidos
  DELETE it_esll WHERE extrow EQ space.
  DELETE it_ekbe WHERE vgabe NE '1' AND
   vgabe NE '2' AND vgabe NE '4'.

  SORT it_ekbe BY ebeln budat vgabe.
  LOOP AT it_ekko INTO ls_ekko.
    LOOP AT it_ekpo INTO ls_ekpo WHERE ebeln EQ ls_ekko-ebeln.
      READ TABLE it_t023t INTO ls_t023t WITH KEY matkl = ls_ekpo-matkl.
      READ TABLE it_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_ekko-lifnr.
      l_node_text = ls_ekko-ebeln.
      gs_item-bsart = ls_ekko-bsart.
      gs_item-ebeln = ls_ekko-ebeln.
      gs_item-kdatb = ls_ekko-kdatb.
      gs_item-kdate = ls_ekko-kdate.
      gs_item-lifnr = ls_ekko-lifnr.
      gs_item-name1 = ls_lfa1-name1.
* Pos blue
      gs_item-ebelp = ls_ekpo-ebelp.
      gs_item-werks = ls_ekpo-werks.
      gs_item-matkl = ls_ekpo-matkl.
      gs_item-wgbez = ls_t023t-wgbez.
      gs_item-menge = ls_ekpo-menge.
      gs_item-meins = ls_ekpo-meins.
      gs_item-brtwr = ls_ekpo-brtwr.
      gs_item-netwr = ls_ekpo-netwr.
      CLEAR:  ls_konv.

      READ TABLE it_konv INTO ls_konv WITH KEY knumv = ls_ekko-knumv
          kposn = ls_ekpo-ebelp kschl = 'ZAAN'.
      IF sy-subrc IS INITIAL. "KBETR %  kwert value
        ls_konv-kbetr = ABS( ls_konv-kbetr ).
        CATCH SYSTEM-EXCEPTIONS OTHERS = 8.
          gs_item-amor1 = ( ls_konv-kbetr / 10 ).
        ENDCATCH.
        gs_item-amor2 = ABS( ls_konv-kwert ).
      ENDIF.
      READ TABLE it_konv INTO ls_konv WITH KEY knumv = ls_ekko-knumv
          kposn = ls_ekpo-ebelp kschl = 'ZRET'.
      IF sy-subrc IS INITIAL.
        ls_konv-kbetr = ABS( ls_konv-kbetr ).
        CATCH SYSTEM-EXCEPTIONS OTHERS = 8.
          gs_item-gara1 = ( ls_konv-kbetr / 10 ).
        ENDCATCH.
        gs_item-gara2  = ABS( ls_konv-kwert ).
      ENDIF.


      gs_item-amor1 = TRUNC( gs_item-amor1 ).
      SHIFT gs_item-amor1 LEFT DELETING LEADING ''.
      IF gs_item-amor1 NE '0'.
        CONCATENATE gs_item-amor1 '%' INTO gs_item-amor1.
      ENDIF.
      gs_item-gara1 = TRUNC( gs_item-gara1 ).
      SHIFT gs_item-gara1 LEFT DELETING LEADING ''.
      IF gs_item-gara1 NE '0'.
        CONCATENATE gs_item-gara1 '%' INTO gs_item-gara1.
      ENDIF.
      PERFORM add_a_folder USING    space l_node_text
                           CHANGING g_top_key.

      CLEAR: gs_item-matkl, gs_item-meins, gs_item-menge,
      gs_item-amor1, gs_item-amor2, gs_item-gara1, gs_item-gara2,
      gs_item-netwr, gs_item-bsart, gs_item-ebeln, gs_item-kdatb,
      gs_item-kdate, gs_item-lifnr, gs_item-name1, gs_item-ebelp,
      gs_item-werks, l_node_text.

      LOOP AT it_esll INTO ls_esll WHERE ebeln EQ ls_ekko-ebeln
        AND ebelp EQ ls_ekpo-ebelp.
** Add items
        gs_item-menge = ls_esll-menge.
        gs_item-meins = ls_esll-meins.
        READ TABLE it_asmdt INTO ls_asmdt WITH KEY asnum = ls_esll-srvpos.

*        l_node_text = 'Nivel Rojo'.
        gs_item-wgbez = ls_asmdt-asktx.
        CLEAR: gs_item-budat, gs_item-dmbtr.
        PERFORM conversion_exit_alpha_output USING ls_esll-srvpos CHANGING gs_item-matkl.
* Check all position
        CLEAR: gs_item-budat,gs_item-meng1,gs_item-waers,gs_item-buda1,
        gs_item-belnr, gs_item-estim,gs_item-pasiv, gs_item-dmbtr, lbal1.

        LOOP AT it_ekbe INTO ls_ekbe WHERE ebeln = ls_esll-ebeln
               AND ebelp = ls_esll-ebelp.
          ADD 1 TO lbal1.
          CLEAR lbal2.
          IF lbal1 EQ 1.
            LOOP AT it_ekbe INTO ls_ekbe1 WHERE ebeln = ls_esll-ebeln
                AND   ebelp = ls_esll-ebelp AND budat = ls_ekbe-budat.
              ADD 1 TO lbal2.
            ENDLOOP.
          ENDIF.

          gs_item-budat = ls_ekbe-budat.
          gs_item-waers = ls_ekbe-waers.
          CASE ls_ekbe-vgabe.
            WHEN 1.
              gs_item-meng1 = ls_ekbe-menge.
              gs_item-estim = ls_ekbe-dmbtr.
            WHEN 2.
              gs_item-pasiv  = ls_ekbe-dmbtr.
              gs_item-buda1 = ls_ekbe-budat.
              gs_item-belnr = ls_ekbe-belnr.
            WHEN 4.
              gs_item-dmbtr = ls_ekbe-dmbtr.
          ENDCASE.
* get amortizacion y garantia
          CONCATENATE ls_ekbe-belnr ls_ekbe-gjahr INTO ls_bkpf-awkey.
          READ TABLE it_bkpf INTO ls_bkpf WITH KEY awkey = ls_bkpf-awkey.
          IF sy-subrc IS INITIAL.
            READ TABLE it_bseg INTO ls_bseg WITH KEY belnr = ls_bkpf-belnr ktosl = 'ZE1'.
            IF sy-subrc IS INITIAL.
              gs_item-gara3 = ls_bseg-dmbtr.
            ENDIF.
            READ TABLE it_bseg INTO ls_bseg WITH KEY belnr = ls_bkpf-belnr ktosl = 'ZE2'.
            IF sy-subrc IS INITIAL.
              gs_item-amor3 = ls_bseg-dmbtr.
            ENDIF.
          ENDIF.
          gs_item-sumat = gs_item-estim + gs_item-amor3 +
          gs_item-gara3.
          IF ( lbal1 EQ 1 AND lbal2 EQ 1 ) OR ( lbal1 GT 1 ).
            PERFORM add_a_node USING ls_ekpo g_top_key
                                     l_node_text 1
                               CHANGING l_item_key.
            CLEAR: gs_item-matkl.
            CLEAR lbal1.
          ENDIF.
        ENDLOOP.
* End position services
        IF sy-subrc IS NOT INITIAL. ""Cuando no hay datos ekbe addiciona las posic, si hay se add dentro de loop
          gs_item-sumat = gs_item-estim + gs_item-amor3 +
          gs_item-gara3.
          PERFORM add_a_node USING ls_ekpo g_top_key
                                   l_node_text 1
                             CHANGING l_item_key.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  CALL METHOD g_alv_tree->expand_node
    EXPORTING
      i_node_key = g_top_key.

* column optimizing
  PERFORM column_optimize.

ENDFORM.                               " create_hierarchy

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_output USING in CHANGING out.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = in
    IMPORTING
      output = out.

ENDFORM.                    "CONVERSION_EXIT_ALPHA_OUTPUT
*----------------------------------------------------------------------*
FORM column_optimize.
  DATA:  p_inchdr VALUE 'X'.
* optimize column-width
  CALL METHOD g_alv_tree->column_optimize
    EXPORTING
      i_include_heading = p_inchdr.
ENDFORM.                    "column_optimize
*         belnr     TYPE ekbe-belnr, "FACTURA
*         buda1     TYPE ekbe-budat, "REGISTRO FACTURA
*gs_item-estim + gs_item-amor3 +
*        gs_item-gara3.
FORM bapi_po_getdetail1 USING ls_ekko TYPE ekko.            "#EC CALLED
  REFRESH it_cond.
  CALL FUNCTION 'BAPI_PO_GETDETAIL1' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      purchaseorder            = ls_ekko-ebeln
*   ACCOUNT_ASSIGNMENT       = ' '
*   ITEM_TEXT                = ' '
*   HEADER_TEXT              = ' '
*   DELIVERY_ADDRESS         = ' '
*   VERSION                  = ' '
*   SERVICES                 = ' '
* IMPORTING
*   POHEADER                 =
*   POEXPIMPHEADER           =
   TABLES
*   RETURN                   =
*   POITEM                   =
*   POADDRDELIVERY           =
*   POSCHEDULE               =
*   POACCOUNT                =
*   POCONDHEADER             =
     pocond                   = it_cond
*   POLIMITS                 =
*   POCONTRACTLIMITS         =
*   POSERVICES               =
*   POSRVACCESSVALUES        =
*   POTEXTHEADER             =
*   POTEXTITEM               =
*   POEXPIMPITEM             =
*   POCOMPONENTS             =
*   POSHIPPINGEXP            =
*   POHISTORY                =
*   POHISTORY_TOTALS         =
*   POCONFIRMATION           =
*   ALLVERSIONS              =
*   POPARTNER                =
*   EXTENSIONOUT             =
            .
ENDFORM.                    "bapi_po_getdetail1
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ABPSP_OUTPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_abpsp_output USING in CHANGING out.    "#EC CALLED
  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input  = in
    IMPORTING
      output = out.
ENDFORM.                    "CONVERSION_EXIT_ABPSP_OUTPUT
