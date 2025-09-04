*&---------------------------------------------------------------------*
*& Include ZMM0080_NOTA_CARGO_TOP                            Report ZMM0080_NOTA_CARGO
*&
*&---------------------------------------------------------------------*

REPORT   zmm0080_nota_cargo.

DATA: it_zutil_parameters TYPE TABLE OF zutil_parameters,
      it_ekbe  TYPE ekbe OCCURS 0 WITH HEADER LINE,
      lit_doc_type TYPE wrf_pbas_esart_rtty, "#EC CI_USAGE_OK[2368913]
      it_ekko TYPE ekko OCCURS 0 WITH HEADER LINE,
      it_headerdata TYPE bapi_incinv_create_header,
      it_addition_hd TYPE	bapi_incinv_save_header_backgr,
      it_refdoc TYPE  bapi_incinv_fld,
      it_addressdata  TYPE  bapi_incinv_create_addressdata,
      it_TAXDATA  type standard table of  BAPI_INCINV_CREATE_TAX,
      it_selectpo TYPE STANDARD TABLE OF  bapi_incinv_select_po,
      it_lfa1 TYPE lfa1 OCCURS 0 WITH HEADER LINE,
      it_ekpo TYPE ekpo OCCURS 0 WITH HEADER LINE,
      it_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME.
PARAMETERS  i_date TYPE datum.
SELECTION-SCREEN END OF BLOCK bloque1.

DATA: BEGIN OF it_contrl OCCURS 0,
      ebeln TYPE ebeln,
      doc_no TYPE re_belnr,
      fisc_year TYPE gjahr,
      bapi_msg TYPE bapi_msg,
      END OF it_contrl.

DATA: lwa_doc_type LIKE LINE OF lit_doc_type,
      wa_selectpo LIKE LINE OF it_selectpo,
      WA_TAXDATA LIKE LINE OF IT_TAXDATA.

DATA  lit_param TYPE STANDARD TABLE OF rsparams.
DATA lwa_param  TYPE rsparams.
DATA: lv_bukrs TYPE bukrs.
DATA: v_doc_no    TYPE bapi_incinv_fld-inv_doc_no.
DATA: v_cont_year TYPE bapi_incinv_fld-fisc_year.
DATA: v_effwr  TYPE effwr.

TYPE-POOLS: slis.

DATA: it_cat TYPE slis_t_fieldcat_alv,
      wa_cat TYPE slis_fieldcat_alv.

TABLES: bapi_incinv_fld.

DATA: v_subrc TYPE subrc,
      v_msger    TYPE bapi_msg.

DATA: v_purc_org TYPE n LENGTH 4.
