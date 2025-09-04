*&---------------------------------------------------------------------*
*  Include           ZMM_0070_EN_PENTOP.
*&---------------------------------------------------------------------*
REPORT  zmm_0070_en_pentop.

DATA it_zutil TYPE TABLE OF zutil_parameters.

DATA: BEGIN OF it_ekko OCCURS 0,
      ebeln TYPE ebeln,
      bsart TYPE bsart,
      lifnr TYPE lifnr,
      END OF it_ekko.

DATA: BEGIN OF it_ekpo OCCURS 0,
      ebeln TYPE ebeln,
      ebelp TYPE ebelp,
      matnr TYPE matnr,
      werks TYPE werks_d,
      menge TYPE menge_d,
      meins TYPE meins,
      END OF it_ekpo.

DATA: BEGIN OF it_contrl OCCURS 0,
      ebeln TYPE ebeln,
      mblnr TYPE mblnr,
      mjahr TYPE mjahr,
      bapi_msg TYPE bapi_msg,
      END OF it_contrl.

      DATA: lit_doc_type TYPE wrf_pbas_esart_rtty. "#EC CI_USAGE_OK[2368913]
      DATA: lwa_doc_type LIKE LINE OF lit_doc_type.

DATA: v_doc_type   TYPE zbsart,
      v_gm_code    TYPE gm_code,
      v_bill_lding TYPE frbnr,
      v_hdr_txt    TYPE bktxt,
      v_move_type  TYPE bwart,
      v_mvt_ind    TYPE kzbew,
      v_no_more_gr TYPE elikz,
      v_subrc      TYPE  subrc,
      v_msg        TYPE bapi_msg.

TABLES: bapi2017_gm_head_01,
        bapi2017_gm_code,
        bapi2017_gm_head_ret.

DATA: t_items TYPE STANDARD TABLE OF bapi2017_gm_item_create
                                            INITIAL SIZE 10
                                            WITH HEADER LINE.

DATA: t_serial     TYPE STANDARD TABLE OF bapi2017_gm_serialnumber
                                                      INITIAL SIZE 10
                                                      WITH HEADER LINE.

DATA: t_return     TYPE STANDARD TABLE OF bapiret2   INITIAL SIZE 10
                                                    WITH HEADER LINE.

TYPES: BEGIN OF ty_po_det,
   matnr TYPE mara-matnr,
   menge TYPE ekpo-menge,
END OF ty_po_det.

DATA: p_msger    TYPE bapi_msg,
      p_mblnr_em TYPE belnr ,
      p_mjahr_em TYPE gjahr,
      p_subrc    TYPE sy-subrc.

TYPE-POOLS: slis.

DATA: it_cat TYPE slis_t_fieldcat_alv,
      wa_cat TYPE slis_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME.
PARAMETERS  i_date TYPE datum.
SELECTION-SCREEN END OF BLOCK bloque1.
