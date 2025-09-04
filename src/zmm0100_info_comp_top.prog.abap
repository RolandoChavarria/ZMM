*&---------------------------------------------------------------------*
*& Include ZMM0100_INFO_COMP_TOP                             Report ZMM0100_INFO_COMP
*&
*&---------------------------------------------------------------------*

REPORT   zmm0100_info_comp.

TABLES: proj,
        lfm1,
        lfa1.

*--> Inicia Modificación RSDK906898 - 8.
TYPES: BEGIN OF ty_header,
             pedido          TYPE string,
             desc_ped        TYPE string,
             no_prov         TYPE string,
             desc_prov       TYPE string,
             moneda          TYPE string,
             contratado      TYPE string,
             fondo_garan     TYPE string,
             total_amortizar TYPE string,
             neto_contratado TYPE string,
             anticipo        TYPE string,
             amort_acum      TYPE string,
             amort_fg        TYPE string,
         END OF ty_header,

         BEGIN OF ty_detalle,
               bldat    TYPE string,
               xblnr    TYPE string,
               srvpos   TYPE string,
               asktx    TYPE string,
               menge    TYPE string,
               g_dmbtr  TYPE string,
               r_dmbtr  TYPE string,
               a_dmbtr  TYPE string,
               rf_dmbtr TYPE string,
               vf_belnr TYPE string,
               dc_belnr TYPE string,
               fch_ver  TYPE string,
               dmbtr    TYPE string,
               mwskz    TYPE string,
               total    TYPE string,
           END OF ty_detalle.

DATA: git_header  TYPE STANDARD TABLE OF ty_header  WITH HEADER LINE,
      git_detalle TYPE STANDARD TABLE OF ty_detalle WITH HEADER LINE,
      gv_moneda   TYPE waers,
      gv_ebeln    TYPE ebeln,
      gv_lifnr    TYPE zmmde_lifnr.

INCLUDE ole2incl.

*Para crear excel
DATA: application TYPE ole2_object,
      workbook    TYPE ole2_object,
      sheet       TYPE ole2_object,
      cells       TYPE ole2_object,
      int         TYPE ole2_object,
      font        TYPE ole2_object,
      we          TYPE ole2_object,
      borders     TYPE ole2_object.
*<-- Fin Modificación RSDK906898 - 8.

DATA: BEGIN OF it_lifnr OCCURS 0,
        lifnr TYPE lifnr,
        name1 TYPE name1_gp,
        ort01 TYPE ort01_gp,
        regio TYPE regio,
        END OF it_lifnr.

DATA: BEGIN OF it_ebeln OCCURS 0,
        ebeln TYPE ebeln,
        bukrs TYPE bukrs,
        bsart TYPE bsart,
        END OF it_ebeln.

DATA: BEGIN OF it_match OCCURS 0,
               shlpname  LIKE ddshretval-shlpname,
               fieldname LIKE ddshretval-fieldname,
               recordpos LIKE ddshretval-recordpos,
               fieldval  LIKE ddshretval-fieldval,
               retfield  LIKE ddshretval-retfield,
           END OF it_match.

DATA: BEGIN OF it_match_p OCCURS 0,
               shlpname  LIKE ddshretval-shlpname,
               fieldname LIKE ddshretval-fieldname,
               recordpos LIKE ddshretval-recordpos,
               fieldval  LIKE ddshretval-fieldval,
               retfield  LIKE ddshretval-retfield,
           END OF it_match_p.

DATA: BEGIN OF it_lfa1 OCCURS 0,
               lifnr TYPE lifnr,
               name1 TYPE name1,
        END OF it_lfa1.

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
      bldat TYPE bldat,
      END OF it_bkpf.

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

DATA: BEGIN OF it_konv OCCURS 0,
  knumh TYPE knumh,
  mwsk1 TYPE mwskz,
  kschl TYPE kschl,
  END OF it_konv.

DATA: BEGIN OF it_konp OCCURS 0,
  knumh TYPE knumh,
  kschl TYPE kschl,
  kbetr TYPE kbetr,
  END OF it_konp.


DATA: lit_doc_type TYPE wrf_pbas_esart_rtty. "#EC CI_USAGE_OK[2368913]
DATA  lwa_doc_type LIKE LINE OF lit_doc_type.
DATA: git_pspidebeln TYPE STANDARD TABLE OF zmmwa_pspidebelnlifnr .
DATA: it_header TYPE TABLE OF zmmwa_0100_header_comp.
DATA: it_detail TYPE TABLE OF zmmwa_0100_detail_comp.
DATA: lit_zutil TYPE TABLE OF zutil_parameters.
DATA: lwa_zutil LIKE LINE OF lit_zutil.
DATA  wa_header LIKE LINE OF it_header.
DATA: v_desc_ped TYPE bapi_msg.
DATA: v_title TYPE bapi_msg.
DATA: v_sociedad TYPE c LENGTH 40.
DATA: ok_code TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME TITLE text-001.

PARAMETERS  p_proyec TYPE proj-pspid OBLIGATORY.
PARAMETERS  p_provdr  TYPE lfm1-lifnr OBLIGATORY.
PARAMETERS  p_pedido  TYPE ekko-ebeln.

SELECTION-SCREEN END OF BLOCK bloque1.

* ----- 0100 ALV HEADER  -->
DATA: gr_alv100h  TYPE REF TO cl_gui_alv_grid,
      gr_cnt100h  TYPE REF TO cl_gui_custom_container,
      gt_fc100h   TYPE lvc_t_fcat,
      gs_layo100h TYPE lvc_s_layo.

* ----- 0100 ALV DETAIL -->
DATA: gr_alv100d  TYPE REF TO cl_gui_alv_grid,
      gr_cnt100d  TYPE REF TO cl_gui_custom_container,
      gt_fc100d   TYPE lvc_t_fcat,
      gs_layo100d TYPE lvc_s_layo,
      gt_srt100d  TYPE lvc_t_sort.


INCLUDE zlo4220_mon_emb_cld.


DATA gr_hnd100  TYPE REF TO lcl_alv100.
DATA gs_stbl TYPE lvc_s_stbl VALUE 'XX'.
CONSTANTS: x TYPE c VALUE 'X'.
DATA:it_detail_p TYPE TABLE OF zmmwa_0100_detail_comp.
