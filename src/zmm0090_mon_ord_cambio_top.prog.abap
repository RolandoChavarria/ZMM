*&---------------------------------------------------------------------*
*& Include ZMM0090_MON_ORD_CAMBIO_TOP                        Report ZMM0090_MON_ORD_CAMBIO
*&
*&---------------------------------------------------------------------*

REPORT   zmm0090_mon_ord_cambio.

TABLES: zmmtt_0040_ch_or, proj, zmmwa0090_ch_or.
DATA it_ch_or TYPE TABLE OF zmmtt_0040_ch_or.
DATA it_ch_or2 TYPE TABLE OF zmmwa0090_ch_or.
DATA it_proj TYPE TABLE OF proj.
DATA it_ekko TYPE TABLE OF ekko.
DATA wa_ch_or LIKE LINE OF it_ch_or.
DATA wa_proj  LIKE LINE OF it_proj.
DATA wa_ch_or2 LIKE LINE OF it_ch_or2.
DATA wa_ekko LIKE LINE OF it_ekko.
DATA wa_ekko2 LIKE LINE OF it_ekko.
DATA ok_code TYPE sy-ucomm.
DATA  cc_0100.
CONSTANTS: x TYPE c VALUE 'X'.
DATA gs_stbl TYPE lvc_s_stbl VALUE 'XX'.
DATA V_CURR TYPE ZMMDE_IMPORTE_AUTOR.

* ----- 0100  -->
DATA: gr_alv100  TYPE REF TO cl_gui_alv_grid,
      gr_cnt100  TYPE REF TO cl_gui_custom_container,
      gt_fc100   TYPE lvc_t_fcat,
      gs_layo100 TYPE lvc_s_layo,
      gt_srt100  TYPE lvc_t_sort.

* ----- 0200  -->
DATA: gr_alv200  TYPE REF TO cl_gui_alv_grid,
      gr_cnt200  TYPE REF TO cl_gui_custom_container,
      gt_fc200   TYPE lvc_t_fcat,
      gs_layo200 TYPE lvc_s_layo,
      gt_srt200  TYPE lvc_t_sort.


SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: so_proy FOR proj-pspid NO-EXTENSION NO INTERVALS ,
                so_folio FOR zmmtt_0040_ch_or-folio NO-EXTENSION NO INTERVALS,
                so_fecha FOR zmmtt_0040_ch_or-fecha_ejec.
SELECTION-SCREEN END OF BLOCK bloque1.
CHECK:so_proy,
      so_folio IS INITIAL.
IF sy-subrc = 0.
  MESSAGE 'Es obligatio especificar un Proyecto o un Folio WEB'
    TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.

INCLUDE zlo0090_mon_emb_cld.
DATA gr_hnd100  TYPE REF TO lcl_alv100.
