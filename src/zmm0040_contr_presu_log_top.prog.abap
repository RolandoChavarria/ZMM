*&---------------------------------------------------------------------*
*& Include ZMM0040_CONTR_PRESU_LOG_TOP                       Report ZMM0040_CONTR_PRESU_LOG
*&
*&---------------------------------------------------------------------*

REPORT  zmm0040_contr_presu_log.
TABLES: zreg_log.



DATA ok_code TYPE sy-ucomm.
DATA it_zreg_log TYPE TABLE OF zreg_log.
DATA:it_log_det TYPE TABLE OF zlog_det.
DATA gs_stbl TYPE lvc_s_stbl VALUE 'XX'.
DATA v_prgn  TYPE rs38l_fnam.
CONSTANTS: x TYPE c VALUE 'X'.

*DATA gs_stbl TYPE lvc_s_stbl VALUE 'XX'.

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

DATA wa_zreg_log LIKE LINE OF it_zreg_log.
DATA v_prg_name  TYPE c LENGTH 30.


SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: so_aedat FOR zreg_log-aedat,
                so_cputm FOR zreg_log-cputm,
                so_ernam FOR zreg_log-ernam.

SELECTION-SCREEN END OF BLOCK bloque1.
CHECK:so_aedat,
      so_cputm,
      so_ernam IS INITIAL.
IF sy-subrc = 0.
  MESSAGE 'OBLIGATORIO LLENAR POR LO MENOS UN CAMPO'
    TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.



SELECTION-SCREEN BEGIN OF BLOCK bloque2 WITH FRAME TITLE text-002.
PARAMETERS: prg_1 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_2 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
PARAMETERS: prg_3 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_4 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_5 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_6 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_7 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_8 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_9 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_10 RADIOBUTTON GROUP rad1.
PARAMETERS: prg_11 RADIOBUTTON GROUP rad1.

SELECTION-SCREEN END OF BLOCK bloque2.

INCLUDE zlo4220_mon_emb_cld.

DATA gr_hnd100  TYPE REF TO lcl_alv100.
