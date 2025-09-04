*&---------------------------------------------------------------------*
*& Include ZMM0050_MON_LOG_OC_TOP                            Report ZMM0050_MON_LOG_OC
*&
*&---------------------------------------------------------------------*

REPORT   zmm0050_mon_log_oc.

TABLES: zmmtt_header_log, zmmtt_detail_log, ekko.
DATA it_header_log TYPE TABLE OF zmmtt_header_log.
DATA it_detail_log TYPE TABLE OF zmmtt_detail_log.
DATA it_hdr  TYPE TABLE OF zmmtt_header_log.
DATA it_dtl  TYPE TABLE OF zmmtt_detail_log.

DATA wa_header_log TYPE zmmtt_header_log.
DATA: BEGIN OF git_id OCCURS 0,
        id_control TYPE zid_referencia,
        END OF git_id.
DATA: ok_code TYPE sy-ucomm.

DATA: gr_dalv   TYPE REF TO cl_gui_alv_grid,
      gr_dcnt   TYPE REF TO cl_gui_custom_container,
      gt_dfcat  TYPE lvc_t_fcat,
      gs_dlayo  TYPE lvc_s_layo,
      gs_stbl TYPE lvc_s_stbl VALUE 'XX'.

DATA gv_dynnr TYPE sy-dynnr.

SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: so_stats FOR zmmtt_header_log-estatus,
                so_aedat FOR zmmtt_header_log-aedat,
                so_cputm FOR zmmtt_header_log-cputm,
                so_ernam FOR zmmtt_header_log-ernam,
                so_hr_cn FOR zmmtt_header_log-hora_conf,
                so_fch_c FOR zmmtt_header_log-fecha_conf.
SELECTION-SCREEN END OF BLOCK bloque1.

SELECTION-SCREEN BEGIN OF BLOCK bloque2 WITH FRAME TITLE text-002.

SELECT-OPTIONS: so_ped   FOR ekko-ebeln,
                so_stadt FOR zmmtt_detail_log-estatus,
                so_aedt  FOR zmmtt_detail_log-aedat,
                so_cpum  FOR zmmtt_detail_log-cputm,
                so_ernm  FOR zmmtt_detail_log-ernam,
                so_hr    FOR zmmtt_detail_log-hora_conf,
                so_fch   FOR zmmtt_detail_log-fecha_conf.
SELECTION-SCREEN END OF BLOCK bloque2.



INCLUDE zmm0050_mon_log_oc_cld.

DATA gr_hand_alv TYPE REF TO lcl_hand_0100.
