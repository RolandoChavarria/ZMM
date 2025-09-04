*&---------------------------------------------------------------------*
*& Include ZMM0110_CARGA_HIST_PROY_TOP                       Report ZMM0110_CARGA_HIST_PROY_XLS
*&
*&---------------------------------------------------------------------*

REPORT   zmm0110_carga_hist_proy_xls.

DATA: git_hist_part TYPE TABLE OF zmmwa_hist_part,
      git_hist_part2 TYPE TABLE OF zmmtt_hist_part.

DATA: BEGIN OF git_log_error OCCURS 0,
      proyecto TYPE c LENGTH 24,
      elemento_pep TYPE c LENGTH 24,
      proveedor TYPE c LENGTH 10,
      factura TYPE c LENGTH 50,
      mensaje_error TYPE c LENGTH 100,
      END OF git_log_error.

DATA: git_zutil TYPE TABLE OF zutil_parameters,
      gwa_zutil LIKE LINE OF git_zutil,
      gv_begin_col TYPE i,
      gv_begin_row TYPE i,
      gv_end_col TYPE i,
      gv_end_row TYPE i.


* ----- 0100 ALV  -->
DATA: gr_alv100  TYPE REF TO cl_gui_alv_grid,
      gr_cnt100  TYPE REF TO cl_gui_custom_container,
      gt_fc100   TYPE lvc_t_fcat,
      gwa_fc100 LIKE LINE OF gt_fc100,
      gs_layo100 TYPE lvc_s_layo.

CONSTANTS: x TYPE c VALUE 'X'.
DATA gs_stbl TYPE lvc_s_stbl VALUE 'XX'.

PARAMETERS: p_arch LIKE rlgrap-filename .

DATA: ok_code TYPE sy-ucomm.
