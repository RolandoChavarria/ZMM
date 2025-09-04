*&---------------------------------------------------------------------*
*& Include ZMM0120_CONS_HIST_PROY_TOP                        Modulpool        ZMM0120_CONS_HIST_PROY
*&
*&---------------------------------------------------------------------*

PROGRAM  zmm0120_cons_hist_proy.

TABLES: zmmtt_hist_part, zmmtt_log_hist_p.

DATA: git_hist_part TYPE TABLE OF zmmtt_hist_part.

DATA: BEGIN OF git_nivel_detalle OCCURS 0,
      proyecto TYPE zmmde_ps_pspid,
      elemento_pep TYPE zmmde_elemento_pep,
      proveedor	TYPE zmmde_lifnr,
      factura	TYPE zmmde_d_factura,
      desc_proyecto TYPE  zmmde_desc_pspid,
      clas_caratula TYPE  zmmde_clas_caratula,
      t_registro TYPE zmmde_tipo_registro,
      clas_partida TYPE zmmde_clas_partida,
      f_contabilizacion TYPE  zmmde_f_contabilizacion,
      n_proveedor TYPE  zmmde_n_proveedor,
      d_factura TYPE  zmmde_denominacion,
      t_cabecero TYPE zmmde_t_cabecero,
      importe TYPE  zmmde_importe,
      moneda TYPE  zmmde_moneda,
      res_factura TYPE zmmde_d_factura,
      END OF git_nivel_detalle.

DATA: BEGIN OF git_nivel_detalle2 OCCURS 0,
      proyecto TYPE zmmde_ps_pspid,
      elemento_pep TYPE zmmde_elemento_pep,
      proveedor	TYPE zmmde_lifnr,
      factura	TYPE zmmde_d_factura,
      desc_proyecto TYPE  zmmde_desc_pspid,
      clas_caratula TYPE  zmmde_clas_caratula,
      t_registro TYPE zmmde_tipo_registro,
      clas_partida TYPE zmmde_clas_partida,
      f_contabilizacion TYPE  zmmde_f_contabilizacion,
      n_proveedor TYPE  zmmde_n_proveedor,
      d_factura TYPE  zmmde_denominacion,
      t_cabecero TYPE zmmde_t_cabecero,
      importe TYPE  zmmde_importe,
      moneda TYPE  zmmde_moneda,
      res_factura TYPE zmmde_d_factura,
   END OF git_nivel_detalle2.

DATA: gwa_nivel_detalle LIKE LINE OF git_nivel_detalle.

DATA: git_log_hist_p TYPE TABLE OF zmmtt_log_hist_p.

DATA: gv_proyecto TYPE zmmtt_hist_part-desc_proyecto.

DATA: BEGIN OF git_t_e_pep OCCURS 0,
     post1 TYPE prps-post1,
     elemento_pep TYPE zmmtt_hist_part-elemento_pep,
     importe TYPE zmmtt_hist_part-importe,
     END OF git_t_e_pep.

DATA: BEGIN OF git_t_prov OCCURS 0,
     n_proveedor TYPE zmmtt_hist_part-n_proveedor,
     proveedor TYPE zmmtt_hist_part-proveedor,
     importe TYPE zmmtt_hist_part-importe,
     END OF git_t_prov.

DATA: BEGIN OF git_t_cc OCCURS 0,
     clas_caratula TYPE zmmtt_hist_part-clas_caratula,
     importe TYPE zmmtt_hist_part-importe,
     END OF git_t_cc.

DATA: BEGIN OF git_t_cp OCCURS 0,
      clas_partida TYPE zmmtt_hist_part-clas_partida,
      importe TYPE zmmtt_hist_part-importe,
      END OF git_t_cp.

DATA: ok_code TYPE sy-ucomm,
      ok_code2 TYPE sy-ucomm.

DATA: BEGIN OF git_proy OCCURS 0,
      proyecto TYPE zmmtt_hist_part-proyecto,
      desc_proyecto TYPE zmmtt_hist_part-desc_proyecto,
      END OF git_proy.

DATA: BEGIN OF git_e_pep OCCURS 0,
      proyecto TYPE zmmtt_hist_part-proyecto,
      elemento_pep TYPE zmmtt_hist_part-elemento_pep,
      post1 TYPE prps-post1,
      e_pep TYPE zmmtt_hist_part-elemento_pep,
      END OF git_e_pep.

DATA: BEGIN OF git_cp OCCURS 0,
      proyecto TYPE zmmtt_hist_part-proyecto,
      clas_partida TYPE zmmtt_hist_part-clas_partida,
      END OF git_cp.

DATA: BEGIN OF git_cc OCCURS 0,
      proyecto TYPE zmmtt_hist_part-proyecto,
      clas_caratula TYPE zmmtt_hist_part-clas_caratula,
      END OF git_cc.


DATA: BEGIN OF git_prov OCCURS 0,
      proyecto TYPE zmmtt_hist_part-proyecto,
      proveedor TYPE zmmtt_hist_part-proveedor,
      n_proveedor TYPE zmmtt_hist_part-n_proveedor,
      END OF git_prov.

DATA: BEGIN OF it_match_proy OCCURS 0,
         shlpname  LIKE ddshretval-shlpname,
         fieldname LIKE ddshretval-fieldname,
         recordpos LIKE ddshretval-recordpos,
         fieldval  LIKE ddshretval-fieldval,
         retfield  LIKE ddshretval-retfield,
     END OF it_match_proy.

DATA: BEGIN OF it_match_e_pep OCCURS 0,
         shlpname  LIKE ddshretval-shlpname,
         fieldname LIKE ddshretval-fieldname,
         recordpos LIKE ddshretval-recordpos,
         fieldval  LIKE ddshretval-fieldval,
         retfield  LIKE ddshretval-retfield,
     END OF it_match_e_pep.


DATA: BEGIN OF it_match_cp OCCURS 0,
         shlpname  LIKE ddshretval-shlpname,
         fieldname LIKE ddshretval-fieldname,
         recordpos LIKE ddshretval-recordpos,
         fieldval  LIKE ddshretval-fieldval,
         retfield  LIKE ddshretval-retfield,
     END OF it_match_cp.

DATA: BEGIN OF it_match_cc OCCURS 0,
         shlpname  LIKE ddshretval-shlpname,
         fieldname LIKE ddshretval-fieldname,
         recordpos LIKE ddshretval-recordpos,
         fieldval  LIKE ddshretval-fieldval,
         retfield  LIKE ddshretval-retfield,
     END OF it_match_cc.

DATA: BEGIN OF it_match_prov OCCURS 0,
         shlpname  LIKE ddshretval-shlpname,
         fieldname LIKE ddshretval-fieldname,
         recordpos LIKE ddshretval-recordpos,
         fieldval  LIKE ddshretval-fieldval,
         retfield  LIKE ddshretval-retfield,
     END OF it_match_prov.

DATA: gv_visual TYPE c LENGTH 1,
      gv_edicion TYPE c LENGTH 1,
      gv_edit TYPE c LENGTH 1.

SELECT-OPTIONS: so_proy FOR zmmtt_hist_part-proyecto NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS: so_e_pep FOR zmmtt_hist_part-elemento_pep.
SELECT-OPTIONS: so_cp FOR zmmtt_hist_part-clas_partida.
SELECT-OPTIONS: so_cc FOR zmmtt_hist_part-clas_caratula.
SELECT-OPTIONS: so_prov FOR zmmtt_hist_part-proveedor.

CONSTANTS: x TYPE c VALUE 'X'.
DATA gs_stbl TYPE lvc_s_stbl VALUE 'XX'.

INCLUDE zmm_0120_cons_hist_proy_cld.

* ----- 0100_1 ALV  -->
DATA: gr_alv100_1  TYPE REF TO cl_gui_alv_grid,
      gr_alv100dc_1  TYPE REF TO lcl_alv100dc_1,
      gr_cnt100_1  TYPE REF TO cl_gui_custom_container,
      gt_fc100_1   TYPE lvc_t_fcat,
      gwa_fc100_1 LIKE LINE OF gt_fc100_1,
      gs_layo100_1 TYPE lvc_s_layo.

* ----- 0100_2 ALV  -->
DATA: gr_alv100_2  TYPE REF TO cl_gui_alv_grid,
      gr_alv100dc_2  TYPE REF TO lcl_alv100dc_2,
      gr_cnt100_2  TYPE REF TO cl_gui_custom_container,
      gt_fc100_2   TYPE lvc_t_fcat,
      gwa_fc100_2 LIKE LINE OF gt_fc100_2,
      gs_layo100_2 TYPE lvc_s_layo.

* ----- 0100_3 ALV  -->
DATA: gr_alv100_3  TYPE REF TO cl_gui_alv_grid,
      gr_alv100dc_3  TYPE REF TO lcl_alv100dc_3,
      gr_cnt100_3  TYPE REF TO cl_gui_custom_container,
      gt_fc100_3   TYPE lvc_t_fcat,
      gwa_fc100_3 LIKE LINE OF gt_fc100_3,
      gs_layo100_3 TYPE lvc_s_layo.

* ----- 0100_4 ALV  -->
DATA: gr_alv100_4  TYPE REF TO cl_gui_alv_grid,
      gr_alv100dc_4  TYPE REF TO lcl_alv100dc_4,
      gr_cnt100_4  TYPE REF TO cl_gui_custom_container,
      gt_fc100_4   TYPE lvc_t_fcat,
      gwa_fc100_4 LIKE LINE OF gt_fc100_4,
      gs_layo100_4 TYPE lvc_s_layo.

* ----- 0200 ALV  -->
DATA: gr_alv200  TYPE REF TO cl_gui_alv_grid,
      gr_cnt200  TYPE REF TO cl_gui_custom_container,
      gt_fc200   TYPE lvc_t_fcat,
      gwa_fc200 LIKE LINE OF gt_fc200,
      gs_layo200 TYPE lvc_s_layo.

* >>> RSDK907480 >>>
TYPES: BEGIN OF gty_wa_proy,
       proyecto TYPE zmmde_ps_pspid,
       END OF gty_wa_proy.
DATA: git_bproy TYPE TABLE OF gty_wa_proy."Proyectos Bloq. p/Edic.
* <<< RSDK907480 <<<
