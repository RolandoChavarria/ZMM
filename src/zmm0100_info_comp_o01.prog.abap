*&---------------------------------------------------------------------*
*&  Include           ZMM0100_INFO_COMP_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0100 output.
  SET PF-STATUS 'STS0100'.
  SET TITLEBAR 'TITLE0100' WITH V_TITLE.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0100H  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ALV_0100H output.

IF gr_alv100H IS INITIAL AND gr_cnt100H IS INITIAL.
    PERFORM f_crt_fcat_0100 CHANGING gt_fc100h.
    PERFORM f_crt_layot_0100 CHANGING gs_layo100H.

    CREATE OBJECT gr_cnt100H
      EXPORTING
        container_name = 'CC0100_H'.

    CREATE OBJECT gr_alv100H
      EXPORTING
        i_parent = gr_cnt100H.

    gr_alv100H->set_table_for_first_display(
     EXPORTING is_layout       = gs_layo100h
     CHANGING  it_outtab       = it_header
               it_fieldcatalog = gt_fc100h ).

    CREATE OBJECT gr_hnd100.
    SET HANDLER gr_hnd100->hand_dblclk FOR gr_alv100H."Manejo de doble click en ALV
*--> Inicio Modificación RSDK906898 - 7.
    SET HANDLER gr_hnd100->hand_tool_bar FOR gr_alv100H. "Manejo de Toolbar
    gr_alv100H->set_toolbar_interactive( ).
    SET HANDLER gr_hnd100->hand_cmd FOR gr_alv100H. "Manejo de Ucomm
*<-- Fin Modificación RSDK906898 - 7.
  ELSE.
    gr_alv100H->refresh_table_displaY( is_stable = gs_stbl ).
  ENDIF.

endmodule.                 " ALV_0100H  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FC100H  text
*----------------------------------------------------------------------*
form F_CRT_FCAT_0100  CHANGING pt_fc TYPE lvc_t_fcat.

 FIELD-SYMBOLS <fc> LIKE LINE OF pt_fc.
  CLEAR: pt_fc.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMWA_0100_HEADER_COMP'
    CHANGING
      ct_fieldcat            = pt_fc
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'PEDIDO'.
  if sy-subrc eq 0.
        <fc>-reptext = 'No.Pedido'.
        <fc>-scrtext_l = 'No.Pedido'.
        <fc>-scrtext_s = 'No.Pedido'.
        <fc>-scrtext_m = 'No.Pedido'.
      ENDIF.

 READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'DESC_PED'.
  if sy-subrc eq 0.
        <fc>-reptext = 'Descripcion del Pedido'.
        <fc>-scrtext_l = 'Descripcion del Pedido'.
        <fc>-scrtext_s = 'Descripcion del Pedido'.
        <fc>-scrtext_m = 'Descripcion del Pedido'.
      ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'NO_PROV'.
  if sy-subrc eq 0.
        <fc>-reptext = 'Proveedor'.
        <fc>-scrtext_l = 'Proveedor'.
        <fc>-scrtext_s = 'Proveedor'.
        <fc>-scrtext_m = 'Proveedor'.
      ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'DESC_PROV'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Descripcion de Proveedor'.
        <fc>-scrtext_l = 'Descripcion de Proveedor'.
        <fc>-scrtext_s = 'Descripcion de Proveedor'.
        <fc>-scrtext_m = 'Descripcion de Proveedor'.
      ENDIF.

   READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'CONTRATADO'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Contratado'.
        <fc>-scrtext_l = 'Contratado'.
        <fc>-scrtext_s = 'Contratado'.
        <fc>-scrtext_m = 'Contratado'.
      ENDIF.

   READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'FONDO_GARAN'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Tot FG'.
        <fc>-scrtext_l = 'Tot FG'.
        <fc>-scrtext_s = 'Tot FG'.
        <fc>-scrtext_m = 'Tot FG'.
      ENDIF.

   READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'TOTAL_AMORTIZAR'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Tot Amortiza '.
        <fc>-scrtext_l = 'Tot Amortiza'.
        <fc>-scrtext_s = 'Tot Amortiza'.
        <fc>-scrtext_m = 'Tot Amortiza'.
      ENDIF.

   READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'NETO_CONTRATADO'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Cont - Ret'.
        <fc>-scrtext_l = 'Cont - Ret'.
        <fc>-scrtext_s = 'Cont - Ret'.
        <fc>-scrtext_m = 'Cont - Ret'.
      ENDIF.

    READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'ANTICIPO'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Anticipo'.
        <fc>-scrtext_l = 'Anticipo'.
        <fc>-scrtext_s = 'Anticipo'.
        <fc>-scrtext_m = 'Anticipo'.
      ENDIF.

    READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'AMORT_ACUM'.
    if sy-subrc eq 0.
        <fc>-reptext = 'Amort Acum'.
        <fc>-scrtext_l = 'Amort Acum'.
        <fc>-scrtext_s = 'Amort Acum'.
        <fc>-scrtext_m = 'Amort Acum'.
      ENDIF.

     READ TABLE pt_fc ASSIGNING <fc> with KEY fieldname = 'AMORT_FG'.
    if sy-subrc eq 0.
        <fc>-reptext = 'FG Acum'.
        <fc>-scrtext_l = 'FG Acum'.
        <fc>-scrtext_s = 'FG Acum'.
        <fc>-scrtext_m = 'FG Acum'.
      ENDIF.

endform.                    " F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*&      Form  F_CRT_LAYOT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYO100  text
*----------------------------------------------------------------------*
form F_CRT_LAYOT_0100  CHANGING p_layo TYPE lvc_s_layo.

  CLEAR p_layo.
  p_layo-no_rowmark = x.
  p_layo-cwidth_opt = x.


endform.                    " F_CRT_LAYOT_0100
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CLEAR_OK output.

CLEAR OK_CODE.

endmodule.                 " CLEAR_OK  OUTPUT
