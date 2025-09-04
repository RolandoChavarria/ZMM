*&---------------------------------------------------------------------*
*&  Include           ZMM0090_MON_ORD_CAMBIO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FC100  text
*----------------------------------------------------------------------*
form F_CRT_FCAT_0100  CHANGING pt_fc TYPE lvc_t_fcat.
  FIELD-SYMBOLS <fc> LIKE LINE OF pt_fc.
  CLEAR: pt_fc.

CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMWA0090_CH_OR'
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

 "Si se requiere mofificar alguna caracteristica en el FCAT (X Columna)
  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'ID_PROYECTO'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'ID Proyecto'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'DESC_PROY'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Desc. Proyecto'."Ajusta titulo mostrado en Columna
  ENDIF.


  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'FOLIO'.
  IF sy-subrc EQ 0.
    <fc>-key    = 'X'.   "Campo llave
    <fc>-style  = 512. "Aparecera subrayado
      <fc>-coltext = 'No. Orden de Cambio WEB'.
   endif.

  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Proveedor'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'NOM_PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Razon Social'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'FECHA'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Fecha de Registro'."Ajusta titulo mostrado en Columna
  ENDIF.


  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'DECREMENTAL'.
  IF sy-subrc EQ 0.
    <fc>-key    = 'X'.   "Campo llave
    <fc>-style  = 512. "Aparecera subrayado
    <fc>-coltext = 'No. Orden Decremental'.
  ENDIF.

    READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'INCREMENTAL'.
  IF sy-subrc EQ 0.
    <fc>-key    = 'X'.   "Campo llave
    <fc>-style  = 512. "Aparecera subrayado
    <fc>-coltext = 'No. Orden Incremental'.
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
*&      Form  F_CRT_SRT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SRT100  text
*----------------------------------------------------------------------*
form F_CRT_SRT_0100   CHANGING pt_sort TYPE lvc_t_sort.

  PERFORM f_add_srt USING '1' 'FECHA' x '' '' CHANGING pt_sort.
  PERFORM f_add_srt USING '2' 'FOLIO' x '' '' CHANGING pt_sort.

endform.                    " F_CRT_SRT_0100
*&---------------------------------------------------------------------*
*&      Form  F_ADD_SRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0153   text
*      -->P_0154   text
*      -->P_X  text
*      -->P_0156   text
*      -->P_0157   text
*      <--P_PT_SORT  text
*----------------------------------------------------------------------*
form F_ADD_SRT   USING    p_spos      TYPE slis_spos
                         p_fieldname TYPE lvc_fname
                         p_up        TYPE char1 "Ord de menor a mayor
                         p_down      TYPE char1 "Or de mayor a menor
                         p_subtot    TYPE slis_dosub "Subtotales
                CHANGING p_srt       TYPE lvc_t_sort.
  DATA ls_srt TYPE lvc_s_sort.

  ls_srt-spos = p_spos.
  ls_srt-fieldname = p_fieldname.
  ls_srt-up        = p_up.
  ls_srt-down      = p_down.
*  ls_srt-subtot    = p_subtot.
  APPEND ls_srt TO p_srt.
  CLEAR ls_srt.

endform.                    " F_ADD_SRT

FORM f_call_me23 USING p_pedido.
  DATA lt_bdc TYPE TABLE OF bdcdata.

  CHECK p_pedido IS NOT INITIAL.

  PERFORM f_add_dyn_bdc TABLES lt_bdc USING 'SAPMM06E' '0105'.
  PERFORM f_add_fld_bdc TABLES lt_bdc USING 'RM06E-BSTNR' p_pedido.
  PERFORM f_add_fld_bdc TABLES lt_bdc USING 'BDC_OKCODE' '/00'.

  PERFORM f_add_dyn_bdc TABLES lt_bdc USING 'SAPLMLSP' '0200'.
  PERFORM f_add_fld_bdc TABLES lt_bdc USING 'BDC_OKCODE' '=ESB'.

  CALL TRANSACTION 'ME23' USING lt_bdc MODE 'E'. "#EC CI_USAGE_OK[1803189]

ENDFORM.

                   " F_CRT_FCAT_0200
*&---------------------------------------------------------------------*
*&      Form  F_ADD_DYN_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BDC  text
*      -->P_0323   text
*      -->P_0324   text
*----------------------------------------------------------------------*
form F_ADD_DYN_BDC  TABLES pt_bdc    STRUCTURE bdcdata
                    USING  p_program TYPE bdc_prog
                           p_dynpro  TYPE bdc_dynr.
  CLEAR: pt_bdc. "Limpia Header Line

  pt_bdc-program  = p_program.
  pt_bdc-dynpro   = p_dynpro.
  pt_bdc-dynbegin = 'X'.
  APPEND pt_bdc.

endform.                    " F_ADD_DYN_BDC
*&---------------------------------------------------------------------*
*&      Form  F_ADD_FLD_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BDC  text
*      -->P_0330   text
*      -->P_P_PEDIDO  text
*----------------------------------------------------------------------*
form F_ADD_FLD_BDC  TABLES pt_bdc STRUCTURE bdcdata
                   USING  p_fnam TYPE	fnam_____4
                          p_fval.
  CLEAR: pt_bdc. "Limpia Header Line

  pt_bdc-fnam = p_fnam.
  pt_bdc-fval = p_fval.
  APPEND pt_bdc.

endform.                    " F_ADD_FLD_BDC
