*----------------------------------------------------------------------*
***INCLUDE ZMM0050_MON_LOG_OC_F_CRT_FCF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_DET
*&---------------------------------------------------------------------*
FORM f_crt_fcat_det.
  FIELD-SYMBOLS <fc> LIKE LINE OF gt_dfcat.
  CLEAR: gt_dfcat.

  "Funcion recupera FCAT de la estructura del mismot tipo que la itab
  "Que vasmos a mostarr en el ALV
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMTT_DETAIL_LOG'
    CHANGING
      ct_fieldcat            = gt_dfcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE gt_dfcat WHERE fieldname = 'MANDT'. "No mostramos campo Mandante

  "Si se requiere mofificar alguna caracteristica en el FCAT (X Columna)
  READ TABLE gt_dfcat ASSIGNING <fc> WITH KEY fieldname = 'ID_CONTROL'.
  IF sy-subrc EQ 0.
    <fc>-key    = 'X'.   "Campo llave
    <fc>-style  = 512. "Aparecera subrayado
  ENDIF.

  READ TABLE gt_dfcat ASSIGNING <fc> WITH KEY fieldname = 'PEDIDO'.
  IF sy-subrc EQ 0.
    <fc>-style  = 512. "Aparecera subrayado
  ENDIF.

  READ TABLE gt_dfcat ASSIGNING <fc> WITH KEY fieldname = 'AEDAT'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Creado el'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE gt_dfcat ASSIGNING <fc> WITH KEY fieldname = 'CPUTM'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Hora Creacion'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE gt_dfcat ASSIGNING <fc> WITH KEY fieldname = 'HORA_CONF'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Hora Confirmacion'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE gt_dfcat ASSIGNING <fc> WITH KEY fieldname = 'FECHA_CONF'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Confirmado el'."Ajusta titulo mostrado en Columna
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_DET
*&---------------------------------------------------------------------*
*&      Form  F_SET_LAYOUT_DET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_set_layout_det .
  CLEAR gs_dlayo.

  gs_dlayo-cwidth_opt = 'X'."Para que ajuste el ancho optimo de columnas

ENDFORM.                    " F_SET_LAYOUT_DET

*&---------------------------------------------------------------------*
*&      Form  F_ADD_DYN_BDC
*&---------------------------------------------------------------------*
FORM f_add_dyn_bdc  TABLES pt_bdc    STRUCTURE bdcdata
                    USING  p_program TYPE bdc_prog
                           p_dynpro  TYPE bdc_dynr.
  CLEAR: pt_bdc. "Limpia Header Line

  pt_bdc-program  = p_program.
  pt_bdc-dynpro   = p_dynpro.
  pt_bdc-dynbegin = 'X'.
  APPEND pt_bdc.

ENDFORM.                    " F_ADD_DYN_BDC
*&---------------------------------------------------------------------*
*&      Form  F_ADD_FLD_BDC
*&---------------------------------------------------------------------*
FORM f_add_fld_bdc TABLES pt_bdc STRUCTURE bdcdata
                   USING  p_fnam TYPE	fnam_____4
                          p_fval.
  CLEAR: pt_bdc. "Limpia Header Line

  pt_bdc-fnam = p_fnam.
  pt_bdc-fval = p_fval.
  APPEND pt_bdc.

ENDFORM.                    " F_ADD_FLD_BDC
*&---------------------------------------------------------------------*
*&      Form  F_CALL_ME23
*&---------------------------------------------------------------------*
*     Call transaction a MM23 para ver un pedido
*----------------------------------------------------------------------*
FORM f_call_me23 USING p_pedido.
  DATA lt_bdc TYPE TABLE OF bdcdata.

  CHECK p_pedido IS NOT INITIAL.

  PERFORM f_add_dyn_bdc TABLES lt_bdc USING 'SAPMM06E' '0105'.
  PERFORM f_add_fld_bdc TABLES lt_bdc USING 'RM06E-BSTNR' p_pedido.
  PERFORM f_add_fld_bdc TABLES lt_bdc USING 'BDC_OKCODE' '/00'.

  PERFORM f_add_dyn_bdc TABLES lt_bdc USING 'SAPLMLSP' '0200'.
  PERFORM f_add_fld_bdc TABLES lt_bdc USING 'BDC_OKCODE' '=ESB'.

  CALL TRANSACTION 'ME23' USING lt_bdc MODE 'E'. "#EC CI_USAGE_OK[1803189]

ENDFORM.                    " F_CALL_ME23
