*----------------------------------------------------------------------*
***INCLUDE ZMM0040_CONTR_PRESU_LOG_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FC100  text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0100  CHANGING pt_fc TYPE lvc_t_fcat.
  FIELD-SYMBOLS <fc> LIKE LINE OF pt_fc.
  CLEAR: pt_fc.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZREG_LOG'
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
  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'ID_REFERENCIA'.
  IF sy-subrc EQ 0.
    <fc>-key    = 'X'.   "Campo llave
    <fc>-style  = 512. "Aparecera subrayado
  ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'AEDAT'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Fecha Creacion'."Ajusta titulo mostrado en Columna
  ENDIF.

  READ TABLE pt_fc ASSIGNING <fc> WITH KEY fieldname = 'CPUTM'.
  IF sy-subrc EQ 0.
    <fc>-coltext = 'Hora Creacion'."Ajusta titulo mostrado en Columna
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*&      Form  F_CRT_SRT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SRT100  text
*----------------------------------------------------------------------*
FORM f_crt_srt_0100  CHANGING pt_sort TYPE lvc_t_sort.

  PERFORM f_add_srt USING '1' 'AEDAT' x '' '' CHANGING pt_sort.
  PERFORM f_add_srt USING '2' 'CPUTM' x '' '' CHANGING pt_sort.

ENDFORM.                    " F_CRT_SRT_0100
*&---------------------------------------------------------------------*
*&      Form  F_ADD_SRT
*&---------------------------------------------------------------------*
FORM f_add_srt  USING    p_spos      TYPE slis_spos
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
  ls_srt-subtot    = p_subtot.
  APPEND ls_srt TO p_srt.
  CLEAR ls_srt.

ENDFORM.                    " F_ADD_SRT
*&---------------------------------------------------------------------*
*&      Form  F_CRT_LAYOT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYO100  text
*----------------------------------------------------------------------*
FORM f_crt_layot_0100  CHANGING p_layo TYPE lvc_s_layo.
  CLEAR p_layo.
  p_layo-no_rowmark = x.
  p_layo-cwidth_opt = x.

ENDFORM.                    " F_CRT_LAYOT_0100
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FC200  text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0200  CHANGING pt_fct TYPE lvc_t_fcat.
  FIELD-SYMBOLS <fct> LIKE LINE OF pt_fct.
  CLEAR: pt_fct.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZLOG_DET'
    CHANGING
      ct_fieldcat            = pt_fct
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE pt_fct WHERE fieldname = 'MANDT'.
  DELETE pt_fct WHERE fieldname = 'AEDAT'.
  DELETE pt_fct WHERE fieldname = 'INT2'.
  DELETE pt_fct WHERE fieldname = 'MOMNT'.
  DELETE pt_fct WHERE fieldname = 'ID_REFERENCIA'.

  READ TABLE pt_fct ASSIGNING <fct> WITH KEY fieldname = 'VAL_PARAM'.
  IF sy-subrc EQ 0.
    <fct>-coltext = 'Valor parámetro'.
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0200
