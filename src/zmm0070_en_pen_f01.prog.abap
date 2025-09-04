*----------------------------------------------------------------------*
***INCLUDE ZMM0070_EN_PEN_F_GET_ZUTIL_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZUTIL  text
*      -->P_0017   text
*      <--P_V_DOC_TYPE  text
*----------------------------------------------------------------------*
FORM f_get_zutil_param  TABLES   pit_zutil STRUCTURE zutil_parameters
                                   "Insertar nombre correcto para <...>
                        USING    p_field
                        CHANGING p_value
                                 p_subrc.

  DATA: lwa_zutil TYPE zutil_parameters.
  CLEAR p_subrc.

  READ TABLE pit_zutil INTO lwa_zutil WITH KEY zfield = p_field.
  IF lwa_zutil-zchar = ' '.
    p_subrc = 01.
    MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS'
    TYPE 'S' DISPLAY LIKE 'E'.
  ELSEIF lwa_zutil-zchar IS NOT INITIAL.
    p_value = lwa_zutil-zchar.
  ELSE.
  ENDIF.

ENDFORM.                    " F_GET_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*&      Form  F_GET_ZUTIL_PARAM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZUTIL  text
*      -->P_0076   text
*      <--P_V_MVT_IND  text
*      <--P_V_SUBRC  text
*      <--P_V_MSG  text
*----------------------------------------------------------------------*
FORM f_get_zutil_param2  TABLES   pit_zutil STRUCTURE zutil_parameters
                                   "Insertar nombre correcto para <...>
                        USING    p_field
                        CHANGING p_value
                                 p_subrc.

  DATA: lwa_zutil TYPE zutil_parameters.
  CLEAR p_subrc.

  READ TABLE pit_zutil INTO lwa_zutil WITH KEY zfield = p_field.
  IF lwa_zutil-zflag = ' '.
    p_subrc = 01.
    MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS'
     TYPE 'S' DISPLAY LIKE 'E'.
  ELSEIF lwa_zutil-zflag IS NOT INITIAL.
    p_value = lwa_zutil-zflag.
  ELSE.
  ENDIF.
ENDFORM.                    " F_GET_ZUTIL_PARAM2
*&---------------------------------------------------------------------*
*&      Form  F_CRT_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
FORM f_crt_zutil_param  CHANGING p_subrc.
  DATA: lwa_zutil TYPE zutil_parameters.

  SELECT * INTO TABLE it_zutil
  FROM zutil_parameters
  WHERE zreport = 'ZMM0070_EN_PEN'.

  LOOP AT it_zutil INTO lwa_zutil  WHERE zfield CS 'DOC_TYPE'.
    lwa_doc_type-sign = 'I'.
    lwa_doc_type-option = 'EQ'.
    lwa_doc_type-low = lwa_zutil-zchar.
    APPEND lwa_doc_type TO lit_doc_type.
    CLEAR: lwa_doc_type, lwa_zutil.
  ENDLOOP.
  IF sy-subrc <> 0.
    p_subrc = 01.
    MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS'
    TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  CHECK p_subrc = 0.

  PERFORM f_get_zutil_param  TABLES  it_zutil
                               USING 'GM_CODE'
                             CHANGING v_gm_code
                                      p_subrc.
  CHECK p_subrc = 0.

  PERFORM f_get_zutil_param  TABLES  it_zutil
                               USING 'BILL_OF_LADING'
                             CHANGING v_bill_lding
                                      p_subrc.
  CHECK p_subrc = 0.


  PERFORM f_get_zutil_param  TABLES  it_zutil
                               USING 'HEADER_TXT'
                             CHANGING v_hdr_txt
                                      p_subrc.
  CHECK p_subrc = 0.

  PERFORM f_get_zutil_param  TABLES  it_zutil
                               USING 'MOVE_TYPE'
                             CHANGING v_move_type
                                      p_subrc.
  CHECK p_subrc = 0.

  PERFORM f_get_zutil_param2  TABLES  it_zutil
                               USING 'MVT_IND'
                             CHANGING v_mvt_ind
                                      p_subrc.
  CHECK p_subrc = 0.

  PERFORM f_get_zutil_param2  TABLES  it_zutil
                               USING 'NO_MORE_GR'
                             CHANGING v_no_more_gr
                                      p_subrc.



ENDFORM.                    " F_CRT_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_field_catalog .

  IF it_cat[] IS INITIAL.
    CLEAR wa_cat.
    wa_cat-col_pos   = 1.
    wa_cat-fieldname = 'EBELN'.
    wa_cat-datatype  = 'CHAR'.
    wa_cat-inttype   = 'C'.
    wa_cat-intlen    = 10.
    wa_cat-seltext_l   = 'Pedido'.
    wa_cat-seltext_m   = 'Pedido'.
    wa_cat-seltext_s   = 'Pedido'.
    APPEND wa_cat TO it_cat.

    CLEAR wa_cat.
    wa_cat-col_pos   = 2.
    wa_cat-fieldname = 'MBLNR'.
    wa_cat-datatype  = 'CHAR'.
    wa_cat-inttype   = 'C'.
    wa_cat-intlen    = 10.
    wa_cat-seltext_l   = 'Núm. de documento'.
    wa_cat-seltext_m   = 'Núm. de documento'.
    wa_cat-seltext_s   = 'Núm. de documento'.
    APPEND wa_cat TO it_cat.

    CLEAR wa_cat.
    wa_cat-col_pos   = 3.
    wa_cat-fieldname = 'MJAHR'.
    wa_cat-datatype  = 'NUMC'.
    wa_cat-inttype   = 'N'.
    wa_cat-intlen    = 4.
    wa_cat-seltext_l   = 'Año Contable'.
    wa_cat-seltext_m   = 'Año Contable'.
    wa_cat-seltext_s   = 'Año Contable'.
    APPEND wa_cat TO it_cat.

    CLEAR wa_cat.
    wa_cat-col_pos   = 4.
    wa_cat-fieldname = 'BAPI_MSG'.
    wa_cat-datatype  = 'CHAR'.
    wa_cat-inttype   = 'C'.
    wa_cat-intlen    = 220.
    wa_cat-seltext_l   = 'Mensaje de Error'.
    wa_cat-seltext_m   = 'Mensaje de Error'.
    wa_cat-seltext_s   = 'Mensaje de Error'.
    APPEND wa_cat TO it_cat.

  ENDIF.

ENDFORM.                    " CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ALV_REUSE_CONTRL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CNTRL  text
*      -->P_IT_CAT  text
*      -->P_PERFORM  text
*      -->P_CREATE_FIELD_CATALOG  text
*----------------------------------------------------------------------*
FORM
   alv_reuse_contrl  TABLES   p_it_cntrl STRUCTURE it_contrl
                     CHANGING p_it_cat TYPE slis_t_fieldcat_alv.

  PERFORM create_field_catalog.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*       I_INTERFACE_CHECK              = ' '
*       I_BYPASSING_BUFFER             =
*       I_BUFFER_ACTIVE                = ' '
        i_callback_program             = 'ZMM0070_EN_PEN'
*       I_CALLBACK_PF_STATUS_SET       = ' '
*       I_CALLBACK_USER_COMMAND        = ' '
*       I_STRUCTURE_NAME               =
*       IS_LAYOUT                      =
        it_fieldcat                    = it_cat[]
*       IT_EXCLUDING                   =
*       IT_SPECIAL_GROUPS              =
*       IT_SORT                        =
*       IT_FILTER                      =
*       IS_SEL_HIDE                    =
        i_default                      = 'X'
        i_save                         = 'X'
*       IS_VARIANT                     =
*       IT_EVENTS                      =
*       IT_EVENT_EXIT                  =
*       IS_PRINT                       =
*       IS_REPREP_ID                   =
*       I_SCREEN_START_COLUMN          =
*       I_SCREEN_START_LINE            =
*       I_SCREEN_END_COLUMN            =
*       I_SCREEN_END_LINE              =
*       IR_SALV_LIST_ADAPTER           =
*       IT_EXCEPT_QINFO                =
*       I_SUPPRESS_EMPTY_DATA          =
*     IMPORTING
*       E_EXIT_CAUSED_BY_CALLER        =
*       ES_EXIT_CAUSED_BY_USER         =
    TABLES
        t_outtab                       = it_contrl
     EXCEPTIONS
        program_error                  = 1
        OTHERS                         = 2
            .

ENDFORM.                    " ALV_REUSE_CONTRL
