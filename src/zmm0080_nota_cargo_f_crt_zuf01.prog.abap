*----------------------------------------------------------------------*
***INCLUDE ZMM0080_NOTA_CARGO_F_CRT_ZUF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CRT_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_SUBRC  text
*----------------------------------------------------------------------*
 FORM f_crt_zutil_param.
   DATA: lwa_zutil TYPE zutil_parameters.

   SELECT * INTO TABLE it_zutil_parameters
   FROM zutil_parameters
   WHERE zreport = 'ZMM0080_NOTA_CARGO'.

   LOOP AT it_zutil_parameters INTO lwa_zutil  WHERE zfield CS 'DOC_TYPE'.
     lwa_doc_type-sign = 'I'.
     lwa_doc_type-option = 'EQ'.
     lwa_doc_type-low = lwa_zutil-zchar.
     APPEND lwa_doc_type TO lit_doc_type.
     CLEAR: lwa_doc_type, lwa_zutil.
   ENDLOOP.
   IF sy-subrc <> 0.
     v_subrc = 01.
     MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS'
     TYPE 'S' DISPLAY LIKE 'E'.
   ENDIF.
   CHECK v_subrc = 0.

   READ TABLE it_zutil_parameters INTO lwa_zutil WITH KEY zfield = 'PURC_ORG'.
   IF lwa_zutil-zchar = ' '.
     v_subrc = 01.
     MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS'
     TYPE 'S' DISPLAY LIKE 'E'.
   ELSEIF lwa_zutil-zchar IS NOT INITIAL.
     V_PURC_ORG = lwa_zutil-zchar.
   ELSE.
   ENDIF.
   CHECK V_SUBRC = 0.

 ENDFORM.                    " F_CRT_ZUTIL_PARAM
*&---------------------------------------------------------------------*
*&      Form  ALV_REUSE_CONTRL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CONTRL  text
*      <--P_IT_CAT  text
*----------------------------------------------------------------------*
form ALV_REUSE_CONTRL tables p_it_cntrl STRUCTURE it_contrl
                                        CHANGING p_it_cat TYPE slis_t_fieldcat_alv.

  PERFORM create_field_catalog.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*       I_INTERFACE_CHECK              = ' '
*       I_BYPASSING_BUFFER             =
*       I_BUFFER_ACTIVE                = ' '
        i_callback_program             = 'ZMM0080_NOTA_CARGO'
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

endform.                    " ALV_REUSE_CONTRL
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CREATE_FIELD_CATALOG .

   IF it_cat[] IS INITIAL.
    CLEAR wa_cat.
    wa_cat-col_pos   = 1.
    wa_cat-fieldname = 'EBELN'.
    wa_cat-datatype  = 'CHAR'.
    wa_cat-inttype   = 'C'.
    wa_cat-intlen    = 11.
    wa_cat-seltext_l   = 'Pedido'.
    wa_cat-seltext_m   = 'Pedido'.
    wa_cat-seltext_s   = 'Pedido'.
    APPEND wa_cat TO it_cat.

    CLEAR wa_cat.
    wa_cat-col_pos   = 2.
    wa_cat-fieldname = 'DOC_NO'.
    wa_cat-datatype  = 'CHAR'.
    wa_cat-inttype   = 'C'.
    wa_cat-intlen    = 11.
    wa_cat-seltext_l   = 'Num.  Nota de Cargo'.
    wa_cat-seltext_m   = 'Num.  Nota de Cargo'.
    wa_cat-seltext_s   = 'Num.  Nota de Cargo'.
    APPEND wa_cat TO it_cat.

    CLEAR wa_cat.
    wa_cat-col_pos   = 3.
    wa_cat-fieldname = 'FISC_YEAR'.
    wa_cat-datatype  = 'NUMC'.
    wa_cat-inttype   = 'N'.
    wa_cat-intlen    = 5.
    wa_cat-seltext_l   = 'Año Fiscal'.
    wa_cat-seltext_m   = 'Año Fiscal'.
    wa_cat-seltext_s   = 'Año Fiscal'.
    APPEND wa_cat TO it_cat.

    CLEAR wa_cat.
    wa_cat-col_pos   = 4.
    wa_cat-fieldname = 'BAPI_MSG'.
    wa_cat-datatype  = 'CHAR'.
    wa_cat-inttype   = 'C'.
    wa_cat-intlen    = 140.
    wa_cat-seltext_l   = 'Mensaje de Error'.
    wa_cat-seltext_m   = 'Mensaje de Error'.
    wa_cat-seltext_s   = 'Mensaje de Error'.
    APPEND wa_cat TO it_cat.

  ENDIF.

endform.                    " CREATE_FIELD_CATALOG
