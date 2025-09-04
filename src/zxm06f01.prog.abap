*----------------------------------------------------------------------*
***INCLUDE ZXM06F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_VAL_TP_DOC
*&---------------------------------------------------------------------*
*       Se valida si los indicadores de tipo de documento y tipo de
*       imputación corresponden con la configuración de zutilparameters
*       para el proyecto de PS
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  0 = OK, 4 = NOK
*----------------------------------------------------------------------*
FORM f_val_tp_doc  TABLES pit_xekpo STRUCTURE uekpo
                           CHANGING pv_subrc
                                    pv_pref_objnr.

  DATA: LIT_ZUTIL TYPE STANDARD TABLE OF ZUTIL_PARAMETERS.
  data: lwa_zutil like LINE OF lit_zutil.

  FIELD-SYMBOLS: <fs_xekpo> LIKE LINE OF pit_xekpo.
  CLEAR pv_subrc.

  SELECT * INTO TABLE lit_zutil
  FROM zutil_parameters
  WHERE zreport = 'EXIT_SAPMM06E_013'.
  IF sy-subrc NE 0.
    pv_subrc = 4.
    EXIT.
  ENDIF.

  READ TABLE pit_xekpo ASSIGNING <fs_xekpo> INDEX 1.
  IF sy-subrc EQ 0.
    READ TABLE lit_zutil TRANSPORTING NO FIELDS WITH KEY
                                              zfield = 'BSTYP'
                                              zchar  = <fs_xekpo>-bstyp.
    IF sy-subrc NE 0.
      pv_subrc = 4.
      EXIT.
    ENDIF.

    READ TABLE lit_zutil TRANSPORTING NO FIELDS WITH KEY
                                              zfield = 'KNTTP'
                                              zchar  = <fs_xekpo>-knttp.
    IF sy-subrc NE 0.
      pv_subrc = 4.
      EXIT.
    ENDIF.

  ELSE.
    pV_subrc = 4.
    EXIT.
  ENDIF.

* Se devuelve el prefijo para objnr
  READ TABLE lit_zutil INTO lwa_zutil with key zfield = 'PREFIJO_OBJNR'.
  IF sy-subrc eq 0.
    pv_pref_objnr = lwa_zutil-zchar.
  else.
    pv_subrc = 4.
  ENDIF.
ENDFORM.                    " F_VAL_TP_DOC
