*&---------------------------------------------------------------------*
*&  Include           ZMM0100_INFO_COMP_PRG
*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN.
INITIALIZATION.

 perform f_get_zutil_parameters.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_provdr.
  PERFORM f_create_matchcode.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pedido.
  perform f_create_matchcode_p.

AT SELECTION-SCREEN ON p_proyec.
  perform f_validate_prov_proy.

AT SELECTION-SCREEN ON p_provdr.
  perform f_validate_prov_proy2.

START-OF-SELECTION.

  IF p_pedido IS INITIAL.
    PERFORM f_get_ebeln_from_pspid TABLES git_pspidebeln
                                    USING p_proyec
                                          p_provdr.
  ELSE.
    PERFORM f_validate_ebeln_from_pspid TABLES git_pspidebeln
                                        USING p_proyec
                                              p_provdr
                                              p_pedido.
  ENDIF.

  PERFORM f_get_info TABLES git_pspidebeln
                            it_header
                            it_detail.
