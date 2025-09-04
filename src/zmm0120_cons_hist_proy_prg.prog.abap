*&---------------------------------------------------------------------*
*&  Include           ZMM0120_CONS_HIST_PROY_PRG
*&---------------------------------------------------------------------*

INITIALIZATION.

  PERFORM f_authority_check.
  PERFORM f_obt_proy_bloq. " <<< RSDK907480 <<<

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_proy-low.
  PERFORM f_f4_proy CHANGING so_proy-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_proy-high.
  PERFORM f_f4_proy CHANGING so_proy-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_e_pep-low.
  PERFORM f_f4_e_pep CHANGING so_e_pep-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_e_pep-high.
  PERFORM f_f4_e_pep CHANGING so_e_pep-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_cp-low.
  PERFORM f_f4_cp CHANGING so_cp-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_cp-high.
  PERFORM f_f4_cp CHANGING so_cp-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_cc-low.
  PERFORM f_f4_cc CHANGING so_cc-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_cc-high.
  PERFORM f_f4_cc CHANGING so_cc-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_prov-low.
  PERFORM f_f4_proveedor CHANGING so_prov-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_prov-high.
  PERFORM f_f4_proveedor CHANGING so_prov-high.

START-OF-SELECTION.

  IF so_proy IS NOT INITIAL.

    SELECT SINGLE desc_proyecto INTO gv_proyecto FROM zmmtt_hist_part
    WHERE proyecto IN so_proy.

    CALL SCREEN 0100.
  ELSE.
    IF gv_edicion IS NOT INITIAL
    OR gv_visual  IS NOT INITIAL.
      MESSAGE text-002 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
