*&---------------------------------------------------------------------*
*&  Include           ZMM0110_CARGA_HIST_PROY_PRG
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arch.
  PERFORM f_f4_path USING p_arch.

AT SELECTION-SCREEN.
  PERFORM f_val_archivo.
  PERFORM f_zutil_parameters.
  PERFORM f_carga_archivo.
  IF git_log_error[] IS INITIAL.
    PERFORM f_val_datos.
    PERFORM f_alimenta_zmmtt_hist_part.
  ENDIF.
  PERFORM f_genera_log_errores.                  " F_GENERA_LOG_ERRORES
