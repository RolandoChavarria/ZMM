*&---------------------------------------------------------------------*
*& Report  ZMM0050_MON_LOG_OC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE zmm0050_mon_log_oc_top                  .    " global Data
INCLUDE zmm0050_mon_log_oc_status_0o01.
INCLUDE zmm0050_mon_log_oc_user_comi01.
INCLUDE zmm0050_mon_log_oc_f_crt_fcf01.
INCLUDE zmm0050_mon_log_oc_cli. "Implementacion de clases locales
* INCLUDE ZMM0050_MON_LOG_OC_O01                  .  " PBO-Modules
* INCLUDE ZMM0050_MON_LOG_OC_I01                  .  " PAI-Modules
* INCLUDE ZMM0050_MON_LOG_OC_F01                  .  " FORM-Routines

**********************************************************************
AT SELECTION-SCREEN. "Para validaciones en Selection Screen
* filtros para busqueda por header
  IF  so_stats[] IS INITIAL
  AND so_aedat[] IS INITIAL
  AND so_cputm[] IS INITIAL
  AND so_ernam[] IS INITIAL
  AND so_hr_cn[] IS INITIAL
  AND so_fch_c[] IS INITIAL
*Filtros para busqueda por detail
  AND so_ped[] IS INITIAL
  AND so_stadt[] IS INITIAL
  AND so_aedt[] IS INITIAL
  AND so_cpum[] IS INITIAL
  AND so_ernm[] IS INITIAL
  AND so_hr[] IS INITIAL
  AND so_fch[] IS INITIAL.

    MESSAGE 'Obligatorio Llenar por lo menos un campo'
        TYPE 'E'.
  ENDIF.
**********************************************************************
START-OF-SELECTION."Evento para recuperar info segun Selection Screen

  CLEAR: it_header_log, it_detail_log, it_hdr, it_dtl.

* Valida que al menos 1 de los filtros de cabecera tengan info
  IF so_stats[] IS NOT INITIAL OR
     so_aedat[] IS NOT INITIAL OR
     so_cputm[] IS NOT INITIAL OR
     so_ernam[] IS NOT INITIAL OR
     so_hr_cn[] IS NOT INITIAL OR
     so_fch_c[] IS NOT INITIAL.

* Si alguno no es inicial se recuperan cabeceras solicitados segun filtros
    SELECT * FROM zmmtt_header_log INTO TABLE it_header_log
    WHERE estatus IN so_stats
    AND   aedat   IN so_aedat
    AND   cputm   IN so_cputm
    AND   ernam   IN so_ernam
    AND   hora_conf IN so_hr_cn
    AND   fecha_conf IN so_fch_c.

    IF it_header_log[] IS NOT INITIAL.
*      Se recuperan detalles de los detalles recuperados.
      SELECT * FROM zmmtt_detail_log INTO TABLE it_detail_log
      FOR ALL ENTRIES IN it_header_log
       WHERE id_control EQ it_header_log-id_control
       AND pedido IN so_ped
       AND estatus IN so_stadt
       AND aedat IN so_aedt
       AND cputm IN so_cpum
       AND ernam IN so_ernm
       AND hora_conf IN so_hr
       AND fecha_conf IN so_fch.
    ENDIF.
  ENDIF.

* Valida que al menos 1 de los filtros de detalles tengan info
  IF so_ped[] IS NOT INITIAL OR
   so_stadt[] IS NOT INITIAL OR
   so_aedt[]  IS NOT INITIAL OR "No esta en el select
   so_cpum[]  IS NOT INITIAL OR
   so_ernm[]  IS NOT INITIAL OR
   so_hr[]    IS NOT INITIAL OR
   so_fch[]   IS NOT INITIAL.

*   Si alguno no es inicial se recuperan detalles solicitados segun filtros
    SELECT * FROM zmmtt_detail_log INTO TABLE it_dtl
    WHERE pedido  IN so_ped
    AND   estatus IN so_stadt
    AND   aedat   IN so_aedt
    AND   cputm   IN so_cpum
    AND   ernam   IN so_ernm
    AND   hora_conf IN so_hr
    AND   fecha_conf IN so_fch.

    IF it_dtl[] IS NOT INITIAL.
*      Se recuperan Cabeceras de los detalles recuperados.
      SELECT * FROM zmmtt_header_log INTO TABLE it_hdr
      FOR ALL ENTRIES IN it_detail_log
       WHERE id_control EQ it_detail_log-id_control
       AND estatus IN so_stats
       AND aedat IN so_aedat
       AND cputm IN so_cputm
       AND ernam IN so_ernam
       AND hora_conf IN so_hr_cn
       AND fecha_conf IN so_fch_c.
    ENDIF.

    "Se agregar registros de it_det1 en caso de haber encontrado
    IF it_dtl[] IS NOT INITIAL.
      APPEND LINES OF it_dtl TO it_detail_log.
      CLEAR it_dtl.
    ENDIF.

    "Se agregar registro de it_hdr en caso de haber encontrado
    IF it_hdr IS NOT INITIAL.
      APPEND LINES OF it_hdr TO it_header_log.
      CLEAR it_hdr.
    ENDIF.

  ENDIF.

  SORT it_header_log BY id_control.
  DELETE ADJACENT DUPLICATES FROM it_header_log COMPARING id_control.

  SORT it_detail_log BY id_control pedido.
  DELETE ADJACENT DUPLICATES FROM it_detail_log COMPARING id_control pedido.

  "Si se tiene info en IT_DETAIL_LOG Llamamos dynpro para mostrar ALV
  IF it_detail_log[] IS NOT INITIAL.
    CALL SCREEN 100.
  ELSE.
    MESSAGE 'Error: no se encontraron registros según filtros' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
