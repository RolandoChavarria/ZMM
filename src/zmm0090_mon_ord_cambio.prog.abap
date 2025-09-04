*&---------------------------------------------------------------------*
*& Report  ZMM0090_MON_ORD_CAMBIO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

 INCLUDE zmm0090_mon_ord_cambio_top              .    " global Data.
 INCLUDE zmm0090_mon_ord_cambio_o01              .  " PBO-Modules
 INCLUDE zmm0090_mon_ord_cambio_i01              .  " PAI-Modules
 INCLUDE zlo0090_mon_emb_cli.

 START-OF-SELECTION.

   SELECT * FROM zmmtt_0040_ch_or INTO TABLE it_ch_or
    WHERE  proyecto IN so_proy
      AND  folio  IN so_folio
      AND  fecha_ejec IN so_fecha.

   SELECT * FROM proj INTO TABLE it_proj
     WHERE pspid IN so_proy.

   IF it_ch_or IS NOT INITIAL.
     SELECT * FROM ekko INTO TABLE it_ekko
       WHERE unsez IN so_folio
       AND ( bsart = 'ZOCI' OR bsart = 'ZOCD' ) .
   ENDIF.

   LOOP AT it_ch_or INTO wa_ch_or
    WHERE proyecto IS NOT INITIAL.
     READ TABLE it_proj INTO wa_proj WITH KEY pspid = wa_ch_or-proyecto.
     LOOP AT it_ekko INTO wa_ekko WHERE unsez = wa_ch_or-folio.
       IF wa_ekko-bsart = 'ZOCI'.
         wa_ch_or2-incremental   = wa_ekko-ebeln.
       ELSEIF wa_ekko-bsart = 'ZOCD'.
         wa_ch_or2-decremental   = wa_ekko-ebeln.
       ENDIF.


***       READ TABLE it_ekko INTO wa_ekko WITH KEY unsez = wa_ch_or-folio
***                                                bsart = 'ZOCI'.
***       READ TABLE it_ekko INTO wa_ekko2 WITH KEY unsez = wa_ch_or-folio
***                                                 bsart = 'ZOCD'.

       wa_ch_or2-id_proyecto   = wa_ch_or-proyecto.
       wa_ch_or2-desc_proy     = wa_proj-post1.
       wa_ch_or2-folio         = wa_ch_or-folio.
*-- JCG 27.03.13 Suprimir campos: numero y nombre del proveedor
*     wa_ch_or2-proveedor     = wa_ch_or-n_proveedor.
*     wa_ch_or2-nom_proveedor = wa_ch_or-proveedor.
*--
       wa_ch_or2-fecha         = wa_ch_or-fecha_ejec.
***       wa_ch_or2-decremental   = wa_ekko2-ebeln.
***       wa_ch_or2-incremental   = wa_ekko-ebeln.

       APPEND wa_ch_or2 TO it_ch_or2.
       CLEAR: wa_ch_or2-incremental,
              wa_ch_or2-decremental.
     ENDLOOP.
     CLEAR: wa_ch_or, wa_proj, wa_ekko, wa_ch_or2.
   ENDLOOP.

   IF sy-subrc <> 0.
     MESSAGE 'No se encontraron Registros segun Filtros'
     TYPE 'S' DISPLAY LIKE 'E'.
   ELSE.
     CALL SCREEN 100.
   ENDIF.



   INCLUDE zmm0090_mon_ord_cambio_f01              .  " FORM-Routines
