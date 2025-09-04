FUNCTION zrefcarga01.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  DATUM
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"  TABLES
*"      T_PROY TYPE  ZTWEB_PED OPTIONAL
*"----------------------------------------------------------------------
***  DATA: t_proy2 TYPE TABLE OF zweb_ped.
  TYPES: BEGIN OF ty_proy2,
          pspnr TYPE ps_posnr,
          pspid TYPE ps_pspid,
          post1 TYPE ps_post1,
         END OF ty_proy2,
         BEGIN OF ty_prps,
          posid TYPE ps_posid,
          usr10 TYPE usr10prps,
         END OF ty_prps.
  DATA: it_proy2 TYPE TABLE OF ty_proy2,
        it_prps TYPE  TABLE OF ty_prps,
        wa_prps TYPE ty_prps,
        wa_proy TYPE zweb_ped,
        wa_proy2 TYPE ty_proy2.

  SELECT pspnr pspid post1 FROM proj INTO TABLE it_proy2
    WHERE erdat = i_date.
  IF sy-subrc EQ 0.
    SELECT posid usr10 FROM prps INTO TABLE it_prps FOR ALL ENTRIES IN it_proy2
      WHERE posid EQ it_proy2-pspid
        AND usr10 = 'X'.
    IF sy-subrc EQ 0.
      LOOP AT it_prps INTO wa_prps.
        READ TABLE it_proy2 INTO wa_proy2 WITH KEY pspid = wa_prps-posid.
        IF sy-subrc EQ 0.
          wa_proy-pspid = wa_proy2-pspid.
          wa_proy-post1 = wa_proy2-post1.
          APPEND wa_proy TO t_proy.
        ENDIF.
      ENDLOOP.
    ELSE.
      e_subrc = '08'.
      e_bapi_msg = text-002.
    ENDIF.
  ELSE.
    e_subrc = '08'.
    e_bapi_msg = text-001.
  ENDIF.
ENDFUNCTION.
