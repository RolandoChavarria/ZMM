*&---------------------------------------------------------------------*
*&  Include           ZXM06U44
*&---------------------------------------------------------------------*

  break devlpext.
************************************************************************
* USER EXIT implementado para el módulo de control presupuestal GGI
* Consultor: Iván Gutiérrez De Haro
* Consultoría: BAF Consulting
* Fecha: Mayo-2012
* Descripción: Se guarda en la cabecera del pedido el ID del proyecto
************************************************************************

  FIELD-SYMBOLS: <fs_ekkn> LIKE LINE OF xekkn.
  FIELD-SYMBOLS: <fs_xekpo> LIKE LINE OF xekpo.
  DATA: lwa_prps TYPE prps.
  DATA: lwa_proj TYPE proj.
  DATA: lv_string TYPE string.
  DATA: lv_verkf TYPE verkf.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lv_prefijo TYPE string.

  IF i_ekko-ebeln IS NOT INITIAL AND xekkn[] IS NOT INITIAL.
    PERFORM f_val_tp_doc TABLES xekpo
                         CHANGING lv_subrc
                                  lv_prefijo.
    IF lv_subrc EQ 0.
      READ TABLE xekkn ASSIGNING <fs_ekkn> INDEX 1.
      IF sy-subrc EQ 0.
        SELECT SINGLE * INTO lwa_prps
        FROM prps
        WHERE pspnr =  <fs_ekkn>-ps_psp_pnr.
        IF sy-subrc EQ 0.
          CONCATENATE lv_prefijo lwa_prps-psphi INTO lv_string.
          SELECT SINGLE * INTO lwa_proj
          FROM proj
          WHERE objnr = lv_string.
          IF sy-subrc EQ 0.
            lv_verkf = lwa_proj-pspid.
            CALL FUNCTION 'ZMMFM_EXIT13_LBL_PO' IN UPDATE TASK
              EXPORTING
                i_ebeln = i_ekko-ebeln
                i_verkf = lv_verkf.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.
*    CHECK i_ekko-ebeln IS NOT INITIAL.



*    MESSAGE i000(zrf1) WITH 'Prueba'.
