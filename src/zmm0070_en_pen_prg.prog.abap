*&---------------------------------------------------------------------*
*&  Include           ZMM_0070_EN_PENF01
*&---------------------------------------------------------------------*
START-OF-SELECTION.


  IF i_date IS INITIAL.
    i_date = sy-datum.
  ENDIF.

  PERFORM f_crt_zutil_param CHANGING v_subrc.
  CHECK v_subrc = 0.

  SELECT ebeln bsart lifnr FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
    WHERE aedat = i_date
    AND   bsart IN lit_doc_type.

  IF it_ekko[] IS NOT INITIAL.
    SELECT ebeln ebelp matnr werks menge meins FROM ekpo INTO CORRESPONDING FIELDS OF TABLE it_ekpo
      FOR ALL ENTRIES IN it_ekko
      WHERE ebeln EQ it_ekko-ebeln
      AND   elikz EQ ' '
      AND   retpo EQ 'X'.
  ELSE.
    v_subrc = 02.
    MESSAGE 'No se encontraron pedidos por penalizar el dia de hoy'
    TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK v_subrc = 0.

  LOOP AT it_ekko. "Se descartan pedidos con Flag de entrega final
    READ TABLE it_ekpo TRANSPORTING NO FIELDS
     WITH KEY ebeln = it_ekko-ebeln.
    IF sy-subrc NE 0.
      DELETE it_ekko WHERE ebeln = it_ekko-ebeln.
    ENDIF.
    CLEAR it_ekko.
  ENDLOOP.

  IF it_ekko[] IS INITIAL.
    v_subrc = 03.
    MESSAGE 'No se encontraron pedidos por penalizar el dia de hoy'
    TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK v_subrc = 0.

  LOOP AT it_ekko.

* datos del tipo de transaccion
    MOVE v_gm_code TO bapi2017_gm_code-gm_code.

*   datos de la cabecera
    MOVE sy-datum       TO bapi2017_gm_head_01-pstng_date.
    MOVE sy-datum       TO bapi2017_gm_head_01-doc_date.
    MOVE v_bill_lding   TO bapi2017_gm_head_01-bill_of_lading.
    MOVE sy-uname       TO bapi2017_gm_head_01-pr_uname.
    MOVE v_hdr_txt      TO bapi2017_gm_head_01-header_txt.
    MOVE it_ekko-ebeln  TO bapi2017_gm_head_01-ref_doc_no.


    LOOP AT it_ekpo WHERE ebeln EQ it_ekko-ebeln.
      MOVE it_ekpo-matnr  TO t_items-material.
      MOVE it_ekpo-werks  TO t_items-plant.
      MOVE it_ekko-lifnr  TO t_items-vendor.
      MOVE it_ekpo-menge  TO t_items-entry_qnt.
      MOVE it_ekpo-meins  TO t_items-entry_uom.
      MOVE it_ekpo-ebeln  TO t_items-po_number.
      MOVE it_ekpo-ebelp  TO t_items-po_item.
      CLEAR it_ekpo.


* datos del ITEM.
      MOVE v_move_type    TO t_items-move_type.
      MOVE v_no_more_gr   TO t_items-no_more_gr.
      MOVE v_mvt_ind      TO t_items-mvt_ind.
      APPEND t_items.
    ENDLOOP. "Aqui termina loop de EKPO

** llamo a la bapi para generar documento
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header       = bapi2017_gm_head_01
        goodsmvt_code         = bapi2017_gm_code
      IMPORTING
        goodsmvt_headret      = bapi2017_gm_head_ret
        materialdocument      = bapi2017_gm_head_ret-mat_doc
        matdocumentyear       = bapi2017_gm_head_ret-doc_year
      TABLES
        goodsmvt_item         = t_items
        goodsmvt_serialnumber = t_serial
        return                = t_return.

* Hubo errores
    IF bapi2017_gm_head_ret-mat_doc IS INITIAL.
      READ TABLE t_return INDEX 1.
      MESSAGE i000(zrf1)
      WITH t_return-message INTO p_msger.
      it_contrl-ebeln = bapi2017_gm_head_01-ref_doc_no.
      it_contrl-bapi_msg = t_return-message.
      p_subrc = 04.  "  No se creó la contabilización (Error en BAPI)
      APPEND it_contrl.
      ROLLBACK WORK.
    ELSE.
      it_contrl-ebeln = bapi2017_gm_head_01-ref_doc_no.
      it_contrl-mblnr = bapi2017_gm_head_ret-mat_doc.
      it_contrl-mjahr = bapi2017_gm_head_ret-doc_year.
      APPEND it_contrl.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.
    ENDIF.

    CLEAR: it_ekko, it_contrl, t_items[], t_items, t_serial,
           t_return, bapi2017_gm_head_ret,
           bapi2017_gm_head_01, bapi2017_gm_code.
  ENDLOOP.

  PERFORM alv_reuse_contrl TABLES it_contrl
                         CHANGING it_cat.
