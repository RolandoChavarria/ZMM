*&---------------------------------------------------------------------*
*&  Include           ZMM0080_NOTA_CARGO_PRG
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  IF i_date IS INITIAL.
    i_date = sy-datum.
  ELSE.
    i_date = i_date.
  ENDIF.

  PERFORM f_crt_zutil_param.
  CHECK v_subrc = 0.

  SELECT * FROM ekbe
    INTO CORRESPONDING FIELDS OF TABLE it_ekbe
    WHERE vgabe = '1'
    AND   bwart = '161'
    AND   budat = i_date.

  IF it_ekbe[] IS NOT INITIAL.
    SELECT * FROM ekbe
    APPENDING CORRESPONDING FIELDS OF TABLE it_ekbe
    FOR ALL ENTRIES IN it_ekbe[]
    WHERE ebeln = it_ekbe-ebeln
    AND vgabe = '2'
    AND budat = i_date.
  ELSE.
    v_subrc = 02.
    MESSAGE 'No se encontraron pedidos para nota de cargo'
    TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  CHECK it_ekbe[] IS NOT INITIAL.

  SORT it_ekbe BY ebeln vgabe.

  LOOP AT it_ekbe
    WHERE vgabe = '2'.
    DELETE  it_ekbe WHERE ebeln = it_ekbe-ebeln.
    CLEAR it_ekbe.
  ENDLOOP.


*De los pedidos restantes, verificar si sean de las clases
*de documento de ZPEN y ZOCD (indicados en la tabla de parámetros) en tabla EKKO.

  IF it_ekbe[] IS NOT INITIAL.
    SELECT * FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
      FOR ALL ENTRIES IN it_ekbe
      WHERE ebeln EQ it_ekbe-ebeln
      AND   bsart IN lit_doc_type.
  ELSE.
    v_subrc = 02.
    MESSAGE 'Pedidos descartados por indicador de Factura'
    TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK v_subrc = 0.

  LOOP AT it_ekbe.
    READ TABLE it_ekko TRANSPORTING NO FIELDS
     WITH KEY ebeln = it_ekbe-ebeln.
    IF sy-subrc NE 0.
      DELETE it_ekbe WHERE ebeln = it_ekbe-ebeln.
    ENDIF.
    CLEAR it_ekbe.
  ENDLOOP.

  IF it_ekbe[] IS INITIAL.
    v_subrc = 02.
    MESSAGE 'No se encontraron pedidos para nota de cargo'
    TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  CHECK v_subrc = 0.

  SELECT * FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE it_lfa1
    FOR ALL ENTRIES IN it_ekko
    WHERE lifnr = it_ekko-lifnr.


  SELECT ebeln ebelp loekz effwr FROM ekpo INTO CORRESPONDING FIELDS OF TABLE it_ekpo
  FOR ALL ENTRIES IN it_ekko
  WHERE ebeln = it_ekko-ebeln.

  LOOP AT it_ekko.
    READ TABLE it_ekbe WITH KEY ebeln = it_ekko-ebeln INTO it_ekbe.
    READ TABLE it_lfa1 WITH KEY lifnr = it_ekko-lifnr INTO it_lfa1.
    READ TABLE it_ekpo WITH KEY ebeln = it_ekko-ebeln INTO it_ekpo.

    LOOP AT it_ekpo
      WHERE ebeln = it_ekko-ebeln.
      v_effwr = v_effwr + it_ekpo-effwr.

*SELECTPO
      wa_selectpo-po_number          = it_ekko-ebeln.  "   numero DE pedido
      wa_selectpo-po_item            = it_ekpo-ebelp.  "  NUMERO de POSICIÓN

      APPEND wa_selectpo TO it_selectpo.
      CLEAR wa_selectpo.
    ENDLOOP.

*  HEADERDATA
    MOVE ' '      TO it_headerdata-invoice_ind .  " Valor fijo para Nota de Cargo
    MOVE 'RE'     TO it_headerdata-doc_type . "   Valor fijo
    MOVE sy-datum TO it_headerdata-doc_date .
    MOVE sy-datum TO it_headerdata-pstng_date .
    MOVE it_lfa1-stcd1 TO it_headerdata-ref_doc_no .
    MOVE it_ekko-bukrs TO it_headerdata-comp_code .
    MOVE it_ekko-waers TO it_headerdata-currency .
    MOVE it_ekko-waers TO it_headerdata-currency_iso .
    MOVE v_effwr TO it_headerdata-gross_amount .
    MOVE 'E0' TO it_headerdata-del_costs_taxc.
    MOVE 'X' TO it_headerdata-calc_tax_ind .  "valor fijo
    MOVE it_ekko-lifnr TO it_headerdata-diff_inv.
    MOVE 'DEVOLUCION' TO it_headerdata-header_txt .  "Valor fijo
    MOVE it_ekko-ebeln TO it_headerdata-po_ref_no .
    MOVE it_lfa1-stcd1 TO it_headerdata-item_text .


*ADDITIONALHEADERDATA
    MOVE 'X' TO it_addition_hd-assign_deliv .    "Valor fijo
    MOVE 'X' TO it_addition_hd-assign_return .    "Valor fijo
    MOVE 'H' TO it_addition_hd-deliv_posting .    "Valor fijo
    MOVE 'H' TO it_addition_hd-return_posting .    "Valor fijo
    MOVE 'X' TO it_addition_hd-sel_goods .    "Valor fijo

*REFDOCCATEGORY
    MOVE '1' TO it_refdoc-ref_doc_category .    " VALOR fijo


    CALL FUNCTION 'BAPI_INCOMINGINVOICE_SAVE'
      EXPORTING
        headerdata                 = it_headerdata
        additionalheaderdata       = it_addition_hd
        refdoccategory             = it_refdoc-ref_doc_category
        addressdata                = it_addressdata
     IMPORTING
       invoicedocnumber            = bapi_incinv_fld-inv_doc_no
       fiscalyear                  = bapi_incinv_fld-fisc_year
      TABLES
        selectpo                   = it_selectpo
*   SELECTDELIVERY             =
*   SELECTBILLLADING           =
*   SELECTSERVICE              =
*   SELECTPLANT                =
*   TAXDATA                    =
*   WITHTAXDATA                =
*   VENDORITEMSPLITDATA        =
        return                     = it_return
*   EXTENSIONIN                =
              .

* Validacion de ejecucion en BAPI_INCOMINGINVOICE_SAVE.
    IF bapi_incinv_fld-inv_doc_no IS INITIAL.
      READ TABLE it_return INDEX 1.
      MESSAGE i000(zrf1)
      WITH it_return-message INTO v_msger.
      it_contrl-ebeln = it_ekko-ebeln.
      it_contrl-bapi_msg = it_return-message.
      APPEND it_contrl.
      v_subrc = 04.  "  No se creó la Nota de cargo(Error en BAPI)
      ROLLBACK WORK.
    ELSE.
      it_contrl-ebeln = it_ekko-ebeln.
      it_contrl-doc_no = bapi_incinv_fld-inv_doc_no.
      it_contrl-fisc_year = bapi_incinv_fld-fisc_year.
      APPEND it_contrl.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.
*****************************************************
*****Implementacion de preverificacion de Factura****
*****************************************************

    v_doc_no    = bapi_incinv_fld-inv_doc_no.
    v_cont_year = bapi_incinv_fld-fisc_year.

    REFRESH: lit_param.
*        Se Indica el Numero de Factura
    MOVE 'SO_BELNR' TO       lwa_param-selname.
    MOVE 'S'        TO lwa_param-kind.
    MOVE 'I'        TO lwa_param-sign.
    MOVE 'EQ'       TO lwa_param-option.
    MOVE v_doc_no  TO lwa_param-low.
    CLEAR lwa_param-high.
    APPEND lwa_param TO lit_param.
*        Se indica el Año Fiscal
    MOVE 'SO_GJAHR' TO       lwa_param-selname.
    MOVE 'S'        TO lwa_param-kind.
    MOVE 'I'        TO lwa_param-sign.
    MOVE 'EQ'       TO lwa_param-option.
    MOVE v_cont_year     TO lwa_param-low.
    CLEAR lwa_param-high.
    APPEND lwa_param TO lit_param.
*        Se indica la Sociedad.
    MOVE 'SO_BUKRS' TO       lwa_param-selname.
    MOVE 'S'        TO lwa_param-kind.
    MOVE 'I'        TO lwa_param-sign.
    MOVE 'EQ'       TO lwa_param-option.
    MOVE it_ekko-bukrs TO lwa_param-low.
    CLEAR lwa_param-high.
*        EJECUTA TRANSACCION BRMP
    APPEND lwa_param TO lit_param.
    SUBMIT rmbabg00 AND RETURN EXPORTING LIST TO MEMORY
                    WITH SELECTION-TABLE lit_param
                    .
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.

    CLEAR: v_effwr, wa_selectpo, it_selectpo, it_addition_hd, it_refdoc,
           bapi_incinv_fld, it_headerdata, v_doc_no, v_cont_year.

  ENDLOOP.

  PERFORM alv_reuse_contrl TABLES it_contrl
                           CHANGING it_cat.
