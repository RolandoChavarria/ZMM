*&---------------------------------------------------------------------*
*&  Include           ZLO4220_INFO_COMP_CLI
*&---------------------------------------------------------------------*
CLASS lcl_alv100 IMPLEMENTATION.

  METHOD hand_dblclk."Manejo de evento doble click
    DATA ls_header LIKE LINE OF it_header.
    DATA ls_detail LIKE LINE OF it_detail.
    DATA lwa_detail LIKE LINE OF it_detail.
    CLEAR: ls_header, ls_detail.

    CLEAR:  wa_header, lwa_detail, it_detail_p.
    "Se recupera la linea del trgistro donde usuario dio doble click
    READ TABLE it_header INTO ls_header INDEX e_row-index.
    CHECK sy-subrc EQ 0.
*--> Inicio Modificación RSDK906898 - 1.
    CLEAR: gv_ebeln,
           gv_lifnr.
    gv_ebeln = ls_header-pedido.
    gv_lifnr = ls_header-no_prov.
*<-- Fin Modificación RSDK906898 - 1.

    LOOP AT it_detail[] INTO lwa_detail WHERE ebeln = ls_header-pedido.
      APPEND lwa_detail TO it_detail_p.
    ENDLOOP.

    IF sy-subrc NE 0.
      CLEAR it_detail_p.
      MESSAGE 'El pedido no tiene Historial' TYPE 'S' DISPLAY LIKE 'E'.
      PERFORM f_create_alv_0100d.
    ELSE.
      PERFORM f_create_alv_0100d.
    ENDIF.

  ENDMETHOD.                    "hand_dblclk

*--> Inicia Modificación RSDK906898 - 2.
  "hand_tool_bar
  METHOD hand_tool_bar.
    DATA: lt_tool LIKE e_object->mt_toolbar[],
          ls_tool LIKE LINE OF lt_tool.
    CLEAR: lt_tool, ls_tool.

    ls_tool-function  = 'ZEXPORT'.
    ls_tool-icon      = icon_export.
    ls_tool-quickinfo = text-m01.
    ls_tool-text      = text-m01.
    ls_tool-butn_type = '0'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.

    APPEND LINES OF e_object->mt_toolbar TO lt_tool.
    CLEAR e_object->mt_toolbar[].
    e_object->mt_toolbar[] = lt_tool[].
  ENDMETHOD.                    "hand_tool_bar

  METHOD hand_cmd.

    CASE e_ucomm.
      WHEN 'ZEXPORT'.

        CLEAR: git_header,
               git_detalle.

        REFRESH: git_header,
                 git_detalle.

* Libera los objetos
        FREE OBJECT: application,
                     workbook,
                     sheet,
                     cells,
                     int,
                     font,
                     we,
                     borders.

        IF NOT it_detail_p[] IS INITIAL AND
               it_header[] IS NOT INITIAL.
          PERFORM f_llena_campos_header.
          PERFORM f_llena_campos_detalle.

          IF NOT git_header[] IS INITIAL AND
                 git_detalle[] IS NOT INITIAL.
            PERFORM f_crea_arch_excel.
          ELSE.
            MESSAGE text-m05 TYPE 'E'.
          ENDIF.
        ELSE.
          MESSAGE text-m02 TYPE 'I'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "hand_cmd
*<-- Fin Modificación RSDK906898 - 2.


ENDCLASS.                    "lcl_alv100 IMPLEMENTATION
