*&---------------------------------------------------------------------*
*&  Include           ZMM_0120_CONS_HIST_PROY_CLD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZCS0063_REP_FLUJO_EFE_COM_CLD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Definicion de doble click para ALV_100DC_1.
*&---------------------------------------------------------------------*
CLASS lcl_alv100DC_1 DEFINITION.

  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.

ENDCLASS.

CLASS lcl_alv100DC_2 DEFINITION.

  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.

ENDCLASS.

CLASS lcl_alv100DC_3 DEFINITION.

  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.

ENDCLASS.

CLASS lcl_alv100DC_4 DEFINITION.

  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.

ENDCLASS.
