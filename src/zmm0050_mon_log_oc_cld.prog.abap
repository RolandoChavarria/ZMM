*&---------------------------------------------------------------------*
*&  Include           ZMM0050_MON_LOG_OC_CLD
*&---------------------------------------------------------------------*

CLASS lcl_hand_0100 DEFINITION.
  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_hand52 DEFINITION
