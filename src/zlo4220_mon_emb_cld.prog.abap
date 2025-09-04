*&---------------------------------------------------------------------*
*&  Include           ZLO4220_MON_EMB_CLD
*&---------------------------------------------------------------------*
CLASS lcl_alv100 DEFINITION.

  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.
*--> Inicia Modificación RSDK906898 - 3.

    METHODS: hand_tool_bar FOR EVENT toolbar OF cl_gui_alv_grid
                           IMPORTING e_object.
*--> Inicio Modificación RSDK906906 - 1.
    METHODS :
*<-- Fin Modificación RSDK906906 - 1.
             hand_cmd      FOR EVENT user_command OF cl_gui_alv_grid
                           IMPORTING e_ucomm..
*<-- Fin Modificación RSDK906898 - 3.

ENDCLASS.                    "lcl_alv100 DEFINITION
