*&---------------------------------------------------------------------*
*&  Include           ZLO4220_MON_EMB_EXT_CLD
*&---------------------------------------------------------------------*
CLASS lcl_alv100 DEFINITION.

  PUBLIC SECTION.

    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.

    METHODS: hand_tool_bar FOR EVENT toolbar OF cl_gui_alv_grid
                           IMPORTING e_object.
    METHODS :
             hand_cmd      FOR EVENT user_command OF cl_gui_alv_grid
                           IMPORTING e_ucomm..

ENDCLASS.                    "lcl_alv100 DEFINITION
