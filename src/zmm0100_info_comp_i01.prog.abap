*&---------------------------------------------------------------------*
*&  Include           ZMM0100_INFO_COMP_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK_0100'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT_0100'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL_100'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRSH_100'.
      CLEAR: it_header[], IT_DETAIL[].

    PERFORM f_get_info TABLES git_pspidebeln
                            it_header
                            it_detail.

    IF IT_HEADER[] IS INITIAL
    OR IT_DETAIL[] IS INITIAL.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
