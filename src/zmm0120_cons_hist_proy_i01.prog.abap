*&---------------------------------------------------------------------*
*&  Include           ZMM0120_CONS_HIST_PROY_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK_0100'.

      CLEAR git_hist_part[].

      IF gr_cnt100_1 IS NOT INITIAL.
        CALL METHOD gr_cnt100_1->free.
        CLEAR: gr_cnt100_1, gr_alv100_1, git_t_e_pep[].
      ENDIF.

      IF gr_cnt100_2 IS NOT INITIAL.
        CALL METHOD gr_cnt100_2->free.
        CLEAR: gr_cnt100_2, gr_alv100_2, git_t_prov[].
      ENDIF.

      IF gr_cnt100_3 IS NOT INITIAL.
        CALL METHOD gr_cnt100_3->free.
        CLEAR: gr_cnt100_3, gr_alv100_3, git_t_cc[].
      ENDIF.

      IF gr_cnt100_4 IS NOT INITIAL.
        CALL METHOD gr_cnt100_4->free.
        CLEAR: gr_cnt100_4, gr_alv100_4, git_t_cp[].
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'LEAVE_0100'.

      CLEAR git_hist_part[].

      IF gr_cnt100_1 IS NOT INITIAL.
        CALL METHOD gr_cnt100_1->free.
        CLEAR: gr_cnt100_1, gr_alv100_1, git_t_e_pep[].
      ENDIF.

      IF gr_cnt100_2 IS NOT INITIAL.
        CALL METHOD gr_cnt100_2->free.
        CLEAR: gr_cnt100_2, gr_alv100_2, git_t_prov[].
      ENDIF.

      IF gr_cnt100_3 IS NOT INITIAL.
        CALL METHOD gr_cnt100_3->free.
        CLEAR: gr_cnt100_3, gr_alv100_3, git_t_cc[].
      ENDIF.

      IF gr_cnt100_4 IS NOT INITIAL.
        CALL METHOD gr_cnt100_4->free.
        CLEAR: gr_cnt100_4, gr_alv100_4, git_t_cp[].
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'EXIT_0100'.

      CLEAR git_hist_part[].

      IF gr_cnt100_1 IS NOT INITIAL.
        CALL METHOD gr_cnt100_1->free.
        CLEAR: gr_cnt100_1, gr_alv100_1, git_t_e_pep[].
      ENDIF.

      IF gr_cnt100_2 IS NOT INITIAL.
        CALL METHOD gr_cnt100_2->free.
        CLEAR: gr_cnt100_2, gr_alv100_2, git_t_prov[].
      ENDIF.

      IF gr_cnt100_3 IS NOT INITIAL.
        CALL METHOD gr_cnt100_3->free.
        CLEAR: gr_cnt100_3, gr_alv100_3, git_t_cc[].
      ENDIF.

      IF gr_cnt100_4 IS NOT INITIAL.
        CALL METHOD gr_cnt100_4->free.
        CLEAR: gr_cnt100_4, gr_alv100_4, git_t_cp[].
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'REFRESH100'.

    PERFORM F_REFRESH_0100.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code2.
    WHEN 'BACK_0200'.
      gv_edit = 'X'.
      IF gr_cnt200 IS NOT INITIAL.
        CALL METHOD gr_cnt200->free.
        CLEAR: gr_cnt200, gr_alv200, git_nivel_detalle.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'LEAVE_0200'.
      gv_edit = 'X'.
      IF gr_cnt200 IS NOT INITIAL.
        CALL METHOD gr_cnt200->free.
        CLEAR: gr_cnt200, gr_alv200, git_nivel_detalle.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT_0200'.
      gv_edit = 'X'.
      IF gr_cnt200 IS NOT INITIAL.
        CALL METHOD gr_cnt200->free.
        CLEAR: gr_cnt200, gr_alv200, git_nivel_detalle.
      ENDIF.
      LEAVE TO SCREEN 0.
    WHEN 'EDIT_0200'.
      PERFORM edit_alv_0200.
    WHEN 'SAVE_0200'.
      PERFORM save_alv_0200.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
