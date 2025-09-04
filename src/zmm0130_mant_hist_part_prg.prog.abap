*&---------------------------------------------------------------------*
*&  Include           ZMM0130_MANT_HIST_PART_PRG
*&---------------------------------------------------------------------*

  AUTHORITY-CHECK OBJECT 'Z_PSGGI01'
         ID 'ACTVT' FIELD '01'.
  IF sy-subrc = 0.
    gv_agregar = 'X'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'Z_PSGGI01'
          ID 'ACTVT' FIELD '02'.
  IF sy-subrc = 0.
    gv_edicion = 'X'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'Z_PSGGI01'
         ID 'ACTVT' FIELD '03'.
  IF sy-subrc = 0.
    gv_visual = 'X'.
  ENDIF.

  IF gv_agregar IS NOT INITIAL
 AND gv_edicion IS NOT INITIAL
 AND gv_visual  IS NOT INITIAL.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                               = 'U'
*      CORR_NUMBER                          = '          '
*      GENERATE_MAINT_TOOL_IF_MISSING       = ' '
      SHOW_SELECTION_POPUP                  = 'X'
        view_name                            = 'ZMMTT_HIST_PART'
*      NO_WARNING_FOR_CLIENTINDEP           = ' '
*      RFC_DESTINATION_FOR_UPGRADE          = ' '
*      CLIENT_FOR_UPGRADE                   = ' '
*      VARIANT_FOR_SELECTION                = ' '
*      COMPLEX_SELCONDS_USED                = ' '
*      CHECK_DDIC_MAINFLAG                  = ' '
*      SUPPRESS_WA_POPUP                    = ' '
*    TABLES
*      DBA_SELLIST                          =
*      EXCL_CUA_FUNCT                       =
     EXCEPTIONS
       client_reference                     = 1
       foreign_lock                         = 2
       invalid_action                       = 3
       no_clientindependent_auth            = 4
       no_database_function                 = 5
       no_editor_function                   = 6
       no_show_auth                         = 7
       no_tvdir_entry                       = 8
       no_upd_auth                          = 9
       only_show_allowed                    = 10
       system_failure                       = 11
       unknown_field_in_dba_sellist         = 12
       view_not_found                       = 13
       maintenance_prohibited               = 14
       OTHERS                               = 15
              .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
