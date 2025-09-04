*&---------------------------------------------------------------------*
*&  Include           ZMM0020_MON_STATUS_PURCH_PRG
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN *
* Validaciones
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_pspid.
  PERFORM validate_pspid.

*---------------------------------------------------------------------*
* TOP-OF-PAGE *
*---------------------------------------------------------------------*
TOP-OF-PAGE.


*---------------------------------------------------------------------*
* START-OF-SELECTION *
*---------------------------------------------------------------------*

START-OF-SELECTION.
  gv_repid = sy-repid.

  PERFORM f_get_ebeln_from_pspid TABLES git_pspidebeln
                                       pr_bedat
                                  USING p_pspid.

  IF git_pspidebeln[] IS INITIAL.
    LEAVE TO TRANSACTION 'ZMM0020'.
  ENDIF.

  PERFORM f_get_info_monitor TABLES git_pspidebeln
                                           git_header_monitor
                                           git_detail_monitor.


*---------------------------------------------------------------------*
* END OF SELECTION *
*
*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM f_show_alv TABLES git_header_monitor
                             git_detail_monitor.
*ZMMWA_0020_HEADER_MONITOR
*ZMMWA_0020_services_monitor
*ZMMWA_0020_DETAIL_SERVICES_MON
