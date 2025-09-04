REPORT ZMMFMF99
       NO STANDARD PAGE HEADING
       LINE-SIZE  255.

DATA: l_err TYPE sy-msgv1,
      file     TYPE string,
      listzeile TYPE STANDARD TABLE OF listzeile  WITH HEADER LINE,
      lt_progtab TYPE STANDARD TABLE OF progtab WITH HEADER LINE.

PARAMETERS: p_ldir TYPE rlgrap-filename.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ldir.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = space
      def_path         = space
      mask             = ',*.*.'
      mode             = 'O'
      title            = 'Seleccionar archivo'
    IMPORTING
      filename         = p_ldir
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.


START-OF-SELECTION.
  file = p_ldir.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = file
      filetype = 'ASC'
    TABLES
      data_tab = lt_progtab.

  IF lt_progtab[] IS NOT INITIAL.
    CALL FUNCTION 'RFC_ABAP_INSTALL_AND_RUN'
      IMPORTING
        errormessage = l_err
      TABLES
        program      = lt_progtab
        writes       = listzeile.

    WRITE:/ l_err.

    LOOP AT listzeile.
      WRITE:/ listzeile-zeile.
    ENDLOOP.

  ENDIF.
