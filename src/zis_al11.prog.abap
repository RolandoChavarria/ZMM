*&---------------------------------------------------------------------*
*& Report  ZFTP_SAP_SERVER
*&---------------------------------------------------------------------*
*& Developer: Omar Romero Aldana - BAF Consulting S.C.
*& Type program: Report
*& Objective: Exchange files between the Presentation Server (GUI)
*&            and the Application Server (SAP instance).
*&---------------------------------------------------------------------*
*REPORT  ZIS_AL11. "Commented for use LDX_FILETOP

**********************************************************************
*** Global Statements ************************************************
**********************************************************************
INCLUDE: ldx_filetop,
         ldx_filef01,
         <list>.
TYPES BEGIN OF gty_list.
        INCLUDE STRUCTURE g_file.
TYPES: icon TYPE icon-name,
       END OF gty_list.

TABLES rcgfiletr.

DATA: gv_path  TYPE dxfields-longpath,
      gv_subrc TYPE subrc,
      gt_list  TYPE TABLE OF gty_list,
      gv_ok    TYPE sy-ucomm,
      gr_alv  TYPE REF TO cl_gui_alv_grid,
      gr_cnt  TYPE REF TO cl_gui_custom_container,
      gt_fc   TYPE lvc_t_fcat,
      gs_stbl TYPE lvc_s_stbl VALUE 'XX',
      gs_layo TYPE lvc_s_layo,
      gv_title200 TYPE bapi_msg.
DATA: gt_file TYPE filetable.

CONSTANTS: gc_m01 TYPE bapi_msg VALUE 'Error while uploading file:',
           gc_m02 TYPE bapi_msg VALUE 'Error while saving file:',
           gc_m03 TYPE bapi_msg VALUE 'Error while reaing file:',
           gc_m04 TYPE bapi_msg VALUE 'Error interpreting code page file:'.

**********************************************************************
*** Local Class ******************************************************
**********************************************************************
CLASS lcl_hand DEFINITION.
  PUBLIC SECTION.
    METHODS: hand_tool_bar FOR EVENT toolbar OF cl_gui_alv_grid
             IMPORTING e_object e_interactive.
    METHODS: hand_cmd FOR EVENT user_command OF cl_gui_alv_grid
             IMPORTING e_ucomm.
    METHODS: hand_dblclk FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_hand DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_hand IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hand IMPLEMENTATION.
  "hand_tool_bar
  METHOD hand_tool_bar.
    DATA: lt_tool LIKE e_object->mt_toolbar[],
          ls_tool LIKE LINE OF lt_tool.
    CLEAR: lt_tool, ls_tool.
    DELETE e_object->mt_toolbar WHERE function = '&DETAIL'
                                OR    function = '&MB_SUM'
                                OR    function = '&MB_SUBTOT'
                                OR    function = '&PRINT_BACK'
                                OR    function = '&MB_VIEW'
                                OR    function = '&GRAPH'
                                OR    function = '&INFO'
                                OR    function CP 'Z*'.
    ls_tool-function  = 'ZREFRESH'.
    ls_tool-icon      = icon_refresh.
    ls_tool-quickinfo = 'Refresh list'.
    ls_tool-butn_type = '0'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.
    ls_tool-function  = 'ZSEP1'.
    ls_tool-butn_type = '3'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.
    ls_tool-function  = 'ZUPLOAD'.
    ls_tool-icon      = icon_import.
    ls_tool-quickinfo = 'Upload File(s)'.
    ls_tool-text      = 'Upload'.
    ls_tool-butn_type = '0'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.
    ls_tool-function  = 'ZDOWNLOAD'.
    ls_tool-icon      = icon_export.
    ls_tool-quickinfo = 'Download File(s)'.
    ls_tool-text      = 'Download'.
    ls_tool-butn_type = '0'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.
    ls_tool-function  = 'ZSEP2'.
    ls_tool-butn_type = '3'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.
    ls_tool-function  = 'ZDELETE'.
    ls_tool-icon      = icon_delete.
    ls_tool-quickinfo = 'Delete File(s)'.
    ls_tool-text      = 'Delete'.
    ls_tool-butn_type = '0'.
    APPEND ls_tool TO lt_tool. CLEAR ls_tool.
    APPEND LINES OF e_object->mt_toolbar TO lt_tool.
    CLEAR e_object->mt_toolbar[].
    e_object->mt_toolbar[] = lt_tool[].
  ENDMETHOD.                    "hand_tool_bar
  "hand_user command
  METHOD hand_cmd.
    DATA: lt_row TYPE TABLE OF lvc_s_row,
          ls_row TYPE lvc_s_row,
          ls_list LIKE LINE OF gt_list,
    lv_flag TYPE flag.
    CLEAR: gv_title200, rcgfiletr, lt_row, ls_row, lv_flag, ls_list.
    rcgfiletr-iefow = 'X'.
    rcgfiletr-ftftype = 'BIN'.

    CASE e_ucomm.
      WHEN 'ZREFRESH'.
        PERFORM f_get_file_list USING gv_path.
      WHEN 'ZUPLOAD'.
        gv_title200 = 'UPLOAD'.
        CALL SCREEN 0200 STARTING AT 6 6.
        CHECK gv_ok EQ 'OK_0200'.
        PERFORM f_upload.
        IF gv_subrc EQ 0.
          MESSAGE 'FILE(S) UPLOADED SUCESSFULLY' TYPE 'S'.
          PERFORM f_get_file_list USING gv_path.
        ENDIF.
      WHEN 'ZDOWNLOAD'.
        gr_alv->get_selected_rows( IMPORTING et_index_rows = lt_row ).
        LOOP AT lt_row INTO ls_row.
          READ TABLE gt_list INTO ls_list INDEX ls_row-index.
          IF ls_list-type NE 'directory'.
            lv_flag = 'X'.
          ELSE.
            DELETE lt_row WHERE index = ls_row-index.
          ENDIF.
          CLEAR: ls_row, ls_list.
        ENDLOOP.
        IF lv_flag IS INITIAL.
          MESSAGE 'SELECT AT LEAST 1 FILE' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          gv_title200 = 'DOWNLOAD'.
          CALL SCREEN 0200 STARTING AT 6 6.
          CHECK gv_ok EQ 'OK_0200'.
          LOOP AT lt_row INTO ls_row.
            READ TABLE gt_list INTO ls_list INDEX ls_row-index.
            PERFORM f_download USING ls_list-name.
            IF gv_subrc NE 0.
              EXIT.
            ENDIF.
            CLEAR: ls_row, ls_list.
          ENDLOOP.
          IF gv_subrc EQ 0.
            MESSAGE 'FILE(S) SUCCESSFULLY DOWNLOADED' TYPE 'S'.
          ENDIF.
        ENDIF.
      WHEN 'ZDELETE'.
        gr_alv->get_selected_rows( IMPORTING et_index_rows = lt_row ).
        LOOP AT lt_row INTO ls_row.
          READ TABLE gt_list INTO ls_list INDEX ls_row-index.
          IF ls_list-type NE 'directory'.
            lv_flag = 'X'.
          ELSE.
            DELETE lt_row WHERE index = ls_row-index.
          ENDIF.
          CLEAR: ls_row, ls_list.
        ENDLOOP.
        IF lv_flag IS INITIAL.
          MESSAGE 'SELECT AT LEAST 1 FILE' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          PERFORM f_popup_to_delete.
          CHECK gv_subrc EQ 0.
          LOOP AT lt_row INTO ls_row.
            READ TABLE gt_list INTO ls_list INDEX ls_row-index.
            PERFORM f_delete USING ls_list-name.
            IF gv_subrc NE 0.
              EXIT.
            ENDIF.
            CLEAR: ls_row, ls_list.
          ENDLOOP.
          IF gv_subrc EQ 0.
            MESSAGE 'FILE(S) SUCCESSFULLY DELETED' TYPE 'S'.
            PERFORM f_get_file_list USING gv_path.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "hand_cmd
  "hand double click
  METHOD hand_dblclk.
    DATA: ls_list LIKE LINE OF gt_list,
          lv_path LIKE gv_path,
          lv_p1 TYPE dirname_al11,
          lv_p2 TYPE dirname_al11,
          lv_p3 TYPE filename_al11,
          lv_len TYPE i.
    CLEAR: ls_list, lv_path, lv_p1, lv_p2, lv_p3.
    READ TABLE gt_list INTO ls_list INDEX es_row_no-row_id.
    IF ls_list-type = 'directory'. "Show File List
      lv_path = gv_path.
      CONCATENATE gv_path ls_list-name '/' INTO gv_path.
      PERFORM f_get_file_list USING gv_path.
      IF gv_subrc NE 0.
        gv_path = lv_path.
      ENDIF.
    ELSE. "Print Screen File
      CONCATENATE gv_path ls_list-name INTO lv_p1.
      lv_len = strlen( gv_path ).
      lv_len = lv_len - 1.
      lv_p2 = gv_path+0(lv_len).
      lv_p3 = ls_list-name.
      EXPORT p1 = lv_p1
             p2 = lv_p2
             p3 = lv_p3
             TO MEMORY ID 'RSWATCH0'.
      SUBMIT rswatch0 AND RETURN.
    ENDIF.
  ENDMETHOD.                    "hand_dblclk
ENDCLASS.                    "lcl_hand IMPLEMENTATION

DATA gr_hand TYPE REF TO lcl_hand.


**********************************************************************
*** Modules **********************************************************
**********************************************************************
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'. " BACK_0100  EXIT_0100
  SET TITLEBAR 'TITLE_0100' WITH 'List of files in:' gv_path. " &  &
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: l_path LIKE gv_path,
        lt_filt TYPE lvc_t_filt.
  CLEAR: l_path, lt_filt.
  CASE gv_ok.
    WHEN 'BACK_0100'. "Back
      IF gv_path EQ '/'.
        PERFORM f_popup_to_leave_program USING 'BACK'.
      ELSE.
        l_path = gv_path.
        PERFORM f_back_path.
        PERFORM f_get_file_list USING gv_path.
        IF gv_subrc NE 0.
          gv_path = l_path.
        ENDIF.
        IF gr_alv IS NOT INITIAL.
          gr_alv->set_filter_criteria( it_filter = lt_filt ).
        ENDIF.
      ENDIF.
    WHEN 'EXIT_0100'. "Leave program
      PERFORM f_popup_to_leave_program USING 'EXIT'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_0100 OUTPUT.
  IF gr_alv IS INITIAL.
    PERFORM f_crt_fcat_0100.
    gs_layo-sel_mode = 'A'.

    CREATE OBJECT gr_cnt
      EXPORTING
        container_name = 'CC_0100'.

    CREATE OBJECT gr_alv
      EXPORTING
        i_parent      = gr_cnt
        i_appl_events = 'X'.

    gr_alv->set_table_for_first_display(
      EXPORTING is_layout       = gs_layo
      CHANGING  it_outtab       = gt_list
                it_fieldcatalog = gt_fc ).

    CREATE OBJECT gr_hand.

    SET HANDLER gr_hand->hand_tool_bar FOR gr_alv.
    gr_alv->set_toolbar_interactive( ).
    SET HANDLER gr_hand->hand_cmd FOR gr_alv.
    SET HANDLER gr_hand->hand_dblclk FOR gr_alv.

  ELSE.
    gr_alv->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.
ENDMODULE.                 " ALV_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'. "OK_0200 EXIT_0200
  SET TITLEBAR  'TITLE_0200' WITH gv_title200." &
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE gv_ok.
    WHEN 'OK_0200'.
      IF ( rcgfiletr-ftfront IS INITIAL AND rcgfiletr-ftappl IS INITIAL )
      OR rcgfiletr-ftftype IS INITIAL.
        MESSAGE 'FILL OBLIGATORY FIELDS' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'EXIT_0200'.
      MESSAGE 'Canceled action' TYPE 'S'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
MODULE clear_ok OUTPUT.
  CLEAR: gv_ok.
ENDMODULE.                 " CLEAR_OK  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE change_0200 OUTPUT.
  IF gv_title200 EQ 'UPLOAD'.
    LOOP AT SCREEN.
      IF screen-name = 'RCGFILETR-FTFRONT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF gv_title200 EQ 'DOWNLOAD'.
    LOOP AT SCREEN.
      IF screen-name = 'RCGFILETR-FTAPPL'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " CHANGE_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_UPLOAD  INPUT
*&---------------------------------------------------------------------*
MODULE f4_upload INPUT.
  DATA: lv_action TYPE i,
        lv_rc     TYPE i,
        ls_file   LIKE LINE OF gt_file.
  CLEAR: lv_action, gt_file, lv_rc, ls_file.

  cl_gui_frontend_services=>file_open_dialog(
  EXPORTING window_title   = 'FILE(S) TO UPLOAD'
            multiselection = 'X'
  CHANGING  file_table     = gt_file
            rc             = lv_rc
            user_action    = lv_action ).
  IF lv_action EQ 0 AND lv_rc EQ 1.
    READ TABLE gt_file INTO ls_file INDEX 1.
    rcgfiletr-ftappl = ls_file.
  ELSEIF lv_action EQ 0 AND lv_rc GT 1.
    rcgfiletr-ftappl = lv_rc.
    CONDENSE rcgfiletr-ftappl.
    CONCATENATE rcgfiletr-ftappl 'FILES SELECTED'
    INTO rcgfiletr-ftappl SEPARATED BY space.
  ELSE.
    MESSAGE 'Canceled action' TYPE 'S'.
  ENDIF.
ENDMODULE.                 " F4_UPLOAD  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_DOWNLOAD  INPUT
*&---------------------------------------------------------------------*
MODULE f4_download INPUT.
  DATA: lv_folder TYPE string.
  CLEAR: lv_folder.

  cl_gui_frontend_services=>directory_browse(
  EXPORTING window_title = 'DOWNLOAD DIRECTORY'
  CHANGING selected_folder = lv_folder ).
  IF lv_folder IS NOT INITIAL.
    rcgfiletr-ftfront = lv_folder.
  ENDIF.
ENDMODULE.                 " F4_DOWNLOAD  INPUT


**********************************************************************
*** Forms ************************************************************
**********************************************************************
*&---------------------------------------------------------------------*
*&      Form  FILL_GT_FILE_LIST
*&---------------------------------------------------------------------*
FORM fill_gt_file_list2
         USING    p_path     LIKE dxfields-longpath
                  p_filemask LIKE dxfields-filemask.

  DATA: l_errcnt(2) TYPE p VALUE 0.

  gt_file_list-rec_level = g_rec_level_120.

  CALL 'C_DIR_READ_FINISH'             " just to be sure
      ID 'ERRNO'  FIELD gt_file_list-errno
      ID 'ERRMSG' FIELD gt_file_list-errmsg.

  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD p_path
                          ID 'FILE'   FIELD p_filemask
                          ID 'ERRNO'  FIELD g_file-errno
                          ID 'ERRMSG' FIELD g_file-errmsg.
  IF sy-subrc <> 0.
*    MESSAGE e204(s_dx_bapi) WITH gt_file_list-errmsg g_file-errmsg.
    MESSAGE s204(s_dx_bapi) WITH gt_file_list-errmsg g_file-errmsg
    DISPLAY LIKE 'E'.
    gv_subrc = 1.
  ENDIF.

  DO.
    CLEAR g_file.
    g_file-rec_level = g_rec_level_120.
    CALL 'C_DIR_READ_NEXT'
      ID 'TYPE'   FIELD g_file-type
      ID 'NAME'   FIELD g_file-name
      ID 'LEN'    FIELD g_file-len
      ID 'OWNER'  FIELD g_file-owner
      ID 'MTIME'  FIELD g_file-mtime
      ID 'MODE'   FIELD g_file-mode
      ID 'ERRNO'  FIELD g_file-errno
      ID 'ERRMSG' FIELD g_file-errmsg.
    g_file-dirname = p_path.
    MOVE sy-subrc TO g_file-subrc.
    CASE sy-subrc.
      WHEN 0.
        CLEAR: g_file-errno, g_file-errmsg.
        CASE g_file-type(1).
          WHEN 'F'.                    " normal file.
            MOVE c_true  TO g_file-usable.
          WHEN 'f'.                    " normal file.
            MOVE c_true  TO g_file-usable.
          WHEN OTHERS. " directory, device, fifo, socket,...
            MOVE c_false TO g_file-usable.
        ENDCASE.
        IF g_file-len = 0.
          MOVE c_false TO g_file-usable.
        ENDIF.
        IF ( g_file-type(1) = 'D' ) OR
           ( g_file-type(1) = 'd' ).
          g_file-dir_flag = c_true.
        ELSE.
          g_file-dir_flag = c_false.
        ENDIF.
      WHEN 1.
        EXIT.
      WHEN OTHERS.                     " SY-SUBRC >= 2
        ADD 1 TO l_errcnt.
        IF l_errcnt > 10.
          EXIT.
        ENDIF.
        IF sy-subrc = 5.
          MOVE: '???' TO g_file-type,
                '???' TO g_file-owner,
                '???' TO g_file-mode.
        ENDIF.
        MOVE c_false TO g_file-usable.
    ENDCASE.
    PERFORM p6_to_date_time_tz(rstr0400) USING g_file-mtime
                                               g_file-mod_time
                                               g_file-mod_date.
    MOVE-CORRESPONDING g_file TO gt_file_list.
    APPEND gt_file_list.
  ENDDO.

  CALL 'C_DIR_READ_FINISH'
      ID 'ERRNO'  FIELD gt_file_list-errno
      ID 'ERRMSG' FIELD gt_file_list-errmsg.

  SORT gt_file_list STABLE BY rec_level DESCENDING
                              name      ASCENDING
                              mtime     DESCENDING.

ENDFORM.                               " FILL_GT_FILE_LIST2
*&---------------------------------------------------------------------*
**&      Form  F_GET_FILE_LIST
**&---------------------------------------------------------------------*
FORM f_get_file_list USING path TYPE dxfields-longpath.
  FIELD-SYMBOLS <list> TYPE gty_list.
  CLEAR: gt_file_list[], gt_file_list, gv_subrc.
  UNASSIGN <list>.

  PERFORM fill_gt_file_list2 USING path '*.*'.
  IF gv_subrc NE 0 OR gt_file_list[] IS INITIAL.
    gv_subrc = 1.
  ELSE.
    CLEAR gt_list[].
    gt_list[] = gt_file_list[].
    DELETE gt_list WHERE name EQ '.'
                        OR    name EQ '..'.
    SORT gt_list BY name.
    LOOP AT gt_list ASSIGNING <list>.
      IF <list>-type = 'directory'.
        <list>-icon = icon_open_folder.
      ELSE.
        <list>-icon = icon_office_document.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_GET_FILE_LIST
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_LEAVE_PROGRAM
*&---------------------------------------------------------------------*
FORM f_popup_to_leave_program USING p_title.

  DATA: lv_ans(1) TYPE c.
  CLEAR: lv_ans.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = p_title
      text_question         = 'DO YOU WANT LEAVE THE PROGRAM?'
      text_button_1         = 'YES'(001)
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'NO'(002)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = ''
    IMPORTING
      answer                = lv_ans
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF lv_ans EQ '1'."YES
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " F_POPUP_TO_LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  F_BACK_PATH
*&---------------------------------------------------------------------*
FORM f_back_path.
  DATA: lt_path  LIKE TABLE OF gv_path,
        lv_path2 LIKE gv_path.

  CLEAR: lt_path, lv_path2.

  DO.
    SPLIT gv_path AT '/' INTO lv_path2 gv_path.
    IF lv_path2 NE ''.
      APPEND lv_path2 TO lt_path.
    ELSEIF gv_path EQ '' AND lv_path2 EQ ''.
      EXIT.
    ENDIF.
    CLEAR lv_path2.
  ENDDO.

  LOOP AT lt_path INTO lv_path2.
    AT LAST.
      CONCATENATE gv_path '/' INTO gv_path.
      EXIT.
    ENDAT.
    CONCATENATE gv_path '/' lv_path2 INTO gv_path.
    CLEAR: lv_path2.
  ENDLOOP.

  IF gv_path IS INITIAL.
    MESSAGE 'Error to get the path' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " F_BACK_PATH
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
FORM f_crt_fcat_0100.
  DATA ls_fc TYPE lvc_s_fcat.

  CLEAR: ls_fc, gt_fc.

  ls_fc-fieldname = 'ICON'.
  ls_fc-ref_table = 'ICON'.
  ls_fc-ref_field = 'NAME'.
  ls_fc-coltext   = 'TYPE'.
  ls_fc-icon      = 'X'.
  ls_fc-key       = 'X'.
  ls_fc-outputlen = 5.
  APPEND ls_fc TO gt_fc.
  CLEAR ls_fc.

  ls_fc-fieldname = 'NAME'.
  ls_fc-inttype  = 'C'.
  ls_fc-intlen   = '255'.
  ls_fc-coltext   = 'FILE NAME'.
  ls_fc-key       = 'X'.
  ls_fc-outputlen = 35.
  APPEND ls_fc TO gt_fc.
  CLEAR ls_fc.

  ls_fc-fieldname = 'LEN'.
  ls_fc-inttype  = 'C'.
  ls_fc-intlen   = '20'.
  ls_fc-coltext   = 'LENGTH'.
  ls_fc-outputlen = 20.
  APPEND ls_fc TO gt_fc.
  CLEAR ls_fc.

  ls_fc-fieldname = 'OWNER'.
  ls_fc-inttype  = 'C'.
  ls_fc-intlen   = '10'.
  ls_fc-coltext   = 'OWNER'.
  ls_fc-outputlen = 11.
  APPEND ls_fc TO gt_fc.
  CLEAR ls_fc.

  ls_fc-fieldname = 'MOD_DATE'.
  ls_fc-inttype  = 'D'.
  ls_fc-coltext   = 'MOD.DATE'.
  ls_fc-outputlen = 11.
  APPEND ls_fc TO gt_fc.
  CLEAR ls_fc.

  ls_fc-fieldname = 'MOD_TIME'.
  ls_fc-inttype  = 'C'.
  ls_fc-intlen   = '8'.
  ls_fc-coltext   = 'MOD.TIME'.
  ls_fc-outputlen = 10.
  APPEND ls_fc TO gt_fc.
  CLEAR ls_fc.

ENDFORM.                    " F_CRT_FCAT_0100
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD
*&---------------------------------------------------------------------*
FORM f_download USING p_name.
  TYPES lty_xline(1024) TYPE x.
  DATA: BEGIN OF ls_rcg,
         ftappl  TYPE string,
         ftfront TYPE string,
        END OF ls_rcg.
  DATA: ls_file LIKE LINE OF gt_file,
        lit_bin  TYPE TABLE OF lty_xline,
        lwa_bin  TYPE lty_xline,
        lv_msg TYPE bapi_msg,
        lit_asc TYPE TABLE OF string,
        lwa_asc TYPE string,
        lv_curr  TYPE i,
        lv_tot   TYPE i.
  CLEAR: ls_rcg, ls_file, lit_bin, lwa_bin, lv_msg, lit_asc, lwa_asc,
         lv_curr, lv_tot, gv_subrc.

  TRANSLATE gv_path TO LOWER CASE."----
  CONCATENATE gv_path p_name INTO ls_rcg-ftappl.

  IF rcgfiletr-ftfront CS '\'.
    CONCATENATE rcgfiletr-ftfront '\' p_name INTO ls_rcg-ftfront.
  ELSE.
    CONCATENATE rcgfiletr-ftfront '/' p_name INTO ls_rcg-ftfront.
  ENDIF.

  CASE rcgfiletr-ftftype.
    WHEN 'ASC'.
      DO 500 TIMES.
        OPEN DATASET ls_rcg-ftappl FOR INPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc EQ 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF sy-subrc NE 0.
        gv_subrc = 4.
        CONCATENATE gc_m03 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      DO.
        TRY.
            READ DATASET ls_rcg-ftappl INTO lwa_asc." ACTUAL LENGTH lv_curr.
          CATCH cx_root.
            gv_subrc = 99.
            EXIT.
        ENDTRY.
        IF sy-subrc = 0.
          APPEND lwa_asc TO lit_asc.
        ELSE.
          EXIT.
        ENDIF.
        CLEAR lwa_asc.
      ENDDO.
      IF gv_subrc = 99.
        CONCATENATE gc_m04 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CLOSE DATASET ls_rcg-ftappl.
      IF sy-subrc NE 0.
        gv_subrc = 4.
        CONCATENATE gc_m03 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
*         bin_filesize            = lv_tot
          filename                = ls_rcg-ftfront
          filetype                = 'ASC'
        CHANGING
          data_tab                = lit_asc
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
      IF sy-subrc <> 0.
        gv_subrc = 4.
        CONCATENATE gc_m02 ls_rcg-ftfront INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.


    WHEN 'BIN'.
      DO 500 TIMES.
        OPEN DATASET ls_rcg-ftappl FOR INPUT IN BINARY MODE.
        IF sy-subrc EQ 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF sy-subrc NE 0.
        gv_subrc = 4.
        CONCATENATE gc_m03 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      DO.
        TRY.
            READ DATASET ls_rcg-ftappl INTO lwa_bin ACTUAL LENGTH lv_curr.
          CATCH cx_root.
            gv_subrc = 99.
            EXIT.
        ENDTRY.
        IF sy-subrc = 0 or lwa_bin  is NOT INITIAL.
          APPEND lwa_bin TO lit_bin.
          ADD lv_curr TO lv_tot. "Suma de peso del archivo
        ELSE.
*          APPEND lwa_bin TO lit_bin.                         <- RSDK907627
*          ADD lv_curr TO lv_tot."Suma de peso del archivo    <- RSDK907627
          EXIT.
        ENDIF.
        CLEAR lwa_bin.
      ENDDO.
      IF gv_subrc = 99.
        CONCATENATE gc_m04 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CLOSE DATASET ls_rcg-ftappl.
      IF sy-subrc NE 0.
        gv_subrc = 4.
        CONCATENATE gc_m03 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize            = lv_tot
          filename                = ls_rcg-ftfront
          filetype                = 'BIN'
        CHANGING
          data_tab                = lit_bin
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
      IF sy-subrc <> 0.
        gv_subrc = 4.
        CONCATENATE gc_m02 ls_rcg-ftfront INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

  ENDCASE.
ENDFORM.                    " F_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
FORM f_upload.
  TYPES lty_xline(1024) TYPE x.
  DATA: BEGIN OF ls_rcg,
         ftappl  TYPE string,
         ftfront TYPE string,
        END OF ls_rcg.
  DATA: ls_file LIKE LINE OF gt_file,
        lit_bin  TYPE TABLE OF lty_xline,
        lwa_bin  TYPE lty_xline,
        lv_msg TYPE bapi_msg,
        lit_asc TYPE TABLE OF string,
        lwa_asc TYPE string.
  CLEAR: ls_rcg, ls_file, lit_bin, lwa_bin, lv_msg, lit_asc, lwa_asc.

  CASE rcgfiletr-ftftype.
    WHEN 'ASC'.
      LOOP AT gt_file INTO ls_file.
        PERFORM f_get_ftappl USING ls_file CHANGING ls_rcg-ftappl.
        ls_rcg-ftfront = ls_file.

        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = ls_rcg-ftfront
            filetype                = 'ASC'
          CHANGING
            data_tab                = lit_asc
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.
        IF sy-subrc <> 0.
          CONCATENATE gc_m01 ls_rcg-ftfront INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        "Elimina el arhcivo en caso de que ya exista.
        DELETE DATASET ls_rcg-ftappl.

        DO 500 TIMES."Por tiempos
          OPEN DATASET ls_rcg-ftappl FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
          IF sy-subrc EQ 0.
            EXIT.
          ENDIF.
        ENDDO.
        IF sy-subrc NE 0.
          CONCATENATE gc_m02 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT lit_asc INTO lwa_asc.
          TRANSFER lwa_asc TO ls_rcg-ftappl.
          CLEAR lwa_asc.
        ENDLOOP.

        CLOSE DATASET ls_rcg-ftappl.
        IF sy-subrc NE 0.
          CONCATENATE gc_m02 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        CLEAR: ls_file, ls_rcg-ftfront, ls_rcg-ftappl, lit_asc,
               lv_msg.
      ENDLOOP.

    WHEN 'BIN'.
      LOOP AT gt_file INTO ls_file.
        PERFORM f_get_ftappl USING ls_file CHANGING ls_rcg-ftappl.
        ls_rcg-ftfront = ls_file.

        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = ls_rcg-ftfront
            filetype                = 'BIN'
          CHANGING
            data_tab                = lit_bin
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.
        IF sy-subrc <> 0.
          CONCATENATE gc_m01 ls_rcg-ftfront INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        "Elimina el arhcivo en caso de que ya exista.
        DELETE DATASET ls_rcg-ftappl.

        DO 500 TIMES."Por tiempos
          OPEN DATASET ls_rcg-ftappl FOR OUTPUT IN BINARY MODE.
          IF sy-subrc EQ 0.
            EXIT.
          ENDIF.
        ENDDO.
        IF sy-subrc NE 0.
          CONCATENATE gc_m02 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT lit_bin INTO lwa_bin.
          TRANSFER lwa_bin TO ls_rcg-ftappl.
          CLEAR lwa_bin.
        ENDLOOP.

        CLOSE DATASET ls_rcg-ftappl.
        IF sy-subrc NE 0.
          CONCATENATE gc_m02 ls_rcg-ftappl INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        CLEAR: ls_file, ls_rcg-ftfront, ls_rcg-ftappl, lit_bin,
               lv_msg.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " F_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_DELETE
*&---------------------------------------------------------------------*
FORM f_popup_to_delete.
  DATA: lv_ans(1) TYPE c.
  CLEAR: lv_ans, gv_subrc.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'DELETE'
      text_question         = 'ARE YOU SURE YOU WANT TO DELETE THE '
      &
      'SELECTED FILE(S)?'
      text_button_1         = 'YES'(001)
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'NO'(002)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = ''
    IMPORTING
      answer                = lv_ans
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF lv_ans NE '1'."YES
    gv_subrc = 4.
  ENDIF.
ENDFORM.                    " F_POPUP_TO_DELETE
*&---------------------------------------------------------------------*
*&      Form  F_DELETE
*&---------------------------------------------------------------------*
FORM f_delete USING p_name.
  DATA: lv_path LIKE gv_path,
        lv_msg  TYPE bapi_msg.
  CLEAR: lv_path, lv_msg, gv_subrc.

  CONCATENATE gv_path p_name INTO lv_path.

  OPEN DATASET lv_path FOR INPUT IN BINARY MODE.
  IF sy-subrc = 0.
    CLOSE DATASET lv_path.
    DELETE DATASET lv_path.
    IF sy-subrc <> 0.
      CONCATENATE 'ERROR WHILE DELETE' p_name INTO lv_msg
      SEPARATED BY ' '.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      gv_subrc = 5.
    ENDIF.
  ELSE.
    CONCATENATE p_name 'FILE DOES NOT EXIST' INTO lv_msg
    SEPARATED BY ' '.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    gv_subrc = 5.
  ENDIF.

ENDFORM.                    " F_DELETE



**********************************************************************
*** Report Events ****************************************************
**********************************************************************
START-OF-SELECTION.
* Get initial list file from root
  gv_path = '/'.
  PERFORM f_get_file_list USING gv_path.
  CHECK gv_subrc EQ 0.
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Form  F_GET_FTAPPL
*&---------------------------------------------------------------------*
FORM f_get_ftappl  USING    p_file
                   CHANGING p_ftappl.
  DATA: BEGIN OF lt_file OCCURS 0,
        str TYPE c LENGTH 100,
        END OF lt_file.
  DATA lv_indx TYPE i.
  CLEAR: lt_file[], lt_file, lv_indx, p_ftappl.

  IF p_file CS '/'.
    SPLIT p_file AT '/' INTO TABLE lt_file.
  ELSEIF p_file CS '\'.
    SPLIT p_file AT '\' INTO TABLE lt_file.
  ENDIF.

  CHECK lt_file[] IS NOT INITIAL.

  DESCRIBE TABLE lt_file[] LINES lv_indx.

  READ TABLE lt_file INDEX lv_indx.
  IF sy-subrc EQ 0.
    TRANSLATE gv_path TO LOWER CASE.
    CONCATENATE gv_path lt_file INTO p_ftappl.
  ENDIF.

ENDFORM.                    " F_GET_FTAPPL
