*&---------------------------------------------------------------------*
*&  Include           ZMM0120_CONS_HIST_PROY_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_F4_PROY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PROY  text
*----------------------------------------------------------------------*
FORM f_f4_proy CHANGING p_proy TYPE zmmtt_hist_part-proyecto.

  CLEAR: git_proy, git_proy[].

  SELECT proyecto desc_proyecto FROM zmmtt_hist_part INTO TABLE git_proy.

  SORT git_proy BY desc_proyecto.

  DELETE ADJACENT DUPLICATES FROM git_proy COMPARING proyecto desc_proyecto.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PROYECTO'
      window_title    = 'Catalogo de proyectos'
      value_org       = 'S'
    TABLES
      value_tab       = git_proy[]
      return_tab      = it_match_proy
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    p_proy = it_match_proy-fieldval.
  ELSE.
    MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                                                    " F_F4_PROY
*&---------------------------------------------------------------------*
*&      Form  F_F4_E_PEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SO_E_PEP_LOW  text
*----------------------------------------------------------------------*
FORM f_f4_e_pep  CHANGING p_so_e_pep.


  CLEAR: git_e_pep, git_e_pep[].

  DATA: BEGIN OF lit_e_pep OCCURS 0,
        elemento_pep TYPE zmmtt_hist_part-elemento_pep,
        post1 TYPE prps-post1,
        END OF lit_e_pep.

  DATA: BEGIN OF it_e_pep OCCURS 0,
      e_pep TYPE zmmtt_hist_part-elemento_pep,
      END OF it_e_pep.

  DATA: lv_e_pep TYPE zmmtt_hist_part-elemento_pep.


  IF so_proy IS NOT INITIAL.

    SELECT proyecto elemento_pep
    INTO TABLE git_e_pep
    FROM zmmtt_hist_part
       WHERE proyecto IN so_proy
         AND clas_partida IN so_cp
         AND clas_caratula IN so_cc
         AND proveedor IN so_prov.

    SORT git_e_pep[] BY elemento_pep.

    DELETE ADJACENT DUPLICATES FROM git_e_pep COMPARING proyecto elemento_pep.

    LOOP AT git_e_pep.
      SPLIT git_e_pep-elemento_pep AT '-' INTO TABLE it_e_pep.
      LOOP AT it_e_pep.
        CONCATENATE lv_e_pep it_e_pep-e_pep INTO lv_e_pep.
      ENDLOOP.

      IF lv_e_pep+0(2) = 'PA'.
        CONCATENATE lv_e_pep+0(13) lv_e_pep+13(2) INTO lv_e_pep SEPARATED BY ' '.
      ENDIF.

      git_e_pep-e_pep  = lv_e_pep.
      MODIFY git_e_pep.
      CLEAR: it_e_pep[], it_e_pep, lv_e_pep.
    ENDLOOP.

    SELECT posid post1 FROM  prps INTO TABLE lit_e_pep
      FOR ALL ENTRIES IN git_e_pep[]
      WHERE posid = git_e_pep-e_pep.

    LOOP AT git_e_pep.
      READ TABLE lit_e_pep WITH KEY elemento_pep = git_e_pep-e_pep.
      IF sy-subrc = 0.
        git_e_pep-post1 = lit_e_pep-post1.
        MODIFY git_e_pep.
      ENDIF.
      CLEAR: lit_e_pep.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ELEMENTO_PEP'
        window_title    = 'Catalogo de Elementos PEP'
        value_org       = 'S'
      TABLES
        value_tab       = git_e_pep[]
        return_tab      = it_match_e_pep
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      p_so_e_pep = it_match_e_pep-fieldval.
    ELSE.
      MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    MESSAGE text-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " F_F4_E_PEP
*&---------------------------------------------------------------------*
*&      Form  F_F4_CP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SO_CP_LOW  text
*----------------------------------------------------------------------*
FORM f_f4_cp  CHANGING p_cp.

  CLEAR: git_cp, git_cp[].


  IF so_proy IS NOT INITIAL.
    SELECT proyecto clas_partida FROM zmmtt_hist_part INTO TABLE git_cp
       WHERE proyecto IN so_proy
         AND elemento_pep IN so_e_pep
*         AND clas_partida IN so_cp
         AND clas_caratula IN so_cc
         AND proveedor IN so_prov.

    SORT git_cp BY clas_partida.

    DELETE ADJACENT DUPLICATES FROM git_cp COMPARING proyecto clas_partida.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CLAS_PARTIDA'
        window_title    = 'Catalogo de Clasificación de la Partida'
        value_org       = 'S'
      TABLES
        value_tab       = git_cp[]
        return_tab      = it_match_cp
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      p_cp = it_match_cp-fieldval.
    ELSE.
      MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    MESSAGE text-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                                                    " F_F4_CP
*&---------------------------------------------------------------------*
*&      Form  F_F4_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SO_CC_LOW  text
*----------------------------------------------------------------------*
FORM f_f4_cc  CHANGING p_cc.

  CLEAR: git_cc, git_cc[].


  IF so_proy IS NOT INITIAL.
    SELECT proyecto clas_caratula FROM zmmtt_hist_part INTO TABLE git_cc
       WHERE proyecto IN so_proy
         AND elemento_pep IN so_e_pep
         AND clas_partida IN so_cp
*         AND clas_caratula IN so_cc
         AND proveedor IN so_prov.

    SORT git_cc BY clas_caratula.

    DELETE ADJACENT DUPLICATES FROM git_cc COMPARING proyecto clas_caratula.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CLAS_CARATULA'
        window_title    = 'Catalogo de Clasificación de la Caratula'
        value_org       = 'S'
      TABLES
        value_tab       = git_cc[]
        return_tab      = it_match_cc
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      p_cc = it_match_cc-fieldval.
    ELSE.
      MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    MESSAGE text-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                                                    " F_F4_CC
*&---------------------------------------------------------------------*
*&      Form  F_F4_PROVEEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SO_PROV_HIGH  text
*----------------------------------------------------------------------*
FORM f_f4_proveedor  CHANGING p_prov.

  CLEAR: git_prov, git_prov[].


  IF so_proy IS NOT INITIAL.
    SELECT proyecto proveedor n_proveedor FROM zmmtt_hist_part INTO TABLE git_prov
       WHERE proyecto IN so_proy
         AND elemento_pep IN so_e_pep
         AND clas_partida IN so_cp
         AND clas_caratula IN so_cc.
*         AND proveedor IN so_prov.

    SORT git_prov BY n_proveedor.

    DELETE ADJACENT DUPLICATES FROM git_prov COMPARING proyecto n_proveedor proveedor.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'PROVEEDOR'
        window_title    = 'Catalogo de Proveedores'
        value_org       = 'S'
      TABLES
        value_tab       = git_prov[]
        return_tab      = it_match_prov
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      p_prov = it_match_prov-fieldval.
    ELSE.
      MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    MESSAGE text-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " F_F4_PROVEEDOR
*&---------------------------------------------------------------------*
*&      Form  F_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_authority_check .

  AUTHORITY-CHECK OBJECT 'Z_PSGGI01'
        ID 'ACTVT' FIELD '02'.

  IF sy-subrc = 0.
    gv_edicion = 'X'.
    gv_edit = 'X'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'Z_PSGGI01'
         ID 'ACTVT' FIELD '03'.
  IF sy-subrc = 0.
    gv_visual = 'X'.
  ENDIF.

  IF gv_edicion IS INITIAL
  AND gv_visual IS INITIAL.
    MESSAGE text-003 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " F_AUTORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  F_RECUPERA_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_recupera_registros.

  DATA: BEGIN OF lit_e_pep OCCURS 0,
       elemento_pep TYPE zmmtt_hist_part-elemento_pep,
       END OF lit_e_pep.

  DATA: BEGIN OF lit_prov OCCURS 0,
       proveedor TYPE zmmtt_hist_part-proveedor,
       END OF lit_prov.

  DATA: BEGIN OF lit_cc OCCURS 0,
       clas_caratula TYPE zmmtt_hist_part-clas_caratula,
       END OF lit_cc.

  DATA: BEGIN OF lit_cp OCCURS 0,
        clas_partida TYPE zmmtt_hist_part-clas_partida,
        END OF lit_cp.

  DATA: lwa_hist_part LIKE LINE OF git_hist_part.

  CLEAR: git_hist_part, git_t_e_pep[], git_t_e_pep, git_t_prov[], git_t_prov,
         git_t_cc[], git_t_cc, git_t_cp[], git_t_cp.

  SELECT * FROM zmmtt_hist_part INTO CORRESPONDING FIELDS OF TABLE git_hist_part
  WHERE proyecto IN so_proy
  AND elemento_pep IN so_e_pep
  AND clas_partida IN so_cp
  AND clas_caratula IN so_cc
  AND proveedor IN so_prov.

  LOOP AT git_hist_part INTO lwa_hist_part.
    lit_e_pep-elemento_pep = lwa_hist_part-elemento_pep.
    lit_prov-proveedor = lwa_hist_part-proveedor.
    lit_cc-clas_caratula = lwa_hist_part-clas_caratula.
    lit_cp-clas_partida = lwa_hist_part-clas_partida.

    APPEND lit_e_pep.
    APPEND lit_prov.
    APPEND lit_cc.
    APPEND lit_cp.

    CLEAR: lit_e_pep, lit_prov, lit_cc, lit_cp.
  ENDLOOP.

  SORT lit_e_pep[].
  SORT lit_prov[].
  SORT lit_cc[].
  SORT lit_cp[].

  DELETE ADJACENT DUPLICATES FROM lit_e_pep[].
  DELETE ADJACENT DUPLICATES FROM lit_prov[].
  DELETE ADJACENT DUPLICATES FROM lit_cc[].
  DELETE ADJACENT DUPLICATES FROM lit_cp[].

  CLEAR: lwa_hist_part.

  IF git_e_pep[] IS INITIAL.
    PERFORM f_get_post1.
  ENDIF.

  IF git_prov[] IS INITIAL.
    PERFORM f_get_proveedores.
  ENDIF.

  SORT git_hist_part BY elemento_pep.
  LOOP AT lit_e_pep.

    git_t_e_pep-elemento_pep = lit_e_pep-elemento_pep.

    READ TABLE git_e_pep WITH KEY elemento_pep = lit_e_pep-elemento_pep.
    IF sy-subrc = 0.
      git_t_e_pep-post1 = git_e_pep-post1.
    ENDIF.

    LOOP AT git_hist_part INTO lwa_hist_part
    WHERE elemento_pep = lit_e_pep-elemento_pep.
      git_t_e_pep-importe = git_t_e_pep-importe + lwa_hist_part-importe.
    ENDLOOP.

    APPEND git_t_e_pep.
    CLEAR: lwa_hist_part, git_t_e_pep.
  ENDLOOP.

  SORT git_hist_part BY proveedor.
  LOOP AT lit_prov.
    git_t_prov-proveedor = lit_prov-proveedor.

    LOOP AT git_hist_part INTO lwa_hist_part
    WHERE proveedor = lit_prov-proveedor.
      IF     git_t_prov-n_proveedor IS INITIAL.
        git_t_prov-n_proveedor = lwa_hist_part-n_proveedor.
      ENDIF.
      git_t_prov-importe = git_t_prov-importe + lwa_hist_part-importe.
    ENDLOOP.

    APPEND git_t_prov.
    CLEAR: lwa_hist_part, git_t_prov.
  ENDLOOP.

  SORT git_hist_part BY clas_caratula.
  LOOP AT lit_cc.
    git_t_cc-clas_caratula = lit_cc-clas_caratula.

    LOOP AT git_hist_part INTO lwa_hist_part
    WHERE clas_caratula = lit_cc-clas_caratula.
      git_t_cc-importe = git_t_cc-importe + lwa_hist_part-importe.
    ENDLOOP.

    APPEND git_t_cc.
    CLEAR: lwa_hist_part, git_t_cc.
  ENDLOOP.

  SORT git_hist_part BY clas_partida.
  LOOP AT lit_cp.
    git_t_cp-clas_partida = lit_cp-clas_partida.

    LOOP AT git_hist_part INTO lwa_hist_part
    WHERE clas_partida = lit_cp-clas_partida.
      git_t_cp-importe = git_t_cp-importe + lwa_hist_part-importe.
    ENDLOOP.

    APPEND git_t_cp.
    CLEAR: lwa_hist_part, git_t_cp.
  ENDLOOP.

ENDFORM.                    " F_RECUPERA_REGISTROS
*&---------------------------------------------------------------------*
*&      Form  F_GET_POST1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_E_PEP  text
*----------------------------------------------------------------------*
FORM f_get_post1.

  DATA: BEGIN OF lit_e_pep OCCURS 0,
          elemento_pep TYPE zmmtt_hist_part-elemento_pep,
          post1 TYPE prps-post1,
          END OF lit_e_pep.

  DATA: BEGIN OF it_e_pep OCCURS 0,
      e_pep TYPE zmmtt_hist_part-elemento_pep,
      END OF it_e_pep.

  DATA: lv_e_pep TYPE zmmtt_hist_part-elemento_pep.

  SELECT proyecto elemento_pep
  INTO TABLE git_e_pep
  FROM zmmtt_hist_part
     WHERE proyecto IN so_proy
       AND clas_partida IN so_cp
       AND clas_caratula IN so_cc
       AND proveedor IN so_prov.

  SORT git_e_pep[] BY elemento_pep.

  DELETE ADJACENT DUPLICATES FROM git_e_pep COMPARING proyecto elemento_pep.

  LOOP AT git_e_pep.
    SPLIT git_e_pep-elemento_pep AT '-' INTO TABLE it_e_pep.
    LOOP AT it_e_pep.
      CONCATENATE lv_e_pep it_e_pep-e_pep INTO lv_e_pep.
    ENDLOOP.
    git_e_pep-e_pep  = lv_e_pep.

    IF git_e_pep-e_pep+0(2) = 'PA'.
      CONCATENATE git_e_pep-e_pep+0(13) git_e_pep-e_pep+13(2) INTO git_e_pep-e_pep SEPARATED BY ' '.
    ENDIF.

    MODIFY git_e_pep.
    CLEAR: it_e_pep[], it_e_pep, lv_e_pep.
  ENDLOOP.

  SELECT posid post1 FROM  prps INTO TABLE lit_e_pep
    FOR ALL ENTRIES IN git_e_pep[]
    WHERE posid = git_e_pep-e_pep.

  LOOP AT git_e_pep.
    READ TABLE lit_e_pep WITH KEY elemento_pep = git_e_pep-e_pep.
    IF sy-subrc = 0.
      git_e_pep-post1 = lit_e_pep-post1.
      MODIFY git_e_pep.
    ENDIF.
    CLEAR: lit_e_pep.
  ENDLOOP.

ENDFORM.                    " F_GET_POST1
*&---------------------------------------------------------------------*
*&      Form  F_CREA_ALV_0100_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_alv_0100_1 .

  IF gr_alv100_1 IS INITIAL AND gr_cnt100_1 IS INITIAL.
    PERFORM f_crt_fcat_0100_1.
    PERFORM f_layout_alv_100_1.

    CREATE OBJECT gr_cnt100_1
      EXPORTING
        container_name = 'CC_0100_1'.

    CREATE OBJECT gr_alv100_1
      EXPORTING
        i_parent = gr_cnt100_1.

    gr_alv100_1->set_table_for_first_display(
   EXPORTING is_layout       = gs_layo100_1
*               it_toolbar_excluding  = git_exclude
   CHANGING  it_outtab       = git_t_e_pep[]
             it_fieldcatalog = gt_fc100_1 ).


    CREATE OBJECT gr_alv100dc_1.
    SET HANDLER gr_alv100dc_1->hand_dblclk FOR gr_alv100_1.

  ELSE.
*    Se refresca tabla
    gr_alv100_1->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.

ENDFORM.                    " F_CREA_ALV_0100_1
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0100_1 .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc100_1.

  CLEAR: gt_fc100_1, gwa_fc100_1.

  gwa_fc100_1-fieldname = 'POST1'.
  gwa_fc100_1-tabname   = 'GIT_T_E_PEP'.
  gwa_fc100_1-outputlen = '20'.
  gwa_fc100_1-edit      = ''.
  APPEND gwa_fc100_1 TO gt_fc100_1.
  CLEAR gwa_fc100_1 .

  gwa_fc100_1-fieldname = 'ELEMENTO_PEP'.
  gwa_fc100_1-tabname   = 'GIT_T_E_PEP'.
  gwa_fc100_1-outputlen = '25'.
  gwa_fc100_1-edit      = ''.
  APPEND gwa_fc100_1 TO gt_fc100_1.
  CLEAR gwa_fc100_1 .

  gwa_fc100_1-fieldname = 'IMPORTE'.
  gwa_fc100_1-tabname   = 'GIT_T_E_PEP'.
  gwa_fc100_1-outputlen = '18'.
  gwa_fc100_1-edit      = ''.
  APPEND gwa_fc100_1 TO gt_fc100_1.
  CLEAR gwa_fc100_1 .

  READ TABLE gt_fc100_1 ASSIGNING <fc> WITH KEY fieldname = 'POST1'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Descripción'.
    <fc>-scrtext_l = 'Descripción'.
    <fc>-scrtext_s = 'Descripción'.
    <fc>-scrtext_m = 'Descripción'.
  ENDIF.

  READ TABLE gt_fc100_1 ASSIGNING <fc> WITH KEY fieldname = 'ELEMENTO_PEP'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Elemento_PEP'.
    <fc>-scrtext_l = 'Elemento_PEP'.
    <fc>-scrtext_s = 'Elemento_PEP'.
    <fc>-scrtext_m = 'Elemento_PEP'.
  ENDIF.

  READ TABLE gt_fc100_1 ASSIGNING <fc> WITH KEY fieldname = 'IMPORTE'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Total X Elemento_PEP'.
    <fc>-scrtext_l = 'Total X Elemento_PEP'.
    <fc>-scrtext_s = 'Total X Elemento_PEP'.
    <fc>-scrtext_m = 'Total X Elemento_PEP'.
  ENDIF.


ENDFORM.                    " F_CRT_FCAT_0100_1
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV_100_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv_100_1 .

  CLEAR gs_layo100_1.
  gs_layo100_1-no_rowmark = x.

ENDFORM.                    " F_LAYOUT_ALV_100_1
*&---------------------------------------------------------------------*
*&      Form  F_GET_PROVEEDORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_proveedores .

ENDFORM.                    " F_GET_PROVEEDORES
*&---------------------------------------------------------------------*
*&      Form  F_CREA_ALV_0100_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_alv_0100_2 .

  IF gr_alv100_2 IS INITIAL AND gr_cnt100_2 IS INITIAL.
    PERFORM f_crt_fcat_0100_2.
    PERFORM f_layout_alv_100_2.

    CREATE OBJECT gr_cnt100_2
      EXPORTING
        container_name = 'CC_0100_2'.

    CREATE OBJECT gr_alv100_2
      EXPORTING
        i_parent = gr_cnt100_2.

    gr_alv100_2->set_table_for_first_display(
   EXPORTING is_layout       = gs_layo100_2
*               it_toolbar_excluding  = git_exclude
   CHANGING  it_outtab       = git_t_prov[]
             it_fieldcatalog = gt_fc100_2 ).

    CREATE OBJECT gr_alv100dc_2.
    SET HANDLER gr_alv100dc_2->hand_dblclk FOR gr_alv100_2.

  ELSE.
*    Se refresca tabla
    gr_alv100_2->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.

ENDFORM.                    " F_CREA_ALV_0100_2
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0100_2 .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc100_2.

  CLEAR: gt_fc100_2, gwa_fc100_2.

  gwa_fc100_2-fieldname = 'N_PROVEEDOR'.
  gwa_fc100_2-tabname   = 'GIT_T_PROV'.
  gwa_fc100_2-outputlen = '40'.
  gwa_fc100_2-edit      = ''.
  APPEND gwa_fc100_2 TO gt_fc100_2.
  CLEAR gwa_fc100_2 .

  gwa_fc100_2-fieldname = 'PROVEEDOR'.
  gwa_fc100_2-tabname   = 'GIT_T_PROV'.
  gwa_fc100_2-outputlen = '20'.
  gwa_fc100_2-edit      = ''.
  APPEND gwa_fc100_2 TO gt_fc100_2.
  CLEAR gwa_fc100_2 .

  gwa_fc100_2-fieldname = 'IMPORTE'.
  gwa_fc100_2-tabname   = 'GIT_T_PROV'.
  gwa_fc100_2-outputlen = '20'.
  gwa_fc100_2-edit      = ''.
  APPEND gwa_fc100_2 TO gt_fc100_2.
  CLEAR gwa_fc100_2 .



  READ TABLE gt_fc100_2 ASSIGNING <fc> WITH KEY fieldname = 'N_PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Nombre del Proveedor'.
    <fc>-scrtext_l = 'Nombre del Proveedor'.
    <fc>-scrtext_s = 'Nombre del Proveedor'.
    <fc>-scrtext_m = 'Nombre del Proveedor'.
  ENDIF.

  READ TABLE gt_fc100_2 ASSIGNING <fc> WITH KEY fieldname = 'PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Núm. Proveedor'.
    <fc>-scrtext_l = 'Núm. Proveedor'.
    <fc>-scrtext_s = 'Núm. Proveedor'.
    <fc>-scrtext_m = 'Núm. Proveedor'.
  ENDIF.

  READ TABLE gt_fc100_2 ASSIGNING <fc> WITH KEY fieldname = 'IMPORTE'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Total X Proveedor'.
    <fc>-scrtext_l = 'Total X Proveedor'.
    <fc>-scrtext_s = 'Total X Proveedor'.
    <fc>-scrtext_m = 'Total X Proveedor'.
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0100_2
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV_100_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv_100_2 .

  CLEAR gs_layo100_2.
  gs_layo100_2-no_rowmark = x.

ENDFORM.                    " F_LAYOUT_ALV_100_2
*&---------------------------------------------------------------------*
*&      Form  F_CREA_ALV_0100_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_alv_0100_3 .

  IF gr_alv100_3 IS INITIAL AND gr_cnt100_3 IS INITIAL.
    PERFORM f_crt_fcat_0100_3.
    PERFORM f_layout_alv_100_3.

    CREATE OBJECT gr_cnt100_3
      EXPORTING
        container_name = 'CC_0100_3'.

    CREATE OBJECT gr_alv100_3
      EXPORTING
        i_parent = gr_cnt100_3.

    gr_alv100_3->set_table_for_first_display(
   EXPORTING is_layout       = gs_layo100_3
*               it_toolbar_excluding  = git_exclude
   CHANGING  it_outtab       = git_t_cc[]
             it_fieldcatalog = gt_fc100_3 ).

    CREATE OBJECT gr_alv100dc_3.
    SET HANDLER gr_alv100dc_3->hand_dblclk FOR gr_alv100_3.

  ELSE.
*    Se refresca tabla
    gr_alv100_3->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.

ENDFORM.                    " F_CREA_ALV_0100_3
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0100_3 .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc100_3.

  CLEAR: gt_fc100_3, gwa_fc100_3.

  gwa_fc100_3-fieldname = 'CLAS_CARATULA'.
  gwa_fc100_3-tabname   = 'GIT_T_CC'.
  gwa_fc100_3-outputlen = '30'.
  gwa_fc100_3-edit      = ''.
  APPEND gwa_fc100_3 TO gt_fc100_3.
  CLEAR gwa_fc100_3 .

  gwa_fc100_3-fieldname = 'IMPORTE'.
  gwa_fc100_3-tabname   = 'GIT_T_CC'.
  gwa_fc100_3-outputlen = '20'.
  gwa_fc100_3-edit      = ''.
  APPEND gwa_fc100_3 TO gt_fc100_3.
  CLEAR gwa_fc100_3 .


  READ TABLE gt_fc100_3 ASSIGNING <fc> WITH KEY fieldname = 'CLAS_CARATULA'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Clasificación de la Caratula'.
    <fc>-scrtext_l = 'Clasificación de la Caratula'.
    <fc>-scrtext_s = 'Clas. Caratula'.
    <fc>-scrtext_m = 'Clas. Caratula'.
  ENDIF.

  READ TABLE gt_fc100_3 ASSIGNING <fc> WITH KEY fieldname = 'IMPORTE'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Total X Clas. Cararula'.
    <fc>-scrtext_l = 'Total X Clas. Cararula'.
    <fc>-scrtext_s = 'Total X Clas. Cararula'.
    <fc>-scrtext_m = 'Total X Clas. Cararula'.
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0100_3
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV_100_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv_100_3 .

  CLEAR gs_layo100_3.
  gs_layo100_3-no_rowmark = x.

ENDFORM.                    " F_LAYOUT_ALV_100_3
*&---------------------------------------------------------------------*
*&      Form  F_CREA_ALV_0100_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crea_alv_0100_4 .

  IF gr_alv100_4 IS INITIAL AND gr_cnt100_4 IS INITIAL.
    PERFORM f_crt_fcat_0100_4.
    PERFORM f_layout_alv_100_4.

    CREATE OBJECT gr_cnt100_4
      EXPORTING
        container_name = 'CC_0100_4'.

    CREATE OBJECT gr_alv100_4
      EXPORTING
        i_parent = gr_cnt100_4.

    gr_alv100_4->set_table_for_first_display(
   EXPORTING is_layout       = gs_layo100_4
*               it_toolbar_excluding  = git_exclude
   CHANGING  it_outtab       = git_t_cp[]
             it_fieldcatalog = gt_fc100_4 ).

    CREATE OBJECT gr_alv100dc_4.
    SET HANDLER gr_alv100dc_4->hand_dblclk FOR gr_alv100_4.

  ELSE.
*    Se refresca tabla
    gr_alv100_4->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.

ENDFORM.                    " F_CREA_ALV_0100_4
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0100_4 .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc100_4.

  CLEAR: gt_fc100_4, gwa_fc100_4.

  gwa_fc100_4-fieldname = 'CLAS_PARTIDA'.
  gwa_fc100_4-tabname   = 'GIT_T_CP'.
  gwa_fc100_4-outputlen = '25'.
  gwa_fc100_4-edit      = ''.
  APPEND gwa_fc100_4 TO gt_fc100_4.
  CLEAR gwa_fc100_4 .

  gwa_fc100_4-fieldname = 'IMPORTE'.
  gwa_fc100_4-tabname   = 'GIT_T_CP'.
  gwa_fc100_4-outputlen = '20'.
  gwa_fc100_4-edit      = ''.
  APPEND gwa_fc100_4 TO gt_fc100_4.
  CLEAR gwa_fc100_4 .


  READ TABLE gt_fc100_4 ASSIGNING <fc> WITH KEY fieldname = 'CLAS_PARTIDA'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Clasificación de la Partida'.
    <fc>-scrtext_l = 'Clasificación de la Partida'.
    <fc>-scrtext_s = 'Clas. Partida'.
    <fc>-scrtext_m = 'Clas. Partida'.
  ENDIF.

  READ TABLE gt_fc100_4 ASSIGNING <fc> WITH KEY fieldname = 'IMPORTE'.
  IF sy-subrc EQ 0.
    <fc>-reptext = 'Total X Clas. Partida'.
    <fc>-scrtext_l = 'Total X Clas. Partida'.
    <fc>-scrtext_s = 'Total X Clas. Partida'.
    <fc>-scrtext_m = 'Total X Clas. Partida'.
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0100_4
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV_100_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv_100_4 .

  CLEAR gs_layo100_4.
  gs_layo100_4-no_rowmark = x.

ENDFORM.                    " F_LAYOUT_ALV_100_4
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_alv_0200.

  IF gr_alv200 IS INITIAL AND gr_cnt200 IS INITIAL.
    PERFORM f_crt_fcat_0200.
    PERFORM f_layout_alv_200.

    CREATE OBJECT gr_cnt200
      EXPORTING
        container_name = 'CC_0200'.

    CREATE OBJECT gr_alv200
      EXPORTING
        i_parent = gr_cnt200.

    gr_alv200->set_table_for_first_display(
   EXPORTING is_layout       = gs_layo200
*               it_toolbar_excluding  = git_exclude
   CHANGING  it_outtab       = git_nivel_detalle[]
             it_fieldcatalog = gt_fc200 ).

  ELSE.
*    Se refresca tabla
    gr_alv200->refresh_table_display( is_stable = gs_stbl ).
  ENDIF.

ENDFORM.                    " F_CREATE_ALV_0200
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0200 .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc200.

  CLEAR: gt_fc200.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       I_BUFFER_ACTIVE           =
        i_structure_name            = 'ZMMTT_HIST_PART'
        i_client_never_display      = 'X'
*      I_BYPASSING_BUFFER         =
     CHANGING
        ct_fieldcat                 = gt_fc200
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'PROYECTO'.
  IF sy-subrc EQ 0.
    <fc>-no_out = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'DESC_PROYECTO'.
  IF sy-subrc EQ 0.
    <fc>-no_out = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'T_REGISTRO'.
  IF sy-subrc EQ 0.
    <fc>-no_out = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'ELEMENTO_PEP'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '25'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '11'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'FACTURA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '18'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'CLAS_CARATULA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '20'.
    <fc>-lowercase = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'CLAS_PARTIDA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '20'.
    <fc>-lowercase = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'F_CONTABILIZACION'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '11'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'N_PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '30'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'D_FACTURA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '30'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'T_CABECERO'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '30'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'IMPORTE'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '20'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'MONEDA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '6'.
  ENDIF.
ENDFORM.                    " F_CRT_FCAT_0200
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv_200 .

  CLEAR gs_layo200.
  gs_layo200-no_rowmark = x.

ENDFORM.                    " F_LAYOUT_ALV_200
*&---------------------------------------------------------------------*
*&      Form  EDIT_ALV_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_alv_0200 .

*  DATA: lwa_nivel_detalle LIKE LINE OF git_nivel_detalle.

  IF gr_cnt200 IS NOT INITIAL.
    CALL METHOD gr_cnt200->free.
    CLEAR: gr_cnt200, gr_alv200.
  ENDIF.

  CLEAR: git_nivel_detalle2[], git_nivel_detalle2, git_nivel_detalle.

  LOOP AT git_nivel_detalle.
    git_nivel_detalle-res_factura = git_nivel_detalle-factura.
    MODIFY git_nivel_detalle[] FROM git_nivel_detalle INDEX sy-tabix.
  ENDLOOP.

  git_nivel_detalle2[] = git_nivel_detalle[].

  gv_edit = ' '.

  PERFORM f_crt_fcat_0200_edit.
  PERFORM f_layout_alv_200.

  CREATE OBJECT gr_cnt200
    EXPORTING
      container_name = 'CC_0200'.

  CREATE OBJECT gr_alv200
    EXPORTING
      i_parent = gr_cnt200.

  gr_alv200->set_table_for_first_display(
 EXPORTING is_layout       = gs_layo200
*               it_toolbar_excluding  = git_exclude
 CHANGING  it_outtab       = git_nivel_detalle[]
           it_fieldcatalog = gt_fc200 ).

  gr_alv200->register_edit_event(
   EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

  gr_alv200->register_edit_event( "Se registra evento Enter para ALV
  EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

ENDFORM.                    " EDIT_ALV_0200
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0200_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crt_fcat_0200_edit .

  FIELD-SYMBOLS <fc> LIKE LINE OF gt_fc200.

  CLEAR: gt_fc200.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       I_BUFFER_ACTIVE           =
        i_structure_name            = 'ZMMTT_HIST_PART'
        i_client_never_display      = 'X'
*      I_BYPASSING_BUFFER         =
     CHANGING
        ct_fieldcat                 = gt_fc200
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'PROYECTO'.
  IF sy-subrc EQ 0.
    <fc>-no_out = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'DESC_PROYECTO'.
  IF sy-subrc EQ 0.
    <fc>-no_out = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'T_REGISTRO'.
  IF sy-subrc EQ 0.
    <fc>-no_out = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'ELEMENTO_PEP'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '25'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '11'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'FACTURA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '18'.
    <fc>-lowercase = 'X'.
    <fc>-edit = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'CLAS_CARATULA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '20'.
    <fc>-lowercase = 'X'.
    <fc>-edit = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'CLAS_PARTIDA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '20'.
    <fc>-edit = 'X'.
    <fc>-lowercase = 'X'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'F_CONTABILIZACION'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '11'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'N_PROVEEDOR'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '30'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'D_FACTURA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '30'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'T_CABECERO'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '30'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'IMPORTE'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '20'.
  ENDIF.

  READ TABLE gt_fc200 ASSIGNING <fc> WITH KEY fieldname = 'MONEDA'.
  IF sy-subrc EQ 0.
    <fc>-outputlen = '6'.
  ENDIF.

ENDFORM.                    " F_CRT_FCAT_0200_EDIT
*&---------------------------------------------------------------------*
*&      Form  SAVE_ALV_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_alv_0200 .

  DATA: lwa_nivel_detalle2 LIKE LINE OF git_nivel_detalle2,
        lv_mensaje TYPE bapi_msg.

  CLEAR: git_log_hist_p, gwa_nivel_detalle.
  gv_edit = 'X'.

  IF git_nivel_detalle2[] = git_nivel_detalle[].
    MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.

    LOOP AT git_nivel_detalle INTO gwa_nivel_detalle.

*      IF gwa_nivel_detalle-factura = gwa_nivel_detalle-res_factura.
      READ TABLE git_nivel_detalle2 INTO lwa_nivel_detalle2
        WITH KEY proyecto = gwa_nivel_detalle-proyecto
                 elemento_pep = gwa_nivel_detalle-elemento_pep
                 proveedor = gwa_nivel_detalle-proveedor
                 res_factura = gwa_nivel_detalle-res_factura.

      IF lwa_nivel_detalle2 <> gwa_nivel_detalle.

        CLEAR: zmmtt_hist_part.

        zmmtt_hist_part-proyecto = gwa_nivel_detalle-proyecto.
        zmmtt_hist_part-elemento_pep  = gwa_nivel_detalle-elemento_pep.
        zmmtt_hist_part-proveedor  = gwa_nivel_detalle-proveedor.
        zmmtt_hist_part-factura  = gwa_nivel_detalle-res_factura.
        zmmtt_hist_part-desc_proyecto  = gwa_nivel_detalle-desc_proyecto.
        zmmtt_hist_part-clas_caratula = gwa_nivel_detalle-clas_caratula.
        zmmtt_hist_part-t_registro  = gwa_nivel_detalle-t_registro.
        zmmtt_hist_part-clas_partida  = gwa_nivel_detalle-clas_partida.
        zmmtt_hist_part-f_contabilizacion  = gwa_nivel_detalle-f_contabilizacion.
        zmmtt_hist_part-n_proveedor  = gwa_nivel_detalle-n_proveedor.
        zmmtt_hist_part-d_factura  = gwa_nivel_detalle-d_factura.
        zmmtt_hist_part-t_cabecero  = gwa_nivel_detalle-t_cabecero.
        zmmtt_hist_part-importe  = gwa_nivel_detalle-importe.
        zmmtt_hist_part-moneda  = gwa_nivel_detalle-moneda.

        IF gwa_nivel_detalle-factura = gwa_nivel_detalle-res_factura.

          IF lwa_nivel_detalle2-clas_partida <> gwa_nivel_detalle-clas_partida.
            PERFORM f_reg_log USING gwa_nivel_detalle-clas_partida
                                    lwa_nivel_detalle2-clas_partida
                                    'CLAS_PARTIDA'.
          ENDIF.

          IF lwa_nivel_detalle2-clas_caratula <> gwa_nivel_detalle-clas_caratula.
            PERFORM f_reg_log USING  gwa_nivel_detalle-clas_caratula
                                     lwa_nivel_detalle2-clas_caratula
                                     'CLAS_CARATULA'.
          ENDIF.

          MODIFY zmmtt_hist_part FROM zmmtt_hist_part.

          READ TABLE git_hist_part TRANSPORTING NO FIELDS
                                   WITH KEY proyecto = gwa_nivel_detalle-proyecto
                                        elemento_pep = gwa_nivel_detalle-elemento_pep
                                           proveedor = gwa_nivel_detalle-proveedor
                                             factura = gwa_nivel_detalle-res_factura.

          MODIFY git_hist_part FROM zmmtt_hist_part INDEX sy-tabix.
          MESSAGE text-007 TYPE 'S'.
          IF sy-subrc EQ 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
        ELSE.
          READ TABLE git_hist_part TRANSPORTING NO FIELDS
                                   WITH KEY proyecto = zmmtt_hist_part-proyecto
                                        elemento_pep = zmmtt_hist_part-elemento_pep
                                           proveedor = zmmtt_hist_part-proveedor
                                             factura = gwa_nivel_detalle-factura.

          IF sy-subrc = 0.
            CONCATENATE 'Ya existe un registro con la llave Proyecto:' zmmtt_hist_part-proyecto
            ',Elemento PEP:' zmmtt_hist_part-elemento_pep
            ',Proveedor:' zmmtt_hist_part-proveedor
            ',Factura:' zmmtt_hist_part-factura INTO lv_mensaje.

            MESSAGE lv_mensaje TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSE.

            DELETE FROM zmmtt_hist_part WHERE proyecto   = zmmtt_hist_part-proyecto
                             AND elemento_pep = zmmtt_hist_part-elemento_pep
                             AND    proveedor = zmmtt_hist_part-proveedor
                             AND      factura = gwa_nivel_detalle-res_factura.

            MESSAGE text-007 TYPE 'S'.
            IF sy-subrc EQ 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.


            IF lwa_nivel_detalle2-clas_partida <> gwa_nivel_detalle-clas_partida.
              PERFORM f_reg_log USING gwa_nivel_detalle-clas_partida
                                      lwa_nivel_detalle2-clas_partida
                                      'CLAS_PARTIDA'.
            ENDIF.

            IF lwa_nivel_detalle2-clas_caratula <> gwa_nivel_detalle-clas_caratula.
              PERFORM f_reg_log USING  gwa_nivel_detalle-clas_caratula
                                       lwa_nivel_detalle2-clas_caratula
                                       'CLAS_CARATULA'.
            ENDIF.

            IF lwa_nivel_detalle2-factura <> gwa_nivel_detalle-factura.
              PERFORM f_reg_log USING  gwa_nivel_detalle-factura
                                       lwa_nivel_detalle2-factura
                                       'FACTURA'.
            ENDIF.

            zmmtt_hist_part-factura = gwa_nivel_detalle-factura.


            INSERT zmmtt_hist_part FROM zmmtt_hist_part.
            MESSAGE text-007 TYPE 'S'.
            IF sy-subrc EQ 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.

            DELETE git_hist_part WHERE proyecto   = zmmtt_hist_part-proyecto
                                 AND elemento_pep = zmmtt_hist_part-elemento_pep
                                 AND proveedor = zmmtt_hist_part-proveedor
                                 AND factura = gwa_nivel_detalle-res_factura.
            MESSAGE text-007 TYPE 'S'.
            IF sy-subrc EQ 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.
            APPEND zmmtt_hist_part TO git_hist_part.
          ENDIF.
        ENDIF.

        CLEAR: lwa_nivel_detalle2.
      ENDIF.
*      ENDIF.
    ENDLOOP.
  ENDIF.

  IF git_log_hist_p IS NOT INITIAL.
    INSERT zmmtt_log_hist_p FROM TABLE git_log_hist_p.
  ENDIF.

  IF gr_cnt200 IS NOT INITIAL.
    CALL METHOD gr_cnt200->free.
    CLEAR: gr_cnt200, gr_alv200.
  ENDIF.

  PERFORM f_crt_fcat_0200.
  PERFORM f_layout_alv_200.

  CREATE OBJECT gr_cnt200
    EXPORTING
      container_name = 'CC_0200'.

  CREATE OBJECT gr_alv200
    EXPORTING
      i_parent = gr_cnt200.

  gr_alv200->set_table_for_first_display(
 EXPORTING is_layout       = gs_layo200
*               it_toolbar_excluding  = git_exclude
 CHANGING  it_outtab       = git_nivel_detalle[]
           it_fieldcatalog = gt_fc200 ).

ENDFORM.                    " SAVE_ALV_0200
*&---------------------------------------------------------------------*
*&      Form  F_REG_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_reg_log USING p_valor_actual
                     p_valor_anterior
                     p_campo.


  DATA: lwa_log LIKE LINE OF git_log_hist_p,
         v_id_ref    TYPE zid_referencia.

  PERFORM f_crt_id_ref CHANGING v_id_ref.

  lwa_log-id_reg_unico     = v_id_ref.
  lwa_log-proyecto         = gwa_nivel_detalle-proyecto.
  lwa_log-elemento_pep     = gwa_nivel_detalle-elemento_pep.
  lwa_log-proveedor        = gwa_nivel_detalle-proveedor.
  lwa_log-factura          = gwa_nivel_detalle-factura.
  lwa_log-campo_modificado = p_campo.
  lwa_log-valor_anterior   = p_valor_anterior.
  lwa_log-valor_actual     = p_valor_actual.
  lwa_log-ernam            = sy-uname. "Usuario firmado en SAP
  lwa_log-aedat            = sy-datum. "Fecha actual en servidor SAP
  lwa_log-cputm            = sy-uzeit. "Hora actual del servidor SAP


  APPEND lwa_log TO git_log_hist_p.
  CLEAR: lwa_log, v_id_ref.
  .

ENDFORM.                    " F_REG_LOG
*&---------------------------------------------------------------------*
*&      Form  F_CRT_ID_REF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_V_ID_REF  text
*----------------------------------------------------------------------*
FORM f_crt_id_ref  CHANGING p_v_id_ref  TYPE zid_referencia.

  DATA: lv_max TYPE zid_referencia,
          lv_id TYPE zid_referencia.

  CLEAR: p_v_id_ref, lv_max, lv_id.

  CONCATENATE sy-datum sy-uzeit '00' INTO p_v_id_ref.
  lv_max = p_v_id_ref + 99.

  SELECT SINGLE id_reg_unico FROM zmmtt_log_hist_p INTO lv_id
  WHERE id_reg_unico = p_v_id_ref.
  IF sy-subrc EQ 0.
    DO 1000 TIMES.
      ADD 1 TO p_v_id_ref.
      SELECT SINGLE id_reg_unico FROM zmmtt_log_hist_p INTO lv_id
  WHERE id_reg_unico = p_v_id_ref.
      IF sy-subrc NE 0.
        EXIT.
      ELSEIF p_v_id_ref EQ lv_max.
        WAIT UP TO 1 SECONDS.
        CONCATENATE sy-datum sy-uzeit '00' INTO p_v_id_ref.
        lv_max = p_v_id_ref + 99.
      ELSEIF sy-index EQ 1000.
        MESSAGE text-006 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.                    " F_CRT_ID_REF
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_refresh_0100 .

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

  PERFORM f_recupera_registros.
  PERFORM f_crea_alv_0100_1.
  PERFORM f_crea_alv_0100_2.
  PERFORM f_crea_alv_0100_3.
  PERFORM f_crea_alv_0100_4.

ENDFORM.                    " F_REFRESH_0100
* >>> RSDK907480 >>>
*&---------------------------------------------------------------------*
*&      Form  F_OBT_PROY_BLOQ
*&---------------------------------------------------------------------*
*   Recupera los proyectos connfigurados como bloqueados para su
*   edición en la Tcode ZMM0120
*----------------------------------------------------------------------*
FORM f_obt_proy_bloq .
  CLEAR: git_proy.

  SELECT zchar FROM zutil_parameters INTO TABLE git_bproy
  WHERE zreport EQ   'ZMM0120_CONS_HIST_PROY'
  AND   zfield  LIKE 'PROY_BLOQ%'.

ENDFORM.                    " F_OBT_PROY_BLOQ
* <<< RSDK907480 <<<
