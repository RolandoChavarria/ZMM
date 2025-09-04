*&---------------------------------------------------------------------*
*&      Form  F_CREATE_MATCHCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIFNR  text
*----------------------------------------------------------------------*
form f_create_matchcode.



* Se genera IT_LIFNR la cual contiene los proveedores existentes
* en la tabla LFA1 y que tambien existen en la tabla LFM1.

  select lfa1~lifnr lfa1~name1 lfa1~ort01 lfa1~regio
  into corresponding fields of table it_lifnr
  from lfa1
  inner join lfm1
  on lfa1~lifnr = lfm1~lifnr.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'LIFNR'
      window_title    = 'No. Proveedor'
      value_org       = 'S'
    tables
      value_tab       = it_lifnr
      return_tab      = it_match
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  if sy-subrc eq 0.
    read table it_match index 1.
    move it_match-fieldval to p_provdr.
  endif.

endform.                    " F_CREATE_MATCHCODE

*&---------------------------------------------------------------------*
*&      Form  F_GET_EBELN_FROM_PSPID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_PSPIDEBELN  text
*      -->P_P_PROYEC  text
*      -->P_P_PROVDR  text
*      -->P_P_PEDIDO  text
*----------------------------------------------------------------------*
form f_get_ebeln_from_pspid  tables   pit_pspidebeln structure zmmwa_pspidebelnlifnr
                             using    p_p_proyec
                                      p_p_provdr.

  loop at lit_zutil into lwa_zutil  where zfield cs 'DOC_TYPE'.
    lwa_doc_type-sign = 'I'.
    lwa_doc_type-option = 'EQ'.
    lwa_doc_type-low = lwa_zutil-zchar.
    append lwa_doc_type to lit_doc_type.
    clear: lwa_doc_type, lwa_zutil.
  endloop.
  if sy-subrc ne 0.
    message text-002 type 'E'.
    exit.
  endif.

  select verkf lifnr ebeln into table pit_pspidebeln
 from ekko
 where verkf = p_proyec
   and lifnr = p_provdr
   and bsart in lit_doc_type.


  if sy-subrc ne 0.
    message text-003 type 'S' display like 'E'.
    exit.
  endif.
endform.                    " F_GET_EBELN_FROM_PSPID
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_EBELN_FROM_PSPID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_SPPIDEBELN  text
*      -->P_P_PROYEC  text
*      -->P_P_PROVDR  text
*      -->P_P_PEDIDO  text
*----------------------------------------------------------------------*
form f_validate_ebeln_from_pspid  tables   pit_pspidebeln structure zmmwa_pspidebelnlifnr
                                  using    p_p_proyec
                                           p_p_provdr
                                           p_p_pedido.

  loop at lit_zutil into lwa_zutil  where zfield cs 'DOC_TYPE'.
    lwa_doc_type-sign = 'I'.
    lwa_doc_type-option = 'EQ'.
    lwa_doc_type-low = lwa_zutil-zchar.
    append lwa_doc_type to lit_doc_type.
    clear: lwa_doc_type, lwa_zutil.
  endloop.
  if sy-subrc ne 0.
    message text-002 type 'E'.
    exit.
  endif.

  select single ebeln into pit_pspidebeln
   from ekko
   where ebeln = p_pedido.

  if sy-subrc ne 0.
    message text-004 type 'S' display like 'E'.
    exit.
  endif.

  select single ebeln into pit_pspidebeln
   from ekko
   where ebeln = p_pedido
   and bsart in lit_doc_type.

  if sy-subrc ne 0.
    message text-005 type 'S' display like 'E'.
    exit.
  endif.


  select single verkf lifnr ebeln into pit_pspidebeln
  from ekko
  where ebeln = p_pedido
    and verkf = p_proyec
    and lifnr = p_provdr
    and bsart in lit_doc_type.

  if sy-subrc ne 0.
    message text-006 type 'S' display like 'E'.
    exit.
  else.
    append pit_pspidebeln to pit_pspidebeln.
  endif.

endform.                    " F_VALIDATE_EBELN_FROM_PSPID
*&---------------------------------------------------------------------*
*&      Form  F_GET_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GIT_PSPIDEBELN  text
*      -->P_IT_HEADER  text
*      -->P_IT_DETAIL  text
*----------------------------------------------------------------------*
form f_get_info  tables   pit_pspidebeln structure zmmwa_pspidebelnlifnr
                          pit_header structure zmmwa_0100_header_comp
                          pit_detail structure zmmwa_0100_detail_comp.

  data: it_ekko type ekko occurs 0 with header line.
  data: it_ekpo type ekpo occurs 0 with header line.
  data: it_ekbe type ekbe occurs 0 with header line.
  data: it_asmdt type asmdt occurs 0 with header line.
  data: lit_pocond type table of bapimepocond.
  data: wa_proj type proj.
  data: wa_t001 type t001.
  data: wa_header like line of it_header.
  data: wa_detail like line of it_detail.
  data: wa_pocond type bapimepocond.
  data: wa_ekbe type ekbe.
  data: wa_bkpf like line of it_bkpf.
  data: wa_awkey like line of it_awkey.
  data: lwa_zutil type zutil_parameters.

  data: v_retencion type bapikbetr1.
  data: v_amortizacion type bapikbetr1.
  data: v_porc type c length 5.
  data: v_porc2 type c length 5.
  data: v_porc_total type c length 5.
  data: v_lfbnr type mblnr.
  data: v_amort_a type zmmde_imp_amortiza.
  data: v_amort_fg type zmmde_imp_amortiza.
  data: v_anticipo_1 type c length 10.
  data: v_anticipo_2 type c length 10.


  perform f_generate_title changing wa_proj
                                    wa_t001
                                    v_title.

  perform f_rescue_info tables pit_pspidebeln
                                it_ekko
                                it_ekpo
                                it_ekbe
                                it_asmdt.



  read table lit_zutil into lwa_zutil with key zfield = 'FIL_ANTICIPO_01'.
  if lwa_zutil-zchar = ' '.
    message 'Falta configuracion en ZUTIL_PARAMETERS' type 'E'.
  elseif lwa_zutil-zchar is not initial.
    v_anticipo_1 = lwa_zutil-zchar.
  endif.
  clear: lwa_zutil.

  read table lit_zutil into lwa_zutil with key zfield = 'FIL_ANTICIPO_02'.
  if lwa_zutil-zchar = ' '.
    message 'Falta configuracion en ZUTIL_PARAMETERS' type 'E'.
  elseif lwa_zutil-zchar is not initial.
    v_anticipo_2 = lwa_zutil-zchar.
  endif.
  clear: lwa_zutil.
*--> Inicia Modificación RSDK906898 - 5
  clear gv_moneda.
*<-- Fin Modificación RSDK906898 - 5

  loop at pit_pspidebeln.


    read table it_ekko with key ebeln = pit_pspidebeln-ebeln into it_ekko.

    sort it_ekko by ebeln.

    read table it_ekpo with key ebeln = pit_pspidebeln-ebeln into it_ekpo.

    sort it_ekpo by ebeln ebelp.

    read table it_lfa1 with key lifnr = pit_pspidebeln-lifnr into it_lfa1.

    perform f_desc_ped   tables pit_pspidebeln
                         changing v_desc_ped.
*--> Inicia Modificación RSDK906898 - 6
    gv_moneda = it_ekko-waers.
*<-- Fin Modificación RSDK906898 - 6

    wa_header-pedido       =  it_ekko-ebeln.
    wa_header-desc_ped     =  v_desc_ped.
    wa_header-no_prov      =  it_ekko-lifnr.
    wa_header-desc_prov    =  it_lfa1-name1.

    loop at it_ekpo where ebeln = pit_pspidebeln-ebeln.
*--> Inicio Modificación RSDK906886 - 1.
      if it_ekpo-loekz eq space.
*<-- Fin Modificación RSDK906886 - 1.
        wa_header-contratado   = wa_header-contratado + it_ekpo-brtwr.
*--> Inicio Modificación RSDK906886 - 2.

*--> Inicio Modificación RSDK907131 - 1.
* IJOF : Se agrega en condición acumulado para campo CONT-RET.
        wa_header-neto_contratado    = wa_header-neto_contratado + it_ekpo-netpr.
*<-- Fin Modificación RSDK907131 - 1.

      endif.
*<-- Fin Modificación RSDK906886 - 2.

*--> Inicio Modificación RSDK907131 - 2.
* IJOF : Se comenta código.
*      wa_header-neto_contratado    = wa_header-neto_contratado + it_ekpo-netpr.
*<-- Fin Modificación RSDK907131 - 2.
    endloop.

    perform f_bapi_po_getdetail1 tables lit_pocond
                                  using pit_pspidebeln-ebeln.

    read table lit_pocond into wa_pocond
    with key cond_type = 'ZRET'.
    v_retencion = wa_pocond-cond_value.
    clear wa_pocond.

    read table lit_pocond into wa_pocond
    with key cond_type = 'ZAAN'.
    v_amortizacion = wa_pocond-cond_value.
    clear wa_pocond.

    wa_header-fondo_garan = ( wa_header-contratado * v_retencion ) / 100.
    wa_header-total_amortizar = ( wa_header-contratado * v_amortizacion ) / 100.


    loop at it_ekbe into wa_ekbe
      where belnr between v_anticipo_1 and v_anticipo_2
      and ebeln = pit_pspidebeln-ebeln
      and   vgabe = '4'.
* Se comenta linea de codigo y se agrega instruccion para que acumule anticipos
*      wa_header-anticipo = wa_ekbe-dmbtr.
      wa_header-anticipo = wa_header-anticipo + wa_ekbe-dmbtr.
      clear wa_ekbe.
*      EXIT.
    endloop.
    data: vl_movi type bwart.
    clear vl_movi.
    loop at it_ekbe where ebeln = it_ekpo-ebeln
*                    AND   ebelp = it_ekpo-ebelp
                    and   vgabe = '1'.

      vl_movi = it_ekbe-bwart.

      read table it_ekpo with key ebeln = it_ekbe-ebeln
                                  ebelp = it_ekbe-ebelp into it_ekpo.

      sort it_ekpo by ebeln ebelp.


      read table it_asmdt with key asnum = it_ekbe-srvpos.

      read table it_awkey with key ebeln  = it_ekbe-ebeln
                                   ebelp  = it_ekbe-ebelp
                                   srvpos = it_ekbe-srvpos
                                   belnr  = it_ekbe-belnr
                                   gjahr  = it_ekbe-gjahr.

      read table it_bkpf  with key awkey = it_awkey-awkey.

      read table it_bseg with key bukrs = it_bkpf-bukrs
                                  belnr = it_bkpf-belnr
                                  gjahr = it_bkpf-gjahr.
*** Comentar el tipo de movimiento 102
*** Ricardo Pichardo 23.04.2015

      if it_ekbe-bwart = '101'
      or it_ekbe-bwart = '102'.
        wa_detail-entrada = 'WE'.
        wa_detail-bwart = it_ekbe-bwart.
      endif.
      wa_detail-ebeln    = it_ekbe-ebeln.
      wa_detail-ebelp    = it_ekbe-ebelp.
      wa_detail-matkl    = it_ekpo-matkl.
      wa_detail-lfbnr    = it_ekbe-lfbnr.
      wa_detail-belnr    = it_ekbe-belnr.
      wa_detail-belnr_d  = it_bkpf-belnr.
      wa_detail-bldat    = it_bkpf-bldat.
      wa_detail-xblnr    = it_ekbe-xblnr.
      wa_detail-srvpos   = it_ekbe-srvpos.
      wa_detail-asktx    = it_asmdt-asktx.
      wa_detail-menge    = it_ekbe-menge.
      v_lfbnr            = it_ekbe-lfbnr.
      loop at it_bseg where bukrs = it_bkpf-bukrs
                       and  belnr = it_bkpf-belnr
                       and  gjahr = it_bkpf-gjahr.
        case it_bseg-ktosl.
          when 'KBS'.
            if it_ekbe-bwart = '102'.
              it_bseg-dmbtr = it_bseg-dmbtr * -1.
              wa_detail-g_dmbtr = it_bseg-dmbtr.
            else.
              wa_detail-g_dmbtr = it_bseg-dmbtr.
            endif.
          when 'ZE1'.
            if it_ekbe-bwart = '102'.
              it_bseg-dmbtr = it_bseg-dmbtr * -1.
              wa_detail-r_dmbtr = it_bseg-dmbtr.
            else.
              wa_detail-r_dmbtr = it_bseg-dmbtr.
            endif.
            v_amort_fg = v_amort_fg + it_bseg-dmbtr.
          when 'ZE2'.
            if it_ekbe-bwart = '102'.
              it_bseg-dmbtr = it_bseg-dmbtr * -1.
              wa_detail-a_dmbtr = it_bseg-dmbtr.
            else.
              wa_detail-a_dmbtr = it_bseg-dmbtr.
            endif.
            v_amort_a = v_amort_a + it_bseg-dmbtr.
          when 'WRX'.
            if it_ekbe-bwart = '102'.
              it_bseg-dmbtr = it_bseg-dmbtr * -1.
              wa_detail-rf_dmbtr = it_bseg-dmbtr.
            else.
              wa_detail-rf_dmbtr = it_bseg-dmbtr.
            endif.
        endcase.
      endloop.

      loop at it_ekbe into wa_ekbe where ebeln = it_ekpo-ebeln
                      and   lfbnr = v_lfbnr
                      and   vgabe = '2'.
        read table it_awkey into wa_awkey with key ebeln  = wa_ekbe-ebeln
                                                   ebelp  = wa_ekbe-ebelp
                                                   srvpos = wa_ekbe-srvpos
                                                   belnr  = wa_ekbe-belnr
                                                   gjahr  = wa_ekbe-gjahr.

        read table it_bkpf into wa_bkpf with key awkey = wa_awkey-awkey.

        read table it_konv with key mwsk1 = wa_ekbe-mwskz.

        read table it_konp with key knumh = it_konv-knumh
                                    kschl = it_konv-kschl.

        wa_detail-vf_belnr = wa_ekbe-belnr.
        wa_detail-fch_ver  = wa_ekbe-budat.
        wa_detail-dc_belnr = wa_bkpf-belnr.
        if wa_ekbe-shkzg = 'H'.
          wa_ekbe-dmbtr = wa_ekbe-dmbtr * -1.
          wa_detail-dmbtr    = wa_ekbe-dmbtr.
        else.
          wa_detail-dmbtr    = wa_ekbe-dmbtr.
        endif.

        v_porc = ( it_konp-kbetr / 10 ).
        condense v_porc.
        concatenate v_porc '%' into v_porc2 separated by ''.
        wa_detail-mwskz = v_porc2.

        v_porc_total = ( it_konp-kbetr / 1000 ) + 1.
        condense v_porc_total.

        wa_detail-total = ( wa_ekbe-dmbtr * v_porc_total ).

        if wa_ekbe-shkzg = 'H' and vl_movi = '102'.
          append wa_detail to it_detail.
        elseif wa_ekbe-shkzg = 'S' and vl_movi = '101'.
          append wa_detail to it_detail.
        elseif wa_ekbe-shkzg = 'H' and vl_movi = '101'.
          append wa_detail to it_detail.
        endif.
        clear: wa_ekbe, wa_bkpf, wa_ekbe, it_bseg, wa_awkey, v_porc, v_porc2,
               v_porc_total, it_konv, it_konp.
      endloop.
***      APPEND wa_detail TO it_detail.
      clear: it_ekbe, it_ekpo, it_asmdt, it_awkey, it_bkpf, it_bseg,
         wa_ekbe, wa_detail, v_lfbnr.

    endloop.

    wa_header-amort_acum = v_amort_a.
    wa_header-amort_fg   = v_amort_fg.

    append wa_header to it_header.

    clear: wa_header, wa_detail, it_lfa1, it_ekko, it_ekpo, lit_pocond,
      lit_pocond[], wa_pocond, v_amortizacion, v_retencion, v_amort_fg, v_amort_a.

  endloop.

  data: it_detail_aux type table of zmmwa_0100_detail_comp,
        wa_det_aux type zmmwa_0100_detail_comp.
  it_detail_aux[] = it_detail[].

  loop at it_detail into wa_detail.
    if wa_detail-bwart = '101'.
      append wa_detail to it_detail_aux.
    endif.
  endloop.
  sort it_detail_aux by lfbnr.
  delete adjacent duplicates from it_detail_aux comparing lfbnr.
  delete it_detail where bwart = '101'.
  loop at it_detail_aux into wa_det_aux.
    append wa_det_aux to it_detail.
  endloop.

  sort it_header by pedido.
  sort it_detail by ebeln xblnr.

  if it_header[] is not initial.

    call screen 0100.

  endif.

endform.                    " F_GET_INFO
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_PROV_PROY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_validate_prov_proy .

  data: lwa_proj type proj.

  select single * into lwa_proj
  from proj
    where pspid = p_proyec.

  if sy-subrc ne 0.
    message 'El proyecto no se encuentra registrado' type 'E'.
    exit.
  endif.

endform.                    " F_VALIDATE_PROV_PROY
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_PROV_PROY2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_validate_prov_proy2 .

  select single lfa1~lifnr lfa1~name1 lfa1~ort01 lfa1~regio
        into it_lifnr
        from lfa1
        inner join lfm1
        on lfa1~lifnr = lfm1~lifnr
        where lfa1~lifnr = p_provdr
        and lfm1~lifnr = p_provdr.

  if sy-subrc ne 0.
    message 'El proveedor indicado no se encuentra registrado' type 'E'.
    exit.
  endif.

endform.                    " F_VALIDATE_PROV_PROY2
*&---------------------------------------------------------------------*
*&      Form  F_GET_ZUTIL_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_zutil_parameters .

  select * into table lit_zutil
    from zutil_parameters
    where zreport = 'ZMM0100_INFO_COMP'.

endform.                    " F_GET_ZUTIL_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_MATCHCODE_P
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_create_matchcode_p .

  loop at lit_zutil into lwa_zutil  where zfield cs 'DOC_TYPE'.
    lwa_doc_type-sign = 'I'.
    lwa_doc_type-option = 'EQ'.
    lwa_doc_type-low = lwa_zutil-zchar.
    append lwa_doc_type to lit_doc_type.
    clear: lwa_doc_type, lwa_zutil.
  endloop.

  if sy-subrc ne 0.
    message 'Falta configuracion en ZUTIL_PARAMETERS' type 'I'.
    exit.
  else.
    select ebeln bukrs bsart into corresponding fields of table it_ebeln
   from ekko
   where verkf = p_proyec
     and lifnr = p_provdr
     and bsart in lit_doc_type.
  endif.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'EBELN'
      window_title    = 'No. Pedido'
      value_org       = 'S'
    tables
      value_tab       = it_ebeln
      return_tab      = it_match_p
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  if sy-subrc eq 0.
    read table it_match_p index 1.
    move it_match_p-fieldval to p_pedido.
  endif.

endform.                    " F_CREATE_MATCHCODE_P
*&---------------------------------------------------------------------*
*&      Form  G_GENERATE_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_PROJ  text
*      <--P_WA_T001  text
*      <--P_V_TITLE  text
*----------------------------------------------------------------------*
form f_generate_title  changing wa_proj type proj
                                wa_t001 type t001
                                v_title.

  data v_name1 type name1.


  select single pspnr post1 vbukr into corresponding fields of  wa_proj
    from proj
    where pspid = p_proyec.

  select single bukrs butxt into corresponding fields of wa_t001
  from t001
  where bukrs = wa_proj-vbukr.

  if sy-subrc = 0.
    concatenate 'Proyecto:' p_proyec '-' wa_proj-post1  into v_title  separated by '  '.
    concatenate 'Sociedad:' wa_t001-bukrs '-'
     wa_t001-butxt into v_sociedad separated by '  '.
    .
  endif.


endform.                    " G_GENERATE_TITLE
*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EKKO  text
*      -->P_IT_LFA1  text
*----------------------------------------------------------------------*
form f_rescue_info tables pit_pspidebeln structure zmmwa_pspidebelnlifnr
                          pit_ekko structure ekko
                          pit_ekpo structure ekpo
                          pit_ekbe structure ekbe
                          pit_asmdt structure asmdt.
  data: wa_ekbe type ekbe.
*& --->Inicio RSDK907327
  data: it_ekbe_aux type standard table of ekbe.
*& <---Fin    RSDK907327


  if pit_pspidebeln[] is not initial.
    select * into corresponding fields of table pit_ekko
    from ekko for all entries in pit_pspidebeln
    where verkf = p_proyec
    and   lifnr = p_provdr
    and   ebeln = pit_pspidebeln-ebeln
    and   bsart in lit_doc_type.
  endif.

  if pit_ekko[] is not initial.
    select lifnr name1 into corresponding fields of table it_lfa1
    from lfa1 for all entries in pit_ekko
    where lifnr = pit_ekko-lifnr.
  endif.

  if pit_pspidebeln[] is not initial.
    select * into corresponding fields of table pit_ekpo
    from ekpo for all entries in pit_pspidebeln
    where ebeln = pit_pspidebeln-ebeln.
  endif.

  if pit_ekpo[] is not initial.
    select * into corresponding fields of table pit_ekbe
    from ekbe for all entries in pit_ekpo
    where ebeln = pit_ekpo-ebeln.
  endif.

  sort pit_ekbe by ebeln ebelp srvpos.

  loop at pit_ekbe into wa_ekbe.
    it_awkey-ebeln  = wa_ekbe-ebeln.
    it_awkey-ebelp  = wa_ekbe-ebelp.
    it_awkey-srvpos = wa_ekbe-srvpos.
    it_awkey-belnr  = wa_ekbe-belnr.
    it_awkey-gjahr  = wa_ekbe-gjahr.
    concatenate wa_ekbe-belnr wa_ekbe-gjahr into it_awkey-awkey.
    append it_awkey.
    clear: it_awkey, wa_ekbe.
  endloop.

  if it_awkey[] is not initial.
    select bukrs belnr gjahr awkey bldat from bkpf into corresponding fields of table it_bkpf
        for all entries in it_awkey
        where awkey = it_awkey-awkey.
  endif.

  sort it_bkpf by bukrs belnr gjahr.

  if pit_ekbe[] is not initial.
    select * from asmdt into corresponding fields of table pit_asmdt
       for all entries in pit_ekbe
       where asnum = pit_ekbe-srvpos.
  endif.

  if it_bkpf[] is not initial.

* Quick Fix Replace SELECT from table BSEG by API Call
* 02.07.2025 17:07:44 LTORRES
* Transport RESK900081 ATC - 2
* Replaced Code:
*    select bukrs belnr gjahr
*           shkzg dmbtr pswsl
*           ktosl from bseg into corresponding fields of table it_bseg
*        for all entries in it_bkpf
*        where bukrs eq it_bkpf-bukrs
*        and   belnr eq it_bkpf-belnr
*        and   gjahr eq it_bkpf-gjahr
*        and (  ktosl eq  'KBS'
*        or     ktosl eq  'ZE2'
*        or     ktosl eq  'ZE1'
*        or     ktosl eq  'WRX' ).

DATA ETL672C4R4443 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L672C4R3626 TYPE FAGL_T_FIELD.
LT_FIELDS_L672C4R3626 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBTR' )
 ( LINE = 'PSWSL' )
 ( LINE = 'KTOSL' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BKPF[]
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND KTOSL EQ 'KBS' OR KTOSL EQ 'ZE2' OR KTOSL EQ 'ZE1' OR KTOSL EQ 'WRX'|
              IT_FIELDLIST = LT_FIELDS_L672C4R3626
    IMPORTING ET_BSEG = ETL672C4R4443
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL672C4R4443 ) > 0.
  CLEAR IT_BSEG.
  TYPES: BEGIN OF TYL672C4R1897,
    BUKRS TYPE BSEG-BUKRS,
    BELNR TYPE BSEG-BELNR,
    GJAHR TYPE BSEG-GJAHR,
    SHKZG TYPE BSEG-SHKZG,
    DMBTR TYPE BSEG-DMBTR,
    PSWSL TYPE BSEG-PSWSL,
    KTOSL TYPE BSEG-KTOSL,
  END OF TYL672C4R1897.
  DATA: LML672C4R526 TYPE TYL672C4R1897,
        LWL672C4R2272 LIKE LINE OF IT_BSEG.
  LOOP AT ETL672C4R4443 REFERENCE INTO DATA(LDRL672C4R7804).
    LML672C4R526-BUKRS = LDRL672C4R7804->BUKRS.
    LML672C4R526-BELNR = LDRL672C4R7804->BELNR.
    LML672C4R526-GJAHR = LDRL672C4R7804->GJAHR.
    LML672C4R526-SHKZG = LDRL672C4R7804->SHKZG.
    LML672C4R526-DMBTR = LDRL672C4R7804->DMBTR.
    LML672C4R526-PSWSL = LDRL672C4R7804->PSWSL.
    LML672C4R526-KTOSL = LDRL672C4R7804->KTOSL.
    MOVE-CORRESPONDING LML672C4R526 TO LWL672C4R2272.
    APPEND LWL672C4R2272 TO IT_BSEG.
  ENDLOOP.
  SY-DBCNT = LINES( ETL672C4R4443 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.

* End of Quick Fix

  endif.

*& --->Inicio RSDK907327
  it_ekbe_aux[] = pit_ekbe[].
  delete it_ekbe_aux where mwskz eq space.

  if it_ekbe_aux[] is not initial.

* Quick Fix Replace KONV table access with access of table PRCD_ELEMENTS
* 02.07.2025 17:07:44 LTORRES
* Transport RESK900081 ATC - 2
* Replaced Code:
*    select knumh mwsk1 kschl from konv into table it_konv
*      for all entries in it_ekbe_aux
*      where mwsk1 = it_ekbe_aux-mwskz.

select knumh mwsk1 kschl from PRCD_ELEMENTS into table it_konv
      for all entries in it_ekbe_aux
      where mwsk1 = it_ekbe_aux-mwskz.

* End of Quick Fix

  endif.
*& <---Fin    RSDK907327
  if it_konv[] is not initial.
    select knumh kschl kbetr from konp into corresponding fields of table it_konp
      for all entries in it_konv
      where knumh = it_konv-knumh
      and   kschl = it_konv-kschl.
  endif.
endform.                    " F_GENERATE_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_DESC_PED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PIT_PSPIDEBELN  text
*      <--P_V_DESC_PED  text
*----------------------------------------------------------------------*
form f_desc_ped    tables pit_pspidebeln structure zmmwa_pspidebelnlifnr
                 changing v_desc_ped.

  data: lit_tlines type standard table of tline.
  field-symbols: <fs_tlines> like line of lit_tlines.
  data: v_text1 type tdline.
  data: v_text2 type tdline.
  data: v_ebeln type thead-tdname.

  v_ebeln = pit_pspidebeln-ebeln.


  call function 'READ_TEXT'
    exporting
     client                         = sy-mandt
      id                            = 'F01'
      language                      = sy-langu
      name                          = v_ebeln
      object                        = 'EKKO'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
    tables
      lines                         = lit_tlines
   exceptions
     id                            = 1
     language                      = 2
     name                          = 3
     not_found                     = 4
     object                        = 5
     reference_check               = 6
     wrong_access_to_archive       = 7
     others                        = 8
            .
  if sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  endif.

  read table lit_tlines index 1 assigning <fs_tlines>.
  if sy-subrc eq 0.
    v_text1 = <fs_tlines>-tdline.
  endif.

  read table lit_tlines index 2 assigning <fs_tlines>.
  if sy-subrc eq 0.
    v_text2 = <fs_tlines>-tdline.
  endif.

  concatenate: v_text1 v_text2 into v_desc_ped separated by ' '.

  clear: v_text1, v_text2, lit_tlines, lit_tlines[].
endform.                    " F_DESC_PED
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_PO_GETDETAIL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_POCOND  text
*      -->P_PIT_PSPIDEBELN_EBELN  text
*----------------------------------------------------------------------*
form f_bapi_po_getdetail1  tables   lit_pocond structure  bapimepocond
                           using    po_ebeln.

  refresh: lit_pocond[].

  call function 'BAPI_PO_GETDETAIL1' "#EC CI_USAGE_OK[2438131]
    exporting
      purchaseorder            = po_ebeln
     account_assignment       = 'X'
     item_text                = 'X'
     header_text              = 'X'
     delivery_address         = 'X'
     version                  = 'X'
     services                 = 'X'
*   IMPORTING
*     POHEADER                 =
*     POEXPIMPHEADER           =
     tables
*     RETURN                   =
*     POITEM                   =
*     POADDRDELIVERY           =
*     POSCHEDULE               =
*     POACCOUNT                =
*     POCONDHEADER             =
      pocond                   = lit_pocond
*     POLIMITS                 =
*     POCONTRACTLIMITS         =
*     POSERVICES               =
*     POSRVACCESSVALUES        =
*     POTEXTHEADER             =
*     POTEXTITEM               =
*     POEXPIMPITEM             =
*     POCOMPONENTS             =
*     POSHIPPINGEXP            =
*     POHISTORY                =
*     POHISTORY_TOTALS         =
*     POCONFIRMATION           =
*     ALLVERSIONS              =
*     POPARTNER                =
*     EXTENSIONOUT             =
            .

endform.                    " F_BAPI_PO_GETDETAIL1
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ALV_0100D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_create_alv_0100d .
  perform f_crt_fcat_0100d changing gt_fc100d.
  perform f_crt_layot_0100d changing gs_layo100d.
  perform f_crt_srt_0100 changing gt_srt100d.

  if gr_alv100d is initial.
    create object gr_cnt100d
      exporting
        container_name = 'CC0100_D'.

    create object gr_alv100d
      exporting
        i_parent = gr_cnt100d.

    gr_alv100d->set_table_for_first_display(
     exporting is_layout       = gs_layo100d
     changing  it_outtab       = it_detail_p
               it_fieldcatalog = gt_fc100d
               it_sort         = gt_srt100d ).
  else.
    gr_alv100d->refresh_table_display( is_stable = gs_stbl ).
  endif.


endform.                    " F_CREATE_ALV_0100D
*&---------------------------------------------------------------------*
*&      Form  F_CRT_FCAT_0100D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FC100D  text
*----------------------------------------------------------------------*
form f_crt_fcat_0100d  changing pt_fc_d type lvc_t_fcat.

  field-symbols <fc> like line of pt_fc_d.
  clear: pt_fc_d.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZMMWA_0100_DETAIL_COMP'
    changing
      ct_fieldcat            = pt_fc_d
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'EBELN'.
  if sy-subrc eq 0.
    <fc>-reptext = 'No.Pedido'.
    <fc>-scrtext_l = 'No.Pedido'.
    <fc>-scrtext_s = 'No.Pedido'.
    <fc>-scrtext_m = 'No.Pedido'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'EBELP'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Posicion'.
    <fc>-scrtext_l = 'Posicion'.
    <fc>-scrtext_s = 'Posicion'.
    <fc>-scrtext_m = 'Posicion'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'MATKL'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Gpo. Art'.
    <fc>-scrtext_l = 'Gpo. Art'.
    <fc>-scrtext_s = 'Gpo. Art'.
    <fc>-scrtext_m = 'Gpo. Art'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'ENTRADA'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Ent.'.
    <fc>-scrtext_l = 'Ent.'.
    <fc>-scrtext_s = 'Ent.'.
    <fc>-scrtext_m = 'Ent.'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'BWART'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Mov.'.
    <fc>-scrtext_l = 'Mov.'.
    <fc>-scrtext_s = 'Mov.'.
    <fc>-scrtext_m = 'Mov.'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'LFBNR'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Hoja Entrada'.
    <fc>-scrtext_l = 'Hoja Entrada'.
    <fc>-scrtext_s = 'Hoja Entrada'.
    <fc>-scrtext_m = 'Hoja Entrada'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'BELNR'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Doc. Material'.
    <fc>-scrtext_l = 'Doc. Material'.
    <fc>-scrtext_s = 'Doc. Material'.
    <fc>-scrtext_m = 'Doc. Material'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'BELNR_D'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Doc. Contable'.
    <fc>-scrtext_l = 'Doc. Contable'.
    <fc>-scrtext_s = 'Doc. Contable'.
    <fc>-scrtext_m = 'Doc. Contable'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'BLDAT'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Fecha Cont.'.
    <fc>-scrtext_l = 'Fecha Cont.'.
    <fc>-scrtext_s = 'Fecha Cont.'.
    <fc>-scrtext_m = 'Fecha Cont.'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'XBLNR'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Referencia'.
    <fc>-scrtext_l = 'Referencia'.
    <fc>-scrtext_s = 'Referencia'.
    <fc>-scrtext_m = 'Referencia'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'SRVPOS'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Servicio'.
    <fc>-scrtext_l = 'Servicio'.
    <fc>-scrtext_s = 'Servicio'.
    <fc>-scrtext_m = 'Servicio'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'ASKTX'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Descripcion del Servicio'.
    <fc>-scrtext_l = 'Descripcion del Servicio'.
    <fc>-scrtext_s = 'Descripcion del Servicio'.
    <fc>-scrtext_m = 'Descripcion del Servicio'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'MENGE'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Cantidad'.
    <fc>-scrtext_l = 'Cantidad'.
    <fc>-scrtext_s = 'Cantidad'.
    <fc>-scrtext_m = 'Cantidad'.
  endif.


  read table pt_fc_d assigning <fc> with key fieldname = 'G_DMBTR'.
  if sy-subrc eq 0.
    <fc>-do_sum = x.
    <fc>-reptext = 'Gasto'.
    <fc>-scrtext_l = 'Gasto'.
    <fc>-scrtext_s = 'Gasto'.
    <fc>-scrtext_m = 'Gasto'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'R_DMBTR'.
  if sy-subrc eq 0.
    <fc>-do_sum = x.
    <fc>-reptext = 'FG'.
    <fc>-scrtext_l = 'FG'.
    <fc>-scrtext_s = 'FG'.
    <fc>-scrtext_m = 'FG'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'A_DMBTR'.
  if sy-subrc eq 0.
    <fc>-do_sum = x.
    <fc>-reptext = 'Amortizado'.
    <fc>-scrtext_l = 'Amortizado'.
    <fc>-scrtext_s = 'Amortizado'.
    <fc>-scrtext_m = 'Amortizado'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'RF_DMBTR'.
  if sy-subrc eq 0.
    <fc>-do_sum = x.
    <fc>-reptext = 'Neto Cont.'.
    <fc>-scrtext_l = 'Neto Cont.'.
    <fc>-scrtext_s = 'Neto Cont.'.
    <fc>-scrtext_m = 'Neto Cont.'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'VF_BELNR'.
  if sy-subrc eq 0.
    <fc>-reptext = 'V. Factura'.
    <fc>-scrtext_l = 'V. Factura'.
    <fc>-scrtext_s = 'V. Factura'.
    <fc>-scrtext_m = 'V. Factura'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'DC_BELNR'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Doc. Contable'.
    <fc>-scrtext_l = 'Doc. Contable'.
    <fc>-scrtext_s = 'Doc. Contable'.
    <fc>-scrtext_m = 'Doc. Contable'.
  endif.


  read table pt_fc_d assigning <fc> with key fieldname = 'FCH_VER'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Fecha Verf. Doc'.
    <fc>-scrtext_l = 'Fecha Verf. Doc'.
    <fc>-scrtext_s = 'Fecha Verf. Doc'.
    <fc>-scrtext_m = 'Fecha Verf. Doc'.
  endif.



  read table pt_fc_d assigning <fc> with key fieldname = 'DMBTR'.
  if sy-subrc eq 0.
    <fc>-do_sum = x.
    <fc>-reptext = 'Verificado'.
    <fc>-scrtext_l = 'Verificado'.
    <fc>-scrtext_s = 'Verificado'.
    <fc>-scrtext_m = 'Verificado'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'MWSKZ'.
  if sy-subrc eq 0.
    <fc>-reptext = 'Impuesto'.
    <fc>-scrtext_l = 'Impuesto'.
    <fc>-scrtext_s = 'Impuesto'.
    <fc>-scrtext_m = 'Impuesto'.
  endif.

  read table pt_fc_d assigning <fc> with key fieldname = 'TOTAL'.
  if sy-subrc eq 0.
    <fc>-do_sum = x.
    <fc>-reptext = 'Total'.
    <fc>-scrtext_l = 'Total'.
    <fc>-scrtext_s = 'Total'.
    <fc>-scrtext_m = 'Total'.
  endif.





endform.                    " F_CRT_FCAT_0100D
*&---------------------------------------------------------------------*
*&      Form  F_CRT_LAYOT_0100D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYO100D  text
*----------------------------------------------------------------------*
form f_crt_layot_0100d  changing p_layo_d type lvc_s_layo.

  clear p_layo_d.
  p_layo_d-no_rowmark = x.
  p_layo_d-cwidth_opt = x.
endform.                    " F_CRT_LAYOT_0100D
*&---------------------------------------------------------------------*
*&      Form  F_CRT_SRT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SRT100D  text
*----------------------------------------------------------------------*
form f_crt_srt_0100  changing pt_sort type lvc_t_sort.

  perform f_add_srt using '1' 'XBLNR' x '' x changing pt_sort.

endform.                    " F_CRT_SRT_0100
*&---------------------------------------------------------------------*
*&      Form  F_ADD_SRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1728   text
*      -->P_1729   text
*      -->P_X  text
*      -->P_1731   text
*      -->P_1732   text
*      <--P_PT_SORT  text
*----------------------------------------------------------------------*
form f_add_srt  using    p_spos      type slis_spos
                         p_fieldname type lvc_fname
                         p_up        type char1 "Ord de menor a mayor
                         p_down      type char1 "Or de mayor a menor
                         p_subtot    type slis_dosub "Subtotales
                changing p_srt       type lvc_t_sort.
  data ls_srt type lvc_s_sort.

  ls_srt-spos = p_spos.
  ls_srt-fieldname = p_fieldname.
  ls_srt-up        = p_up.
  ls_srt-down      = p_down.
  ls_srt-subtot    = p_subtot.
  append ls_srt to p_srt.
  clear ls_srt.

endform.                    " F_ADD_SRT


**&---------------------------------------------------------------------*
**&      Form  F_CREATE_MATCHCODE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_IT_LIFNR  text
**----------------------------------------------------------------------*
*FORM f_create_matchcode.
*
*
*
** Se genera IT_LIFNR la cual contiene los proveedores existentes
** en la tabla LFA1 y que tambien existen en la tabla LFM1.
*
*  SELECT lfa1~lifnr lfa1~name1 lfa1~ort01 lfa1~regio
*  INTO CORRESPONDING FIELDS OF TABLE it_lifnr
*  FROM lfa1
*  INNER JOIN lfm1
*  ON lfa1~lifnr = lfm1~lifnr.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'LIFNR'
*      window_title    = 'No. Proveedor'
*      value_org       = 'S'
*    TABLES
*      value_tab       = it_lifnr
*      return_tab      = it_match
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc EQ 0.
*    READ TABLE it_match INDEX 1.
*    MOVE it_match-fieldval TO p_provdr.
*  ENDIF.
*
*ENDFORM.                    " F_CREATE_MATCHCODE
*
**&---------------------------------------------------------------------*
**&      Form  F_GET_EBELN_FROM_PSPID
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GIT_PSPIDEBELN  text
**      -->P_P_PROYEC  text
**      -->P_P_PROVDR  text
**      -->P_P_PEDIDO  text
**----------------------------------------------------------------------*
*FORM f_get_ebeln_from_pspid  TABLES   pit_pspidebeln STRUCTURE zmmwa_pspidebelnlifnr
*                             USING    p_p_proyec
*                                      p_p_provdr.
*
*  LOOP AT lit_zutil INTO lwa_zutil  WHERE zfield CS 'DOC_TYPE'.
*    lwa_doc_type-sign = 'I'.
*    lwa_doc_type-option = 'EQ'.
*    lwa_doc_type-low = lwa_zutil-zchar.
*    APPEND lwa_doc_type TO lit_doc_type.
*    CLEAR: lwa_doc_type, lwa_zutil.
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    MESSAGE text-002 TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*  SELECT verkf lifnr ebeln INTO TABLE pit_pspidebeln
* FROM ekko
* WHERE verkf = p_proyec
*   AND lifnr = p_provdr
*   AND bsart IN lit_doc_type.
*
*
*  IF sy-subrc NE 0.
*    MESSAGE text-003 TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*ENDFORM.                    " F_GET_EBELN_FROM_PSPID
**&---------------------------------------------------------------------*
**&      Form  F_VALIDATE_EBELN_FROM_PSPID
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GIT_SPPIDEBELN  text
**      -->P_P_PROYEC  text
**      -->P_P_PROVDR  text
**      -->P_P_PEDIDO  text
**----------------------------------------------------------------------*
*FORM f_validate_ebeln_from_pspid  TABLES   pit_pspidebeln STRUCTURE zmmwa_pspidebelnlifnr
*                                  USING    p_p_proyec
*                                           p_p_provdr
*                                           p_p_pedido.
*
*  LOOP AT lit_zutil INTO lwa_zutil  WHERE zfield CS 'DOC_TYPE'.
*    lwa_doc_type-sign = 'I'.
*    lwa_doc_type-option = 'EQ'.
*    lwa_doc_type-low = lwa_zutil-zchar.
*    APPEND lwa_doc_type TO lit_doc_type.
*    CLEAR: lwa_doc_type, lwa_zutil.
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    MESSAGE text-002 TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*  SELECT SINGLE ebeln INTO pit_pspidebeln
*   FROM ekko
*   WHERE ebeln = p_pedido.
*
*  IF sy-subrc NE 0.
*    MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  SELECT SINGLE ebeln INTO pit_pspidebeln
*   FROM ekko
*   WHERE ebeln = p_pedido
*   AND bsart IN lit_doc_type.
*
*  IF sy-subrc NE 0.
*    MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*
*  SELECT SINGLE verkf lifnr ebeln INTO pit_pspidebeln
*  FROM ekko
*  WHERE ebeln = p_pedido
*    AND verkf = p_proyec
*    AND lifnr = p_provdr
*    AND bsart IN lit_doc_type.
*
*  IF sy-subrc NE 0.
*    MESSAGE text-006 TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ELSE.
*    APPEND pit_pspidebeln TO pit_pspidebeln.
*  ENDIF.
*
*ENDFORM.                    " F_VALIDATE_EBELN_FROM_PSPID
**&---------------------------------------------------------------------*
**&      Form  F_GET_INFO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_GIT_PSPIDEBELN  text
**      -->P_IT_HEADER  text
**      -->P_IT_DETAIL  text
**----------------------------------------------------------------------*
*FORM f_get_info  TABLES   pit_pspidebeln STRUCTURE zmmwa_pspidebelnlifnr
*                          pit_header STRUCTURE zmmwa_0100_header_comp
*                          pit_detail STRUCTURE zmmwa_0100_detail_comp.
*
*  DATA: it_ekko TYPE ekko OCCURS 0 WITH HEADER LINE.
*  DATA: it_ekpo TYPE ekpo OCCURS 0 WITH HEADER LINE.
*  DATA: it_ekbe TYPE ekbe OCCURS 0 WITH HEADER LINE.
*  DATA: it_asmdt TYPE asmdt OCCURS 0 WITH HEADER LINE.
*  DATA: lit_pocond TYPE TABLE OF bapimepocond.
*  DATA: wa_proj TYPE proj.
*  DATA: wa_t001 TYPE t001.
*  DATA: wa_header LIKE LINE OF it_header.
*  DATA: wa_detail LIKE LINE OF it_detail.
*  DATA: wa_pocond TYPE bapimepocond.
*  DATA: wa_ekbe TYPE ekbe.
*  DATA: wa_bkpf LIKE LINE OF it_bkpf.
*  DATA: wa_awkey LIKE LINE OF it_awkey.
*  DATA: lwa_zutil TYPE zutil_parameters.
*
*  DATA: v_retencion TYPE bapikbetr1.
*  DATA: v_amortizacion TYPE bapikbetr1.
*  DATA: v_porc TYPE c LENGTH 5.
*  DATA: v_porc2 TYPE c LENGTH 5.
*  DATA: v_porc_total TYPE c LENGTH 5.
*  DATA: v_lfbnr TYPE mblnr.
*  DATA: v_amort_a TYPE zmmde_imp_amortiza.
*  DATA: v_amort_fg TYPE zmmde_imp_amortiza.
*  DATA: v_anticipo_1 TYPE c LENGTH 10.
*  DATA: v_anticipo_2 TYPE c LENGTH 10.
*
*
*  PERFORM f_generate_title CHANGING wa_proj
*                                    wa_t001
*                                    v_title.
*
*  PERFORM f_rescue_info TABLES pit_pspidebeln
*                                it_ekko
*                                it_ekpo
*                                it_ekbe
*                                it_asmdt.
*
*
*
*  READ TABLE lit_zutil INTO lwa_zutil WITH KEY zfield = 'FIL_ANTICIPO_01'.
*  IF lwa_zutil-zchar = ' '.
*    MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS' TYPE 'E'.
*  ELSEIF lwa_zutil-zchar IS NOT INITIAL.
*    v_anticipo_1 = lwa_zutil-zchar.
*  ENDIF.
*  CLEAR: lwa_zutil.
*
*  READ TABLE lit_zutil INTO lwa_zutil WITH KEY zfield = 'FIL_ANTICIPO_02'.
*  IF lwa_zutil-zchar = ' '.
*    MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS' TYPE 'E'.
*  ELSEIF lwa_zutil-zchar IS NOT INITIAL.
*    v_anticipo_2 = lwa_zutil-zchar.
*  ENDIF.
*  CLEAR: lwa_zutil.
**--> Inicia Modificación RSDK906898 - 5
*  CLEAR gv_moneda.
**<-- Fin Modificación RSDK906898 - 5
*
*  LOOP AT pit_pspidebeln.
*
*
*    READ TABLE it_ekko WITH KEY ebeln = pit_pspidebeln-ebeln INTO it_ekko.
*
*    SORT it_ekko BY ebeln.
*
*    READ TABLE it_ekpo WITH KEY ebeln = pit_pspidebeln-ebeln INTO it_ekpo.
*
*    SORT it_ekpo BY ebeln ebelp.
*
*    READ TABLE it_lfa1 WITH KEY lifnr = pit_pspidebeln-lifnr INTO it_lfa1.
*
*    PERFORM f_desc_ped   TABLES pit_pspidebeln
*                         CHANGING v_desc_ped.
**--> Inicia Modificación RSDK906898 - 6
*    gv_moneda = it_ekko-waers.
**<-- Fin Modificación RSDK906898 - 6
*
*    wa_header-pedido       =  it_ekko-ebeln.
*    wa_header-desc_ped     =  v_desc_ped.
*    wa_header-no_prov      =  it_ekko-lifnr.
*    wa_header-desc_prov    =  it_lfa1-name1.
*
*    LOOP AT it_ekpo WHERE ebeln = pit_pspidebeln-ebeln.
**--> Inicio Modificación RSDK906886 - 1.
*      IF it_ekpo-loekz EQ space.
**<-- Fin Modificación RSDK906886 - 1.
*        wa_header-contratado   = wa_header-contratado + it_ekpo-brtwr.
**--> Inicio Modificación RSDK906886 - 2.
*
**--> Inicio Modificación RSDK907131 - 1.
** IJOF : Se agrega en condición acumulado para campo CONT-RET.
*        wa_header-neto_contratado    = wa_header-neto_contratado + it_ekpo-netpr.
**<-- Fin Modificación RSDK907131 - 1.
*
*      ENDIF.
**<-- Fin Modificación RSDK906886 - 2.
*
**--> Inicio Modificación RSDK907131 - 2.
** IJOF : Se comenta código.
**      wa_header-neto_contratado    = wa_header-neto_contratado + it_ekpo-netpr.
**<-- Fin Modificación RSDK907131 - 2.
*    ENDLOOP.
*
*    PERFORM f_bapi_po_getdetail1 TABLES lit_pocond
*                                  USING pit_pspidebeln-ebeln.
*
*    READ TABLE lit_pocond INTO wa_pocond
*    WITH KEY cond_type = 'ZRET'.
*    v_retencion = wa_pocond-cond_value.
*    CLEAR wa_pocond.
*
*    READ TABLE lit_pocond INTO wa_pocond
*    WITH KEY cond_type = 'ZAAN'.
*    v_amortizacion = wa_pocond-cond_value.
*    CLEAR wa_pocond.
*
*    wa_header-fondo_garan = ( wa_header-contratado * v_retencion ) / 100.
*    wa_header-total_amortizar = ( wa_header-contratado * v_amortizacion ) / 100.
*
*
*    LOOP AT it_ekbe INTO wa_ekbe
*      WHERE belnr BETWEEN v_anticipo_1 AND v_anticipo_2
*      AND ebeln = pit_pspidebeln-ebeln
*      AND   vgabe = '4'.
** Se comenta linea de codigo y se agrega instruccion para que acumule anticipos
**      wa_header-anticipo = wa_ekbe-dmbtr.
*      wa_header-anticipo = wa_header-anticipo + wa_ekbe-dmbtr.
*      CLEAR wa_ekbe.
**      EXIT.
*    ENDLOOP.
*    DATA: vl_movi TYPE bwart.
*    CLEAR vl_movi.
*    LOOP AT it_ekbe WHERE ebeln = it_ekpo-ebeln
**                    AND   ebelp = it_ekpo-ebelp
*                    AND   vgabe = '1'.
*
*      vl_movi = it_ekbe-bwart.
*
*      READ TABLE it_ekpo WITH KEY ebeln = it_ekbe-ebeln
*                                  ebelp = it_ekbe-ebelp INTO it_ekpo.
*
*      SORT it_ekpo BY ebeln ebelp.
*
*
*      READ TABLE it_asmdt WITH KEY asnum = it_ekbe-srvpos.
*
*      READ TABLE it_awkey WITH KEY ebeln  = it_ekbe-ebeln
*                                   ebelp  = it_ekbe-ebelp
*                                   srvpos = it_ekbe-srvpos
*                                   belnr  = it_ekbe-belnr
*                                   gjahr  = it_ekbe-gjahr.
*
*      READ TABLE it_bkpf  WITH KEY awkey = it_awkey-awkey.
*
*      READ TABLE it_bseg WITH KEY bukrs = it_bkpf-bukrs
*                                  belnr = it_bkpf-belnr
*                                  gjahr = it_bkpf-gjahr.
**** Comentar el tipo de movimiento 102
**** Ricardo Pichardo 23.04.2015
*
*      IF it_ekbe-bwart = '101'
*      OR it_ekbe-bwart = '102'.
*        wa_detail-entrada = 'WE'.
*        wa_detail-bwart = it_ekbe-bwart.
*      ENDIF.
*      wa_detail-ebeln    = it_ekbe-ebeln.
*      wa_detail-ebelp    = it_ekbe-ebelp.
*      wa_detail-matkl    = it_ekpo-matkl.
*      wa_detail-lfbnr    = it_ekbe-lfbnr.
*      wa_detail-belnr    = it_ekbe-belnr.
*      wa_detail-belnr_d  = it_bkpf-belnr.
*      wa_detail-bldat    = it_bkpf-bldat.
*      wa_detail-xblnr    = it_ekbe-xblnr.
*      wa_detail-srvpos   = it_ekbe-srvpos.
*      wa_detail-asktx    = it_asmdt-asktx.
*      wa_detail-menge    = it_ekbe-menge.
*      v_lfbnr            = it_ekbe-lfbnr.
*      LOOP AT it_bseg WHERE bukrs = it_bkpf-bukrs
*                       AND  belnr = it_bkpf-belnr
*                       AND  gjahr = it_bkpf-gjahr.
*        CASE it_bseg-ktosl.
*          WHEN 'KBS'.
*            IF it_ekbe-bwart = '102'.
*              it_bseg-dmbtr = it_bseg-dmbtr * -1.
*              wa_detail-g_dmbtr = it_bseg-dmbtr.
*            ELSE.
*              wa_detail-g_dmbtr = it_bseg-dmbtr.
*            ENDIF.
*          WHEN 'ZE1'.
*            IF it_ekbe-bwart = '102'.
*              it_bseg-dmbtr = it_bseg-dmbtr * -1.
*              wa_detail-r_dmbtr = it_bseg-dmbtr.
*            ELSE.
*              wa_detail-r_dmbtr = it_bseg-dmbtr.
*            ENDIF.
*            v_amort_fg = v_amort_fg + it_bseg-dmbtr.
*          WHEN 'ZE2'.
*            IF it_ekbe-bwart = '102'.
*              it_bseg-dmbtr = it_bseg-dmbtr * -1.
*              wa_detail-a_dmbtr = it_bseg-dmbtr.
*            ELSE.
*              wa_detail-a_dmbtr = it_bseg-dmbtr.
*            ENDIF.
*            v_amort_a = v_amort_a + it_bseg-dmbtr.
*          WHEN 'WRX'.
*            IF it_ekbe-bwart = '102'.
*              it_bseg-dmbtr = it_bseg-dmbtr * -1.
*              wa_detail-rf_dmbtr = it_bseg-dmbtr.
*            ELSE.
*              wa_detail-rf_dmbtr = it_bseg-dmbtr.
*            ENDIF.
*        ENDCASE.
*      ENDLOOP.
*
*      LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = it_ekpo-ebeln
*                      AND   lfbnr = v_lfbnr
*                      AND   vgabe = '2'.
*        READ TABLE it_awkey INTO wa_awkey WITH KEY ebeln  = wa_ekbe-ebeln
*                                                   ebelp  = wa_ekbe-ebelp
*                                                   srvpos = wa_ekbe-srvpos
*                                                   belnr  = wa_ekbe-belnr
*                                                   gjahr  = wa_ekbe-gjahr.
*
*        READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = wa_awkey-awkey.
*
*        READ TABLE it_konv WITH KEY mwsk1 = wa_ekbe-mwskz.
*
*        READ TABLE it_konp WITH KEY knumh = it_konv-knumh
*                                    kschl = it_konv-kschl.
*
*        wa_detail-vf_belnr = wa_ekbe-belnr.
*        wa_detail-fch_ver  = wa_ekbe-budat.
*        wa_detail-dc_belnr = wa_bkpf-belnr.
*        IF wa_ekbe-shkzg = 'H'.
*          wa_ekbe-dmbtr = wa_ekbe-dmbtr * -1.
*          wa_detail-dmbtr    = wa_ekbe-dmbtr.
*        ELSE.
*          wa_detail-dmbtr    = wa_ekbe-dmbtr.
*        ENDIF.
*
*        v_porc = ( it_konp-kbetr / 10 ).
*        CONDENSE v_porc.
*        CONCATENATE v_porc '%' INTO v_porc2 SEPARATED BY ''.
*        wa_detail-mwskz = v_porc2.
*
*        v_porc_total = ( it_konp-kbetr / 1000 ) + 1.
*        CONDENSE v_porc_total.
*
*        wa_detail-total = ( wa_ekbe-dmbtr * v_porc_total ).
*
*        IF wa_ekbe-shkzg = 'H' AND vl_movi = '102'.
*          APPEND wa_detail TO it_detail.
*        ELSEIF wa_ekbe-shkzg = 'S' AND vl_movi = '101'.
*          APPEND wa_detail TO it_detail.
*        ELSEIF wa_ekbe-shkzg = 'H' AND vl_movi = '101'.
*          APPEND wa_detail TO it_detail.
*        ENDIF.
*        CLEAR: wa_ekbe, wa_bkpf, wa_ekbe, it_bseg, wa_awkey, v_porc, v_porc2,
*               v_porc_total, it_konv, it_konp.
*      ENDLOOP.
*      IF wa_detail-entrada IS NOT INITIAL AND
*         wa_detail-bwart   IS NOT INITIAL.
*        APPEND wa_detail TO it_detail.
*      ENDIF.
*      CLEAR: it_ekbe, it_ekpo, it_asmdt, it_awkey, it_bkpf, it_bseg,
*         wa_ekbe, wa_detail, v_lfbnr.
*
*    ENDLOOP.
*
*    wa_header-amort_acum = v_amort_a.
*    wa_header-amort_fg   = v_amort_fg.
*
*    APPEND wa_header TO it_header.
*
*    CLEAR: wa_header, wa_detail, it_lfa1, it_ekko, it_ekpo, lit_pocond,
*      lit_pocond[], wa_pocond, v_amortizacion, v_retencion, v_amort_fg, v_amort_a.
*
*  ENDLOOP.
*
*  DATA: it_detail_aux TYPE TABLE OF zmmwa_0100_detail_comp,
*        wa_det_aux TYPE zmmwa_0100_detail_comp.
*  it_detail_aux[] = it_detail[].
*
*  LOOP AT it_detail INTO wa_detail.
*    IF wa_detail-bwart = '101'.
*      APPEND wa_detail TO it_detail_aux.
*    ENDIF.
*  ENDLOOP.
*  SORT it_detail_aux BY lfbnr.
*  DELETE ADJACENT DUPLICATES FROM it_detail_aux COMPARING lfbnr.
*  DELETE it_detail WHERE bwart = '101'.
*  LOOP AT it_detail_aux INTO wa_det_aux.
*    APPEND wa_det_aux TO it_detail.
*  ENDLOOP.
*
*  SORT it_detail BY belnr.
*  DELETE ADJACENT DUPLICATES FROM it_detail COMPARING belnr.
*
*  SORT it_header BY pedido.
*  SORT it_detail BY ebeln xblnr.
*
*  IF it_header[] IS NOT INITIAL.
*
*    CALL SCREEN 0100.
*
*  ENDIF.
*
*ENDFORM.                    " F_GET_INFO
**&---------------------------------------------------------------------*
**&      Form  F_VALIDATE_PROV_PROY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_validate_prov_proy .
*
*  DATA: lwa_proj TYPE proj.
*
*  SELECT SINGLE * INTO lwa_proj
*  FROM proj
*    WHERE pspid = p_proyec.
*
*  IF sy-subrc NE 0.
*    MESSAGE 'El proyecto no se encuentra registrado' TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*ENDFORM.                    " F_VALIDATE_PROV_PROY
**&---------------------------------------------------------------------*
**&      Form  F_VALIDATE_PROV_PROY2
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_validate_prov_proy2 .
*
*  SELECT SINGLE lfa1~lifnr lfa1~name1 lfa1~ort01 lfa1~regio
*        INTO it_lifnr
*        FROM lfa1
*        INNER JOIN lfm1
*        ON lfa1~lifnr = lfm1~lifnr
*        WHERE lfa1~lifnr = p_provdr
*        AND lfm1~lifnr = p_provdr.
*
*  IF sy-subrc NE 0.
*    MESSAGE 'El proveedor indicado no se encuentra registrado' TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*ENDFORM.                    " F_VALIDATE_PROV_PROY2
**&---------------------------------------------------------------------*
**&      Form  F_GET_ZUTIL_PARAMETERS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_get_zutil_parameters .
*
*  SELECT * INTO TABLE lit_zutil
*    FROM zutil_parameters
*    WHERE zreport = 'ZMM0100_INFO_COMP'.
*
*ENDFORM.                    " F_GET_ZUTIL_PARAMETERS
**&---------------------------------------------------------------------*
**&      Form  F_CREATE_MATCHCODE_P
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_create_matchcode_p .
*
*  LOOP AT lit_zutil INTO lwa_zutil  WHERE zfield CS 'DOC_TYPE'.
*    lwa_doc_type-sign = 'I'.
*    lwa_doc_type-option = 'EQ'.
*    lwa_doc_type-low = lwa_zutil-zchar.
*    APPEND lwa_doc_type TO lit_doc_type.
*    CLEAR: lwa_doc_type, lwa_zutil.
*  ENDLOOP.
*
*  IF sy-subrc NE 0.
*    MESSAGE 'Falta configuracion en ZUTIL_PARAMETERS' TYPE 'I'.
*    EXIT.
*  ELSE.
*    SELECT ebeln bukrs bsart INTO CORRESPONDING FIELDS OF TABLE it_ebeln
*   FROM ekko
*   WHERE verkf = p_proyec
*     AND lifnr = p_provdr
*     AND bsart IN lit_doc_type.
*  ENDIF.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'EBELN'
*      window_title    = 'No. Pedido'
*      value_org       = 'S'
*    TABLES
*      value_tab       = it_ebeln
*      return_tab      = it_match_p
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc EQ 0.
*    READ TABLE it_match_p INDEX 1.
*    MOVE it_match_p-fieldval TO p_pedido.
*  ENDIF.
*
*ENDFORM.                    " F_CREATE_MATCHCODE_P
**&---------------------------------------------------------------------*
**&      Form  G_GENERATE_TITLE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      <--P_WA_PROJ  text
**      <--P_WA_T001  text
**      <--P_V_TITLE  text
**----------------------------------------------------------------------*
*FORM f_generate_title  CHANGING wa_proj TYPE proj
*                                wa_t001 TYPE t001
*                                v_title.
*
*  DATA v_name1 TYPE name1.
*
*
*  SELECT SINGLE pspnr post1 vbukr INTO CORRESPONDING FIELDS OF  wa_proj
*    FROM proj
*    WHERE pspid = p_proyec.
*
*  SELECT SINGLE bukrs butxt INTO CORRESPONDING FIELDS OF wa_t001
*  FROM t001
*  WHERE bukrs = wa_proj-vbukr.
*
*  IF sy-subrc = 0.
*    CONCATENATE 'Proyecto:' p_proyec '-' wa_proj-post1  INTO v_title  SEPARATED BY '  '.
*    CONCATENATE 'Sociedad:' wa_t001-bukrs '-'
*     wa_t001-butxt INTO v_sociedad SEPARATED BY '  '.
*    .
*  ENDIF.
*
*
*ENDFORM.                    " G_GENERATE_TITLE
**&---------------------------------------------------------------------*
**&      Form  F_GENERATE_HEADER
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_IT_EKKO  text
**      -->P_IT_LFA1  text
**----------------------------------------------------------------------*
*FORM f_rescue_info TABLES pit_pspidebeln STRUCTURE zmmwa_pspidebelnlifnr
*                          pit_ekko STRUCTURE ekko
*                          pit_ekpo STRUCTURE ekpo
*                          pit_ekbe STRUCTURE ekbe
*                          pit_asmdt STRUCTURE asmdt.
*  DATA: wa_ekbe TYPE ekbe.
**& --->Inicio RSDK907327
*  DATA: it_ekbe_aux TYPE STANDARD TABLE OF ekbe.
**& <---Fin    RSDK907327
*
*
*  IF pit_pspidebeln[] IS NOT INITIAL.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE pit_ekko
*    FROM ekko FOR ALL ENTRIES IN pit_pspidebeln
*    WHERE verkf = p_proyec
*    AND   lifnr = p_provdr
*    AND   ebeln = pit_pspidebeln-ebeln
*    AND   bsart IN lit_doc_type.
*  ENDIF.
*
*  IF pit_ekko[] IS NOT INITIAL.
*    SELECT lifnr name1 INTO CORRESPONDING FIELDS OF TABLE it_lfa1
*    FROM lfa1 FOR ALL ENTRIES IN pit_ekko
*    WHERE lifnr = pit_ekko-lifnr.
*  ENDIF.
*
*  IF pit_pspidebeln[] IS NOT INITIAL.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE pit_ekpo
*    FROM ekpo FOR ALL ENTRIES IN pit_pspidebeln
*    WHERE ebeln = pit_pspidebeln-ebeln.
*  ENDIF.
*
*  IF pit_ekpo[] IS NOT INITIAL.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE pit_ekbe
*    FROM ekbe FOR ALL ENTRIES IN pit_ekpo
*    WHERE ebeln = pit_ekpo-ebeln.
*  ENDIF.
*
*  SORT pit_ekbe BY ebeln ebelp srvpos.
*
*  LOOP AT pit_ekbe INTO wa_ekbe.
*    it_awkey-ebeln  = wa_ekbe-ebeln.
*    it_awkey-ebelp  = wa_ekbe-ebelp.
*    it_awkey-srvpos = wa_ekbe-srvpos.
*    it_awkey-belnr  = wa_ekbe-belnr.
*    it_awkey-gjahr  = wa_ekbe-gjahr.
*    CONCATENATE wa_ekbe-belnr wa_ekbe-gjahr INTO it_awkey-awkey.
*    APPEND it_awkey.
*    CLEAR: it_awkey, wa_ekbe.
*  ENDLOOP.
*
*  IF it_awkey[] IS NOT INITIAL.
*    SELECT bukrs belnr gjahr awkey bldat FROM bkpf INTO CORRESPONDING FIELDS OF TABLE it_bkpf
*        FOR ALL ENTRIES IN it_awkey
*        WHERE awkey = it_awkey-awkey.
*  ENDIF.
*
*  SORT it_bkpf BY bukrs belnr gjahr.
*
*  IF pit_ekbe[] IS NOT INITIAL.
*    SELECT * FROM asmdt INTO CORRESPONDING FIELDS OF TABLE pit_asmdt
*       FOR ALL ENTRIES IN pit_ekbe
*       WHERE asnum = pit_ekbe-srvpos.
*  ENDIF.
*
*  IF it_bkpf[] IS NOT INITIAL.
*    SELECT bukrs belnr gjahr
*           shkzg dmbtr pswsl
*           ktosl FROM bseg INTO CORRESPONDING FIELDS OF TABLE it_bseg
*        FOR ALL ENTRIES IN it_bkpf
*        WHERE bukrs EQ it_bkpf-bukrs
*        AND   belnr EQ it_bkpf-belnr
*        AND   gjahr EQ it_bkpf-gjahr
*        AND (  ktosl EQ  'KBS'
*        OR     ktosl EQ  'ZE2'
*        OR     ktosl EQ  'ZE1'
*        OR     ktosl EQ  'WRX' ).
*  ENDIF.
*
**& --->Inicio RSDK907327
*  it_ekbe_aux[] = pit_ekbe[].
*  DELETE it_ekbe_aux WHERE mwskz EQ space.
*
*  IF it_ekbe_aux[] IS NOT INITIAL.
*    SELECT knumh mwsk1 kschl FROM konv INTO TABLE it_konv
*      FOR ALL ENTRIES IN it_ekbe_aux
*      WHERE mwsk1 = it_ekbe_aux-mwskz.
*  ENDIF.
**& <---Fin    RSDK907327
*  IF it_konv[] IS NOT INITIAL.
*    SELECT knumh kschl kbetr FROM konp INTO CORRESPONDING FIELDS OF TABLE it_konp
*      FOR ALL ENTRIES IN it_konv
*      WHERE knumh = it_konv-knumh
*      AND   kschl = it_konv-kschl.
*  ENDIF.
*ENDFORM.                    " F_GENERATE_HEADER
**&---------------------------------------------------------------------*
**&      Form  F_DESC_PED
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_PIT_PSPIDEBELN  text
**      <--P_V_DESC_PED  text
**----------------------------------------------------------------------*
*FORM f_desc_ped    TABLES pit_pspidebeln STRUCTURE zmmwa_pspidebelnlifnr
*                 CHANGING v_desc_ped.
*
*  DATA: lit_tlines TYPE STANDARD TABLE OF tline.
*  FIELD-SYMBOLS: <fs_tlines> LIKE LINE OF lit_tlines.
*  DATA: v_text1 TYPE tdline.
*  DATA: v_text2 TYPE tdline.
*  DATA: v_ebeln TYPE thead-tdname.
*
*  v_ebeln = pit_pspidebeln-ebeln.
*
*
*  CALL FUNCTION 'READ_TEXT'
*    EXPORTING
*     client                         = sy-mandt
*      id                            = 'F01'
*      language                      = sy-langu
*      name                          = v_ebeln
*      object                        = 'EKKO'
**   ARCHIVE_HANDLE                = 0
**   LOCAL_CAT                     = ' '
** IMPORTING
**   HEADER                        =
*    TABLES
*      lines                         = lit_tlines
*   EXCEPTIONS
*     id                            = 1
*     language                      = 2
*     name                          = 3
*     not_found                     = 4
*     object                        = 5
*     reference_check               = 6
*     wrong_access_to_archive       = 7
*     OTHERS                        = 8
*            .
*  IF sy-subrc <> 0.
**    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    EXIT.
*  ENDIF.
*
*  READ TABLE lit_tlines INDEX 1 ASSIGNING <fs_tlines>.
*  IF sy-subrc EQ 0.
*    v_text1 = <fs_tlines>-tdline.
*  ENDIF.
*
*  READ TABLE lit_tlines INDEX 2 ASSIGNING <fs_tlines>.
*  IF sy-subrc EQ 0.
*    v_text2 = <fs_tlines>-tdline.
*  ENDIF.
*
*  CONCATENATE: v_text1 v_text2 INTO v_desc_ped SEPARATED BY ' '.
*
*  CLEAR: v_text1, v_text2, lit_tlines, lit_tlines[].
*ENDFORM.                    " F_DESC_PED
**&---------------------------------------------------------------------*
**&      Form  F_BAPI_PO_GETDETAIL1
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LIT_POCOND  text
**      -->P_PIT_PSPIDEBELN_EBELN  text
**----------------------------------------------------------------------*
*FORM f_bapi_po_getdetail1  TABLES   lit_pocond STRUCTURE  bapimepocond
*                           USING    po_ebeln.
*
*  REFRESH: lit_pocond[].
*
*  CALL FUNCTION 'BAPI_PO_GETDETAIL1'
*    EXPORTING
*      purchaseorder            = po_ebeln
*     account_assignment       = 'X'
*     item_text                = 'X'
*     header_text              = 'X'
*     delivery_address         = 'X'
*     version                  = 'X'
*     services                 = 'X'
**   IMPORTING
**     POHEADER                 =
**     POEXPIMPHEADER           =
*     TABLES
**     RETURN                   =
**     POITEM                   =
**     POADDRDELIVERY           =
**     POSCHEDULE               =
**     POACCOUNT                =
**     POCONDHEADER             =
*      pocond                   = lit_pocond
**     POLIMITS                 =
**     POCONTRACTLIMITS         =
**     POSERVICES               =
**     POSRVACCESSVALUES        =
**     POTEXTHEADER             =
**     POTEXTITEM               =
**     POEXPIMPITEM             =
**     POCOMPONENTS             =
**     POSHIPPINGEXP            =
**     POHISTORY                =
**     POHISTORY_TOTALS         =
**     POCONFIRMATION           =
**     ALLVERSIONS              =
**     POPARTNER                =
**     EXTENSIONOUT             =
*            .
*
*ENDFORM.                    " F_BAPI_PO_GETDETAIL1
**&---------------------------------------------------------------------*
**&      Form  F_CREATE_ALV_0100D
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM f_create_alv_0100d .
*  PERFORM f_crt_fcat_0100d CHANGING gt_fc100d.
*  PERFORM f_crt_layot_0100d CHANGING gs_layo100d.
*  PERFORM f_crt_srt_0100 CHANGING gt_srt100d.
*
*  IF gr_alv100d IS INITIAL.
*    CREATE OBJECT gr_cnt100d
*      EXPORTING
*        container_name = 'CC0100_D'.
*
*    CREATE OBJECT gr_alv100d
*      EXPORTING
*        i_parent = gr_cnt100d.
*
*    gr_alv100d->set_table_for_first_display(
*     EXPORTING is_layout       = gs_layo100d
*     CHANGING  it_outtab       = it_detail_p
*               it_fieldcatalog = gt_fc100d
*               it_sort         = gt_srt100d ).
*  ELSE.
*    gr_alv100d->refresh_table_display( is_stable = gs_stbl ).
*  ENDIF.
*
*
*ENDFORM.                    " F_CREATE_ALV_0100D
**&---------------------------------------------------------------------*
**&      Form  F_CRT_FCAT_0100D
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      <--P_GT_FC100D  text
**----------------------------------------------------------------------*
*FORM f_crt_fcat_0100d  CHANGING pt_fc_d TYPE lvc_t_fcat.
*
*  FIELD-SYMBOLS <fc> LIKE LINE OF pt_fc_d.
*  CLEAR: pt_fc_d.
*
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name       = 'ZMMWA_0100_DETAIL_COMP'
*    CHANGING
*      ct_fieldcat            = pt_fc_d
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'EBELN'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'No.Pedido'.
*    <fc>-scrtext_l = 'No.Pedido'.
*    <fc>-scrtext_s = 'No.Pedido'.
*    <fc>-scrtext_m = 'No.Pedido'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'EBELP'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Posicion'.
*    <fc>-scrtext_l = 'Posicion'.
*    <fc>-scrtext_s = 'Posicion'.
*    <fc>-scrtext_m = 'Posicion'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'MATKL'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Gpo. Art'.
*    <fc>-scrtext_l = 'Gpo. Art'.
*    <fc>-scrtext_s = 'Gpo. Art'.
*    <fc>-scrtext_m = 'Gpo. Art'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'ENTRADA'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Ent.'.
*    <fc>-scrtext_l = 'Ent.'.
*    <fc>-scrtext_s = 'Ent.'.
*    <fc>-scrtext_m = 'Ent.'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'BWART'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Mov.'.
*    <fc>-scrtext_l = 'Mov.'.
*    <fc>-scrtext_s = 'Mov.'.
*    <fc>-scrtext_m = 'Mov.'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'LFBNR'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Hoja Entrada'.
*    <fc>-scrtext_l = 'Hoja Entrada'.
*    <fc>-scrtext_s = 'Hoja Entrada'.
*    <fc>-scrtext_m = 'Hoja Entrada'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'BELNR'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Doc. Material'.
*    <fc>-scrtext_l = 'Doc. Material'.
*    <fc>-scrtext_s = 'Doc. Material'.
*    <fc>-scrtext_m = 'Doc. Material'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'BELNR_D'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Doc. Contable'.
*    <fc>-scrtext_l = 'Doc. Contable'.
*    <fc>-scrtext_s = 'Doc. Contable'.
*    <fc>-scrtext_m = 'Doc. Contable'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'BLDAT'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Fecha Cont.'.
*    <fc>-scrtext_l = 'Fecha Cont.'.
*    <fc>-scrtext_s = 'Fecha Cont.'.
*    <fc>-scrtext_m = 'Fecha Cont.'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'XBLNR'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Referencia'.
*    <fc>-scrtext_l = 'Referencia'.
*    <fc>-scrtext_s = 'Referencia'.
*    <fc>-scrtext_m = 'Referencia'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'SRVPOS'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Servicio'.
*    <fc>-scrtext_l = 'Servicio'.
*    <fc>-scrtext_s = 'Servicio'.
*    <fc>-scrtext_m = 'Servicio'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'ASKTX'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Descripcion del Servicio'.
*    <fc>-scrtext_l = 'Descripcion del Servicio'.
*    <fc>-scrtext_s = 'Descripcion del Servicio'.
*    <fc>-scrtext_m = 'Descripcion del Servicio'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'MENGE'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Cantidad'.
*    <fc>-scrtext_l = 'Cantidad'.
*    <fc>-scrtext_s = 'Cantidad'.
*    <fc>-scrtext_m = 'Cantidad'.
*  ENDIF.
*
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'G_DMBTR'.
*  IF sy-subrc EQ 0.
*    <fc>-do_sum = x.
*    <fc>-reptext = 'Gasto'.
*    <fc>-scrtext_l = 'Gasto'.
*    <fc>-scrtext_s = 'Gasto'.
*    <fc>-scrtext_m = 'Gasto'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'R_DMBTR'.
*  IF sy-subrc EQ 0.
*    <fc>-do_sum = x.
*    <fc>-reptext = 'FG'.
*    <fc>-scrtext_l = 'FG'.
*    <fc>-scrtext_s = 'FG'.
*    <fc>-scrtext_m = 'FG'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'A_DMBTR'.
*  IF sy-subrc EQ 0.
*    <fc>-do_sum = x.
*    <fc>-reptext = 'Amortizado'.
*    <fc>-scrtext_l = 'Amortizado'.
*    <fc>-scrtext_s = 'Amortizado'.
*    <fc>-scrtext_m = 'Amortizado'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'RF_DMBTR'.
*  IF sy-subrc EQ 0.
*    <fc>-do_sum = x.
*    <fc>-reptext = 'Neto Cont.'.
*    <fc>-scrtext_l = 'Neto Cont.'.
*    <fc>-scrtext_s = 'Neto Cont.'.
*    <fc>-scrtext_m = 'Neto Cont.'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'VF_BELNR'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'V. Factura'.
*    <fc>-scrtext_l = 'V. Factura'.
*    <fc>-scrtext_s = 'V. Factura'.
*    <fc>-scrtext_m = 'V. Factura'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'DC_BELNR'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Doc. Contable'.
*    <fc>-scrtext_l = 'Doc. Contable'.
*    <fc>-scrtext_s = 'Doc. Contable'.
*    <fc>-scrtext_m = 'Doc. Contable'.
*  ENDIF.
*
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'FCH_VER'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Fecha Verf. Doc'.
*    <fc>-scrtext_l = 'Fecha Verf. Doc'.
*    <fc>-scrtext_s = 'Fecha Verf. Doc'.
*    <fc>-scrtext_m = 'Fecha Verf. Doc'.
*  ENDIF.
*
*
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'DMBTR'.
*  IF sy-subrc EQ 0.
*    <fc>-do_sum = x.
*    <fc>-reptext = 'Verificado'.
*    <fc>-scrtext_l = 'Verificado'.
*    <fc>-scrtext_s = 'Verificado'.
*    <fc>-scrtext_m = 'Verificado'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'MWSKZ'.
*  IF sy-subrc EQ 0.
*    <fc>-reptext = 'Impuesto'.
*    <fc>-scrtext_l = 'Impuesto'.
*    <fc>-scrtext_s = 'Impuesto'.
*    <fc>-scrtext_m = 'Impuesto'.
*  ENDIF.
*
*  READ TABLE pt_fc_d ASSIGNING <fc> WITH KEY fieldname = 'TOTAL'.
*  IF sy-subrc EQ 0.
*    <fc>-do_sum = x.
*    <fc>-reptext = 'Total'.
*    <fc>-scrtext_l = 'Total'.
*    <fc>-scrtext_s = 'Total'.
*    <fc>-scrtext_m = 'Total'.
*  ENDIF.
*
*
*
*
*
*ENDFORM.                    " F_CRT_FCAT_0100D
**&---------------------------------------------------------------------*
**&      Form  F_CRT_LAYOT_0100D
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      <--P_GS_LAYO100D  text
**----------------------------------------------------------------------*
*FORM f_crt_layot_0100d  CHANGING p_layo_d TYPE lvc_s_layo.
*
*  CLEAR p_layo_d.
*  p_layo_d-no_rowmark = x.
*  p_layo_d-cwidth_opt = x.
*ENDFORM.                    " F_CRT_LAYOT_0100D
**&---------------------------------------------------------------------*
**&      Form  F_CRT_SRT_0100
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      <--P_GT_SRT100D  text
**----------------------------------------------------------------------*
*FORM f_crt_srt_0100  CHANGING pt_sort TYPE lvc_t_sort.
*
*  PERFORM f_add_srt USING '1' 'XBLNR' x '' x CHANGING pt_sort.
*
*ENDFORM.                    " F_CRT_SRT_0100
**&---------------------------------------------------------------------*
**&      Form  F_ADD_SRT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_1728   text
**      -->P_1729   text
**      -->P_X  text
**      -->P_1731   text
**      -->P_1732   text
**      <--P_PT_SORT  text
**----------------------------------------------------------------------*
*FORM f_add_srt  USING    p_spos      TYPE slis_spos
*                         p_fieldname TYPE lvc_fname
*                         p_up        TYPE char1 "Ord de menor a mayor
*                         p_down      TYPE char1 "Or de mayor a menor
*                         p_subtot    TYPE slis_dosub "Subtotales
*                CHANGING p_srt       TYPE lvc_t_sort.
*  DATA ls_srt TYPE lvc_s_sort.
*
*  ls_srt-spos = p_spos.
*  ls_srt-fieldname = p_fieldname.
*  ls_srt-up        = p_up.
*  ls_srt-down      = p_down.
*  ls_srt-subtot    = p_subtot.
*  APPEND ls_srt TO p_srt.
*  CLEAR ls_srt.
*
*ENDFORM.                    " F_ADD_SRT
