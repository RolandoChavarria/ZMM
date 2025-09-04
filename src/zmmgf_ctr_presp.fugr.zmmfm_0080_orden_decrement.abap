FUNCTION zmmfm_0080_orden_decrement.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_VENDOR) TYPE  ZMMDE_LIFNR
*"     VALUE(I_PUR_GRP) TYPE  ZMMDE_EKGRP
*"     VALUE(I_PROYECT) TYPE  ZMMDE_VERKF
*"     VALUE(I_FOLIO_OC) TYPE  ZMMDE_FOLIO_ORD
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SUBRC
*"     VALUE(E_BAPI_MSG) TYPE  BAPI_MSG
*"  TABLES
*"      IT_POS_PARTIDAS TYPE  ZMMIT_POS_DECR
*"----------------------------------------------------------------------


  DATA: it_parameters TYPE TABLE OF zutil_parameters,
        v_hkont_00  TYPE hkont,
        v_hkont_01  TYPE hkont,
        v_hkont_02  TYPE hkont,
        v_hkont_03  TYPE hkont,
        v_hkont_04  TYPE hkont,
        v_hkont_05  TYPE hkont,
        v_hkont_06  TYPE hkont,
        v_hkont_07  TYPE hkont,
        v_hkont_08  TYPE hkont,
        v_hkont_09  TYPE hkont,
        v_hkont_10  TYPE hkont,
        v_hkont_11  TYPE hkont,
        v_doc_type  TYPE esart,
        v_matrn     TYPE matnr,
        v_purchorg  TYPE bapiekko-purch_org,
        v_quantity  TYPE bstmg,
        v_po_number TYPE bapiekkoc-po_number.
  DATA: it_po_header TYPE bapiekkoc,
        it_po_header_add_data TYPE  bapiekkoa,
        it_po_items TYPE STANDARD TABLE OF  bapiekpoc,
        it_po_item_schedules TYPE STANDARD TABLE OF  bapieket,
        it_po_item_account_assignment  TYPE STANDARD TABLE OF  bapiekkn,
        it_return TYPE bapireturn OCCURS 0 WITH HEADER LINE.


  PERFORM validaciones TABLES   it_parameters
                                it_pos_partidas
                       USING    i_vendor
                       CHANGING v_hkont_00
                                v_hkont_01
                                v_hkont_02
                                v_hkont_03
                                v_hkont_04
                                v_hkont_05
                                v_hkont_06
                                v_hkont_07
                                v_hkont_08
                                v_hkont_09
                                v_hkont_10
                                v_hkont_11
                                v_doc_type
                                v_matrn
                                v_purchorg
                                v_quantity
                                e_subrc
                                e_bapi_msg.
  CHECK e_subrc = 0.


  PERFORM crt_elemto_pep  TABLES it_pos_partidas
                          CHANGING e_subrc
                                   e_bapi_msg .
  CHECK e_subrc = 0.

  PERFORM const_bapi_po_create TABLES it_po_items
                                      it_po_item_schedules
                                      it_po_item_account_assignment
                                      it_pos_partidas
                                      it_return
                                USING i_vendor
                                      i_pur_grp
                                      i_proyect
                                      i_folio_oc
                                      v_hkont_00
                                      v_hkont_01
                                      v_hkont_02
                                      v_hkont_03
                                      v_hkont_04
                                      v_hkont_05
                                      v_hkont_06
                                      v_hkont_07
                                      v_hkont_08
                                      v_hkont_09
                                      v_hkont_10
                                      v_hkont_11
                                      v_doc_type
                                      v_matrn
                                      v_purchorg
                                      v_quantity
                             CHANGING it_po_header
                                      it_po_header_add_data
                                      v_po_number
                                      e_subrc
                                      e_bapi_msg.





ENDFUNCTION.
