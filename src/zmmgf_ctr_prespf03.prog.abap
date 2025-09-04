*&---------------------------------------------------------------------*
*&  Include           ZMMGF_CTR_PRESPF03
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Fill the ranges to select the POs
*----------------------------------------------------------------------*
FORM fill_rangesz TABLES r_ebeln STRUCTURE r_ebeln
                        r_bsart STRUCTURE r_bsart
                        r_bedat STRUCTURE r_bedat
                        r_ekgrp STRUCTURE r_ekgrp
                        r_ekorg STRUCTURE r_ekorg
                        r_lifnr STRUCTURE r_lifnr
                        r_reswk STRUCTURE r_reswk
                        r_ematn STRUCTURE r_ematn
                        r_matnr STRUCTURE r_matnr
                        r_matkl STRUCTURE r_matkl
                        r_pstyp STRUCTURE r_pstyp
                        r_knttp STRUCTURE r_knttp
                        r_werks STRUCTURE r_werks
                        r_bednr STRUCTURE r_bednr
                        r_afnam STRUCTURE r_afnam
                        stab1   STRUCTURE stab1
                        stab2   STRUCTURE stab2
                        return STRUCTURE bapireturn
                 USING  purchaseorder TYPE bapiekko-po_number
                        doc_type      TYPE bapiekko-doc_type
                        doc_date      TYPE bapiekko-doc_date
                        pur_group     TYPE bapiekko-pur_group
                        purch_org     TYPE bapiekko-purch_org
                        vendor        TYPE bapiekko-vendor
                        suppl_plnt    TYPE bapiekko-suppl_plnt
                        material      TYPE bapiekpo-material
                        pur_mat       TYPE bapiekpo-pur_mat
                        matl_group    TYPE bapiekpo-mat_grp
                        item_cat      TYPE bapiekpo-item_cat
                        acctasscat    TYPE bapiekpo-acctasscat
                        plant         TYPE bapiekpo-plant
                        trackingno    TYPE bapiekpo-trackingno
                        preq_name     TYPE bapiekpoc-preq_name
                        short_text    TYPE bapiekpo-short_text
                        read_header   TYPE xfeld
                        read_item     TYPE xfeld
                        created_by    TYPE bapiekko-created_by.

  CLEAR read_header.
  CLEAR read_item.
  CLEAR exitflag.

* fill ranges for the po number
  IF NOT purchaseorder IS INITIAL.
    r_ebeln-sign = 'I'.
    r_ebeln-option = 'EQ'.
    r_ebeln-low = purchaseorder.
    APPEND r_ebeln.
    read_header = 'X'.
  ENDIF.

* fill ranges for the document type
  IF NOT doc_type IS INITIAL.
    r_bsart-sign = 'I'.
    r_bsart-option = 'EQ'.
    r_bsart-low = doc_type.
    APPEND r_bsart.
    read_header = 'X'.
  ENDIF.

* fill ranges for the date
  IF NOT doc_date IS INITIAL.
    r_bedat-sign = 'I'.
    r_bedat-option = 'EQ'.
    r_bedat-low = doc_date.
    APPEND r_bedat.
    read_header = 'X'.
  ENDIF.

* fill ranges for the purchasing group
  IF NOT pur_group IS INITIAL.
    r_ekgrp-sign = 'I'.
    r_ekgrp-option = 'EQ'.
    r_ekgrp-low = pur_group.
    APPEND r_ekgrp.
    read_header = 'X'.
  ENDIF.

* fill ranges for the purchasing organization
  IF NOT purch_org IS INITIAL.
    r_ekorg-sign = 'I'.
    r_ekorg-option = 'EQ'.
    r_ekorg-low = purch_org.
    APPEND r_ekorg.
    read_header = 'X'.
  ENDIF.

* fill ranges for the vendor
  IF NOT vendor IS INITIAL.
    r_lifnr-sign = 'I'.
    r_lifnr-option = 'EQ'.
    r_lifnr-low = vendor.
    APPEND r_lifnr.
    read_header = 'X'.
  ENDIF.

* fill ranges for the supplying plant
  IF NOT suppl_plnt IS INITIAL.
    r_reswk-sign = 'I'.
    r_reswk-option = 'EQ'.
    r_reswk-low = suppl_plnt.
    APPEND r_reswk.
    read_header = 'X'.
  ENDIF.

* fill ranges for the mpn-material
  IF NOT material IS INITIAL.
    r_ematn-sign = 'I'.
    r_ematn-option = 'EQ'.
    r_ematn-low = material.
    APPEND r_ematn.
    read_item = 'X'.
  ENDIF.

* fill ranges for the material
  IF NOT pur_mat IS INITIAL.
    r_matnr-sign = 'I'.
    r_matnr-option = 'EQ'.
    r_matnr-low = pur_mat.
    APPEND r_matnr.
    read_item = 'X'.
  ENDIF.

* fill ranges for the material group
  IF NOT matl_group IS INITIAL.
    r_matkl-sign = 'I'.
    r_matkl-option = 'EQ'.
    r_matkl-low = matl_group.
    APPEND r_matkl.
    read_item = 'X'.
  ENDIF.

* fill ranges for the item category
  IF NOT item_cat IS INITIAL.
    r_pstyp-sign = 'I'.
    r_pstyp-option = 'EQ'.
    r_pstyp-low = item_cat.
    APPEND r_pstyp.
    read_item = 'X'.
  ENDIF.

* fill ranges for the account assignment category
  IF NOT acctasscat IS INITIAL.
    r_knttp-sign = 'I'.
    r_knttp-option = 'EQ'.
    r_knttp-low = acctasscat.
    APPEND r_knttp.
    read_item = 'X'.
  ENDIF.

* fill ranges for the plant
  IF NOT plant IS INITIAL.
    r_werks-sign = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low = plant.
    APPEND r_werks.
    read_item = 'X'.
  ENDIF.

* fill ranges for the tracking number
  IF NOT trackingno IS INITIAL.
    r_bednr-sign = 'I'.
    r_bednr-option = 'EQ'.
    r_bednr-low = trackingno.
    APPEND r_bednr.
    read_item = 'X'.
  ENDIF.

* fill ranges for the preq name
  IF NOT preq_name IS INITIAL.
    r_afnam-sign = 'I'.
    r_afnam-option = 'EQ'.
    r_afnam-low = preq_name.
    APPEND r_afnam.
    read_item = 'X'.
  ENDIF.

* fill dynamic selection table for the short text
  IF short_text CA mask1.
    TRANSLATE short_text USING mask2.
    stab1-bed+3(10) = 'TXZ01'.
    stab1-bed+14(4) = 'LIKE'.
    stab1-bed+19(1) = ''''.
    stab1-bed+20(40) = short_text.
    stab1-bed+60(1) = ''''.
    APPEND stab1.
    read_item = 'X'.
  ELSE.
    IF short_text NE space.
      stab1-bed+3(10) = 'TXZ01'.
      stab1-bed+14(4) = 'EQ'.
      stab1-bed+19(1) = ''''.
      stab1-bed+20(40) = short_text.
      stab1-bed+60(1) = ''''.
      APPEND stab1.
      read_item = 'X'.
    ENDIF.
  ENDIF.

* fill dynamic selection table for the creator
  IF created_by CA mask1.
    read_header = 'X'.
    TRANSLATE created_by USING mask2.
    stab2-bed+3(10) = 'ERNAM'.
    stab2-bed+14(4) = 'LIKE'.
    stab2-bed+19(1) = ''''.
    stab2-bed+20(12) = created_by.
    stab2-bed+32(1) = ''''.
    APPEND stab2.
  ELSE.
    IF created_by NE space.
      read_header = 'X'.
      stab2-bed+3(10) = 'ERNAM'.
      stab2-bed+14(4) = 'EQ'.
      stab2-bed+19(1) = ''''.
      stab2-bed+20(12) = created_by.
      stab2-bed+32(1) = ''''.
      APPEND stab2.
    ENDIF.
  ENDIF.

* determine the best select on database
* no select options filled - error
  IF read_header EQ space AND
     read_item EQ space.
    PERFORM fill_bapireturn TABLES return
                            USING  'E'
                                   'W5'
                                   '036'
                                   space
                                   space
                                   space
                                   space.
    IF 1 = 2. MESSAGE e036(w5). ENDIF.
    exitflag = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    "FILL_RANGES
