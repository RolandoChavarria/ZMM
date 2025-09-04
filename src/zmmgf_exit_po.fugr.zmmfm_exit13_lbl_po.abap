FUNCTION ZMMFM_EXIT13_LBL_PO.
*"----------------------------------------------------------------------
*"*"Módulo funciones actualiz.
*"
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_EBELN) TYPE  EBELN
*"     VALUE(I_VERKF) TYPE  VERKF
*"----------------------------------------------------------------------


  if i_verkf IS NOT INITIAL.
          UPDATE EKKO SET verkf = i_verkf
                WHERE ebeln EQ i_ebeln.
  ENDIF.


ENDFUNCTION.
