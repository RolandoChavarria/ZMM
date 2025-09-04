*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_WERKS_SUST................................*
DATA:  BEGIN OF STATUS_ZMMTT_WERKS_SUST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_WERKS_SUST              .
CONTROLS: TCTRL_ZMMTT_WERKS_SUST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMTT_WERKS_SUST              .
TABLES: ZMMTT_WERKS_SUST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
