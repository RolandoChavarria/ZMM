*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_PSPIDEBELN................................*
DATA:  BEGIN OF STATUS_ZMMTT_PSPIDEBELN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_PSPIDEBELN              .
CONTROLS: TCTRL_ZMMTT_PSPIDEBELN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMTT_PSPIDEBELN              .
TABLES: ZMMTT_PSPIDEBELN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
