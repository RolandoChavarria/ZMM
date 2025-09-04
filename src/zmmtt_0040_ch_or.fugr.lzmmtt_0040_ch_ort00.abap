*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_0040_CH_OR................................*
DATA:  BEGIN OF STATUS_ZMMTT_0040_CH_OR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_0040_CH_OR              .
CONTROLS: TCTRL_ZMMTT_0040_CH_OR
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMTT_0040_CH_OR              .
TABLES: ZMMTT_0040_CH_OR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
