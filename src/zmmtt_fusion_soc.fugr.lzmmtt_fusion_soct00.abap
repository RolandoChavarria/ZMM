*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_FUSION_SOC................................*
DATA:  BEGIN OF STATUS_ZMMTT_FUSION_SOC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_FUSION_SOC              .
CONTROLS: TCTRL_ZMMTT_FUSION_SOC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMTT_FUSION_SOC              .
TABLES: ZMMTT_FUSION_SOC               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
