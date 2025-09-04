*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_HIST_PART.................................*
DATA:  BEGIN OF STATUS_ZMMTT_HIST_PART               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_HIST_PART               .
CONTROLS: TCTRL_ZMMTT_HIST_PART
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMTT_HIST_PART               .
TABLES: ZMMTT_HIST_PART                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
