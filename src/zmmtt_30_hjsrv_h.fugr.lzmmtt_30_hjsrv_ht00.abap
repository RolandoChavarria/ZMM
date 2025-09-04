*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_30_HJSRV_H................................*
DATA:  BEGIN OF STATUS_ZMMTT_30_HJSRV_H              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_30_HJSRV_H              .
CONTROLS: TCTRL_ZMMTT_30_HJSRV_H
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMTT_30_HJSRV_H              .
TABLES: ZMMTT_30_HJSRV_H               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
