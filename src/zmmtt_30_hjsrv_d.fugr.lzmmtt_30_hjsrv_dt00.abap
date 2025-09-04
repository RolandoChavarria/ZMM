*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMTT_30_HJSRV_D................................*
DATA:  BEGIN OF STATUS_ZMMTT_30_HJSRV_D              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMTT_30_HJSRV_D              .
CONTROLS: TCTRL_ZMMTT_30_HJSRV_D
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMTT_30_HJSRV_D              .
TABLES: ZMMTT_30_HJSRV_D               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
