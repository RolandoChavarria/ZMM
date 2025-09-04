*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMMTT_30_HJSRV_H
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMMTT_30_HJSRV_H   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
