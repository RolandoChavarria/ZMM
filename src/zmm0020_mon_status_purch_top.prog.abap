*&---------------------------------------------------------------------*
*& Include ZMM0020_MON_STATUS_PURCH_TOP                      Modulpool        ZMM0020_MON_STATUS_PURCH
*&
*&---------------------------------------------------------------------*

PROGRAM  zmm0020_mon_status_purch.
TABLES: zmmtt_pspidebeln, ekko.
TYPE-POOLS: slis.
INCLUDE: <icons>.

DATA: gv_subrc TYPE sy-subrc.
DATA: git_pspidebeln TYPE STANDARD TABLE OF zmmwa_pspidebeln .


DATA: git_header_monitor TYPE STANDARD TABLE OF  zmmwa_0020_header_monitor.
DATA: git_detail_monitor TYPE STANDARD TABLE OF zmmwa_0020_detail_services_mon.

DATA:GIT_HEAD TYPE slis_t_listheader WITH HEADER LINE.


DATA: git_fieldcat TYPE slis_t_fieldcat_alv.
DATA: git_events     TYPE slis_t_event.
DATA gd_prntparams TYPE slis_print_alv.

DATA: gv_tabname_item TYPE slis_tabname.
DATA: gwa_keyinfo TYPE slis_keyinfo_alv.
DATA: gv_tabname_header TYPE slis_tabname.

CONSTANTS:GC_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.

DATA: bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA: nodata VALUE '' ,          " nodata
      it_messtab LIKE STANDARD TABLE OF bdcmsgcoll WITH HEADER LINE.
  DATA: l_options LIKE ctu_params.



DATA: gv_repid LIKE sy-repid.


PARAMETERS: p_pspid TYPE zmmtt_pspidebeln-pspid OBLIGATORY.
SELECT-OPTIONS: pr_bedat FOR ekko-bedat.
