FUNCTION-POOL zmmgf_ctr_presp2           MESSAGE-ID sv.

TYPES: ty_it_hdr TYPE TABLE OF zmmtt_30_hjsrv_h,
       ty_it_det TYPE TABLE OF zmmtt_30_hjsrv_d.

TYPES: ty_it_po_item TYPE TABLE OF bapiekpo,
       ty_it_po_serv TYPE TABLE OF bapiesll,
       ty_it_po_accs TYPE TABLE OF bapieskl.

TYPES: ty_it_hojas TYPE TABLE OF zmmwa_0030_exp_hoja,
       ty_it_msg   TYPE TABLE OF zmmwa_0030_exp_msg.
TYPES: BEGIN OF gty_wa_pedidos,
        ebeln TYPE ebeln,
        verkf TYPE verkf,
        END OF gty_wa_pedidos.
TYPES: gty_it_pedidos TYPE TABLE OF gty_wa_pedidos.

TYPE-POOLS slis.
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzmmgf_ctr_presp2t00                    . "view rel. data dcl.

*field-symbols: <fs_det> type zmmwa_0030_hjsrv_d.
DATA: wa_det      TYPE zmmwa_0030_hjsrv_d.
DATA: vl_rfc      TYPE zrfcemi,
      vl_subrc    TYPE subrc,
      vl_msgerr   TYPE bapi_msg,
      vl_msg_aux  TYPE string,
      vl_pedido   TYPE zmmde_ebeln,
      vl_prov     TYPE zmmde_lifnr,
      vl_factura  TYPE zmmde_0030_factura,
      vl_fact2    TYPE zserfol_uuid.
DATA: wa_msg_error TYPE zmmwa_0030_exp_msg.
DATA: it_datos TYPE STANDARD TABLE OF zwa_lblni,
      wa_datos TYPE zwa_lblni.
TABLES: ekbe.
