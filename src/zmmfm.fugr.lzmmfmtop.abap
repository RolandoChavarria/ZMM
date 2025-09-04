FUNCTION-POOL zmmfm.                        "MESSAGE-ID ..
* Bapi declarations header
DATA: gs_poheader           TYPE  bapimepoheader,
      gs_poheaderx          TYPE  bapimepoheaderx,
      gs_order              TYPE  bapimepoheader-po_number,
* itesm
      it_return             TYPE STANDARD TABLE OF   bapiret2 WITH HEADER LINE,
      it_poitem             TYPE STANDARD TABLE OF   bapimepoitem WITH HEADER LINE,
      it_poitemx            TYPE STANDARD TABLE OF   bapimepoitemx WITH HEADER LINE,
      it_poschedule         TYPE STANDARD TABLE OF   bapimeposchedule WITH HEADER LINE,
      it_poschedulex        TYPE STANDARD TABLE OF   bapimeposchedulx WITH HEADER LINE,
      it_poaccount          TYPE STANDARD TABLE OF   bapimepoaccount WITH HEADER LINE,
      it_poaccountx         TYPE STANDARD TABLE OF   bapimepoaccountx WITH HEADER LINE,
      it_pocond             TYPE STANDARD TABLE OF   bapimepocond WITH HEADER LINE,
      it_pocondx            TYPE STANDARD TABLE OF   bapimepocondx WITH HEADER LINE,
      it_poservices         TYPE STANDARD TABLE OF   bapiesllc WITH HEADER LINE,
      it_posrvaccessvalues  TYPE STANDARD TABLE OF   bapiesklc WITH HEADER LINE,
      it_popartner          TYPE STANDARD TABLE OF   bapiekkop WITH HEADER LINE.

* Global variables from the parameters table
DATA: g_bukrs       TYPE t001k-bukrs, "Sociedad
      g_bsart       TYPE esart,  "@Clase de documento de compras
      g_elpep       TYPE wbs_elem,"Elemento del plan de estructura de proyecto (elemento PEP)
      vnum18(18)    TYPE n, "" Material   n,Número de artículo
      g_ekorg       TYPE ekorg, ""Organización de compras
      g_etmen       TYPE etmen, ""Cantidad de reparto
      g_meins       TYPE meins, ""Unidad de medida base
      g_matkl       TYPE matkl, ""Grupo de artículos
      g_matnr       TYPE matnr, ""Material
      g_bus_area    TYPE num4,  ""Business area
      g_tax         TYPE c LENGTH 2,
      gs_bapiesllc  TYPE bapiesllc,
      g_test        TYPE bapiflag-bapiflag. ""Test bapi
* Structures to start to fill tables
TYPES: BEGIN OF ty_item.
        INCLUDE STRUCTURE zmmfm_0070_azul.
TYPES: ebelp TYPE ekpo-ebelp,
END OF ty_item.
DATA: it_t023t TYPE SORTED TABLE OF t023t WITH UNIQUE DEFAULT KEY.
* T001w
TYPES: BEGIN OF ty_t001w,
   werks TYPE t001w-werks,
   vkorg TYPE t001w-vkorg,
END OF ty_t001w.
DATA: it_t001w TYPE SORTED TABLE OF ty_t001w WITH UNIQUE KEY werks.
* T001k
TYPES: BEGIN OF ty_t001k,
   bwkey TYPE t001k-bwkey,
   bukrs TYPE t001k-bukrs,
END OF ty_t001k.
DATA: it_t001k TYPE SORTED TABLE OF ty_t001k WITH UNIQUE KEY bwkey.
* Services dexcriptions asnum asktx INTO TABLE it_asmdt and account item
TYPES: BEGIN OF ty_asmdt,
   asnum TYPE asnum,
   asktx TYPE asktx,
   matkl TYPE asmd-matkl,
   bklas TYPE asmd-bklas,
END OF ty_asmdt.
DATA: it_asmdt TYPE SORTED TABLE OF ty_asmdt WITH UNIQUE KEY asnum.
DATA: g_asmdt TYPE ty_asmdt.
* PARAMTERS
TYPES: BEGIN OF ty_prmtrs,
      zfield LIKE zutil_parameters-zfield,
      zchar  LIKE zutil_parameters-zchar,
      END OF ty_prmtrs.
DATA: t_prmtrs TYPE SORTED TABLE OF ty_prmtrs WITH UNIQUE KEY zfield.

DATA: v_subrc  TYPE  subrc.   "Variable de subrc para LOG
DATA: v_id_rfc TYPE zid_referencia. "Variable para ID de LOG.
DATA  v_conse TYPE zmmde_conse. "Para llevar el consecutivo de cada Param

TYPE-POOLS slis.

"Estructuras de datos ZMMFM_0080_FINALIZAR_ORDEN
TYPES: ty_positions TYPE TABLE OF zmmwa_0080_positions,
       ty_msg TYPE TABLE OF zmmwa_0030_exp_msg,
       ty_return TYPE TABLE OF zreturn.


* transpasos
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.

*** Id
DATA: g_id TYPE c LENGTH 1.
