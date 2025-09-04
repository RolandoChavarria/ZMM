*&---------------------------------------------------------------------*
*& Report  ZMM0100_INFO_COMP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Yuridia Flores
*& Fecha            : 16-DIC-2013
*& Objetivo         : Mejoras del reporte de compensación ZMM0100.
*& Transporte       : RSDK906886.
*&---------------------------------------------------------------------*
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Yuridia Flores
*& Fecha            : 18-DIC-2013
*& Objetivo         : Implementación de Botón para exportar reporte,
*&                    cabecero y detalle en un archivo excel.
*& Transporte       : RSDK906898, RSDK906906 y RSDK906912.
*&---------------------------------------------------------------------*
*& Desarrollador    : Jazmín Osuna Flores - BAF Consulting S.C.
*& Funcional        : Angélica González.
*& Fecha            : 31-MARZO-2014
*& Objetivo         : Modificar la transacción ZMM0100, en el calculo
*&                    que realiza para la obtención del resultado que
*&                    aparecen en la columna CONT-RET.
*& Transporte       : RSDK907131.
*&---------------------------------------------------------------------*
*& Desarrollador    : Ricardo Pichardo - BAF Consulting S.C.
*& Funcional        : Yuridia Flores.
*& Fecha            : 27-Junio-2014
*& Objetivo         : Mejora al performance,
*&                    evitar DUMP en PRD por timeout
*& Transporte       : .
*&---------------------------------------------------------------------*

*--> Inicio Modificación RSDK906912 - 1.
 INCLUDE: zmm0100_info_comp_top,    " global Data
          zlo4220_info_comp_cli,
          zmm0100_info_comp_f01,
          zmm0100_info_comp_f02,
          zmm0100_info_comp_i01,
          zmm0100_info_comp_o01,
          zmm0100_info_comp_prg.

*<-- Fin Modificación RSDK906912 - 1.
