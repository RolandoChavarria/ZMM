*&---------------------------------------------------------------------*
*& Report  ZMM0020_MON_STATUS_PURCH
*& TCode  ZMM0020
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Este desarrollo deberá cumplir los siguientes objetivos de funcionalidad:
*&    Se requiere conocer desde SAP el estatus histórico y actual de la
*&    entrega de cada servicio, así como las facturas que lo soportan,
*&    para cada documento de compras. Para ello será realizado un desarrollo
*&    que consulte el status de las OC en SAP respecto a la entrada de
*&    servicios y verificación de factura  y lo presente en un monitor.
*&    El desarrollo tendrá que solicitar como parámetro de entrada el Id
*&    del proyecto a consultar, teniendo un matchcode que despliegue los
*&    proyectos existentes junto con sus nombres (Tabla PROJ campos PSPID
*&    y POST1).
*&---------------------------------------------------------------------*
*&  Funcional:        Octavio Romero Nieto
*&  Especificaciones: Octavio Romero Nieto
*&  Desarrollador:    Ulises Romero
*&  Fecha:            Junio 2012
*&  Consultoria:      BAF Consulting SC
*&---------------------------------------------------------------------*


INCLUDE zmm0020_mon_status_purch_top            .    " global Data
INCLUDE zmm0020_mon_status_purch_prg.
INCLUDE zmm0020_mon_status_purch_o01            .  " PBO-Modules
INCLUDE zmm0020_mon_status_purch_i01            .  " PAI-Modules
INCLUDE zmm0020_mon_status_purch_f01            .  " FORM-Routines
