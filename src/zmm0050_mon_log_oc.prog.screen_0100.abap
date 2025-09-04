
**********************************************************************
***    Eventos antes de mostrar pantalla  ***
PROCESS BEFORE OUTPUT.
  MODULE status_0100. "comandos y botones toolbar de dynpro
  MODULE alv_0100. "Construye y hace Refresh de ALV ontra Itab
  module init_dynnr.
  CALL SUBSCREEN as_0100 INCLUDING 'ZMM0050_MON_LOG_OC' gv_dynnr.
  MODULE clear_ok. "Siempre se limpria el OK al final del PBO

**********************************************************************
***    Eventos despues de que el usuario hizo algo ***
PROCESS AFTER INPUT. "
  CALL SUBSCREEN as_0100.
  MODULE user_command_0100.
