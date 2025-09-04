
PROCESS BEFORE OUTPUT.
  MODULE status_0200.
  MODULE change_0200.
  MODULE clear_ok.
*
PROCESS AFTER INPUT.
  MODULE user_command_0200.

PROCESS ON VALUE-REQUEST.
  FIELD rcgfiletr-ftappl MODULE f4_upload.
  FIELD rcgfiletr-ftfront MODULE f4_download.
