*&---------------------------------------------------------------------*
*& Report Z_ALV_FROM_CDS_VIEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_alv_from_cds_view.

START-OF-SELECTION.
cl_salv_gui_table_ida=>create_for_cds_view( CONV #( 'Z_MY_CDS_VIEW_NAME' ) )->fullscreen( )->display( ).
