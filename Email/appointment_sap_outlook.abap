REPORT zappointment_sap_outlook.


DATA:
  gt_text         TYPE so_txttab,
  gs_text         LIKE LINE OF gt_text,
  gs_participant  TYPE scspart,
  gv_title        TYPE scsappt-txt_short,
  gv_sent_to_all  TYPE os_boolean,
  go_appointment  TYPE REF TO cl_appointment,
  go_send_request TYPE REF TO cl_bcs.


CREATE OBJECT go_appointment.

gs_participant-participan = sy-uname.
go_appointment->add_participant( participant = gs_participant ).
go_appointment->set_participant_status( participant = gs_participant status = '1' ).

go_appointment->set_date(
  date_from = sy-datum
  time_from = '080000'
  time_to   = '083000' ).

go_appointment->set_location_string( 'POA' ).
go_appointment->set_type( 'ABSENT' ).
gs_text-line = |Test text { sy-datum } { sy-uzeit }|.
APPEND gs_text TO gt_text.
go_appointment->set_text( gt_text ).
gv_title = |Test title { sy-datum } { sy-uzeit }|.
go_appointment->set_title( gv_title ).
go_appointment->save( send_invitation = abap_false ).

TRY .
    go_send_request = go_appointment->create_send_request( ).
    go_send_request->set_status_attributes(
      i_requested_status = 'N'
      i_status_mail      = 'N' ).
    gv_sent_to_all = go_send_request->send( i_with_error_screen = abap_true ).
  CATCH cx_bcs.
    MESSAGE 'Error!' TYPE 'E'.
ENDTRY.