*&---------------------------------------------------------------------*
*& Report  ZALV_GRID_EVENTS
*&
*&---------------------------------------------------------------------*
*& ABAP Sample Code
*& ALV Grid Report with events
*&
*& Mauricio Lauffer
*& http://www.linkedin.com/in/mauriciolauffer
*&
*& This sample explains how to create an ALV Grid Report using the
*& standard class CL_SALV_TABLE using some events.
*&---------------------------------------------------------------------*

REPORT zalv_grid_events.


CLASS lcl_event_handler DEFINITION DEFERRED. " Just to declare the variable 'go_event_handler' before the real class definition

" Declaration of the global variables
DATA:
  gt_sflight         TYPE STANDARD TABLE OF sflight, " Table used into ALV to show data
  gv_error_message   TYPE string, " Variable used to get the error message
  go_alv             TYPE REF TO cl_salv_table, " ALV object
  go_events          TYPE REF TO cl_salv_events_table,
  go_event_handler   TYPE REF TO lcl_event_handler,
  gx_salv_msg        TYPE REF TO cx_salv_msg,
  gx_salv_not_found  TYPE REF TO cx_salv_not_found.

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS : on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
    METHODS : on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    MESSAGE i001(00) WITH 'Double click: column- ' column ' / line- ' row.
  ENDMETHOD.                    "on_double_click
  METHOD on_link_click.
    MESSAGE i001(00) WITH 'Click on link: column- ' column ' / line- ' row.
  ENDMETHOD.                    "on_link_click
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION



START-OF-SELECTION.
  PERFORM select_data.

END-OF-SELECTION.
  PERFORM create_alv_object.
  PERFORM format_columns.
  PERFORM set_events.
  PERFORM show_alv.





*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select data to use and show into the ALV
*----------------------------------------------------------------------*
FORM select_data .

  " Select data from DB
  SELECT *
    FROM sflight
    INTO TABLE gt_sflight.

ENDFORM.                    " SELECT_DATA




*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_OBJECT
*&---------------------------------------------------------------------*
*       To get an instance of an ALV using the class CL_SALV_TABLE
*       you must call the factory method.
*----------------------------------------------------------------------*
FORM create_alv_object .

  " Get ALV object instance ready to use.
  " You don't need to inform any field detail to the ALV such as Name, Text, Position, Data Type, F1 Help, etc.
  " The ALV will assume all information from the Data Element used in the table and fields.
  TRY .
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_sflight
      ).

    CATCH cx_salv_msg INTO gx_salv_msg.
      gv_error_message = gx_salv_msg->get_text( ).
      WRITE gv_error_message.
  ENDTRY.

ENDFORM.                    " CREATE_ALV_OBJECT



*&---------------------------------------------------------------------*
*&      Form  format_columns
*&---------------------------------------------------------------------*
*       Get all columns reference and than get the especific column
*       which will have hotspot cell type
*----------------------------------------------------------------------*
FORM format_columns.

  DATA:
    lo_columns TYPE REF TO cl_salv_columns_table, " ALV columns - all columns from your ALV Table
    lo_column  TYPE REF TO cl_salv_column_table. " To set the column type we must use another class which is one column reference


  " Get the reference for ALV Columns, all columns from your ALV Table
  lo_columns = go_alv->get_columns( ).

  TRY.
      " Get reference for ALV column object
      lo_column ?= lo_columns->get_column( columnname = 'CARRID' ).

      " Set hotspot
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    CATCH cx_salv_not_found INTO gx_salv_not_found.
      gv_error_message = gx_salv_not_found->get_text( ).
      WRITE gv_error_message.
  ENDTRY.


ENDFORM.                    "format_columns




*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
*       Set which events will be handled by the program
*----------------------------------------------------------------------*
FORM set_events .

  " Set event handling
  go_events = go_alv->get_event( ).

  " Create handler instance
  CREATE OBJECT go_event_handler.

  " Set event handler
  SET HANDLER go_event_handler->on_double_click FOR go_events.
  SET HANDLER go_event_handler->on_link_click FOR go_events.

ENDFORM.                    " SET_EVENTS




*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       Show the ALV Grid in a new window
*----------------------------------------------------------------------*
FORM show_alv .

  " Show ALV Grid
  go_alv->display( ).

ENDFORM.                    " SHOW_ALV