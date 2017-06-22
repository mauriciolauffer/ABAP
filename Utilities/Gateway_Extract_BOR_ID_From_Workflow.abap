*&---------------------------------------------------------------------*
*& Class ZCL_LOG
*&
*&---------------------------------------------------------------------*
*& ABAP Sample Code
*& BAL Log (tcode SLG1)
*&
*& Mauricio Lauffer
*& http://www.linkedin.com/in/mauriciolauffer
*&
*&---------------------------------------------------------------------*

METHOD get_bor_key_from_workflow.

    DATA:
      lt_objects        TYPE STANDARD TABLE OF swr_object,
      lt_objects_2      TYPE STANDARD TABLE OF swr_obj_2,
      lt_message_lines  TYPE sapi_msg_lines,
      lv_return         TYPE sy-subrc,
      lv_exc_msg        TYPE /iwbep/mgw_bop_rfc_excep_text,
      ls_leading_object TYPE swr_object.

    TRY .
        DATA(lo_dp_facade) = /iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).
        DATA(lv_destination) = /iwbep/cl_sb_gen_dpc_rt_util=>get_rfc_destination( io_dp_facade = lo_dp_facade ).
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

    CALL FUNCTION 'SAP_WAPI_GET_OBJECTS' DESTINATION lv_destination
      EXPORTING
        workitem_id           = i_workitem_id
      IMPORTING
        leading_object        = ls_leading_object
        return_code           = lv_return
      TABLES
        objects               = lt_objects
        message_lines         = lt_message_lines
        objects_2             = lt_objects_2
      EXCEPTIONS
        system_failure        = 1000 MESSAGE lv_exc_msg
        communication_failure = 1001 MESSAGE lv_exc_msg
        OTHERS                = 1002.

    DELETE lt_objects_2 WHERE typeid <> 'YOUR_BOR_TYPE_ID'.
    IF line_exists( lt_objects_2[ 1 ] ).
      r_bor_key = lt_objects_2[ 1 ]-instid.
    ENDIF.

  ENDMETHOD.