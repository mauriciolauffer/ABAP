CLASS zcl_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC . "Maurício Lauffer

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

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_header TYPE bal_s_log
      RAISING
        cx_law_log .
    METHODS add_message
      IMPORTING
        !is_message TYPE bal_s_msg
      RAISING
        cx_law_log .
    METHODS add_message_free_text
      IMPORTING
        !iv_message_type TYPE symsgty
        !iv_message_text TYPE string
      RAISING
        cx_law_log .
    CLASS-METHODS factory
      IMPORTING
        !is_header    TYPE bal_s_log
      RETURNING
        VALUE(ro_log) TYPE REF TO zcl_log
      RAISING
        cx_law_log .
    METHODS save
      RAISING
        cx_law_log .
    METHODS display
      RAISING
        cx_law_log .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_handle TYPE balloghndl .
ENDCLASS.



CLASS zcl_log IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOG->ADD_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MESSAGE                     TYPE        BAL_S_MSG
* | [!CX!] CX_LAW_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_handle
        i_s_msg          = is_message
*   IMPORTING
*       E_S_MSG_HANDLE   =
*       E_MSG_WAS_LOGGED =
*       E_MSG_WAS_DISPLAYED       =
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_law_log
        EXPORTING
          textid = cx_law_log=>bal_log_msg_add.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOG->ADD_MESSAGE_FREE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE_TYPE                TYPE        SYMSGTY
* | [--->] IV_MESSAGE_TEXT                TYPE        STRING
* | [!CX!] CX_LAW_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message_free_text.

    DATA:
      lv_text TYPE char200.


    lv_text = iv_message_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_handle
        i_msgty          = iv_message_type
*       I_PROBCLASS      = '4'
        i_text           = lv_text
*       I_S_CONTEXT      =
*       I_S_PARAMS       =
*     IMPORTING
*       E_S_MSG_HANDLE   =
*       E_MSG_WAS_LOGGED =
*       E_MSG_WAS_DISPLAYED       =
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_law_log
        EXPORTING
          textid = cx_law_log=>bal_log_msg_add.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOG->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_HEADER                      TYPE        BAL_S_LOG
* | [!CX!] CX_LAW_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    "Create new log session
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = is_header
      IMPORTING
        e_log_handle            = mv_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_law_log
        EXPORTING
          textid = cx_law_log=>bal_log_create.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOG->DISPLAY
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_LAW_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display.

    DATA:
      lt_handle TYPE bal_t_logh.


    APPEND mv_handle TO lt_handle.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
*       I_S_DISPLAY_PROFILE  = I_S_DISPLAY_PROFILE
        i_t_log_handle       = lt_handle
*       I_T_MSG_HANDLE       = I_T_MSG_HANDLE
*       I_S_LOG_FILTER       = I_S_LOG_FILTER
*       I_S_MSG_FILTER       = I_S_MSG_FILTER
*       I_T_LOG_CONTEXT_FILTER       = I_T_LOG_CONTEXT_FILTER
*       I_T_MSG_CONTEXT_FILTER       = I_T_MSG_CONTEXT_FILTER
*       I_AMODAL             = ' '
*       I_SRT_BY_TIMSTMP     = ' '
* IMPORTING
*       E_S_EXIT_COMMAND     = E_S_EXIT_COMMAND
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_law_log
        EXPORTING
          textid = cx_law_log=>cx_law_log.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_LOG=>FACTORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_HEADER                      TYPE        BAL_S_LOG
* | [<-()] RO_LOG                         TYPE REF TO ZCL_LOG
* | [!CX!] CX_LAW_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD factory.
    DATA:
      lx_log TYPE REF TO cx_law_log.


    TRY.
        CREATE OBJECT ro_log
          EXPORTING
            is_header = is_header.
      CATCH cx_law_log INTO lx_log.
        RAISE EXCEPTION lx_log.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOG->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_LAW_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save.

    DATA:
      lt_handle TYPE bal_t_logh.


    APPEND mv_handle TO lt_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
*       I_CLIENT         = SY-MANDT
*       i_in_update_task = abap_true
*       I_SAVE_ALL       = ' '
        i_t_log_handle   = lt_handle
*       I_2TH_CONNECTION = ' '
*       I_2TH_CONNECT_COMMIT       = ' '
*       I_LINK2JOB       = 'X'
*   IMPORTING
*       E_NEW_LOGNUMBERS =
*       E_SECOND_CONNECTION        =
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_law_log
        EXPORTING
          textid = cx_law_log=>bal_db_save.
    ENDIF.

  ENDMETHOD.
ENDCLASS.