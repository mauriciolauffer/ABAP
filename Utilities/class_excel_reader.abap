CLASS zcl_excel_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC . "Maurício Lauffer

*&---------------------------------------------------------------------*
*& Class ZCL_EXCEL_READER
*&
*&---------------------------------------------------------------------*
*& ABAP Sample Code
*& Read Excel File
*&
*& Mauricio Lauffer
*& http://www.linkedin.com/in/mauriciolauffer
*&
*&---------------------------------------------------------------------*

  PUBLIC SECTION.
    TYPE-POOLS soi .

*"* public components of class ZCL_EXCEL_READER
*"* do not include other source files here!!!
    METHODS constructor
      IMPORTING
        !iv_filename   TYPE localfile
        !iv_rows_to    TYPE i
        !iv_columns_to TYPE i .
    CLASS-METHODS get_file_data
      IMPORTING
        !iv_filename   TYPE localfile
        !iv_rows_to    TYPE i
        !iv_columns_to TYPE i
      RETURNING
        VALUE(rt_data) TYPE soi_generic_table
      RAISING
        cx_ios_spreadsheet .
  PROTECTED SECTION.
*"* protected components of class ZCL_EXCEL_READER
*"* do not include other source files here!!!
  PRIVATE SECTION.

    DATA mif_control TYPE REF TO i_oi_container_control .
    DATA mif_document_proxy TYPE REF TO i_oi_document_proxy .
    DATA mif_error TYPE REF TO i_oi_error .
    DATA mif_spreadsheet TYPE REF TO i_oi_spreadsheet .
    DATA mv_filename TYPE localfile .
    DATA mv_rows_from TYPE i .
    DATA mv_columns_from TYPE i .
    DATA mv_rows_to TYPE i .
    DATA mv_columns_to TYPE i .

    METHODS init_document_proxy
      RAISING
        cx_ios_spreadsheet .
    METHODS read_file
      RETURNING
        VALUE(rt_file_data) TYPE soi_generic_table
      RAISING
        cx_ios_spreadsheet .
ENDCLASS.



CLASS zcl_excel_reader IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EXCEL_READER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        LOCALFILE
* | [--->] IV_ROWS_TO                     TYPE        I
* | [--->] IV_COLUMNS_TO                  TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    mv_filename     = iv_filename.
    mv_rows_from    = 1.
    mv_columns_from = 1.
    mv_rows_to      = iv_rows_to.
    mv_columns_to   = iv_columns_to.

  ENDMETHOD.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EXCEL_READER=>GET_FILE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        LOCALFILE
* | [--->] IV_ROWS_TO                     TYPE        I
* | [--->] IV_COLUMNS_TO                  TYPE        I
* | [<-()] RT_DATA                        TYPE        SOI_GENERIC_TABLE
* | [!CX!] CX_IOS_SPREADSHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_file_data.
*-----------------------------------------------------------------------*
* ITS GROUP                                                             *
* Cliente  : Zaffari                                                    *
* Módulo   : N/A                                                        *
* Transação: N/A                                                        *
* Descrição: Ler arquivo Excel                                          *
* Autor    : Maurício Lauffer                                           *
* Data     : 12/11/2014                                                 *
*-----------------------------------------------------------------------*

    DATA:
      lo_excel_reader TYPE REF TO zcl_excel_reader.


    TRY .
        CREATE OBJECT lo_excel_reader
          EXPORTING
            iv_filename   = iv_filename
*           i_rows_from   = i_rows_from
*           i_columns_from = i_columns_from
            iv_rows_to    = iv_rows_to
            iv_columns_to = iv_columns_to.

        lo_excel_reader->init_document_proxy( ).
        rt_data = lo_excel_reader->read_file( ).

      CATCH cx_root.
        RAISE EXCEPTION TYPE cx_ios_spreadsheet.
    ENDTRY.

  ENDMETHOD.                    "get_file_data


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EXCEL_READER->INIT_DOCUMENT_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_IOS_SPREADSHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_document_proxy.

    DATA:
      lo_container TYPE REF TO cl_gui_custom_container.


    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = mif_control
        error   = mif_error
*       retcode =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
    ENDIF.

    CREATE OBJECT lo_container
      EXPORTING
*       parent                      =
        container_name              = 'CONT'
*       style                       =
*       lifetime                    = lifetime_default
*       repid                       =
*       dynnr                       =
*       no_autodef_progid_dynnr     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ios_spreadsheet.
    ENDIF.

    CALL METHOD mif_control->init_control
      EXPORTING
*       dynpro_nr            = SY-DYNNR
*       gui_container        = ' '
        inplace_enabled      = 'X'
*       inplace_mode         = 0
*       inplace_resize_documents = ' '
*       inplace_scroll_documents = ' '
*       inplace_show_toolbars    = 'X'
*       no_flush             = ' '
*       parent_id            = cl_gui_cfw=>dynpro_0
        r3_application_name  = 'EXCEL CONTAINER'
*       register_on_close_event  = ' '
*       register_on_customv_event = ' '
*       rep_id               = SY-REPID
*       shell_style          = 1384185856
        parent               = lo_container
*       name                 =
*       autoalign            = 'x'
      IMPORTING
        error                = mif_error
*       retcode              =
      EXCEPTIONS
        javabeannotsupported = 1
        OTHERS               = 2.
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
    ENDIF.

    CALL METHOD mif_control->get_document_proxy
      EXPORTING
*       document_format    = 'NATIVE'
        document_type  = soi_doctype_excel_sheet
*       no_flush       = ' '
*       register_container = ' '
      IMPORTING
        document_proxy = mif_document_proxy
        error          = mif_error
*       retcode        =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
    ENDIF.

  ENDMETHOD.                    "init_document_proxy


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_EXCEL_READER->READ_FILE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_FILE_DATA                   TYPE        SOI_GENERIC_TABLE
* | [!CX!] CX_IOS_SPREADSHEET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_file.

    DATA:
      lv_document_url TYPE char300,
      lt_sheets       TYPE soi_sheets_table,
      ls_sheets       TYPE soi_sheets,
      lt_ranges       TYPE soi_range_list.


    lv_document_url = 'FILE://' && mv_filename.

    CALL METHOD mif_document_proxy->open_document
      EXPORTING
        document_title = text-002
        document_url   = lv_document_url
*       no_flush       = ' '
        open_inplace   = 'X'
*       open_readonly  = ' '
*       protect_document = ' '
*       onsave_macro   = ' '
*       startup_macro  = ''
*       user_info      =
      IMPORTING
        error          = mif_error
*       retcode        =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

    CALL METHOD mif_document_proxy->get_spreadsheet_interface
*    EXPORTING
*      no_flush        = ' '
      IMPORTING
        error           = mif_error
        sheet_interface = mif_spreadsheet
*       retcode         =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

    CALL METHOD mif_spreadsheet->get_sheets
*    EXPORTING
*      no_flush = ' '
*     updating = -1
      IMPORTING
        sheets = lt_sheets
        error  = mif_error
*       retcode  =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

    READ TABLE lt_sheets INTO ls_sheets INDEX 1.

    CALL METHOD mif_spreadsheet->select_sheet
      EXPORTING
        name  = ls_sheets-sheet_name
*       no_flush = ' '
      IMPORTING
        error = mif_error
*       retcode  =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

    CALL METHOD mif_spreadsheet->set_selection
      EXPORTING
        top     = mv_rows_from
        left    = mv_columns_from
        rows    = mv_rows_to
        columns = mv_columns_to.

    CALL METHOD mif_spreadsheet->insert_range
      EXPORTING
        name    = text-003
        rows    = mv_rows_to
        columns = mv_columns_to
*       no_flush = ' '
      IMPORTING
        error   = mif_error.
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

    CALL METHOD mif_spreadsheet->get_ranges_data
      EXPORTING
*       no_flush = ' '
        all      = abap_true
*       updating = -1
*       rangesdef =
      IMPORTING
        contents = rt_file_data
        error    = mif_error
*       retcode  =
      CHANGING
        ranges   = lt_ranges.

    CALL METHOD mif_document_proxy->close_document
*  EXPORTING
*    do_save     = ' '
*    no_flush    = ' '
      IMPORTING
        error = mif_error
*       has_changed =
*       retcode     =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

    CALL METHOD mif_document_proxy->release_document
*  EXPORTING
*    no_flush = ' '
      IMPORTING
        error = mif_error
*       retcode  =
      .
    IF mif_error->has_failed = abap_true.
      mif_error->raise_message( type = 'E' ).
      RETURN.
    ENDIF.

  ENDMETHOD.                    "read_file
ENDCLASS.