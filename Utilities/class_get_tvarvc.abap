CLASS zcl_tvarv_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*&---------------------------------------------------------------------*
*& Class ZCL_TVARV_UTILITIES
*&
*&---------------------------------------------------------------------*
*& ABAP Sample Code
*& Get data from TVARVC
*&
*& Maurício Lauffer - 10.05.2013
*& http://www.linkedin.com/in/mauriciolauffer
*&
*& This class helps you to get data from TVARVC
*&
*&---------------------------------------------------------------------*

  PUBLIC SECTION.
*"* public components of class ZCL_TVARV_UTILITIES
*"* do not include other source files here!!!

    CONSTANTS c_type_parameter TYPE tvarvc-type VALUE 'P'.  "#EC NOTEXT
    CONSTANTS c_type_select_opt TYPE tvarvc-type VALUE 'S'. "#EC NOTEXT

    CLASS-METHODS get_parameter
      IMPORTING
        !i_name TYPE tvarvc-name
      RETURNING
        value(r_value) TYPE tvarvc-low .
    CLASS-METHODS get_select_options
      IMPORTING
        !i_name TYPE tvarvc-name
      RETURNING
        value(rt_range) TYPE oij_el_range_t .
  PROTECTED SECTION.
*"* protected components of class ZCL_TVARV_UTILITIES
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_TVARV_UTILITIES
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_TVARV_UTILITIES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARV_UTILITIES=>GET_PARAMETER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        TVARVC-NAME
* | [<-()] R_VALUE                        TYPE        TVARVC-LOW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_parameter.

    "Get parameter (table is buffered)
    SELECT SINGLE low
      FROM tvarvc
      INTO r_value
      WHERE type = c_type_parameter
        AND name = i_name.

  ENDMETHOD.                    "get_parameter


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARV_UTILITIES=>GET_SELECT_OPTIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        TVARVC-NAME
* | [<-()] RT_RANGE                       TYPE        OIJ_EL_RANGE_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_select_options.

    DATA:
      lt_tvarvc TYPE STANDARD TABLE OF tvarvc.

    FIELD-SYMBOLS:
      <ls_tvarvc> LIKE LINE OF lt_tvarvc,
      <ls_range>  LIKE LINE OF rt_range.


    "Get select-options (table is buffered)
    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc
      WHERE type = c_type_select_opt
        AND name = i_name.

    LOOP AT lt_tvarvc ASSIGNING <ls_tvarvc> FROM sy-tabix.
      APPEND INITIAL LINE TO rt_range ASSIGNING <ls_range>.
      <ls_range>-sign   = <ls_tvarvc>-sign.
      <ls_range>-option = <ls_tvarvc>-opti.
      <ls_range>-low    = <ls_tvarvc>-low.
      <ls_range>-high   = <ls_tvarvc>-high.
    ENDLOOP.

  ENDMETHOD.                    "get_select_options
ENDCLASS.