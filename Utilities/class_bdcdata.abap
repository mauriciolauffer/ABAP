CLASS zcl_bdcdata DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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

    METHODS set_screen
      IMPORTING
        !i_program  TYPE bdc_prog
        !i_dynpro   TYPE bdc_dynr
        !i_dynbegin TYPE bdc_start OPTIONAL .
    METHODS set_field
      IMPORTING
        !i_fieldname TYPE fnam_____4
        !i_value     TYPE bdc_fval .
    METHODS set_field_select_option
      IMPORTING
        !i_so_ucomm  TYPE sy-ucomm
        !i_fieldname TYPE fnam_____4
        !i_value     TYPE bdc_fval .
    METHODS get_bdcdata_table
      RETURNING
        VALUE(rt_bdcdata) TYPE bdcdata_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_bdcdata TYPE bdcdata_tab .
ENDCLASS.



CLASS ZCL_BDCDATA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BDCDATA->GET_BDCDATA_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_BDCDATA                     TYPE        BDCDATA_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_bdcdata_table.
    rt_bdcdata = mt_bdcdata.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BDCDATA->SET_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FIELDNAME                    TYPE        FNAM_____4
* | [--->] I_VALUE                        TYPE        BDC_FVAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_field.
    APPEND INITIAL LINE TO mt_bdcdata ASSIGNING FIELD-SYMBOL(<ls_bdcdata>).
    <ls_bdcdata>-fnam = i_fieldname.
    <ls_bdcdata>-fval = i_value.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BDCDATA->SET_FIELD_SELECT_OPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SO_UCOMM                     TYPE        SY-UCOMM
* | [--->] I_FIELDNAME                    TYPE        FNAM_____4
* | [--->] I_VALUE                        TYPE        BDC_FVAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_field_select_option.
    "Should call it before
*    set_field(
*      i_fieldname = 'BDC_OKCODE'
*      i_value     = '=%004'
*    ).

    "Clear Select-Option Match-Code screen
    set_screen(
      i_program  = 'SAPLALDB'
      i_dynpro   = '3000'
      i_dynbegin = abap_true
    ).
    set_field(
      i_fieldname = 'BDC_OKCODE'
      i_value     = '/EDELA'
    ).

    "Clear field and open options
    set_screen(
      i_program  = 'SAPLALDB'
      i_dynpro   = '3000'
      i_dynbegin = abap_true
    ).
    set_field(
      i_fieldname = 'RSCSEL_255-SLOW_I(01)'
      i_value     = space
    ).
    set_field(
      i_fieldname = 'BDC_OKCODE'
      i_value     = '=SI01'
    ).

    "Select EQUAL (EQ)
    set_screen(
      i_program  = 'SAPMSSY0'
      i_dynpro   = '0120'
      i_dynbegin = abap_true
    ).
    set_field(
      i_fieldname = 'BDC_CURSOR'
      i_value     = '06/09'
    ).
    set_field(
      i_fieldname = 'BDC_OKCODE'
      i_value     = '=OKAY'
    ).

    "Set value
    set_screen(
      i_program  = 'SAPLALDB'
      i_dynpro   = '3000'
      i_dynbegin = abap_true
    ).
    set_field(
      i_fieldname = 'RSCSEL_255-SLOW_I(01)'
      i_value     = i_value
    ).
    set_field(
      i_fieldname = 'BDC_OKCODE'
      i_value     = '=ACPT'
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BDCDATA->SET_SCREEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_PROGRAM                      TYPE        BDC_PROG
* | [--->] I_DYNPRO                       TYPE        BDC_DYNR
* | [--->] I_DYNBEGIN                     TYPE        BDC_START(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_screen.
    APPEND INITIAL LINE TO mt_bdcdata ASSIGNING FIELD-SYMBOL(<ls_bdcdata>).
    <ls_bdcdata>-program  = i_program.
    <ls_bdcdata>-dynpro   = i_dynpro.
    <ls_bdcdata>-dynbegin = i_dynbegin.
  ENDMETHOD.
ENDCLASS.