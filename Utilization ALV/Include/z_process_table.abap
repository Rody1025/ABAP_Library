*&---------------------------------------------------------------------*
*&  Include  z_process_table
*&---------------------------------------------------------------------*

CLASS Operations DEFINITION.
  public section.
    methods:
      append_comment_header IMPORTING
                                      str                   TYPE string
                            returning value(comments_table) type slis_t_listheader,
      append_comment_pair IMPORTING
                                    key                   TYPE string
                                    value                 TYPE string
                          returning value(comments_table) type slis_t_listheader,
      append_comment_italic IMPORTING
                                      str                   TYPE string
                            returning value(comments_table) type slis_t_listheader,

      init_ALV_column importing
                                pos                     type i
                                header                  type string
                                col_name                type string
                                len                     type i
                      returning value(col_header_table) type SLIS_T_FIELDCAT_ALV,

      get_table_length IMPORTING
                         value(type_any_table) TYPE any TABLE
                       RETURNING
                         VALUE(result)         TYPE i,
      init_ALV_columns,

      display_table importing
                      is_inital    type abap_bool
                      value(table) TYPE any TABLE OPTIONAL,

      check_if_exist IMPORTING
                               ID               TYPE I
                     RETURNING VALUE(is_exists) TYPE abap_bool,

      free_tables,
      generate_report.
  protected section.
  private section.
    DATA: comments TYPE slis_listheader.

endclass.

CLASS Operations implementation.
  "********************************************************************************
  "* Private Method: append_comment_header
  "* Purpose: Assign a header
  "********************************************************************************
  METHOD append_comment_header.
    comments-typ  =  'H'.
    comments-info = str.
    APPEND comments TO comments_table.
    CLEAR comments.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: append_comment_italic
  "* Purpose: Assign an italic text
  "********************************************************************************
  METHOD append_comment_italic.
    comments-typ  =  'A'.
    comments-info = str.
    APPEND comments TO comments_table.
    CLEAR comments.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: append_comment_pair
  "* Purpose: Assign a pair (value, key)
  "********************************************************************************
  METHOD append_comment_pair.
    comments-typ =  'S'.
    comments-key = key .
    comments-info = value.
    APPEND comments TO comments_table.
    CLEAR comments.
  ENDMETHOD.
    "********************************************************************************
  "* Private Method: init_ALV_column
  "* Purpose: Initialize the column header
  "********************************************************************************
  method init_ALV_column.
    DATA: column TYPE SLIS_FIELDCAT_ALV.
    column-col_pos = pos.
    column-fieldname = header.
    column-seltext_m = col_name.
    column-outputlen = len.
    append column to col_header_table.
  endmethod.
  "********************************************************************************
  "* Private Method: get_table_length
  "* Purpose: Return the length of the table
  "********************************************************************************
  method get_table_length.
    result = LINES( type_any_table ).
  endmethod.
  "********************************************************************************
  "* Private Method: check_if_exist
  "* Purpose: check if the item exists, this is based on its ID
  "********************************************************************************
  METHOD check_if_exist.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: init_ALV_columns
  "* Purpose: Initialize the column headers
  "********************************************************************************
  method init_ALV_columns.
  endmethod.
  "********************************************************************************
  "* Private Method: free_tables
  "* Purpose: Free the table
  "********************************************************************************
  method free_tables.
  endmethod.
  "********************************************************************************
  "* Private Method: display_table
  "* Purpose: Display the whole table
  "********************************************************************************
  method display_table.
  endmethod.
    "********************************************************************************
  "* Private Method: generate_report
  "* Purpose: Generate a report
  "********************************************************************************
  method generate_report.
  endmethod.
endclass.