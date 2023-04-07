*&---------------------------------------------------------------------*
*&  Include  z_operations
*&---------------------------------------------------------------------*

CLASS Process_table DEFINITION.
  public section.
    TYPES:  asem_dossier_table TYPE STANDARD TABLE OF /ASEM/DOSSIER.

    methods:
      search_by_id importing
                     id                      type /ASEM/DE_DOSSIER_NO
                     value(col_header_table) type SLIS_T_FIELDCAT_ALV,

      search_by_id_range importing
                           low                     type /ASEM/DOSSIER-dossier
                           high                    type /ASEM/DOSSIER-dossier
                           row                     type i
                           value(col_header_table) type SLIS_T_FIELDCAT_ALV,

      search_by_customer_id importing
                              customer_id             type KUNNR
                              row                     type i
                              value(col_header_table) type SLIS_T_FIELDCAT_ALV,

      search_by_creation_date importing
                                input_year              type i
                                row                     type i
                                value(col_header_table) type SLIS_T_FIELDCAT_ALV,

      search_by_interest_year importing
                                input_year              type i
                                row                     type i
                                value(col_header_table) type SLIS_T_FIELDCAT_ALV,

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
      convert_date_to_YYYYMMDD importing
                                         input_year    type i
                               returning value(result) type String.
  protected section.
  private section.
    DATA: comments      TYPE slis_listheader,
          dossier_table TYPE STANDARD TABLE OF /ASEM/DOSSIER.

    methods:
      convert_number_to_YYYYMMDD importing
                                           input_year    type i
                                 returning value(result) type d,
      display_table importing
                      value(col_header_table) type SLIS_T_FIELDCAT_ALV
                      value(dossier_table)    type asem_dossier_table.
endclass.

CLASS Process_table implementation.
  "********************************************************************************
  "* Method: search_by_id
  "* Purpose: Search by id
  "********************************************************************************
  method search_by_id.
    SELECT * FROM /ASEM/DOSSIER
        WHERE DOSSIER = @id INTO TABLE @dossier_table.
    display_table( col_header_table = col_header_table  dossier_table = dossier_table ).
  endmethod.
  "********************************************************************************
  "* Method: search_by_id_range
  "* Purpose: Search by id range
  "********************************************************************************
  method search_by_id_range.
    SELECT * FROM /ASEM/DOSSIER
            WHERE DOSSIER between @low and @high
            INTO TABLE @dossier_table
                up to @row Rows.
    display_table( col_header_table = col_header_table  dossier_table = dossier_table ).
  endmethod.
  "********************************************************************************
  "* Private Method: search_by_customer_id
  "* Purpose: Search by customer id
  "********************************************************************************
  method search_by_customer_id.
    SELECT * FROM /ASEM/DOSSIER
       WHERE KUNNR = @customer_id
       INTO TABLE @dossier_table
           up to @row Rows.
    display_table( col_header_table = col_header_table  dossier_table = dossier_table ).
  endmethod.
  "********************************************************************************
  "* Private Method: search_by_creation_date
  "* Purpose: Search by creation date
  "********************************************************************************
  method search_by_creation_date.
    DATA(wanted_date) = convert_number_to_YYYYMMDD( input_year ).

    SELECT * FROM /ASEM/DOSSIER
       WHERE ERFDATE >= @wanted_date
       INTO TABLE @dossier_table
           up to @max_item Rows.

    display_table( col_header_table = col_header_table  dossier_table = dossier_table ).
  endmethod.
  "********************************************************************************
  "* Private Method: search_by_interest_year
  "* Purpose: Search by customer id
  "********************************************************************************
  method search_by_interest_year.
    DATA(wanted_date) = convert_number_to_YYYYMMDD( input_year ).

    SELECT * FROM /ASEM/DOSSIER
    WHERE INT_CALC_DATE >= @wanted_date
    INTO TABLE @dossier_table
        up to @max_item Rows.

    display_table( col_header_table = col_header_table  dossier_table = dossier_table ).
  endmethod.
  "********************************************************************************
  "* Private Method: display_table
  "* Purpose: Display table
  "********************************************************************************
  method display_table.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = dossier_table.
  endmethod.
  "********************************************************************************
  "* Method: init_ALV_column
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
  "* Private Method: convert_number_to_YYYYMMDD
  "* Purpose: Convert year to date
  "********************************************************************************
  method convert_number_to_YYYYMMDD.
    DATA(year) = sy-datum(4) - input_year.
    DATA(month) = sy-datum+4(2).
    DATA(day) = sy-datum+6(2).
    result = |{ year }| && month && day.
  endmethod.
  "********************************************************************************
  "* Method: convert_date_to_YYYYMMDD
  "* Purpose: Convert date to form YYYY/MM/DD
  "********************************************************************************
  METHOD convert_date_to_YYYYMMDD.
    DATA(res_date) = convert_number_to_YYYYMMDD( input_year ).
    result = res_date(4) && '/' && res_date+4(2) && '/' && res_date+6(2).
  ENDMETHOD.
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
endclass.