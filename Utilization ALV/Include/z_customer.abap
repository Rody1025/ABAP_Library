*&---------------------------------------------------------------------*
*&  Include  z_customer
*&---------------------------------------------------------------------*
"********************************************************************************
"* Class: User
"* Purpose: DEFINITION for User Class
"********************************************************************************
CLASS User DEFINITION inheriting from Operations.
  PUBLIC SECTION.
    CLASS-DATA: customer_id_counter TYPE I VALUE 1.
    TYPES: BEGIN OF user_readable,
             customer_id   TYPE I,
             name          TYPE string,
             email         TYPE string,
             password      TYPE string,
             premium       TYPE string,
             flag          TYPE string,
             flag_comments TYPE string,
           END OF user_readable.

    DATA: tmp_table          TYPE TABLE OF user_readable,
          tmp_table_instance TYPE user_readable.

    METHODS:
      add_customer    IMPORTING
                        ID            TYPE I
                        name          TYPE string
                        email         TYPE string
                        password      TYPE string
                        premium       TYPE abap_bool DEFAULT abap_false
                        flag          TYPE abap_bool DEFAULT abap_false
                        flag_comments TYPE string DEFAULT '',

      update_customer  IMPORTING
                         ID            TYPE I
                         name          TYPE string
                         email         TYPE string
                         password      TYPE string
                         premium       TYPE abap_bool DEFAULT abap_false
                         flag          TYPE abap_bool DEFAULT abap_false
                         flag_comments TYPE string,

      find_all_flagged,

      find_users_by_name IMPORTING
                           name TYPE string,

      delete_cusomter    IMPORTING
                           ID TYPE I,

      init_ALV_columns redefinition,
      free_tables redefinition,
      check_if_exist redefinition,
      display_table redefinition.
  PRIVATE SECTION.
    DATA: col_header_table TYPE SLIS_T_FIELDCAT_ALV,
          column           TYPE SLIS_FIELDCAT_ALV.
    METHODS:

      display_single_row IMPORTING
                            instance      type  User_struct
                  returning value(result) type string,

      make_table_readable IMPORTING
                                    table      Like Customer
                          returning VALUE(res) Like tmp_table.
ENDCLASS.
"********************************************************************************
"* Class: User
"* Purpose: implementation for User Class
"********************************************************************************
CLASS User IMPLEMENTATION.
  "********************************************************************************
  "* Method: add_customer
  "* Purpose: Add customer
  "********************************************************************************
  METHOD add_customer.
    customer_instance = VALUE #( customer_id = ID name = name email =
    email password = password premium = premium flag = flag flag_comments = flag_comments ).
    INSERT customer_instance INTO TABLE Customer.
    customer_id_counter = customer_id_counter + 1.
  ENDMETHOD.
  "********************************************************************************
  "* Method: init_ALV_columns
  "* Purpose: Initialize the table headers
  "********************************************************************************
  method init_ALV_columns.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'customer_id' col_name = 'Id' len = 2 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'name' col_name = 'Name' len = 15 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'email' col_name = 'Email' len = 25  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 4 header = 'password' col_name = 'Password' len =  20 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 5 header = 'premium' col_name = 'Premium' len = 9 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 5 header = 'flag' col_name = 'Flag' len = 7 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 6 header = 'flag_comments' col_name = 'Comments' len = 60 ) to col_header_table.
  endmethod.
  "********************************************************************************
  "* Method: display_Customer_table
  "* Purpose: Display Customer table
  "********************************************************************************
  METHOD display_table.
    if is_inital = abap_true.
      APPEND LINES OF append_comment_header( 'Customer Table' ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Total customer :' value = '' && get_table_length( Customer ) ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Date :' value = '' && date ) to comments_table.
      APPEND LINES OF append_comment_italic( 'Look at the table and check the item.' ) to comments_table.
      APPEND LINES OF append_comment_italic( 'After pressing ESC, we will process/manipulate the data.' ) to comments_table.

      DATA(tmp_table) = make_table_readable( Customer ).
    else.
      tmp_table = make_table_readable( table ).
    endif.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = tmp_table.
    " Freeing the table, while the first element is kept.
    LOOP AT comments_table FROM 2 INTO DATA(ls_header).
      DELETE comments_table INDEX 2.
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_users_by_name
  "* Purpose: Find users by name
  "********************************************************************************
  METHOD find_users_by_name.
    APPEND LINES OF append_comment_pair( key = 'Find by name' value = name ) to comments_table.
    APPEND LINES OF append_comment_italic( 'SELECT * from Customer where'  ) to comments_table.
    APPEND LINES OF append_comment_italic( 'name LIKE %' && name && '% ') to comments_table.
    DATA: tmp_table like Customer.

    LOOP AT Customer INTO customer_instance WHERE name CS name.
      append customer_instance to tmp_table.
    ENDLOOP.
    display_table( is_inital = abap_false table = tmp_table ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_customer
  "* Purpose: Update customer
  "********************************************************************************
  METHOD update_customer.
    APPEND LINES OF append_comment_italic( 'UPDATE Customer set ... where id = ' && ID ) to comments_table.
    FIELD-symbols: <fs_customer_instance> TYPE User_struct.

    LOOP AT Customer ASSIGNING <fs_customer_instance> WHERE customer_id = ID.
      DATA(res) = display_single_row( instance = <fs_customer_instance> ).
      APPEND LINES OF append_comment_italic( res ) to comments_table.
      <fs_customer_instance>-name = name.
      <fs_customer_instance>-email = email.
      <fs_customer_instance>-password = password.
      <fs_customer_instance>-premium = premium.
      <fs_customer_instance>-flag = flag.
      <fs_customer_instance>-flag_comments = flag_comments.
      APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_cusomter
  "* Purpose: Delete customer
  "********************************************************************************
  METHOD delete_cusomter.
    APPEND LINES OF append_comment_italic( 'DELETE 1 form Customer where id = ' && ID ) to comments_table.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = ID.
    IF sy-subrc = 0.
      DATA(res) = display_single_row( instance = customer_instance ).
      APPEND LINES OF append_comment_italic( res ) to comments_table.
      DELETE Customer WHERE customer_id = ID.
      APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    ELSE.
      APPEND LINES OF append_comment_italic( 'No Cusomer exists with ID ' && ID ) to comments_table.
    ENDIF.
    display_table( is_inital = abap_false table = Customer ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_all_flagged
  "* Purpose: Display all the flagged Customer
  "********************************************************************************
  METHOD find_all_flagged.
    APPEND LINES OF append_comment_italic( 'SELECT * from Customer where flag = true' ) to comments_table.
    DATA: tmp_table like Customer.

    LOOP AT Customer INTO customer_instance WHERE flag = abap_true.
      append customer_instance to tmp_table.
    ENDLOOP.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    APPEND LINES OF append_comment_italic( 'Table after processing' ) to comments_table.
    display_table( is_inital = abap_false table = tmp_table ).
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_single_row
  "* Purpose: Display single Customer
  "********************************************************************************
  METHOD display_single_row.
    DATA: is_permium TYPE string value 'Regular user',
          is_flagged TYPE string value 'NA'.

    IF instance-premium <> abap_false.
      is_permium = 'Permium user'.
    ENDIF.

    IF instance-flag = abap_true.
      is_flagged = 'Flagged!'.
    ENDIF.
    DATA(id) = ` ` && instance-customer_id.

    CONCATENATE id instance-name instance-password is_permium is_flagged INTO result SEPARATED BY ` `.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: free_tables
  "* Purpose: Freeing the tables before next process begin
  "********************************************************************************
  method free_tables.
    FREE comments_table.
    FREE col_header_table.
  endmethod.
  "********************************************************************************
  "* Private Method: check_if_exist
  "* Purpose: check if the item exists, this is based on its ID
  "********************************************************************************
  METHOD check_if_exist.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = ID.
    is_exists = sy-subrc = 0.
  ENDMETHOD.
  "********************************************************************************
  "* Method: make_table_readable
  "* Purpose: We change the abap_bool value to a string to read it as Premium or Flagged
  "* instead of x or empty
  "********************************************************************************
  method make_table_readable.
    LOOP AT table INTO customer_instance.
      tmp_table_instance-customer_id = customer_instance-customer_id.
      tmp_table_instance-name = customer_instance-name.
      tmp_table_instance-email = customer_instance-email.
      tmp_table_instance-password = customer_instance-password.
      tmp_table_instance-flag_comments = customer_instance-flag_comments.
      tmp_table_instance-premium = 'NA'.
      tmp_table_instance-flag = 'NA'.
      if customer_instance-premium = abap_true.
        tmp_table_instance-premium = 'Premium'.
      endif.
      if customer_instance-flag = abap_true.
        tmp_table_instance-flag = 'Flagged'.
      endif.
      append tmp_table_instance to res.
    ENDLOOP.
  endmethod.
ENDCLASS.