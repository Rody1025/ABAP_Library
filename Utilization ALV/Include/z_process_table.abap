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

    METHODS:
      add_customer    IMPORTING
                                ID            TYPE I
                                name          TYPE string
                                email         TYPE string
                                password      TYPE string
                                premium       TYPE abap_bool DEFAULT abap_false
                                flag          TYPE abap_bool DEFAULT abap_false
                                flag_comments TYPE string DEFAULT ''
                      returning value(result) type string,

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

      check_file_table_length importing
                                        file_path  type String
                                        value(tab) like Customer
                              returning value(len) type I,

      save_to_file redefinition,
      load_from_file redefinition,
      update_id_before_loading redefinition,
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
                         returning value(result) type string.
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
    customer_instance = VALUE #( customer_id = ID
                                 name = name
                                 email = email
                                 password = password
                                 premium = premium
                                 premium_str = 'NA'
                                 flag = flag
                                 flag_str = 'NA'
                                 flag_comments = flag_comments ).
    if premium = abap_true.
      customer_instance-premium_str = 'Premium'.
    endif.
    if flag = abap_true.
      customer_instance-flag_str = 'Flagged!'.
    endif.

    INSERT customer_instance INTO TABLE Customer.
    customer_id_counter = customer_id_counter + 1.
    result = 'Itme has been inserted. You have (' && get_table_length( Customer ) && ') items' .

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
    APPEND LINES OF init_ALV_column( pos = 5 header = 'premium_str' col_name = 'Premium' len = 9 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 5 header = 'flag_str' col_name = 'Flag' len = 7 ) to col_header_table.
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
      DATA(parameter_table) = Customer.
    else.
      parameter_table = table.
    endif.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = parameter_table.
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

      <fs_customer_instance>-premium_str = 'NA'.
      <fs_customer_instance>-flag_str = 'NA'.
      if premium = abap_true.
        <fs_customer_instance>-premium_str = 'Premium'.
      endif.
      if flag = abap_true.
        <fs_customer_instance>-flag_str = 'Flagged!'.
      endif.

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
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    APPEND LINES OF append_comment_italic( 'Table after processing' ) to comments_table.
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
  "* Method: save_in_file
  "* Purpose: Saving the table content to a file in user-given path
  "********************************************************************************
  method save_to_file.
    if get_table_length( Customer ) > 0.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = file_path
          filetype              = 'ASC' " File type either ASC or ASCII
          write_field_separator = 'X'     " Whether or not to include field separators in the file.
        TABLES
          data_tab              = Customer.

      IF sy-subrc <> 0.
        MESSAGE 'Error downloading file' TYPE 'E'.
      ENDIF.
    endif.
  endmethod.
  "********************************************************************************
  "* Private Method: load_from_file
  "* Purpose: Load Customer from a file
  "********************************************************************************
  method load_from_file.
    Data(inserted_item_table) = Customer.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = file_path
        filetype                = 'DAT'
        has_field_separator     = ' '
      TABLES
        data_tab                = Customer
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        table_not_supported     = 11
        table_without_header    = 12
        empty_file              = 13
        dp_error_create         = 14
        dp_error_send           = 15
        dp_error_write          = 16
        unknown_dp_error        = 17
        access_denied           = 18
        dp_out_of_memory        = 19
        disk_full               = 20
        dp_timeout              = 21
        file_not_found          = 22
        dataprovider_exception  = 23
        control_flush_error     = 24.
    " Add the inserted items
    APPEND LINES OF inserted_item_table TO Customer.
    IF sy-subrc <> 0.
      MESSAGE 'Error uploading file. Error code :' && sy-subrc TYPE 'E'.
    ENDIF.
  endmethod.
  "********************************************************************************
  "* Private Method: update_id_before_loading
  "* Purpose: Update id before loading from a file to avoid data-lost
  "********************************************************************************
  method update_id_before_loading.
    DATA: tmp TYPE TABLE OF User_struct.
    User=>customer_id_counter = check_file_table_length( tab = tmp file_path = file_path ).
    Data(counter) = 1.
    Loop at Customer reference into ref_customer_instance.
      ref_customer_instance->customer_id = User=>customer_id_counter + counter.
      counter = counter + 1.
    endloop.
  endmethod.
  "********************************************************************************
  "* Private Method:check_file_table_lengthdisplay_single_row
  "* Purpose: Check how many products are in the file, to adjust the id's
  "********************************************************************************
  method check_file_table_length.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename            = file_path
        filetype            = 'DAT'
        has_field_separator = ' '
      TABLES
        data_tab            = tab.

    len = lines( tab ).
  endmethod.
  "**********
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
ENDCLASS.