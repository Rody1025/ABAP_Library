*&---------------------------------------------------------------------*
*&  Include  z_store
*&---------------------------------------------------------------------*.
INCLUDE z_process_table.

DATA: comments_table TYPE slis_t_listheader,
      comments       TYPE slis_listheader.

"********************************************************************************
"* Subroutine: top_of_page
"* Purpose: Append the comments to the comments table
"********************************************************************************
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = comments_table.
ENDFORM.
"********************************************************************************
"* Class: Store
"* Purpose: DEFINITION for Store Class
"********************************************************************************
CLASS Store DEFINITION inheriting from Operations.
  PUBLIC SECTION.
    CLASS-DATA: product_id_counter TYPE I VALUE 1.
    METHODS:
      add_product IMPORTING
                            ID               TYPE I
                            name             TYPE string
                            desc             TYPE string
                            category         TYPE string
                            price            TYPE p
                            production_date  TYPE String
                            is_available     TYPE abap_bool
                            is_available_str TYPE String default 'Available'
                  returning value(result)    type string,

      find_by_keyword IMPORTING
                        keyword TYPE string,

      find_by_price_range IMPORTING
                            start TYPE p
                            END   TYPE p,

      update_availability_by_id IMPORTING
                                  ID TYPE I,

      update_availability_by_keyword IMPORTING
                                       keyword TYPE string,

      delete_by_id IMPORTING
                     ID TYPE I,
      find_product_by_date IMPORTING
                             input_str TYPE String,

      check_file_table_length importing
                                        file_path  type String
                                        value(tab) like Inventory
                              returning value(len) type I,

      save_to_file redefinition,
      load_from_file redefinition,
      update_id_before_loading redefinition,
      init_ALV_columns redefinition,
      free_tables redefinition,
      display_table redefinition,
      check_if_exist redefinition,
      generate_report redefinition.
  PRIVATE SECTION.
    DATA: col_header_table TYPE SLIS_T_FIELDCAT_ALV,
          column           TYPE SLIS_FIELDCAT_ALV.

    DATA: go_alv       TYPE REF TO cl_salv_table,
          gt_outtab    TYPE TABLE OF Product,
          go_functions TYPE REF TO cl_salv_functions_list,
          go_layout    TYPE REF TO cl_salv_layout,
          gv_page_size TYPE i VALUE 10.

    METHODS:
      display_single_row IMPORTING
                                   instance      type  Product
                         returning value(result) type string.
ENDCLASS.
"********************************************************************************
"* Class: Store
"* Purpose: IMPLEMENTATION for Store Inventory
"********************************************************************************
CLASS store IMPLEMENTATION.

  "********************************************************************************
  "* Method: add_product
  "* Purpose: Adds a new product to the inventory if a similar product with the same ID does not already exist.
  "*          The product details are provided as input parameters to the method.
  "********************************************************************************
  METHOD add_product.
    DATA(exists) = check_if_exist( ID = ID ).
    data(date) = convert_str_to_YYYYMMDD( production_date ).
    product_instance = VALUE #( product_id  = ID
                                name = name
                                desc = desc
                                category = category
                                price = price
                                production_date = date
                                is_available = is_available
                                is_available_str = 'Available' ).

    if get_table_length( Inventory ) >= 1.
      if is_available = abap_false.
        product_instance-is_available_str = 'Not Available'.
      endif.
      if exists = abap_true.
        APPEND LINES OF append_comment_italic( 'A similar product with the same ID (' && ID &&  ') already exists') to comments_table.
        result = 'A similar product with the same ID (' && ID &&  ') already exists'.
      ELSE.
        INSERT product_instance INTO TABLE Inventory.
        product_id_counter = product_id_counter + 1.
        result = 'Itme has been inserted. You have (' && get_table_length( Inventory ) && ') items' .
      ENDIF.
    else.
      INSERT product_instance INTO TABLE Inventory.
      product_id_counter = product_id_counter + 1.
      result = 'Itme has been inserted. You have (' && exists && get_table_length( Inventory ) && ') items' .
    endif.
  ENDMETHOD.
  "********************************************************************************
  "* Method: init_ALV_columns
  "* Purpose: initializes the table headers for displaying the inventory using ALV (ABAP List Viewer).
  "********************************************************************************
  method init_ALV_columns.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'product_id' col_name = 'Id' len = 2 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'name' col_name = 'Name' len = 30 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'category' col_name = 'Category' len = 13  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 4 header = 'price' col_name = 'Price' len =  10 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 5 header = 'production_date' col_name = 'Production Date' len = 13 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 5 header = 'is_available_str' col_name = 'Availability' len = 10 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 6 header = 'desc' col_name = 'Description' len = 60 ) to col_header_table.
  endmethod.
  "********************************************************************************
  "* Method:  display_table
  "* Purpose: Displays the inventory table using ALV.
  "*          If the input parameter "is_initial" is true, the method first adds comments to the comments table indicating
  "*           the total number of products and the date and time of the display.
  "********************************************************************************
  method display_table.
    if is_inital = abap_true.
      APPEND LINES OF append_comment_header( 'Product Table' ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Total Product :' value = '' && get_table_length( Inventory ) ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Date :' value = '' && date ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Time :' value = '' && time ) to comments_table.
      APPEND LINES OF append_comment_italic( 'Look at the table and check the item.' ) to comments_table.
      APPEND LINES OF append_comment_italic( 'After pressing ESC, we will process/manipulate the data.' ) to comments_table.
      DATA(paramter_table) =  Inventory.
    else.
      paramter_table =  table.
    endif.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = paramter_table.
    " Freeing the table, while the first element is kept.
    LOOP AT comments_table FROM 2 INTO DATA(ls_header).
      DELETE comments_table INDEX 2.
    ENDLOOP.
  endmethod.
  "********************************************************************************
  "* Method: find_by_keyword
  "* Purpose: Finds products in the inventory that match a keyword provided as input.
  "*          The keyword can be either a name or a category.
  "********************************************************************************
  METHOD find_by_keyword.
    DATA: tmp_table like Inventory.
    split keyword at ':' into DATA(str1) DATA(str2).
    if str1 = 'name'.
      APPEND LINES OF append_comment_pair( key = 'Find by ' value = str1 ) to comments_table.
      APPEND LINES OF append_comment_italic( 'SELECT * from Inventory where'  )  to comments_table.
      APPEND LINES OF append_comment_italic( 'name LIKE %' && str2 && '% ') to comments_table.
      APPEND LINES OF append_comment_italic( 'or') to comments_table.
      APPEND LINES OF append_comment_italic( 'desc LIKE %' && str2 && '%' ) to comments_table.

      LOOP AT Inventory INTO product_instance WHERE name CS str2 OR desc CS str2.
        append product_instance to tmp_table.
      ENDLOOP.

    elseif str1 = 'category'.
      APPEND LINES OF append_comment_pair( key = 'Find by' value = str1 ) to comments_table.
      APPEND LINES OF append_comment_italic( 'SELECT * from Inventory where category = ' && str2  ) to comments_table.

      LOOP AT Inventory INTO product_instance WHERE category CS str2.
        append product_instance to tmp_table.
      ENDLOOP.
    endif.
    display_table( is_inital = abap_false table = tmp_table ).
  ENDMETHOD.
  "********************************************************************************
  "* Method:   find_by_price_range
  "* Purpose:  Finds products in the inventory that fall within a price range provided as input.
  "********************************************************************************
  METHOD find_by_price_range.
    APPEND LINES OF append_comment_pair( key = 'Find by Price range' value = '' && start && '-' && end ) to comments_table.
    APPEND LINES OF append_comment_italic( 'SELECT * from Inventory where' ) to comments_table.
    APPEND LINES OF append_comment_italic( '         name price in > ' && start && ' and price < ' && END ) to comments_table.
    DATA: tmp_table like Inventory.

    LOOP AT Inventory INTO product_instance WHERE price BETWEEN start AND END.
      append product_instance to tmp_table.
    ENDLOOP.
    display_table( is_inital = abap_false table = tmp_table ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_availability_by_id
  "* Purpose: Updates the availability status of a product with a given ID to available
  "********************************************************************************
  METHOD update_availability_by_id.
    APPEND LINES OF append_comment_italic( 'UPDATE Inventory set is_available = true where id =' && ID ) to comments_table.
    FIELD-symbols: <fs_instance_product> TYPE Product.
    READ TABLE Inventory INTO product_instance WITH KEY product_id = ID.
    if sy-subrc = 0.
      DATA(res) = display_single_row( instance = product_instance ).
      APPEND LINES OF append_comment_italic( res ) to comments_table.
      product_instance-is_available = abap_true.
      product_instance-is_available_str = 'Available'.
      APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    endif.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_availability_by_keyword
  "* Purpose: Updates the availability status of products that match a keyword provided as input to available.
  "*          The keyword can be either a name or a category
  "********************************************************************************
  METHOD update_availability_by_keyword.
    APPEND LINES OF append_comment_italic( 'UPDATE Inventory set is_available = true where') to comments_table.
    APPEND LINES OF append_comment_italic( 'name LIKE :' && keyword && ' AND is_available = false' ) to comments_table.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.

    LOOP AT Inventory ASSIGNING <fs_instance_product> WHERE name CS keyword and is_available = abap_false.
      DATA(res) = display_single_row( <fs_instance_product> ).
      APPEND LINES OF append_comment_italic( res ) to comments_table.
      <fs_instance_product>-is_available = abap_true.
      <fs_instance_product>-is_available_str = 'Available'.
      APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_product_by_date
  "* Purpose:  Finds products in the inventory that were produced a certain number of years ago,
  "*           where the number of years is provided as input
  "********************************************************************************
  method find_product_by_date.
    DATA: words           TYPE standard table of String,
          year            TYPE I,
          calculated_year TYPE I,
          calculated_date TYPE d.
    DATA: tmp_table like Inventory.

    SPLIT input_str at ' ' INTO table words.
    LOOP AT words INTO DATA(word).
      IF word CO '0123456789'.
        year = word.
        EXIT.
      ENDIF.
    ENDLOOP.
    calculated_year = sy-datum(4) - year.
    calculated_date = |{ calculated_year }0101|.

    APPEND LINES OF append_comment_italic( 'Search for all products that were made ~ :' && year && ' Years ago') to comments_table.
    APPEND LINES OF append_comment_italic( convert_date_to_YYYYMMDD( calculated_date )
                                           && ' -- Until -- ' &&
                                           convert_date_to_YYYYMMDD( sy-datum  ) ) to comments_table.

    LOOP at Inventory into product_instance where production_date >= calculated_date.
      append product_instance to tmp_table.
    endloop.
    display_table( is_inital = abap_false table = tmp_table ).
  endmethod.
  "********************************************************************************
  "* Method: delete_by_id
  "* Purpose: Deletes a product from the inventory with a given ID
  "********************************************************************************
  METHOD delete_by_id.
    APPEND LINES OF append_comment_italic( 'Delete 1 from Inventory where product_id = ' && ID ) to comments_table.

    READ TABLE inventory INTO product_instance WITH KEY product_id = ID.
    IF sy-subrc = 0.
      DATA(res) = display_single_row( instance = product_instance ).
      APPEND LINES OF append_comment_italic( res ) to comments_table.
      DELETE inventory WHERE product_id = ID.
    ENDIF.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    APPEND LINES OF append_comment_italic( 'Table after processing' ) to comments_table.
    display_table( is_inital = abap_false table = inventory ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: generate_report
  "* Purpose: Generates a report on the inventory, displaying the total number of products,
  "*           the number of products in each category, the average price of products in each category,
  "*           and the total price of products in each category. The method uses a temporary table to store
  "*           the report data and displays the report using ALV.
  "********************************************************************************
  method generate_report.
    free comments_table.
    free col_header_table.

    TYPES: begin of report_struct,
             category                type string,
             num_product_in_category type i,
             average_price           type P length 10 decimals 2,
             total_price             type P length 10 decimals 2,
           end of report_struct.
    DATA: Report              type table of report_struct,
          report_instance     type report_struct,
          ref_report_instance type ref to report_struct,
          counter             type i value 0.

    Loop at Inventory into product_instance.
      Read table Report reference into ref_report_instance with key category = product_instance-category.
      if sy-subrc = 0.
        ref_report_instance->num_product_in_category = ref_report_instance->num_product_in_category + 1.
        ref_report_instance->total_price = ref_report_instance->total_price + product_instance-price.
      else.
        report_instance = value #( category = product_instance-category
                                   num_product_in_category = 1 average_price = product_instance-price
                                   total_price = product_instance-price ).
        append report_instance to Report.
      endif.
      counter = counter + 1.
    endloop.

    loop at Report reference into ref_report_instance.
      ref_report_instance->average_price =  ref_report_instance->total_price /
                                            ref_report_instance->num_product_in_category.
    endloop.

    APPEND LINES OF init_ALV_column( pos = 1 header = 'category' col_name = 'Category' len = 10  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'num_product_in_category' col_name = 'Number of product' len = 15  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'total_price' col_name = 'Total Items Price' len = 20  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 4 header = 'average_price' col_name = 'Average Price' len = 10  ) to col_header_table.

    APPEND LINES OF append_comment_header( 'Final Report For Products' ) to comments_table.
    APPEND LINES OF append_comment_pair( key = 'Data :' value = date ) to comments_table.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = Report.

  endmethod.
  "********************************************************************************
  "* Method: save_in_file
  "* Purpose: Saving the table content to a file in user-given path
  "********************************************************************************
  method save_to_file.
    if get_table_length( Inventory ) > 0.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = file_path
          filetype              = 'DAT' " File type either ASC or ASCII
          write_field_separator = 'X'     " Whether or not to include field separators in the file.
        TABLES
          data_tab              = Inventory.

      IF sy-subrc <> 0.
        MESSAGE 'Error downloading file' TYPE 'E'.
      ENDIF.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: load_from_file
  "* Purpose: Load from file
  "********************************************************************************
  method load_from_file.
    Data(inserted_item_table) = Inventory.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = file_path
        filetype                = 'DAT'
        has_field_separator     = ' '
      TABLES
        data_tab                = Inventory
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
    APPEND LINES OF inserted_item_table TO Inventory.
    IF sy-subrc <> 0.
      MESSAGE 'Error uploading file. Error code :' && sy-subrc TYPE 'E'.
    ENDIF.
  endmethod.
  "********************************************************************************
  "* Private Method:update_id_before_loading
  "* Purpose: Updates id of inserted products, before loading products from the file
  "********************************************************************************
  method update_id_before_loading.
    DATA: tmp TYPE TABLE OF Product.
    Store=>product_id_counter = check_file_table_length( tab = tmp file_path = file_path ).
    Data(counter) = 1.
    Loop at Inventory reference into ref_product_instnace.
      ref_product_instnace->product_id = Store=>product_id_counter + counter.
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
  "********************************************************************************
  "* Private Method: display_single_row
  "* Purpose: Private method used to format a single row of the inventory table for display.
  "*          The method concatenates the product ID, price, name, category, and availability status into a single string
  "********************************************************************************
  METHOD display_single_row.
    DATA: is_available TYPE string.

    is_available = 'Not Available'.
    IF instance-is_available = abap_true.
      is_available = 'Available'.
    ENDIF.

    DATA(id) = ` ` && instance-product_id.
    DATA(price) = ` ` && instance-price.
    CONCATENATE id price instance-name instance-category is_available INTO result SEPARATED BY ` `.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: free_tables
  "* Purpose: Private method used to free the comments table and column header table before the next process begins.
  "********************************************************************************
  method free_tables.
    FREE comments_table.
    FREE col_header_table.
  endmethod.
  "********************************************************************************
  "* Private Method: check_if_exists
  "* Purpose: Private method used to check if a product with a given ID already exists in the inventory.
  "*          The method returns true if the product exists and false otherwise.
  "********************************************************************************
  METHOD check_if_exist.
    READ TABLE inventory INTO product_instance WITH KEY product_id = ID.
    if sy-subrc = 0.
      is_exists = abap_true.
    else.
      is_exists = abap_false.
    endif.
  ENDMETHOD.
ENDCLASS.