*&---------------------------------------------------------------------*
*&  Include  z_cart
*&---------------------------------------------------------------------*
CLASS Cart DEFINITION inheriting from Operations.
  PUBLIC SECTION.
    CLASS-DATA: cart_id_counter TYPE i VALUE 1.
    DATA: no_historization TYPE abap_bool VALUE abap_false.

    TYPES: begin of product_cart_table,
             cart_id       type i,
             customer_id   type i,
             customer_name type string,
             product_id    type i,
             price         type p length 10 decimals 2,
             product_name  type string,
           end of product_cart_table.

    DATA: tmp_table2          type table of product_cart_table,
          tmp_table_instance2 type product_cart_table.

    TYPES: BEGIN OF cart_readable,
             cart_id      TYPE I,
             customer_id  TYPE I,
             product_id   TYPE I,
             history_flag TYPE string,
           END OF cart_readable.

    DATA: tmp_table          TYPE TABLE OF cart_readable,
          tmp_table_instance TYPE cart_readable.

    METHODS:
      add_item IMPORTING
                 cart_id      TYPE i
                 customer_id  TYPE i
                 product_id   TYPE i
                 history_flag TYPE abap_bool,
      update_item IMPORTING
                    cart_id      TYPE i
                    customer_id  TYPE i
                    product_id   TYPE i
                    history_flag TYPE abap_bool,
      delete_item IMPORTING
                    ID TYPE i,
      display_customer_cart  IMPORTING
                               customer_id TYPE i,
      display_product_cart IMPORTING
                             product_id TYPE i,
      checkout IMPORTING
                 customer_id TYPE i,
      purchase  IMPORTING
                  customer_id TYPE i,
      login_and_view IMPORTING
                       customer_id       TYPE i
                       customer_email    TYPE string
                       customer_password TYPE string
                       history_flag      TYPE abap_bool,
      init_ALV_columns redefinition,
      free_tables redefinition,
      display_table redefinition.

  PRIVATE SECTION.
    DATA: col_header_table TYPE SLIS_T_FIELDCAT_ALV,
          column           TYPE SLIS_FIELDCAT_ALV.
    METHODS:
      display_single_row IMPORTING
                                   instance      TYPE Cart_struct
                         returning value(result) type String,

      make_table_readable importing
                                    table      like Customer_cart
                          returning value(res) like tmp_table,

      display_tmp_table,

      flag_item_by_customer_id IMPORTING
                                 customer_id TYPE i,

      get_customer_name IMPORTING
                                  customer_id TYPE i
                        RETURNING VALUE(name) TYPE string.
ENDCLASS.

CLASS Cart IMPLEMENTATION.
  "********************************************************************************
  "* Private Method: get_customer_name
  "* Purpose: Getting Customer name
  "********************************************************************************
  METHOD get_customer_name.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      name = customer_instance-name.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: add_item
  "* Purpose: Add item to the cart
  "********************************************************************************
  METHOD add_item.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      READ TABLE Inventory INTO product_instance WITH KEY product_id = product_id.
      IF sy-subrc = 0.
        cart_instance = VALUE #( cart_id = cart_id customer_id = customer_id product_id = product_id history_flag = history_flag ).
        INSERT cart_instance INTO TABLE Customer_cart.
        cart_id_counter = cart_id_counter + 1.
      ELSE.
        APPEND LINES OF append_comment_italic( 'Product of the ID :' && product_id &&' does not exists!' ) to comments_table.
        APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
      ENDIF.
    ELSE.
      APPEND LINES OF append_comment_italic( 'Customer of the ID :' && customer_id &&' does not exists!') to comments_table.
      APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: init_ALV_columns
  "* Purpose: Initialize the table headers
  "********************************************************************************
  method init_ALV_columns.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'cart_id' col_name = 'Id' len = 2  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'customer_id' col_name = 'Customer Id' len = 10 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'product_id' col_name = 'Product Id' len = 10  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 4 header = 'history_flag' col_name = 'historized' len =  10 ) to col_header_table.
  endmethod.
  "********************************************************************************
  "* Method: display_cart_table
  "* Purpose: Display Cart table
  "********************************************************************************
  method display_table.
    if is_inital = abap_true.
      APPEND LINES OF append_comment_header( 'Cart Table' ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Total item in Cart :' value = '' && get_table_length( Customer_cart ) ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Date :' value = '' && date ) to comments_table.
      APPEND LINES OF append_comment_italic( 'Look at the table and check the item.' ) to comments_table.
      APPEND LINES OF append_comment_italic( 'After pressing ESC, we will process the data.' ) to comments_table.
      DATA(tmp_table) = make_table_readable( Customer_cart ).
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
  endmethod.
  "********************************************************************************
  "* Method: display_tmp_table
  "* Purpose: Display tmp table
  "********************************************************************************
  method display_tmp_table.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = tmp_table2.
    " Freeing the table, while the first element is kept.
    LOOP AT comments_table FROM 2 INTO DATA(ls_header).
      DELETE comments_table INDEX 2.
    ENDLOOP.
    free tmp_table2.
  endmethod.
  "********************************************************************************
  "* Method: display_customer_cart
  "* Purpose: Find all product for certain customer
  "********************************************************************************
  METHOD display_customer_cart.
    free col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'cart_id' col_name = 'Id' len = 2  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'customer_id' col_name = 'Customer Id' len = 10 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'customer_name' col_name = 'Customer Name' len = 20 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'product_id' col_name = 'Product Id' len = 10  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'product_name' col_name = 'Product Name' len = 40  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'price' col_name = 'Price' len = 10  ) to col_header_table.

    APPEND LINES OF append_comment_italic( 'SELECT * form Customer_cart where customer_id = ' && customer_id ) to comments_table.
    DATA total_price TYPE p length 10 DECIMALS 2.

    LOOP AT Customer_cart INTO cart_instance WHERE customer_id = customer_id AND history_flag = abap_false.
      READ TABLE Inventory INTO product_instance WITH KEY product_id = cart_instance-product_id.
      IF sy-subrc = 0.
        READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
        if  sy-subrc = 0.
          tmp_table_instance2-cart_id = cart_instance-cart_id.
          tmp_table_instance2-customer_id = cart_instance-customer_id.
          tmp_table_instance2-customer_name = customer_instance-name.
          tmp_table_instance2-product_id = cart_instance-product_id.
          tmp_table_instance2-product_name = product_instance-name.
          tmp_table_instance2-price = product_instance-price.
          append tmp_table_instance2 to tmp_table2.
          total_price = total_price + product_instance-price.
        endif.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF append_comment_italic(' Total reconds => ' && total_price && 'CHF' ) to comments_table.
    display_tmp_table( ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: display_product_cart
  "* Purpose: Find all customer for certain product
  "********************************************************************************
  METHOD display_product_cart.
    free col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'cart_id' col_name = 'Id' len = 2  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'customer_id' col_name = 'Customer Id' len = 10 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'customer_name' col_name = 'Customer Name' len = 20 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'product_id' col_name = 'Product Id' len = 10  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'product_name' col_name = 'Product Name' len = 40  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'price' col_name = 'Price' len = 10  ) to col_header_table.

    APPEND LINES OF append_comment_italic( 'SELECT * form Customer_cart where product_id = ' && product_id ) to comments_table.
    LOOP AT Customer_cart INTO cart_instance WHERE product_id = product_id AND history_flag <> abap_true.
      READ TABLE Inventory INTO product_instance WITH KEY product_id = cart_instance-product_id.
      IF sy-subrc = 0.
        READ TABLE Customer INTO customer_instance WITH KEY customer_id = cart_instance-customer_id.
        if  sy-subrc = 0.
          tmp_table_instance2-cart_id = cart_instance-cart_id.
          tmp_table_instance2-customer_id = cart_instance-customer_id.
          tmp_table_instance2-customer_name = customer_instance-name.
          tmp_table_instance2-product_id = cart_instance-product_id.
          tmp_table_instance2-product_name = product_instance-name.
          tmp_table_instance2-price = product_instance-price.
          append tmp_table_instance2 to tmp_table2.
        endif.
      ENDIF.
    ENDLOOP.
    display_tmp_table( ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_item
  "* Purpose: Update item to the cart
  "********************************************************************************
  METHOD update_item.
    APPEND LINES OF append_comment_italic( 'UPDATE Customer_cart set ... where id = ' && cart_id ) to comments_table.
    READ TABLE Customer_cart INTO cart_instance WITH KEY cart_id = cart_id.
    IF sy-subrc = 0.
      READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
      IF sy-subrc = 0.
        READ TABLE Inventory INTO product_instance WITH KEY product_id = product_id.
        IF sy-subrc = 0.
          APPEND LINES OF append_comment_italic( 'Customer cart before editing:' ) to comments_table.
          Data(res) = display_single_row( instance = cart_instance ).
          APPEND LINES OF append_comment_italic( res ) to comments_table.
          cart_instance-customer_id = customer_id.
          cart_instance-product_id = product_id.
          cart_instance-history_flag = history_flag.
          APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
          APPEND LINES OF append_comment_italic( 'Customer cart after editing:' ) to comments_table.
          Data(res2) = display_single_row( instance = cart_instance ).
          APPEND LINES OF append_comment_italic( res2 ) to comments_table.
          APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
        ELSE.
          APPEND LINES OF append_comment_italic( 'You cant update this customer. The product_id :') to comments_table.
          APPEND LINES OF append_comment_italic(  product_id && ' does not exists!' ) to comments_table.
        ENDIF.
      ELSE.
        APPEND LINES OF append_comment_italic( 'You cant update this customer. The customer_id :' ) to comments_table.
        APPEND LINES OF append_comment_italic( customer_id && ' does not exists!' ) to comments_table.
      ENDIF.
    ELSE.
      APPEND LINES OF append_comment_italic(  'No item with the Id: ' && cart_id && ' exists!' ) to comments_table.
    ENDIF.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_item
  "* Purpose: Delete item from the cart by its id
  "********************************************************************************
  METHOD delete_item.
    APPEND LINES OF append_comment_italic( 'DELETE 1 form Customer_cart where id = ' && ID ) to comments_table.
    READ TABLE Customer_cart INTO cart_instance WITH KEY cart_id = ID.
    IF sy-subrc = 0.
      DELETE Customer_cart WHERE cart_id = ID.
    ELSE.
      APPEND LINES OF append_comment_italic( 'No Cusomer Cart exists with ID ' && ID ) to comments_table.
    ENDIF.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    free col_header_table.
    init_ALV_columns(  ).
    display_table( is_inital = abap_false table = Customer_cart ).
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: flag_item_by_customer_id
  "* Purpose: Flag an item to be historized
  "********************************************************************************
  METHOD flag_item_by_customer_id.
    LOOP AT Customer_cart ASSIGNING <fs_cart_instance> WHERE customer_id = customer_id.
      <fs_cart_instance>-history_flag = abap_true.
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: checkout
  "* Purpose: Checkout for a certain customer
  "********************************************************************************
  METHOD checkout.
    clear comments_table.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      APPEND LINES OF append_comment_pair( key = 'Checkout for user = ' value =  customer_instance-name ) to comments_table.
    ELSE.
      APPEND LINES OF append_comment_italic( 'No Cart exists for the user with the id of:' && customer_id ) to comments_table.
    ENDIF.
    APPEND LINES OF append_comment_italic( '--------------------------------------------------------------' ) to comments_table.
  ENDMETHOD.
  "********************************************************************************
  "* Method: purchase
  "* Purpose: Purchase after checking out for a certain customer
  "********************************************************************************
  METHOD purchase.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      APPEND LINES OF append_comment_italic( 'Purchase for the user = ' && customer_instance-name ) to comments_table.
      APPEND LINES OF append_comment_italic( 'Purchasing ... ') to comments_table.
      APPEND LINES OF append_comment_italic( 'Thank your for buying from us. An email has ben send to your email address :' && customer_instance-email ) to comments_table.
      APPEND LINES OF append_comment_italic( '--------------------------------------------------------------' ) to comments_table.
      APPEND LINES OF append_comment_italic( 'List of the product that you bought:') to comments_table.
      LOOP AT Customer_cart INTO cart_instance WHERE customer_id = customer_id AND history_flag <> abap_true.
        display_customer_cart( customer_id ).
        flag_item_by_customer_id( customer_id ).
      ENDLOOP.
    ELSE.
      APPEND LINES OF append_comment_italic( 'No Cart exists for the user with the id of:' && customer_id ) to comments_table.
    ENDIF.
    clear comments_table.
  ENDMETHOD.
  "********************************************************************************
  "* Method: login_and_view
  "* Purpose: This method enable us to view the login_and_view of the purchased items for a certain customer
  "********************************************************************************
  METHOD login_and_view.
    APPEND LINES OF append_comment_italic( 'Login |') to comments_table.
    APPEND LINES OF append_comment_italic( 'Enter you username and password |') to comments_table.
    APPEND LINES OF append_comment_italic( ' ') to comments_table.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      DATA(name) = customer_instance-name.
      DATA(email) = customer_instance-email.
      DATA(password) = customer_instance-password.
      APPEND LINES OF append_comment_italic( 'User entered their username and password | Checking ...') to comments_table.
      APPEND LINES OF append_comment_italic( 'UserInput :' && customer_email && ' ---- ' && customer_password ) to comments_table.
      APPEND LINES OF append_comment_italic( 'Databases :' && email && ' ---- ' &&  password ) to comments_table.

      if customer_email = email and customer_password = password.
        DATA total_price TYPE p length 10 DECIMALS 2.
        APPEND LINES OF append_comment_italic( ' ') to comments_table.
        APPEND LINES OF append_comment_italic( 'Login done successfully. Welcome :' && name ) to comments_table.
        APPEND LINES OF append_comment_italic( '-------------------') to comments_table.
        APPEND LINES OF append_comment_italic( 'List of items you bought' ) to comments_table.
        LOOP AT Customer_cart INTO cart_instance WHERE customer_id = customer_id AND history_flag = history_flag.
          READ TABLE Inventory INTO product_instance WITH KEY product_id = cart_instance-product_id.
          IF sy-subrc = 0.
            tmp_table_instance2-cart_id = cart_instance-cart_id.
            tmp_table_instance2-customer_id = cart_instance-customer_id.
            tmp_table_instance2-product_id = cart_instance-product_id.
            tmp_table_instance2-price = product_instance-price.
            tmp_table_instance2-product_name = product_instance-name.
            append tmp_table_instance2 to tmp_table2.
            total_price = total_price + product_instance-price.
            WRITE:/.
          ENDIF.
        ENDLOOP.
        display_tmp_table( ).
      else.
        APPEND LINES OF append_comment_italic( 'The Email or the password is incorrect' ) to comments_table.
      endif.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_cart
  "* Purpose: Display single Customer Cart
  "********************************************************************************
  METHOD display_single_row.
    data: is_history type string value 'NA'.

    if instance-history_flag = abap_true.
      is_history = 'Flagged for historization'.
    endif.
    result = instance-product_id && ' | ' && instance-product_id && ' | ' && instance-product_id.
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
  "* Method: make_table_readable
  "* Purpose: We change the abap_bool value to a string to read it as Premium or Flagged
  "* instead of x or empty
  "********************************************************************************
  method make_table_readable.
    LOOP AT customer_Cart INTO cart_instance.
      tmp_table_instance-cart_id = cart_instance-cart_id.
      tmp_table_instance-customer_id = cart_instance-customer_id.
      tmp_table_instance-product_id = cart_instance-product_id.
      tmp_table_instance-history_flag = 'NA'.
      if cart_instance-history_flag = abap_true.
        tmp_table_instance-history_flag = 'Historized'.
      endif.
      append tmp_table_instance to res.
    ENDLOOP.
  endmethod.
ENDCLASS.