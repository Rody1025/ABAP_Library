*&---------------------------------------------------------------------*
*&  Include  z_cart
*&---------------------------------------------------------------------*
CLASS Cart DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: cart_id_counter TYPE i VALUE 1.
    DATA: no_historization TYPE abap_bool VALUE abap_false.

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
      sort_by_customer_id,
      sort_by_product_id,
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
      display_cart_table  IMPORTING
                            history_flag TYPE abap_bool,
      display_cart_with_names IMPORTING
                                cart_instance    TYPE Cart_struct
                                product_instance TYPE Product.

  PRIVATE SECTION.
    METHODS:
      flag_item_by_customer_id IMPORTING
                                 customer_id TYPE i,
      display_cart IMPORTING
                     instance TYPE Cart_struct,
      get_customer_name IMPORTING
                                  customer_id TYPE i
                        RETURNING VALUE(name) TYPE string,
      display_header.
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
        WRITE:/ 'Creating Customer cart ...'. write: '|Product does not exists!' COLOR 6.
      ENDIF.
    ELSE.
      WRITE:/ 'Creating Customer cart ....'. write: '|Customer does not exists!' COLOR 6.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_item
  "* Purpose: Update item to the cart
  "********************************************************************************
  METHOD update_item.
    DATA(TITLE) = 'UPDATE Customer_cart set ... where id = ' && cart_id.
    WRITE:/ TITLE COLOR 2.
    READ TABLE Customer_cart INTO cart_instance WITH KEY cart_id = cart_id.
    IF sy-subrc = 0.
      READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
      IF sy-subrc = 0.
        READ TABLE Inventory INTO product_instance WITH KEY product_id = product_id.
        IF sy-subrc = 0.
          WRITE:/'Customer cart before editing:'.
          display_cart( instance = cart_instance ).
          cart_instance-customer_id = customer_id.
          cart_instance-product_id = product_id.
          cart_instance-history_flag = history_flag.
          WRITE:/'Customer cart after editing:'.
          display_cart( instance = cart_instance ).
        ELSE.
          WRITE:/ 'You cant update this customer. The product_id : ' && product_id && ' does not exists!' COLOR 6.
        ENDIF.
      ELSE.
        WRITE:/ 'You cant update this customer. The customer_id : ' && customer_id && ' does not exists!' COLOR 6.
      ENDIF.
    ELSE.
      WRITE:/ 'No item with the Id: ' && cart_id && ' exists!'.
    ENDIF.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_item
  "* Purpose: Delete item from the cart by its id
  "********************************************************************************
  METHOD delete_item.
    DATA(TITLE) = 'DELETE 1 form Customer_cart where id = ' && ID.
    WRITE:/ TITLE COLOR 2.
    READ TABLE Customer_cart INTO cart_instance WITH KEY cart_id = ID.
    IF sy-subrc = 0.
      WRITE:/ 'Found one customer:'.
      display_cart( instance = cart_instance ).
      DELETE Customer_cart WHERE cart_id = ID.
    ELSE.
      WRITE:/ 'No Cusomer Cart exists with ID ' && ID.
    ENDIF.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: delete_item
  "* Purpose: Delete item from the cart by its customer id
  "********************************************************************************
  METHOD flag_item_by_customer_id.
    DATA(TITLE) = 'Update Customer_cart set history_flag = true where customer_id = ' && customer_id.
    WRITE:/ TITLE COLOR 2.
    LOOP AT Customer_cart ASSIGNING <fs_cart_instance> WHERE customer_id = customer_id.
      <fs_cart_instance>-history_flag = abap_true.
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: sort_by_customer_id
  "* Purpose: Sort by customer id
  "********************************************************************************
  METHOD sort_by_customer_id.
    DATA(TITLE) = 'SELECT * form Customer_cart SORT by customer_id'.
    WRITE: TITLE COLOR 2.
    SORT Customer_cart BY customer_id.
    display_cart_table( no_historization ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: sort_by_product_id
  "* Purpose: Sort by product id
  "********************************************************************************
  METHOD sort_by_product_id.
    DATA(TITLE) = 'SELECT * form Customer_cart SORT by product_id'.
    WRITE: TITLE COLOR 2.
    SORT Customer_cart BY product_id.
    display_cart_table( no_historization ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: display_customer_cart
  "* Purpose: Find all product for certain customer
  "********************************************************************************
  METHOD display_customer_cart.
    DATA(TITLE) = 'SELECT * form Customer_cart where customer_id = ' && customer_id.
    WRITE: TITLE COLOR 2.
    DATA total_price TYPE p length 10 DECIMALS 2.
    LOOP AT Customer_cart INTO cart_instance WHERE customer_id = customer_id AND history_flag <> abap_true.
      READ TABLE Inventory INTO product_instance WITH KEY product_id = cart_instance-product_id.
      IF sy-subrc = 0.
        display_header( ).
        display_cart_with_names( cart_instance = cart_instance product_instance = product_instance ).
        total_price = total_price + product_instance-price.
      ENDIF.
    ENDLOOP.
    DATA(total_in_string) = '=> Total reconds:' && total_price.
    WRITE: total_in_string COLOR 5.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Method: display_product_cart
  "* Purpose: Find all customer for certain product
  "********************************************************************************
  METHOD display_product_cart.
    DATA(TITLE) = 'SELECT * form Customer_cart where product_id = ' && product_id.
    WRITE:/ TITLE COLOR 2.
    LOOP AT Customer_cart INTO cart_instance WHERE product_id = product_id AND history_flag <> abap_true.
      READ TABLE Inventory INTO product_instance WITH KEY product_id = cart_instance-product_id.
      IF sy-subrc = 0.
        display_header( ).
        display_cart_with_names( cart_instance = cart_instance product_instance = product_instance ).
      ENDIF.
    ENDLOOP.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Method: checkout
  "* Purpose: Checkout for a certain customer
  "********************************************************************************
  METHOD checkout.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      DATA(TITLE) = 'Checkout for the user = ' && customer_instance-name.
      WRITE: TITLE COLOR 2.
      WRITE:/ 'Checkout ... List of the product that you want to buy:'.
      display_customer_cart( customer_id ).
    ELSE.
      WRITE:/ 'No Cart exists for the user with the id of:' && customer_id.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: purchase
  "* Purpose: Purchase after checking out for a certain customer
  "********************************************************************************
  METHOD purchase.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      DATA(TITLE) = 'Purchase for the user = ' && customer_instance-name.
      WRITE:/ TITLE COLOR 2.
      WRITE:/ 'Purchasing ... ',/ 'Thank your for buying from us. An email has ben send to your email address :' && customer_instance-email.
      LOOP AT Customer_cart INTO cart_instance WHERE customer_id = customer_id AND history_flag <> abap_true.
        flag_item_by_customer_id( customer_id ).
      ENDLOOP.
    ELSE.
      WRITE:/ 'No Cart exists for the user with the id of:' && customer_id.
    ENDIF.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Method: login_and_view
  "* Purpose: This method enable us to view the login_and_view of the purchased items for a certain customer
  "********************************************************************************
  METHOD login_and_view.
    uline.
    DATA(TITLE) = 'Login |' && ' Enter you username and password'.
    WRITE:/ TITLE COLOR 2.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = customer_id.
    IF sy-subrc = 0.
      DATA(name) = customer_instance-name.
      DATA(email) = customer_instance-email.
      DATA(password) = customer_instance-password.
      write:/ 'User entered their username and password | Checking ...'.
      WRITE: /    |Type    |,  10 |Email    |,  30 |Password            |.
      WRITE: / |{ 'Input' WIDTH = 15 }| COLOR 4,    10 |{ customer_email WIDTH = 30 }| COLOR 4, 38 |{ customer_password WIDTH = 20 }| COLOR 4.
      WRITE: / |{ 'Database' WIDTH = 15 }| COLOR 4, 10 |{ email WIDTH = 30 }| COLOR 4, 38 |{ password WIDTH = 20 }| COLOR 4.
      if customer_email = email and customer_password = password.
        DATA total_price TYPE p length 10 DECIMALS 2.
        write:/ 'Login done successfully. Welcome :' && name color 1.
        WRITE:/ '------------------------------------------Purchased item for :' && name && '----------------------------------------------------------------------------------------------' COLOR 2.
        WRITE: /       |ID    |,  15 |Product ID            |, 30 |Product Name            |, 70 |Product Price            |.
        WRITE:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.
        LOOP AT Customer_cart INTO cart_instance WHERE customer_id = customer_id AND history_flag = history_flag.
          READ TABLE Inventory INTO product_instance WITH KEY product_id = cart_instance-product_id.
          IF sy-subrc = 0.
            WRITE: / |{ cart_instance-cart_id WIDTH = 15 }| COLOR 4,
            15 |{ cart_instance-product_id WIDTH = 20 }| COLOR 4,
            30 |{ product_instance-name  WIDTH = 40 }| COLOR 4,
            70 |{ product_instance-price  WIDTH = 20 }| COLOR 4.
            total_price = total_price + product_instance-price.
            WRITE:/.
          ENDIF.
        ENDLOOP.
        DATA(total_in_string) = '=> Total reconds:' && total_price.
        WRITE: total_in_string COLOR 5.
      else.
        write:/ 'The Email or the password is incorrect'.
      endif.

    ENDIF.

  ENDMETHOD.
  "********************************************************************************
  "* Method: display_cart_table
  "* Purpose: Display Customer Cart table
  "********************************************************************************
  METHOD display_cart_table.
    WRITE:/ '------------------------------------------Customer Cart Table----------------------------------------------------------------------------------------------' COLOR 2.
    WRITE: /       |ID    |, 15 |Customer ID           |, 35 |Product ID            |, 50 |Historization       |.
    WRITE:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------'.
    Data counter type i value 0.
    LOOP AT Customer_cart INTO cart_instance WHERE history_flag = history_flag.
      display_cart( instance = cart_instance ).
      counter = counter + 1.
    ENDLOOP.
    DATA(total_in_string) = '=> Total items :' && counter.
    WRITE:/ total_in_string COLOR 5.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_cart
  "* Purpose: Display single Customer Cart
  "********************************************************************************
  METHOD display_cart.
    data: is_history type string,
          color1     TYPE sy-linct,
          color2     TYPE sy-linct.

    color1 = 4.
    color2 = 4.
    if instance-history_flag = abap_true.
      is_history = 'Flagged for historization'.
      color2 = 6.
    else.
      is_history = 'NA'.
    endif.
    WRITE: / |{ instance-cart_id WIDTH = 15 }| COLOR = color1,
    15 |{ instance-customer_id WIDTH = 45 }| COLOR = color1,
    35 |{ instance-product_id  WIDTH = 40 }| COLOR = color1,
    50 |{ is_history  WIDTH = 30 }| COLOR = color2.
    WRITE:/.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_cart_with_names
  "* Purpose: Display single Customer Cart full details
  "********************************************************************************
  METHOD display_cart_with_names.
    WRITE: / |{ cart_instance-cart_id WIDTH = 15 }| COLOR 4,
    15 |{ cart_instance-customer_id WIDTH = 45 }| COLOR 4,
    35 |{ cart_instance-product_id  WIDTH = 40 }| COLOR 4,
    52 |{ product_instance-price  WIDTH = 40 }| COLOR 4,
    70 |{ product_instance-name  WIDTH = 40 }| COLOR 4.
    WRITE:/.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_header
  "* Purpose: Display table headers
  "********************************************************************************
  method display_header.
    WRITE:/ '------------------------------------------Customer Cart Table----------------------------------------------------------------------------------------------' COLOR 2.
    WRITE: /       |ID    |, 15 |Customer ID           |, 35 |Product ID           |,  52 |Product Title            |, 70 |Price       |.
    WRITE:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------'.

  endmethod.
ENDCLASS.