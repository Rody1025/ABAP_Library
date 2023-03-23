*&---------------------------------------------------------------------*
*&  Include  z_cart
*&---------------------------------------------------------------------*
Class Cart definition.
  public section.
    Class-DATA: cart_id_counter type i value 1.
    DATA: no_historization type abap_bool value abap_false.

    methods: add_item importing cart_id type i customer_id type i product_id type i history_flag type abap_bool,
      update_item importing cart_id type i customer_id type i product_id type i history_flag type abap_bool,
      delete_item importing id type i,
      sort_by_customer_id,
      sort_by_product_id,
      display_customer_cart importing customer_id type i,
      display_product_cart importing product_id type i,
      checkout importing customer_id type i,
      purchase importing customer_id type i,
      history importing customer_id type i history_flag type abap_bool,
      display_cart_table importing history_flag type abap_bool.

  private section.
    methods: delete_itemby_customer_id importing customer_id type i,
      display_cart importing instance type Cart_struct,
      get_customer_name importing customer_id type i returning value(name) type string.



endclass.

Class Cart implementation.
  "********************************************************************************
  "* Private Method: get_customer_name
  "* Purpose: Getting Customer name
  "********************************************************************************
  method get_customer_name.
    READ TABLE Customer into customer_instance with key customer_id = customer_id.
    if sy-subrc = 0.
      name = customer_instance-name.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: add_item
  "* Purpose: Add item to the cart
  "********************************************************************************
  method add_item.
    READ table Customer into customer_instance with key customer_id = customer_id.
    if sy-subrc = 0.
      READ table Inventory into product_instance with key product_id = product_id.
      if sy-subrc = 0.
        cart_instance = value #( cart_id = cart_id customer_id = customer_id product_id = product_id history_flag = history_flag ).
        insert cart_instance into table Customer_cart.
        cart_id_counter = cart_id_counter + 1.
      else.
        write:/ 'Product does not exists!' color 6.
      endif.
    else.
      write:/ 'Customer does not exists!' color 6.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: update_item
  "* Purpose: Update item to the cart
  "********************************************************************************
  method update_item.
    DATA(title) = 'UPDATE Customer_cart set ... where id = ' && cart_id.
    write:/ title color 2.
    Read table Customer_cart into cart_instance with key cart_id = cart_id.
    if sy-subrc = 0.
      READ table Customer into customer_instance with key customer_id = customer_id.
      if sy-subrc = 0.
        READ table Inventory into product_instance with key product_id = product_id.
        if sy-subrc = 0.
          write:/'Customer cart before editing:'.
          display_cart( instance = cart_instance ).
          cart_instance-customer_id = customer_id.
          cart_instance-product_id = product_id.
          cart_instance-history_flag = history_flag.
          write:/'Customer cart after editing:'.
          display_cart( instance = cart_instance ).
        else.
          write:/ 'You cant update this customer. The product_id : ' && product_id && ' does not exists!' color 6.
        endif.
      else.
        write:/ 'You cant update this customer. The customer_id : ' && customer_id && ' does not exists!' color 6.
      endif.
    else.
      write:/ 'No item with the Id: ' && cart_id && ' exists!'.
    endif.
    uline.
  endmethod.
  "********************************************************************************
  "* Method: delete_item
  "* Purpose: Delete item from the cart by its id
  "********************************************************************************
  method delete_item.
    DATA(title) = 'DELETE 1 form Customer_cart where id = ' && id.
    write:/ title color 2.
    Read table Customer_cart into cart_instance with key cart_id = id.
    if sy-subrc = 0.
      write:/ 'Found one customer:'.
      display_cart( instance = cart_instance ).
      DELETE Customer_cart where cart_id = id.
    else.
      write:/ 'No Cusomer Cart exists with ID ' && id.
    endif.
    uline.
  endmethod.
  "********************************************************************************
  "* Private Method: delete_item
  "* Purpose: Delete item from the cart by its customer id
  "********************************************************************************
  method delete_itemby_customer_id.
    DATA(title) = 'DELETE 1 form Customer_cart where customer_id = ' && customer_id.
    write:/ title color 2.
    Loop at Customer_cart assigning <fs_cart_instance> where customer_id = customer_id.
      <fs_cart_instance>-history_flag = abap_true.
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: sort_by_customer_id
  "* Purpose: Sort by customer id
  "********************************************************************************
  method sort_by_customer_id.
    DATA(title) = 'SELECT * form Customer_cart SORT by customer_id'.
    write: title color 2.
    SORT Customer_cart by customer_id.
    display_cart_table( no_historization ).
  endmethod.
  "********************************************************************************
  "* Method: sort_by_product_id
  "* Purpose: Sort by product id
  "********************************************************************************
  method sort_by_product_id.
    DATA(title) = 'SELECT * form Customer_cart SORT by product_id'.
    write: title color 2.
    SORT Customer_cart by product_id.
    display_cart_table( no_historization ).
  endmethod.
  "********************************************************************************
  "* Method: display_customer_cart
  "* Purpose: Find all product for certain customer
  "********************************************************************************
  method display_customer_cart.
    DATA(title) = 'SELECT * form Customer_cart where customer_id = ' && customer_id.
    write: title color 2.
    Data total_price type p length 10 decimals 2.
    loop at Customer_cart into cart_instance where customer_id = customer_id and history_flag <> abap_true.
      display_cart( cart_instance ).
      READ table Inventory into product_instance with key product_id = cart_instance-product_id.
      if sy-subrc = 0.
        total_price = total_price + product_instance-price.
      endif.
    endloop.
    Data(total_in_string) = '=> Total reconds:' && total_price.
    write: total_in_string color 5.
    uline.
  endmethod.
  "********************************************************************************
  "* Method: display_product_cart
  "* Purpose: Find all customer for certain product
  "********************************************************************************
  method display_product_cart.
    DATA(title) = 'SELECT * form Customer_cart where product_id = ' && product_id.
    write:/ title color 2.
    loop at Customer_cart into cart_instance where product_id = product_id and history_flag <> abap_true.
      display_cart( cart_instance ).
    endloop.
    uline.
  endmethod.
  "********************************************************************************
  "* Method: checkout
  "* Purpose: Checkout for a certain customer
  "********************************************************************************
  method checkout.
    READ TABLE Customer into customer_instance with key customer_id = customer_id.
    if sy-subrc = 0.
      DATA(title) = 'Checkout for the user = ' && customer_instance-name.
      write: title color 2.
      write:/ 'Checkout ... List of the product that you want to buy:'.
      display_customer_cart( customer_id ).
    else.
      write:/ 'No Cart exists for the user with the id of:' && customer_id.
    endif.
    uline.
  endmethod.
  "********************************************************************************
  "* Method: purchase
  "* Purpose: Purchase after checking out for a certain customer
  "********************************************************************************
  method purchase.
    READ TABLE Customer into customer_instance with key customer_id = customer_id.
    if sy-subrc = 0.
      DATA(title) = 'Purchase for the user = ' && customer_instance-name.
      write:/ title color 2.
      write:/ 'Purchasing ... ',/ 'Thank your for buying from us. An email has ben send to your email address :' && customer_instance-email.
      loop at Customer_cart into cart_instance where customer_id = customer_id and history_flag <> abap_true.
        delete_itemby_customer_id( customer_id ).
      endloop.
    else.
      write:/ 'No Cart exists for the user with the id of:' && customer_id.
    endif.
    uline.
  endmethod.
  "********************************************************************************
  "* Method: history
  "* Purpose: This method enable us to view the history of the purchased items for a certain customer
  "********************************************************************************
  method history.
    DATA(name) = get_customer_name( customer_id ).
    Data total_price type p length 10 decimals 2.
    write:/ '------------------------------------------Purchased item for :' && name && '----------------------------------------------------------------------------------------------' color 2.
    WRITE: /       |ID    |,  15 |Product ID            |, 30 |Product Name            |, 60 |Product Price            |..
    write:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.
    Loop AT Customer_cart into cart_instance where customer_id = customer_id and history_flag = history_flag.
      READ table Inventory into product_instance with key product_id = cart_instance-product_id.
      if sy-subrc = 0.
        WRITE: / |{ cart_instance-cart_id WIDTH = 15 }| COLOR 4,
      15 |{ cart_instance-product_id WIDTH = 20 }| COLOR 4,
      30 |{ product_instance-name  WIDTH = 40 }| COLOR 4,
      60 |{ product_instance-price  WIDTH = 20 }| COLOR 4.
        total_price = total_price + product_instance-price.
        write:/.
      endif.
    endloop.
    Data(total_in_string) = '=> Total reconds:' && total_price.
    write: total_in_string color 5.
  endmethod.
  "********************************************************************************
  "* Method: display_cart_table
  "* Purpose: Display Customer Cart table
  "********************************************************************************
  method display_cart_table.
    write:/ '------------------------------------------Customer Cart Table----------------------------------------------------------------------------------------------' color 2.
    WRITE: /       |ID    |, 15 |Customer ID           |, 35 |Product ID            |.
    write:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    Loop AT Customer_cart into cart_instance where history_flag = no_historization.
      display_cart( instance = cart_instance ).
    endloop.
    Data(total_records) =  lines( Customer_cart ).
    Data(total_in_string) = '=> Total price :' && total_records.
    write:/ total_in_string color 5.
    uline.
  endmethod.
  "********************************************************************************
  "* Private Method: display_cart
  "* Purpose: Display single Customer Cart
  "********************************************************************************
  method display_cart.
    WRITE: / |{ instance-cart_id WIDTH = 15 }| COLOR 4,
           15 |{ instance-customer_id WIDTH = 45 }| COLOR 4,
           35 |{ instance-product_id  WIDTH = 40 }| COLOR 4.
    write:/.
  endmethod.
endclass.