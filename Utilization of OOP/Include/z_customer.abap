*&---------------------------------------------------------------------*
*&  Include  z_customer
*&---------------------------------------------------------------------*

"********************************************************************************
"* Class: User
"* Purpose: DEFINITION for User Class
"********************************************************************************
Types: begin of User_struct,
         customer_id   type i,
         name          type string,
         email         type string,
         password      type string,
         premium       type abap_bool,
         flag          type abap_bool,
         flag_comments type string,
       end of User_struct.

DATA: Customer              type table of User_struct,
      customer_instance     type User_struct,
      ref_customer_instance type ref to  User_struct.

Types: begin of Cart_struct,
         cart_id     type i,
         customer_id type i,
         product_id  type i,
       end of Cart_struct.

DATA: Customer_cart type table of Cart_struct,
      cart_instance type Cart_struct.

Class User definition.
  public section.
    Class-DATA: customer_id_counter type i value 1.

    methods:
    add_customer importing id type i name type string email type string password
                                    type string premium type abap_bool
                                    flag type abap_bool flag_comments type string,
    update_customer importing id type i name type string email type string password
                              type string premium type abap_bool
                              flag type abap_bool flag_comments type string,
    find_all_flagged,
    find_users_by_name importing name type string,
    delete_cusomter importing id type i,
    check_if_exists importing id type i returning value(is_exists) type abap_bool,
    display_Customer_table,
    display_Customer importing instance type User_struct.
  private section.
endclass.
"********************************************************************************
"* Class: User
"* Purpose: implementation for User Class
"********************************************************************************
Class User implementation.
  "********************************************************************************
  "* Private Method: check_if_exists
  "* Purpose: check if the item exists, this is based on its ID
  "********************************************************************************
  METHOD check_if_exists.
    Read table Customer into customer_instance with key customer_id = id.
    is_exists = sy-subrc = 0.
  endmethod.
  "********************************************************************************
  "* Method: add_customer
  "* Purpose: Add customer
  "********************************************************************************
  method add_customer.
    Data(is_exists) = check_if_exists( id ).
    if is_exists = abap_true.
      write: 'Cusomer with the same ID already exists'.
    else.
      customer_instance = value #( customer_id = id name = name email =
                                email password = password premium = premium flag = flag flag_comments = flag_comments ).
      insert customer_instance into table Customer.
      customer_id_counter = customer_id_counter + 1.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: update_customer
  "* Purpose: Update customer
  "********************************************************************************
  method update_customer.
    DATA(title) = 'UPDATE Customer set ... where id = ' && id.
    write: title color 2.
    FIELD-symbols: <fs_customer_instance> type User_struct.

    Loop at Customer assigning <fs_customer_instance> where customer_id = id.
      write:/'User before editing:'.
      display_Customer( instance = <fs_customer_instance> ).
      <fs_customer_instance>-name = name.
      <fs_customer_instance>-email = email.
      <fs_customer_instance>-password = password.
      <fs_customer_instance>-premium = premium.
      <fs_customer_instance>-flag = flag.
      <fs_customer_instance>-flag_comments = flag_comments.
      write'User after editing:'.
      display_Customer( instance = <fs_customer_instance> ).

    endloop.
  endmethod.
  "********************************************************************************
  "* Method: delete_cusomter
  "* Purpose: Delete customer
  "********************************************************************************
  method delete_cusomter.
    DATA(title) = 'DELETE 1 form Customer where id = ' && id.
    write: title color 2.
    Read table Customer into customer_instance with key customer_id = id.
    if sy-subrc = 0.
      write:/ 'Found one customer:'.
      display_Customer( instance = customer_instance ).
      DELETE Customer where customer_id = id.
    else.
      write: 'No Cusomer exists with ID ' && id.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: find_all_flagged
  "* Purpose: Display all the flagged Customer
  "********************************************************************************
  method find_all_flagged.
    DATA(title) = 'SELECT * from Customer where flag = true'.
    write: title color 2.
    Data(counter) = 0.
    loop at Customer into customer_instance where flag = abap_true.
      display_Customer( instance = customer_instance ).
      counter = counter + 1.
    endloop.
    Data(total_in_string) = '=> Total reconds:' && counter.
    write: total_in_string color 5.
  endmethod.
  "********************************************************************************
  "* Method: find_users_by_name
  "* Purpose: Find users by name
  "********************************************************************************
  method find_users_by_name.
    DATA(title) = 'SELECT * from Customer where name LIKE' && name.
    write: title color 2.
    Data(counter) = 0.
    loop at Customer into customer_instance where name CS name.
      display_Customer( instance = customer_instance ).
      counter = counter + 1.
    endloop.
    Data(total_in_string) = '=> Total reconds:' && counter.
    write: total_in_string color 5.
  endmethod.
  "********************************************************************************
  "* Method: display_Customer_table
  "* Purpose: Display Customer table
  "********************************************************************************
  method display_Customer_table.
    write:/ '------------------------------------------Customer Table----------------------------------------------------------------------------------------------' color 2.
    WRITE: /       |Customer ID    |, 15 |Name          |, 35 |email           |,
                65 |Password         | , 85 |premium         | , 100 |Flag         | , 110 |Flag-Comments         |.
    write:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    Loop AT Customer into customer_instance.
      display_Customer( instance = customer_instance ).
    endloop.
    Data(total_records) =  lines( Customer ).
    Data(total_in_string) = '=> Total reconds:' && total_records.
    write: total_in_string color 5.
    uline.
  endmethod.
  "********************************************************************************
  "* Private Method: display_Customer
  "* Purpose: Display single Customer
  "********************************************************************************
  method display_Customer.
    DATA: is_permium type string,
          is_flagged type string,
          color1     TYPE sy-linct,
          color2     TYPE sy-linct,
          color3     TYPE sy-linct.

    color1 = 4.
    color2 = 4.
    color3 = 4.
    is_permium = 'Regular user'.
    if instance-premium <> abap_false.
      color2 = 7.
      is_permium = 'Permium user'.
    endif.

    if instance-flag = abap_true.
      color3 = 6.
      is_flagged = 'Flagged!'.
    endif.
    WRITE: / |{ instance-customer_id WIDTH = 15 }| COLOR = color1,
           15 |{ instance-name WIDTH = 45 }| COLOR = color1,
           35 |{ instance-email  WIDTH = 40 }| COLOR = color1,
           70 |{ instance-password WIDTH = 20 }| COLOR = color1,
           85 |{ is_permium WIDTH = 20  }| COLOR = color2,
           100 |{ is_flagged WIDTH = 15  }| COLOR = color3,
           110 |{ instance-flag_comments WIDTH = 55  }| COLOR = color1.
    write:/.
  endmethod.
endclass.

Class Cart definition.
  public section.
    Class-DATA: cart_id_counter type i value 1.

    methods: add_item importing cart_id type i customer_id type i product_id  type i,
      update_item importing cart_id type i customer_id type i product_id  type i,
      delete_item importing id type i,
      sort_by_customer_id,
      sort_by_product_id,
      find_all_customer_id importing customer_id type i,
      find_all_product_id importing product_id type i,
      display_cutomer_cart,
      display_cart importing instance type Cart_struct.
  private section.
    methods check_if_customer_exists importing id type i returning value(is_exists) type abap_bool.
endclass.

Class Cart implementation.
  "********************************************************************************
  "* Private Method: check_if_customer_exists
  "* Purpose: Checking if a customer exists before processing
  "********************************************************************************
  method check_if_customer_exists.
    READ table Customer into customer_instance with key customer_id = id.
    is_exists = sy-subrc = 0.
  endmethod.
  "********************************************************************************
  "* Method: add_item
  "* Purpose: Add item to the cart
  "********************************************************************************
  method add_item.
    READ table Customer into customer_instance with key customer_id = customer_id.
    if sy-subrc = 0.
      cart_instance = value #( cart_id = cart_id customer_id = customer_id product_id = product_id ).
      insert cart_instance into table Customer_cart.
      cart_id_counter = cart_id_counter + 1.
    else.
      write:/ 'Customer does not exists!'.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: update_item
  "* Purpose: Update item to the cart
  "********************************************************************************
  method update_item.
    DATA(title) = 'UPDATE Customer_cart set ... where id = ' && cart_id.
    write: title color 2.
    Read table Customer_cart into cart_instance with key cart_id = cart_id.
    if sy-subrc = 0.
      READ table Customer into customer_instance with key customer_id = customer_id.
      if sy-subrc = 0.
        write:/'Customer cart before editing:'.
        display_cart( instance = cart_instance ).
        cart_instance-customer_id = customer_id.
        cart_instance-product_id = product_id.
        write:/'Customer cart after editing:'.
        display_cart( instance = cart_instance ).
      else.
        write: 'You cant update this customer. The Id : ' && customer_id && ' does not exists!'.
      endif.
    else.
      write: 'No item with the Id: ' && cart_id && ' exists!'.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: delete_item
  "* Purpose: Delete item from the cart
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
  endmethod.
  "********************************************************************************
  "* Method: sort_by_customer_id
  "* Purpose: Sort by customer id
  "********************************************************************************
  method sort_by_customer_id.
    DATA(title) = 'SELECT * form Customer_cart SORT by customer_id'.
    write: title color 2.
    SORT Customer_cart by customer_id.
    display_cutomer_cart( ).
  endmethod.
  "********************************************************************************
  "* Method: sort_by_product_id
  "* Purpose: Sort by product id
  "********************************************************************************
  method sort_by_product_id.
    DATA(title) = 'SELECT * form Customer_cart SORT by product_id'.
    write: title color 2.
    SORT Customer_cart by product_id.
    display_cutomer_cart( ).
  endmethod.
  "********************************************************************************
  "* Method: find_all_customer_id
  "* Purpose: Find all product for certain customer
  "********************************************************************************
  method find_all_customer_id.
    DATA(title) = 'SELECT * form Customer_cart where customer_id = ' && customer_id.
    write: title color 2.
    loop at Customer_cart into cart_instance where customer_id = customer_id.
      display_cart( cart_instance ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: find_all_product_id
  "* Purpose: Find all customer for certain product
  "********************************************************************************
  method find_all_product_id.
    DATA(title) = 'SELECT * form Customer_cart where product_id = ' && product_id.
    write: title color 2.
    loop at Customer_cart into cart_instance where product_id = product_id.
      display_cart( cart_instance ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: display_cutomer_cart
  "* Purpose: Display Customer Cart table
  "********************************************************************************
  method display_cutomer_cart.
    write:/ '------------------------------------------Customer Cart Table----------------------------------------------------------------------------------------------' color 2.
    WRITE: /       |ID    |, 15 |Customer ID           |, 35 |Product ID            |.
    write:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    Loop AT Customer_cart into cart_instance.
      display_cart( instance = cart_instance ).
    endloop.
    Data(total_records) =  lines( Customer_cart ).
    Data(total_in_string) = '=> Total reconds:' && total_records.
    write: total_in_string color 5.
    uline.
  endmethod.
  "********************************************************************************
  "* Private Method: display_cart
  "* Purpose: Display single Customer Cart
  "********************************************************************************
  method display_cart.
    DATA: color1 TYPE sy-linct,
          color2 TYPE sy-linct.

    color1 = 4.
    WRITE: / |{ instance-cart_id WIDTH = 15 }| COLOR = color1,
           15 |{ instance-customer_id WIDTH = 45 }| COLOR = color1,
           35 |{ instance-product_id  WIDTH = 40 }| COLOR = color1.
    write:/.
  endmethod.
endclass.