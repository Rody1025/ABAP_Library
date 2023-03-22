*&---------------------------------------------------------------------*
*& Report  z_13_oop_advance
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

" Scenario: A small online store wants to manage its product inventory with the help of a
"  simple software that can perform basic operations like adding, deleting, sorting, and data
"  processing and manipulation.

" Step 1: Creating the Internal Table structure
"   To start, we need to create an internal table structure called "Product" to hold the necessary
"   data fields: Product ID, Product Name, Product Description, Product Category, Price, and Availability.

" Step 2: Adding Products to the Internal Table
"   Store staff can add Products to the internal table using the 'add' functionality.
"   This action will insert a new row with the required data fields.

" Step 3: Deleting Products from the Internal Table
"   Products can be removed from the internal table using the 'delete' functionality.
"   This action will delete a row based on a unique identifier, such as the Product ID.

" Step 4: Sorting Products in the Internal Table
"   The Store staff can sort the Products in the internal table using the 'sort' functionality.
"   They can choose to sort by one or more data fields (e.g., by Product Category and Price).

" Step 5: Data processing and manipulation
"   The Store staff can process and manipulate the data in the internal table using various
"   functions, such as finding all Products in a specific category, searching for Products within
"   a certain price range, or updating the availability of a Product.


REPORT z_13_oop_advance.

"********************************************************************************
"* Class: Store
"* Purpose: DEFINITION for Store Inventory
"********************************************************************************
CLASS Store DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF Product,
             product_id   TYPE I,
             name         TYPE string,
             desc         TYPE string,
             category     TYPE string,
             price        TYPE p length 10 decimals 2,
             is_available TYPE abap_bool,  " Availability
           END OF Product.

    DATA: Inventory            TYPE TABLE OF Product,
          product_instance     TYPE Product,
          ref_product_instnace type ref to  Product.
    Class-DATA: id_counter type i value 1.
    METHODS:
    add_product IMPORTING ID TYPE I name TYPE string desc TYPE string
                          category TYPE string price TYPE p is_available TYPE abap_bool,
    find_by_category IMPORTING category TYPE string,
    find_by_keyword IMPORTING keyword TYPE string,
    find_by_price_range IMPORTING start TYPE p END TYPE p,
    update_availability_by_id importing id type i,
    update_availability_by_keyword importing keyword type string,
    delete_by_id IMPORTING ID TYPE I,
    sort_by_price,
    sort_by_id,
    display_inventory.
  PRIVATE SECTION.
    methods: check_if_exists importing id type i RETURNING VALUE(is_exists) TYPE abap_bool,
      display_product importing instance type Product.
ENDCLASS.
"********************************************************************************
"* Class: Store
"* Purpose: IMPLEMENTATION for Store Inventory
"********************************************************************************
CLASS store IMPLEMENTATION.
  "********************************************************************************
  "* Method: add_product
  "* Purpose: Add product to the Inventory
  "********************************************************************************
  METHOD add_product.
    data(exists) = check_if_exists( id = ID ).
    Data(totalRecords) =  lines( Inventory ).
    product_instance = VALUE #( product_id  = ID name = name desc = desc category = category
                                    price = price  is_available = is_available ).
    if totalRecords > 1.
      if exists = abap_true.
        write:/ 'A similar product with the same ID already exists'.
      else.
        INSERT product_instance INTO TABLE Inventory.
        Store=>id_counter = id_counter + 1.
      endif.
    else.
      INSERT product_instance INTO TABLE Inventory.
      Store=>id_counter = id_counter + 1.
    endif.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_by_category
  "* Purpose: Find product by category
  "********************************************************************************
  method find_by_category.
    DATA(title) = 'SELECT * from Inventory where category = ' && category.
    write: title color 2.
    loop at Inventory into product_instance where category = category.
      display_product( instance = product_instance ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: find_by_keyword
  "* Purpose: Find product by keyword
  "********************************************************************************
  method find_by_keyword.
    DATA(title) = 'SELECT * from Inventory where name LIKE : ' && keyword && ' or desc LIKE : ' && keyword.
    write: title color 2.
    loop at Inventory into product_instance where name CS keyword or desc CS keyword.
      display_product( instance = product_instance ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: find_by_price_range
  "* Purpose: Find products by a price range
  "********************************************************************************
  method find_by_price_range.
    DATA(title) = 'SELECT * from Inventory where name price in > ' && start && ' and price < ' && end.
    write: title color 2.
    loop at Inventory into product_instance where price between start and end.
      display_product( instance = product_instance ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: update_availability_by_id
  "* Purpose: Update the availability of a product by its id
  "********************************************************************************
  method update_availability_by_id.
    DATA(title) = 'UPDATE Inventory set is_available = true where id =' && id.
    write: title color 2.
    FIELD-symbols: <fs_instance_product> type Product.
    loop at Inventory assigning <fs_instance_product> where product_id = id.
      <fs_instance_product>-is_available = abap_true.
      display_product( instance = <fs_instance_product> ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: update_availability_by_keyword
  "* Purpose: Update the availability of a product by keyword
  "********************************************************************************
  method update_availability_by_keyword.
    DATA(title) = 'UPDATE Inventory set is_available = true where name LIKE :' && keyword.
    write: title color 2.
    FIELD-symbols: <fs_instance_product> type Product.
    loop at Inventory assigning <fs_instance_product> where name CS keyword.
      <fs_instance_product>-is_available = abap_true.
      display_product( instance = <fs_instance_product> ).
    endloop.
  endmethod.
  "********************************************************************************
  "* Method: delete_by_id
  "* Purpose: Delete product form the Inventory
  "********************************************************************************
  method delete_by_id.
    DATA(title) = 'Delete 1 from Inventory where product_id = ' && id.
    write: title color 2.
    write:/ 'Reading the table ...'.
    Read table inventory into product_instance with key product_id = id.
    if sy-subrc = 0.
      write:/ 'Found one item:'.
      display_product( instance = product_instance ).
      DELETE inventory where product_id = id.
    endif.
  endmethod.
  "********************************************************************************
  "* Method: delete_by_id
  "* Purpose: Sort by price
  "********************************************************************************
  method sort_by_price.
    DATA(title) = 'SELECT * from Inventory sort by price'.
    write: title color 2.
    SORT Inventory by price.
    display_inventory(  ).
  endmethod.
  "********************************************************************************
  "* Method: sort_by_id
  "* Purpose: Sort by id
  "********************************************************************************
  method sort_by_id.
    DATA(title) = 'SELECT * from Inventory sort by product_id'.
    write: title color 2.
    SORT Inventory by product_id.
    display_inventory(  ).
  endmethod.
  "********************************************************************************
  "* Method: display_inventory
  "* Purpose: Display the Inventory table in a structured manners
  "********************************************************************************
  method display_inventory.
    write:/ '------------------------------------------Loan Table--------------------------------------------------------------------------------------------------'.
    WRITE: /       |Product ID    |, 15 |Name          |, 60 |Category           |,
                80 |Price         |, 90 |Availability     |, 105 |Description     |.
    write:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    Loop AT Inventory into product_instance.
      display_product( instance = product_instance ).
    endloop.
    Data(total_records) =  lines( Inventory ).
    Data(total_in_string) = '=> Total reconds:' && total_records.
    write: total_in_string color 5.
    uline.
  endmethod.

  "********************************************************************************
  "* Private Method: display_product
  "* Purpose: Display the coloring to avoid code duplication
  "********************************************************************************
  method  display_product.
    DATA: is_available type string,
          color1       TYPE sy-linct,
          color2       TYPE sy-linct.

    if instance-is_available = abap_true.
      color1 = 4.
      color2 = 4.
      is_available = 'Available'.
    else.
      color1 = sy-linct.
      color2 = 6.
      is_available = 'Not Available'.
    endif.
    WRITE: / |{ instance-product_id WIDTH = 15 }| COLOR = color1,
           15 |{ instance-name WIDTH = 45 }| COLOR = color1,
           60 |{ instance-category  WIDTH = 40 }| COLOR = color1,
           80 |{ instance-price WIDTH = 20 }| COLOR = color1,
           90 |{ is_available WIDTH = 20  }| COLOR = color2,
           105 |{ instance-desc WIDTH = 70  }| COLOR = color1.
    write:/.
  endmethod.
  "********************************************************************************
  "* Private Method: check_if_exists
  "* Purpose: check if the item exists, this is based on its ID
  "********************************************************************************
  METHOD check_if_exists.
    Read table inventory into product_instance with key product_id = id.
    is_exists = sy-subrc = 0.
  endmethod.
ENDCLASS.

START-OF-SELECTION.
  DATA: gamestore TYPE REF TO Store.
  gamestore = New Store(  ).
  gamestore->add_product( ID = Store=>id_counter name = 'PlayStation 5' desc = 'The latest gaming console from Sony'
  category = 'Consoles' price = '499.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>id_counter name = 'Xbox Series X' desc = 'The latest gaming console from Microsoft'
  category = 'Consoles' price = '499.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>id_counter name = 'Nintendo Switch' desc = 'A hybrid console that can be played on-the-go or on a TV'
  category = 'Consoles' price = '299.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'FIFA 23' desc = 'The latest installment in the popular soccer video game franchise'
  category = 'Games' price = '59.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'Call of Duty: Vanguard' desc = 'A first-person shooter video game set in World War II'
  category = 'Games' price = '69.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'Super Mario 3D World + Bowser''s Fury' desc = 'A platformer video game featuring Mario and friends'
  category = 'Games' price = '59.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'DualShock 4 Wireless Controller' desc = 'A wireless controller for the PlayStation 4 console'
  category = 'Accessories' price = '59.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>id_counter name = 'Xbox Wireless Controller' desc = 'A wireless controller for the Xbox Series X and S consoles'
  category = 'Accessories' price = '59.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'Nintendo Switch Pro Controller' desc = 'A wireless controller for the Nintendo Switch console'
  category = 'Accessories' price = '69.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'LG 27 Inch UHD Ultragear Gaming Monitor' desc = 'A high-end gaming monitor with a 4K resolution and a 144Hz refresh rate'
  category = 'Accessories' price = '1099.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>id_counter name = 'Razer BlackWidow V3 Wireless Keyboard' desc = 'A wireless mechanical gaming keyboard with RGB lighting'
  category = 'Accessories' price = '229.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>id_counter name = 'Logitech G Pro Wireless Gaming Mouse' desc = 'A wireless gaming mouse with a high-precision sensor'
  category = 'Accessories' price = '129.99' is_available = abap_true ).

  gamestore->display_inventory(  ).
  gamestore->find_by_category( category = 'Games' ).
  gamestore->find_by_keyword( keyword = 'PlayStation' ).
  gamestore->find_by_price_range( start = '50.10' end = '200.20' ).

  gamestore->update_availability_by_id( id = 11 ).
  gamestore->update_availability_by_keyword( keyword = 'PlayStation' ).
  gamestore->display_inventory(  ).

  gamestore->delete_by_id( id = 12 ).

  gamestore->sort_by_price(  ).
  gamestore->sort_by_id(  ).