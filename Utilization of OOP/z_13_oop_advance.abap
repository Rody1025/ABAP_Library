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

" Extend:

" 1) Add a user system:
"     Create a User structure with fields like User ID, Name, Email, and Password.
"     Implement forms to add, update, and delete users.

" 2) Implement a cart system:
"     Add a Cart structure to hold the products that the customer has added to their cart.
"     Create methods to add, update and delete products from the cart.
"     Implement a form to display the contents of the cart and the total price.
"     Implement a form to checkout and purchase the products in the cart.

" 3) Connect Cart with Customer table
"     Modify the cart system to associate a user with their cart.
"     Implement a login system that allows users to log in and view their cart and purchase history.

" 4) Add a search functionality:
"     Implement a form to search for products by keywords in the name or description fields.
"     Implement a form to search for products by a range of prices.

" 5) Implement a basic review system:
"     Add a Review structure to hold the reviews that customers have left for products.
"     Create methods to add and delete reviews.
"     Implement a form to display the reviews for a product.
"     Calculate and display the average rating for each product based on the reviews.

REPORT z_13_oop_advance.

INCLUDE z_customer.
"********************************************************************************
"* Class: Store
"* Purpose: DEFINITION for Store Class
"********************************************************************************
CLASS Store DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: product_id_counter TYPE I VALUE 1.

    METHODS:
      add_product IMPORTING
                    ID           TYPE I
                    name         TYPE string
                    desc         TYPE string
                    category     TYPE string
                    price        TYPE p
                    is_available TYPE abap_bool,
      find_by_category IMPORTING
                         category TYPE string,
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
      sort_by_price,
      sort_by_id,
      display_inventory.
  PRIVATE SECTION.
    METHODS:
      display_product IMPORTING instance TYPE Product,
      check_if_exists IMPORTING ID TYPE I RETURNING VALUE(is_exists) TYPE abap_bool.
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
    DATA(exists) = check_if_exists( ID = ID ).
    DATA(totalRecords) =  LINES( Inventory ).
    product_instance = VALUE #( product_id  = ID name = name desc = desc category = category
    price = price  is_available = is_available ).
    IF totalRecords > 1.
      IF exists = abap_true.
        WRITE:/ 'A similar product with the same ID already exists'.
      ELSE.
        INSERT product_instance INTO TABLE Inventory.
        product_id_counter = product_id_counter + 1.
      ENDIF.
    ELSE.
      INSERT product_instance INTO TABLE Inventory.
      product_id_counter = product_id_counter + 1.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_by_category
  "* Purpose: Find product by category
  "********************************************************************************
  METHOD find_by_category.
    DATA(TITLE) = 'SELECT * from Inventory where category = ' && category.
    WRITE: TITLE COLOR 2.
    LOOP AT Inventory INTO product_instance WHERE category = category.
      display_product( instance = product_instance ).
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_by_keyword
  "* Purpose: Find product by keyword
  "********************************************************************************
  METHOD find_by_keyword.
    DATA(TITLE) = 'SELECT * from Inventory where name LIKE : ' && keyword && ' or desc LIKE : ' && keyword.
    WRITE: TITLE COLOR 2.
    LOOP AT Inventory INTO product_instance WHERE name CS keyword OR desc CS keyword.
      display_product( instance = product_instance ).
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_by_price_range
  "* Purpose: Find products by a price range
  "********************************************************************************
  METHOD find_by_price_range.
    DATA(TITLE) = 'SELECT * from Inventory where name price in > ' && start && ' and price < ' && END.
    WRITE: TITLE COLOR 2.
    LOOP AT Inventory INTO product_instance WHERE price BETWEEN start AND END.
      display_product( instance = product_instance ).
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_availability_by_id
  "* Purpose: Update the availability of a product by its id
  "********************************************************************************
  METHOD update_availability_by_id.
    DATA(TITLE) = 'UPDATE Inventory set is_available = true where id =' && ID.
    WRITE: TITLE COLOR 2.
    FIELD-symbols: <fs_instance_product> TYPE Product.
    LOOP AT Inventory ASSIGNING <fs_instance_product> WHERE product_id = ID.
      <fs_instance_product>-is_available = abap_true.
      display_product( instance = <fs_instance_product> ).
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_availability_by_keyword
  "* Purpose: Update the availability of a product by keyword
  "********************************************************************************
  METHOD update_availability_by_keyword.
    DATA(TITLE) = 'UPDATE Inventory set is_available = true where name LIKE :' && keyword.
    WRITE: TITLE COLOR 2.
    FIELD-symbols: <fs_instance_product> TYPE Product.
    LOOP AT Inventory ASSIGNING <fs_instance_product> WHERE name CS keyword.
      <fs_instance_product>-is_available = abap_true.
      display_product( instance = <fs_instance_product> ).
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_by_id
  "* Purpose: Delete product form the Inventory
  "********************************************************************************
  METHOD delete_by_id.
    DATA(TITLE) = 'Delete 1 from Inventory where product_id = ' && ID.
    WRITE: TITLE COLOR 2.
    WRITE:/ 'Reading the table ...'.
    READ TABLE inventory INTO product_instance WITH KEY product_id = ID.
    IF sy-subrc = 0.
      WRITE:/ 'Found one item:'.
      display_product( instance = product_instance ).
      DELETE inventory WHERE product_id = ID.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_by_id
  "* Purpose: Sort by price
  "********************************************************************************
  METHOD sort_by_price.
    DATA(TITLE) = 'SELECT * from Inventory sort by price'.
    WRITE: TITLE COLOR 2.
    SORT Inventory BY price.
    display_inventory(  ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: sort_by_id
  "* Purpose: Sort by id
  "********************************************************************************
  METHOD sort_by_id.
    DATA(TITLE) = 'SELECT * from Inventory sort by product_id'.
    WRITE: TITLE COLOR 2.
    SORT Inventory BY product_id.
    display_inventory(  ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: display_inventory
  "* Purpose: Display the Inventory table in a structured manners
  "********************************************************************************
  METHOD display_inventory.
    WRITE:/ '------------------------------------------Loan Table--------------------------------------------------------------------------------------------------'.
    WRITE: /       |Product ID    |, 15 |Name          |, 60 |Category           |,
    80 |Price         |, 90 |Availability     |, 105 |Description     |.
    WRITE:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    LOOP AT Inventory INTO product_instance.
      display_product( instance = product_instance ).
    ENDLOOP.
    DATA(total_records) =  LINES( Inventory ).
    DATA(total_in_string) = '=> Total reconds:' && total_records.
    WRITE: total_in_string COLOR 5.
    ULINE.
  ENDMETHOD.

  "********************************************************************************
  "* Private Method: display_product
  "* Purpose: Display the coloring to avoid code duplication
  "********************************************************************************
  METHOD  display_product.
    DATA: is_available TYPE string,
          color1       TYPE sy-linct,
          color2       TYPE sy-linct.

    IF instance-is_available = abap_true.
      color1 = 4.
      color2 = 4.
      is_available = 'Available'.
    ELSE.
      color1 = sy-linct.
      color2 = 6.
      is_available = 'Not Available'.
    ENDIF.
    WRITE: / |{ instance-product_id WIDTH = 15 }| COLOR = color1,
    15 |{ instance-name WIDTH = 45 }| COLOR = color1,
    60 |{ instance-category  WIDTH = 40 }| COLOR = color1,
    80 |{ instance-price WIDTH = 20 }| COLOR = color1,
    90 |{ is_available WIDTH = 20  }| COLOR = color2,
    105 |{ instance-desc WIDTH = 70  }| COLOR = color1.
    WRITE:/.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: check_if_exists
  "* Purpose: check if the item exists, this is based on its ID
  "********************************************************************************
  METHOD check_if_exists.
    READ TABLE inventory INTO product_instance WITH KEY product_id = ID.
    is_exists = sy-subrc = 0.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: gamestore TYPE REF TO Store.
  gamestore = NEW Store(  ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation 5' desc = 'The latest gaming console from Sony'
  category = 'Consoles' price = '499.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Xbox Series X' desc = 'The latest gaming console from Microsoft'
  category = 'Consoles' price = '499.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Nintendo Switch' desc = 'A hybrid console that can be played on-the-go or on a TV'
  category = 'Consoles' price = '299.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'FIFA 23' desc = 'The latest installment in the popular soccer video game franchise'
  category = 'Games' price = '59.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Call of Duty: Vanguard' desc = 'A first-person shooter video game set in World War II'
  category = 'Games' price = '69.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Super Mario 3D World + Bowser''s Fury' desc = 'A platformer video game featuring Mario and friends'
  category = 'Games' price = '59.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'DualShock 4 Wireless Controller' desc = 'A wireless controller for the PlayStation 4 console'
  category = 'Accessories' price = '59.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Xbox Wireless Controller' desc = 'A wireless controller for the Xbox Series X and S consoles'
  category = 'Accessories' price = '59.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Nintendo Switch Pro Controller' desc = 'A wireless controller for the Nintendo Switch console'
  category = 'Accessories' price = '69.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'LG 27 Inch UHD Ultragear Gaming Monitor' desc = 'A high-end gaming monitor with a 4K resolution and a 144Hz refresh rate'
  category = 'Accessories' price = '1099.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Razer BlackWidow V3 Wireless Keyboard' desc = 'A wireless mechanical gaming keyboard with RGB lighting'
  category = 'Accessories' price = '229.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Logitech G Pro Wireless Gaming Mouse' desc = 'A wireless gaming mouse with a high-precision sensor'
  category = 'Accessories' price = '129.99' is_available = abap_true ).

  gamestore->display_inventory(  ).
  gamestore->find_by_category( category = 'Games' ).
  gamestore->find_by_keyword( keyword = 'PlayStation' ).
  gamestore->find_by_price_range( start = '50.10' END = '200.20' ).

  gamestore->update_availability_by_id( ID = 11 ).
  gamestore->update_availability_by_keyword( keyword = 'PlayStation' ).
  gamestore->display_inventory(  ).

  gamestore->delete_by_id( ID = 12 ).

  gamestore->sort_by_price(  ).
  gamestore->sort_by_id(  ).

  DATA: gamestore_customer TYPE REF TO User.
  gamestore_customer = NEW User(  ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'John Smith' email = 'john.smith@example.com'
  password = 'P@ssw0rd' premium = abap_true flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Alice Johnson' email = 'alice.johnson@example.com'
  password = 'abc123' premium = abap_false flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Bob Davis' email = 'bob.davis@example.com'
  password = 'davis2022' premium = abap_false flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Karen Williams' email = 'karen.williams@example.com'
  password = 'K@ren123' premium = abap_false flag = abap_true flag_comments = 'Provided incorrect billing address.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Mike Jones' email = 'mike.jones@example.com'
  password = 'jones456' premium = abap_false flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Emily Lee' email = 'emily.lee@example.com'
  password = 'Elee2022' premium = abap_true flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'David Brown' email = 'david.brown@example.com'
  password = 'br0wnDav1d' premium = abap_false flag = abap_true flag_comments = 'Attempted to use a stolen credit card.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Julia Taylor' email = 'julia.taylor@example.com'
  password = 'Tayl0rJ2022' premium = abap_false flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Kevin Miller' email = 'kevin.miller@example.com'
  password = 'M1ll3rKev1n' premium = abap_false flag = abap_false flag_comments = '' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Olivia Green' email = 'olivia.green@example.com'
  password = 'gr33nOl1v1a' premium = abap_false flag = abap_true flag_comments = 'Provided fake contact information.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'NA' email = 'NA@example.com'
  password = 'NAANNAAN' premium = abap_false flag = abap_true flag_comments = 'Flagged for deletion' ).

  gamestore_customer->display_customer_table(  ).
  gamestore_customer->update_customer( ID = 6 name = 'Emily Lee' email = 'emily.lee@example.com'
  password = 'Elee2022' premium = abap_false flag = abap_true flag_comments = 'Removed from Permium, cause of suspicious activity' ).
  gamestore_customer->delete_cusomter( 11 ).
  gamestore_customer->display_customer_table(  ).
  gamestore_customer->find_all_flagged(  ).
  gamestore_customer->find_users_by_name( 'Lee' ).

  DATA: gamestore_customer_cart TYPE REF TO Cart.
  gamestore_customer_cart = NEW Cart(  ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 1 product_id = 10 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 1 product_id = 2 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 1 product_id = 8 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 2 product_id = 5 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 2 product_id = 6 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 2 product_id = 4 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 3 product_id = 1 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 4 product_id = 4 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 6 product_id = 6 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 8 product_id = 8 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 8 product_id = 8 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 8 product_id = 1 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 20 product_id = 8  history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 1 product_id = 56  history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 50 product_id = 50 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 10 product_id = 10 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 9 product_id = 9 history_flag = abap_false ).

  gamestore_customer_cart->display_cart_table( abap_false ).
  gamestore_customer_cart->update_item( cart_id = 1 customer_id = 50 product_id = 2 history_flag = abap_false ).
  gamestore_customer_cart->update_item( cart_id = 1 customer_id = 1 product_id = 50 history_flag = abap_false ).
  gamestore_customer_cart->delete_item( 1 ).

  gamestore_customer_cart->sort_by_product_id(  ).
  gamestore_customer_cart->sort_by_customer_id(  ).

  gamestore_customer_cart->display_customer_cart( 1 ).
  gamestore_customer_cart->display_product_cart( 8 ).

  gamestore_customer_cart->display_cart_table( abap_false ).
  gamestore_customer_cart->checkout( 1 ). gamestore_customer_cart->purchase( 1 ).
  gamestore_customer_cart->checkout( 2 ). gamestore_customer_cart->purchase( 2 ).
  gamestore_customer_cart->checkout( 3 ). gamestore_customer_cart->purchase( 3 ).
  gamestore_customer_cart->checkout( 4 ). gamestore_customer_cart->purchase( 4 ).
  gamestore_customer_cart->checkout( 6 ). gamestore_customer_cart->purchase( 6 ).
  gamestore_customer_cart->checkout( 8 ). gamestore_customer_cart->purchase( 8 ).
  gamestore_customer_cart->checkout( 10 ). gamestore_customer_cart->purchase( 10 ).
  gamestore_customer_cart->display_cart_table( abap_false ).
  write:/ 'Display all the historized items' color 1.
  gamestore_customer_cart->display_cart_table( abap_true ).

  gamestore_customer_cart->history( customer_id = 1 history_flag = abap_true ).
  gamestore_customer_cart->history( customer_id = 2 history_flag = abap_true ).
  gamestore_customer_cart->history( customer_id = 8 history_flag = abap_true ).


  DATA: gamestore_customer_review TYPE REF TO Review.
  gamestore_customer_review = NEW Review(  ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
    customer_id = 1 product_id = 2
    review = 'I absolutely love this product! It works perfectly and has made my life so much easier.' rating = 1 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
    customer_id = 1 product_id = 8 review = 'This product is amazing! It has exceeded my expectations in every way.' rating = 4 ).
  gamestore_customer_review->add_review( review_id =
  Review=>review_id_counter
  customer_id = 2 product_id = 8 review = 'It''s well-designed and very versatile.' rating = 2 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
    customer_id = 2 product_id = 4
    review = 'I''m very impressed with this product. It''s well-designed and very versatile.' rating = 1 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
    customer_id = 2 product_id = 5
    review = 'This product is great! It works exactly as advertised and has made a big difference in my life.' rating = 2 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
    customer_id = 2 product_id = 6
    review = 'I''m very happy with this product! It''s made my daily routine so much smoother and more efficient.' rating = 5 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 3 product_id = 1
  review = 'I love this product! It exceeded my expectations.' rating = 4 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 8 product_id = 1
  review = 'This is a great product! It works very well and is easy to use.' rating = 1 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 8 product_id = 8
  review = 'I''m very happy with this product! It''s exactly what I was looking for.' rating = 2 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 8 product_id = 8
  review = 'This product is amazing! It''s changed the way I work for the better.' rating = 3 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 4 product_id = 4
  review = 'I''m impressed with this product! It''s well-designed and very versatile.' rating = 4 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 6 product_id = 6
  review = 'This product is excellent! It''s helped me save time and increase my productivity.' rating = 3 ).
  gamestore_customer_review->add_review( review_id = Review=>review_id_counter
  customer_id = 9 product_id = 10
  review = 'This product is excellent! It''s helped me save time and increase my productivity.' rating = 3 ).

  gamestore_customer_review->display_review_table(  ).
  gamestore_customer_review->update_review( review_id = 8 review = 'This has been edited!' rating = 5  ).
  gamestore_customer_review->delete_review( 50 ).
  gamestore_customer_review->cal_average_rating( ).