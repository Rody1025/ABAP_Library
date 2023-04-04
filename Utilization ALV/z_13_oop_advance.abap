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

" Extend using other Concept in ABAP:

" 6) Implement a report functionality:
"     Create a report that displays various information about the products in the internal table,
"     such as the total number of products, the number of products in each category, the average
"     price of products, and so on. You can use ABAP function modules like
"     REUSE_ALV_GRID_DISPLAY or SALV_TABLE to display the report output

REPORT z_13_oop_advance.

INCLUDE z_decleration.
Include z_store.
Include z_customer.
Include z_cart.
Include z_review.

START-OF-SELECTION.

  DATA: gamestore TYPE REF TO Store.
  gamestore = NEW Store(  ).
  perform populate_store_with_products.

  gamestore->init_ALV_columns(  ).
  gamestore->display_table( is_inital = abap_true ).

  gamestore->find_by_keyword( keyword = 'category:Games' ).
  gamestore->find_by_keyword( keyword = 'name:PlayStation' ).
  gamestore->find_by_price_range( start = '50.10' END = '200.20' ).

  gamestore->update_availability_by_id( ID = 11 ).
  gamestore->update_availability_by_keyword( keyword = 'PlayStation' ).
  gamestore->delete_by_id( ID = 12 ).

  gamestore->generate_report(  ).
  gamestore->free_tables(  ).
**********************************************************************
  DATA: gamestore_customer TYPE REF TO User.
  gamestore_customer = NEW User(  ).
  perform populate_store_with_customers.

  gamestore_customer->init_ALV_columns(  ).
  gamestore_customer->display_table( is_inital = abap_true ).

  gamestore_customer->find_users_by_name( 'Lee' ).

  gamestore_customer->update_customer( ID = 6
                                       name = 'Emily Lee'
                                       email = 'emily.lee@example.com'
                                       password = 'Elee2022'
                                       premium = abap_false
                                       flag = abap_true
                                       flag_comments = 'Removed from Permium, cause of suspicious activity' ).
  gamestore_customer->delete_cusomter( 11 ).
  gamestore_customer->find_all_flagged(  ).
  gamestore_customer->free_tables(  ).
**********************************************************************
  DATA: gamestore_customer_cart TYPE REF TO Cart.
  gamestore_customer_cart = NEW Cart(  ).
  perform populate_customer_cart.

  gamestore_customer_cart->init_ALV_columns(  ).
  gamestore_customer_cart->display_table( is_inital = abap_true ).

  gamestore_customer_cart->display_customer_cart( 1 ).
  gamestore_customer_cart->display_product_cart( 8 ).

  gamestore_customer_cart->update_item( cart_id = 1 customer_id = 50 product_id = 2 history_flag = abap_false ).
  gamestore_customer_cart->update_item( cart_id = 1 customer_id = 1 product_id = 50 history_flag = abap_false ).
  gamestore_customer_cart->delete_item( 1 ).

  gamestore_customer_cart->checkout( 1 ). gamestore_customer_cart->purchase( 1 ).
  gamestore_customer_cart->checkout( 2 ). gamestore_customer_cart->purchase( 2 ).
  gamestore_customer_cart->checkout( 3 ). gamestore_customer_cart->purchase( 3 ).
  gamestore_customer_cart->checkout( 4 ). gamestore_customer_cart->purchase( 4 ).
  gamestore_customer_cart->checkout( 6 ). gamestore_customer_cart->purchase( 6 ).
  gamestore_customer_cart->checkout( 8 ). gamestore_customer_cart->purchase( 8 ).
  gamestore_customer_cart->checkout( 10 ). gamestore_customer_cart->purchase( 10 ).

  gamestore_customer_cart->login_and_view( customer_id = 1 customer_email = 'john.smith@example.com'
  customer_password = 'P@ssw0rd'  history_flag = abap_true ).
  gamestore_customer_cart->login_and_view( customer_id = 2 customer_email = 'alice.johnson@example.com'
  customer_password = 'abc123'  history_flag = abap_true ).
  gamestore_customer_cart->login_and_view( customer_id = 8 customer_email = 'julia.taylor@example.com'
  customer_password = 'Tayl0rJ2022'  history_flag = abap_true ).
  gamestore_customer_cart->login_and_view( customer_id = 8 customer_email = 'julia.taylor@example.com'
  customer_password = 'Tayl0rJ20222'  history_flag = abap_true ).
**********************************************************************
  DATA: gamestore_customer_review TYPE REF TO Review.
  gamestore_customer_review = NEW Review(  ).
  perform populate_customer_review.

  gamestore_customer_review->init_ALV_columns( ).
  gamestore_customer_review->display_table( is_inital = abap_true ).
  gamestore_customer_review->update_review( review_id = 8 review = 'This has been edited!' rating = 5  ).
  gamestore_customer_review->delete_review( 50 ).
  gamestore_customer_review->cal_average_rating( ).

  "********************************************************************************
  "* Class: Populate subroutines
  "* Purpose: The below subroutines are used to populate the table with
  "* Store product, members, customer carts, and customers reviews
  "********************************************************************************
form populate_store_with_products.
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
  gamestore->add_product( ID = Store=>product_id_counter name = 'Apple iPhone 13' desc = 'The latest smartphone from Apple'
  category = 'Electronics' price = '1099.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Samsung Galaxy Watch 4' desc = 'Smartwatch from Samsung with a long-lasting battery'
   category = 'Wearables' price = '349.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Bose QuietComfort 35 II Wireless Headphones' desc = 'Noise-cancelling headphones from Bose'
   category = 'Audio' price = '329.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Canon EOS R5' desc = 'A professional full-frame mirrorless camera'
  category = 'Photography' price = '3899.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Microsoft Surface Laptop 4' desc = 'A premium laptop from Microsoft with touchscreen display'
   category = 'Computers' price = '1299.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Fitbit Charge 5' desc = 'A fitness tracker from Fitbit with GPS and heart rate monitor'
  category = 'Fitness' price = '179.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Nest Learning Thermostat' desc = 'A smart thermostat from Google'
   category = 'Smart Home' price = '249.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'KitchenAid Stand Mixer' desc = 'A classic mixer for all your baking needs'
  category = 'Kitchen Appliances' price = '399.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'YETI Tundra Haul Portable Wheeled Cooler' desc = 'A rugged cooler with wheels from YETI'
  category = 'Outdoor Gear' price = '399.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'LEGO Star Wars Millennium Falcon' desc = 'A large and detailed LEGO set of the iconic Millennium Falcon spaceship'
  category = 'Toys' price = '799.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Samsung Galaxy S21 Ultra' desc = 'A high-end smartphone with advanced camera features'
  category = 'Electronics' price = '1299.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Fitbit Charge 5' desc = 'A fitness tracker with 7-day battery life'
  category = 'Wearables' price = '179.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Bose QuietComfort 45' desc = 'Wireless noise-cancelling headphones'
  category = 'Audio' price = '329.99' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Nikon D850' desc = 'A professional-grade DSLR camera with 45.7 megapixels'
  category = 'Photography' price = '3799.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'MacBook Pro 16-inch' desc = 'A powerful laptop for professional use'
  category = 'Computers' price = '2399.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Peloton Bike+' desc = 'An indoor exercise bike with live and on-demand classes'
  category = 'Fitness' price = '2495.00' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Google Nest Hub (2nd gen)' desc = 'A smart speaker with a 7-inch touchscreen display'
  category = 'Smart Home' price = '99.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Instant Pot Duo 7-in-1' desc = 'A multi-cooker that can pressure cook, sauté, steam, and more'
  category = 'Kitchen Appliances' price = '89.99' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'The North Face Venture 2 Jacket' desc = 'A waterproof and breathable rain jacket for outdoor activities'
  category = 'Outdoor Gear' price = '99.00' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'LEGO Harry Potter Hogwarts Castle' desc = 'A detailed LEGO set of the iconic Hogwarts Castle'
  category = 'Toys' price = '399.99' is_available = abap_false ).
endform.

FORM populate_store_with_customers.
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'John Smith' email = 'john.smith@example.com'
  password = 'P@ssw0rd' premium = abap_true  ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Alice Johnson' email = 'alice.johnson@example.com'
  password = 'abc123'  ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Bob Davis' email = 'bob.davis@example.com'
  password = 'davis2022'   ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Karen Williams' email = 'karen.williams@example.com'
  password = 'K@ren123'  flag = abap_true flag_comments = 'Provided incorrect billing address.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Mike Jones' email = 'mike.jones@example.com'
  password = 'jones456'  ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Emily Lee' email = 'emily.lee@example.com'
  password = 'Elee2022' premium = abap_true ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'David Brown' email = 'david.brown@example.com'
  password = 'br0wnDav1d'  flag = abap_true flag_comments = 'Attempted to use a stolen credit card.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Julia Taylor' email = 'julia.taylor@example.com'
  password = 'Tayl0rJ2022' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Kevin Miller' email = 'kevin.miller@example.com'
  password = 'M1ll3rKev1n' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Olivia Green' email = 'olivia.green@example.com'
  password = 'gr33nOl1v1a'  flag = abap_true flag_comments = 'Provided fake contact information.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'NA' email = 'NA@example.com'
  password = 'NAANNAAN'  flag = abap_true flag_comments = 'Flagged for deletion' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Jacob Smith' email = 'jacob.smith@example.com'
  password = 'smithJacob123' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Sophia Lee' email = 'sophia.lee@example.com'
  password = 'leeSophia456' flag = abap_true flag_comments = 'Multiple complaints received.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Michael Johnson' email = 'michael.johnson@example.com'
  password = 'johnsonMichael789' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Emma Davis' email = 'emma.davis@example.com'
  password = 'davisEmma456' premium = abap_true flag = abap_true flag_comments = 'Provided incorrect billing information.' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'William Taylor' email = 'william.taylor@example.com'
  password = 'taylorWilliam123' premium = abap_true ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Mia Wilson' email = 'mia.wilson@example.com'
  password = 'wilsonMia789' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Benjamin Martinez' email = 'benjamin.martinez@example.com'
   password = 'martinezBenjamin456').
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Avery Jones' email = 'avery.jones@example.com'
  password = 'jonesAvery123' ).
  gamestore_customer->add_customer( ID = User=>customer_id_counter name = 'Sofia Jackson' email = 'sofia.jackson@example.com'
  password = 'jacksonSofia789' premium = abap_true flag = abap_true flag_comments = 'Multiple failed payment attempts.' ).
endform.

Form populate_customer_cart.
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
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 8 product_id = 1 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 20 product_id = 8  history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 50 product_id = 50 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 10 product_id = 10 history_flag = abap_false ).
  gamestore_customer_cart->add_item( cart_id = Cart=>cart_id_counter customer_id = 9 product_id = 9 history_flag = abap_false ).
endform.

form populate_customer_review.
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
endform.