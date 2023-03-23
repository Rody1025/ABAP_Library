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

" 7) Implement a data validation functionality:
"     Add data validation logic to ensure that the user-entered data is correct and valid.
"     For example, you can check that the entered product price is a valid number, or that the
"     entered email address is in the correct format. You can use ABAP built-in functions like
"     ISNUMERIC or CL_ABAP_REGEX to perform data validation.

" 8) Implement a logging functionality:
"     Add logging functionality to track all the important actions performed by the users
"     and the store staff, such as adding or deleting products, updating user information, or
"     checking out a cart. You can use ABAP logging frameworks like SLG1 or CL_RSAN_UT_LOG to
"     create logs and store them in a database table.

" 9) Implement a user roles functionality:
"     Add user roles functionality to differentiate between different types of users, such
"     as customers, store staff, or administrators. You can create a custom table to hold the
"     user roles and use ABAP authorization objects like S_TABU_DIS to control access to
"     different parts of the application based on the user's role.

" 10) Implement a search help functionality:
"      Add search help functionality to assist the users in finding the correct product
"      or user information. You can create search help objects using the ABAP Dictionary and
"      assign them to the appropriate input fields in your application.

" 11) Implement a lock object functionality:
"      Add lock object functionality to prevent concurrent access to the same data by
"      multiple users. You can create lock objects using the ABAP Dictionary and use them to
"      lock and unlock the relevant data fields in your application.

REPORT z_13_oop_advance.

INCLUDE z_customer.

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

  gamestore->generate_report(  ).

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

  gamestore_customer_cart->login_and_view( customer_id = 1 customer_email = 'john.smith@example.com' customer_password = 'P@ssw0rd'  history_flag = abap_true ).
  gamestore_customer_cart->login_and_view( customer_id = 2 customer_email = 'alice.johnson@example.com' customer_password = 'abc123'  history_flag = abap_true ).
  gamestore_customer_cart->login_and_view( customer_id = 8 customer_email = 'julia.taylor@example.com' customer_password = 'Tayl0rJ2022'  history_flag = abap_true ).
  gamestore_customer_cart->login_and_view( customer_id = 8 customer_email = 'julia.taylor@example.com' customer_password = 'Tayl0rJ20222'  history_flag = abap_true ).
  uline.
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