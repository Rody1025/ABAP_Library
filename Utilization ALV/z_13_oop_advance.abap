*&---------------------------------------------------------------------*
*& Report  z_13_oop_advance
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_13_oop_advance.

INCLUDE z_decleration.
INCLUDE z_store.
INCLUDE z_customer.
INCLUDE z_cart.
INCLUDE z_review.

DATA: gamestore_path          type String value 'C:\Library Data\gamestore_Products.txt',
      gamestore_customer_path type String value 'C:\Library Data\gamestore_Customers.txt',
      gamestore_cart_path     type String value 'C:\Library Data\gamestore_cart.txt',
      gamestore_reviews_path  type String value 'C:\Library Data\gamestore_reviews.txt',

      import_default_products type abap_bool value abap_false,
      import_default_customer type abap_bool value abap_false,
      check_store             type abap_bool value abap_false,
      check_customer          type abap_bool value abap_false,
      check_customer_cart     type abap_bool value abap_false,
      check_customer_reviews  type abap_bool value abap_false.

DATA: gamestore          TYPE REF TO Store,
      gamestore_customer TYPE REF TO User.

include z_parameters.

include z_selection_screen.

include z_initialization.

START-OF-SELECTION.
  gamestore = NEW Store(  ).
  " Check if Checkboxs are checked
  if ch_store = abap_true.
    " Before importing data, change the id of the already inserted products, to avoid data-lost
    gamestore->update_id_before_loading( gamestore_path ).
    gamestore->load_from_file( gamestore_path ).
  endif.
  if import_default_products = abap_true.
    Store=>product_id_counter = LINES( Inventory ) + 1.
    perform populate_store_with_products.
  endif.
  gamestore->save_to_file( gamestore_path ).

  gamestore->init_ALV_columns(  ).
  gamestore->display_table( is_inital = abap_true ).

  gamestore->find_by_keyword( keyword = 'category:Games' ).
  gamestore->find_by_keyword( keyword = 'name:PlayStation' ).
  gamestore->find_by_price_range( start = '50.10' END = '200.20' ).
  gamestore->find_product_by_date( 'produced 3 years ago' ).

  gamestore->update_availability_by_id( ID = 11 ).
  gamestore->update_availability_by_keyword( keyword = 'PlayStation' ).
  gamestore->delete_by_id( ID = 12 ).

  gamestore->generate_report(  ).
  gamestore->free_tables(  ).
  gamestore->save_to_file( gamestore_path  ).
**********************************************************************
  gamestore_customer = NEW User(  ).
  if ch_cust = abap_true.
    gamestore_customer->update_id_before_loading( gamestore_customer_path ).
    gamestore_customer->load_from_file( gamestore_customer_path ).
  endif.
  if import_default_customer = abap_true.
    User=>customer_id_counter = LINES( Customer ) + 1.
    perform populate_store_with_customers.
  endif.
  gamestore_customer->save_to_file( gamestore_customer_path ).

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
  gamestore_customer->save_to_file( gamestore_customer_path ).
**********************************************************************
  DATA: gamestore_customer_cart TYPE REF TO Cart.
  gamestore_customer_cart = NEW Cart(  ).

  " Populate the table (if no items were inserted) before processing to the cart
  if lines( Inventory ) < 0.
    perform populate_store_with_products.
  endif.
  if lines( Customer ) < 0.
    perform populate_store_with_customers.
  endif.

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
  "* Class: Populate check_file_existence
  "* Purpose: Check the existence of a file
  "********************************************************************************
FORM check_file_existence
  USING file_path TYPE string
  CHANGING is_exist TYPE abap_bool.

  TRY.
      CALL METHOD cl_gui_frontend_services=>file_exist
        EXPORTING
          file                 = file_path
        RECEIVING
          result               = is_exist
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.

    CATCH cx_root INTO DATA(e).
      WRITE: / 'Error: ', e->get_text( ).
  ENDTRY.
ENDFORM.
"********************************************************************************
"* Class: Populate subroutines
"* Purpose: The below subroutines are used to populate the table with
"* Store product, members, customer carts, and customers reviews
"********************************************************************************
form populate_store_with_products.
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation 5' desc = 'The latest gaming console from Sony'
  category = 'Consoles' price = '499.99' production_date = '2020/11/12' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Xbox Series X' desc = 'The most powerful Xbox console yet'
  category = 'Consoles' price = '499.99' production_date = '2020/11/10' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'The Legend of Zelda: Breath of the Wild' desc = 'An epic adventure game for the Nintendo Switch'
  category = 'Games' price = '59.99' production_date = '2017/03/03' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Logitech G502 HERO' desc = 'A high-performance gaming mouse'
  category = 'Accessories' price = '79.99' production_date = '2018/09/03' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Canon EOS Rebel T7' desc = 'A beginner-friendly DSLR camera'
   category = 'Photography' price = '399.99' production_date = '2019/02/25' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Fitbit Versa 3' desc = 'A stylish and versatile smartwatch'
  category = 'Wearables' price = '229.95' production_date = '2020/09/25' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Bose QuietComfort 35 II' desc = 'Wireless noise-cancelling headphones'
  category = 'Audio' price = '299.00' production_date = '2017/09/22' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Apple MacBook Air' desc = 'A thin and lightweight laptop'
  category = 'Computers' price = '999.00' production_date = '2020/11/17' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Peloton Bike+' desc = 'A high-tech indoor exercise bike'
  category = 'Fitness' price = '2495.00' production_date = '2021/01/05' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Instant Pot Duo' desc = 'A multi-functional electric pressure cooker'
  category = 'Kitchen Appliances' price = '99.99' production_date = '2018/01/01' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Nest Learning Thermostat' desc = 'A smart thermostat for your home'
  category = 'Smart Home' price = '249.00' production_date = '2011/10/25' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Osprey Atmos AG 65' desc = 'A comfortable and durable hiking backpack'
  category = 'Outdoor Gear' price = '270.00' production_date = '2017/05/23' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'LEGO Star Wars Millennium Falcon' desc = 'An iconic LEGO set for Star Wars fans'
  category = 'Toys' price = '149.99' production_date = '2017/09/01' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'FIFA 22' desc = 'The latest installment in the popular soccer game franchise'
  category = 'Games' price = '59.99' production_date = '2021/10/01' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'GoPro HERO10 Black' desc = 'A high-quality action camera for capturing outdoor adventures'
  category = 'Photography' price = '499.99' production_date = '2021/09/16' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Apple AirPods Pro' desc = 'Wireless earbuds with active noise cancellation'
  category = 'Audio' price = '249.00' production_date = '2019/10/30' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Samsung Galaxy Watch 4' desc = 'A stylish and feature-packed smartwatch'
  category = 'Wearables' price = '249.99' production_date = '2021/08/27' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Dell XPS 13' desc = 'A powerful and portable laptop'
  category = 'Computers' price = '999.99' production_date = '2021/01/14' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Bowflex SelectTech 552' desc = 'Adjustable dumbbells for versatile workouts at home'
  category = 'Fitness' price = '349.99' production_date = '2015/01/01' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Weber Spirit II E-310' desc = 'A high-quality gas grill for outdoor cooking'
  category = 'Outdoor Gear' price = '529.00' production_date = '2018/02/01' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Amazon Echo Dot' desc = 'A compact and affordable smart speaker'
  category = 'Smart Home' price = '49.99' production_date = '2018/10/11' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'Nikon D3500' desc = 'A beginner-friendly DSLR camera with great image quality'
  category = 'Photography' price = '449.99' production_date = '2018/08/30' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation 4 Pro' desc = 'A powerful gaming console from Sony with 4K capabilities'
  category = 'Consoles' price = '399.99' production_date = '2016/11/10' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation VR' desc = 'An immersive virtual reality experience on PlayStation'
  category = 'Accessories' price = '299.99' production_date = '2016/10/13' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation 3' desc = 'The classic gaming console from Sony with a huge library of games'
  category = 'Consoles' price = '199.99' production_date = '2006/11/17' is_available = abap_false ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation Vita' desc = 'A portable gaming console with a stunning OLED screen'
  category = 'Consoles' price = '199.99' production_date = '2012/02/22' is_available = abap_true ).
  gamestore->add_product( ID = Store=>product_id_counter name = 'PlayStation Classic' desc = 'Relive the nostalgia of classic PlayStation games in a mini console'
  category = 'Consoles' price = '99.99' production_date = '2018/12/03' is_available = abap_true ).
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