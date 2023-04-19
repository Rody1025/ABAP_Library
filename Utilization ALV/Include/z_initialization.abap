*&---------------------------------------------------------------------*
*&  Include  z_initialization
*&---------------------------------------------------------------------*
initialization.
  b_header = `A real gamestore simulation`.

  b_prodct = `Insert a product into Product table`.
  _import1 = `Import pre-defined products!`.
  _ID = `ID`.
  _name = `Name`.
  _desc = `Description`.
  _catgory = `Category`.
  _price = `Price`.
  _date = `Date`.
  _ava = `Availability`.
  insrt_bt = `Insert`.
  clear_bt = `Clear`.

  b_custm = 'Insert Customer into Customer table'.
  _import2 = 'Import pre-defined Customers'.
  _id2 = 'ID'.
  _name2 = 'Name'.
  _email = 'Email'.
  _pass = 'Password'.
  _premium = 'Premium User'.
  _flag = 'Suspicious Account'.
  _comment = 'Comments'.
  insrt_t2 = `Insert`.
  clear_t2 = `Clear`.

  extern = `Import Data from extern files`.
  b_first = `Products`.
  b_secon = `Customer List`.
  b_third = `Customers cart`.
  b_forth = `Customer Reviews`.

  _store = `Found!, want to import them?`.
  _cust = `Found!, want to import them?`.
  _cart = `Found!, want to import them?`.
  _review = `Found!, want to import them?`.

  perform check_file_existence using gamestore_path changing check_store.
  perform check_file_existence using gamestore_customer_path changing check_customer.
  perform check_file_existence using gamestore_cart_path changing check_customer_cart.
  perform check_file_existence using gamestore_reviews_path changing check_customer_reviews.

  if check_store = abap_false.
    _store = `No external file was found!`.
  endif.
  if check_customer = abap_false.
    _cust = `No external file was found!`.
    _cart = `No external file was found!`.
    _review = `No external file was found!`.
  else.
    if check_customer_cart = abap_false.
      _cart = `No external file was found!`.
      _review = `No external file was found!`.
    else.
      if check_customer_reviews = abap_false.
        _review = `No external file was found!`.
      endif.
    endif.
  endif.
  " Setting the boolean values to their initialize state
  check_store = abap_false.
  check_customer = abap_false.
  check_customer_cart = abap_false.
  check_customer_reviews = abap_false.