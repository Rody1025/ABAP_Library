*&---------------------------------------------------------------------*
*&  Include  z_selection_screen
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  " User can't change this field
  LOOP AT SCREEN.
    IF screen-name = 'ID' or screen-name = 'ID2'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  " We can add a specific filter to an input
  "AT SELECTION-SCREEN ON VALUE-REQUEST FOR pod_date.

  " Checking if the user want to import from file or not
  IF ch_store  = abap_true.
    check_store = abap_true.
  ENDIF.
  IF ch_cust  = abap_true.
    check_customer = abap_true.
  ENDIF.
  IF ch_cart  = abap_true.
    check_customer_cart = abap_true.
  ENDIF.
  IF ch_reviw  = abap_true.
    check_customer_reviews = abap_true.
  ENDIF.

AT SELECTION-SCREEN.
  " Check if the user want to import from file or the predefined data
  if import1 = abap_true.
    if ch_store = abap_true.
      MESSAGE 'Note! you have imported Products from the file and the pre-defined Products' TYPE 'I'.
    endif.
    import_default_products = abap_true.
  else.
    import_default_products = abap_false.
  endif.

  " Check if the user want to import from file or the predefined data
  if import2 = abap_true.
    if ch_cust = abap_true.
      MESSAGE 'Note! you have imported Customers from the file and the pre-defined Customers' TYPE 'I'.
    endif.
    import_default_customer = abap_true.
  else.
    import_default_customer = abap_false.
  endif.

  " Handler Products
  IF sy-ucomm = 'PUSHBUTTON1'.
    DATA(date_to_str) = pod_date(4) && '/' && pod_date+4(2) && '/' && pod_date+6(2).

    gamestore = NEW Store(  ).
    if name is not initial or category is not initial.
      DATA(inventory_len) = gamestore->add_product(
                                              ID = Store=>product_id_counter
                                              name = name
                                              desc = desc
                                              category = category
                                              price = price
                                              production_date = date_to_str
                                              is_available = is_ava ).
      MESSAGE 'Result :' && inventory_len TYPE 'I'.
      " After adding a user, set the Id parameter to the newly incremented ID
      ID = Store=>product_id_counter.
    else.
      MESSAGE 'ID cant be empty!' TYPE 'I'.
    endif.

    " Handler Customer
  elseif sy-ucomm = 'PUSHBUTTON2'.
    CLEAR name.
    CLEAR desc.
    CLEAR category.
    CLEAR price.
    CLEAR pod_date.
    CLEAR is_ava.
  ENDIF.

  IF sy-ucomm = 'PUSHBUTTON3'.
    gamestore_customer = NEW User(  ).

    if name2 is not initial or password is not initial.
      DATA(customer_len) = gamestore_customer->add_customer(
                                              ID = User=>customer_id_counter
                                              name = name2
                                              email = email
                                              password = password
                                              premium = premium
                                              flag = flag
                                              flag_comments = comment ).
      MESSAGE 'Result :' && customer_len TYPE 'I'.
      " After adding a user, set the Id parameter to the newly incremented ID
      ID2 = User=>customer_id_counter.
    else.
      MESSAGE 'ID cant be empty!' TYPE 'I'.
    endif.

  elseif sy-ucomm = 'PUSHBUTTON4'.
    CLEAR name2.
    CLEAR email.
    CLEAR password.
    CLEAR premium.
    CLEAR flag.
    CLEAR comment.
  ENDIF.