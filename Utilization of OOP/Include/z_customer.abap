*&---------------------------------------------------------------------*
*&  Include  z_customer
*&---------------------------------------------------------------------*
"********************************************************************************
"* Class: User
"* Purpose: DEFINITION for User Class
"********************************************************************************
INCLUDE z_decleration.
INCLUDE z_cart.
INCLUDE z_review.

CLASS User DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: customer_id_counter TYPE I VALUE 1.

    METHODS:
      add_customer IMPORTING
                     ID            TYPE I
                     name          TYPE string
                     email         TYPE string
                     password      TYPE string
                     premium       TYPE abap_bool
                     flag          TYPE abap_bool
                     flag_comments TYPE string,
      update_customer IMPORTING
                        ID            TYPE I
                        name          TYPE string
                        email         TYPE string
                        password      TYPE string
                        premium       TYPE abap_bool
                        flag          TYPE abap_bool
                        flag_comments TYPE string,
      find_all_flagged,
      find_users_by_name IMPORTING
                           name TYPE string,
      delete_cusomter IMPORTING
                        ID TYPE I,
      display_Customer_table.
  PRIVATE SECTION.
    METHODS: check_if_exists IMPORTING
                                       ID               TYPE I
                             returning VALUE(is_exists) TYPE abap_bool,
      display_Customer IMPORTING
                         instance TYPE User_struct.

ENDCLASS.
"********************************************************************************
"* Class: User
"* Purpose: implementation for User Class
"********************************************************************************
CLASS User IMPLEMENTATION.
  "********************************************************************************
  "* Private Method: check_if_exists
  "* Purpose: check if the item exists, this is based on its ID
  "********************************************************************************
  METHOD check_if_exists.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = ID.
    is_exists = sy-subrc = 0.
  ENDMETHOD.
  "********************************************************************************
  "* Method: add_customer
  "* Purpose: Add customer
  "********************************************************************************
  METHOD add_customer.
    DATA(is_exists) = check_if_exists( ID ).
    IF is_exists = abap_true.
      WRITE: 'Cusomer with the same ID already exists'.
    ELSE.
      customer_instance = VALUE #( customer_id = ID name = name email =
      email password = password premium = premium flag = flag flag_comments = flag_comments ).
      INSERT customer_instance INTO TABLE Customer.
      customer_id_counter = customer_id_counter + 1.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_customer
  "* Purpose: Update customer
  "********************************************************************************
  METHOD update_customer.
    DATA(TITLE) = 'UPDATE Customer set ... where id = ' && ID.
    WRITE: TITLE COLOR 2.
    FIELD-symbols: <fs_customer_instance> TYPE User_struct.

    LOOP AT Customer ASSIGNING <fs_customer_instance> WHERE customer_id = ID.
      WRITE:/'User before editing:'.
      display_Customer( instance = <fs_customer_instance> ).
      <fs_customer_instance>-name = name.
      <fs_customer_instance>-email = email.
      <fs_customer_instance>-password = password.
      <fs_customer_instance>-premium = premium.
      <fs_customer_instance>-flag = flag.
      <fs_customer_instance>-flag_comments = flag_comments.
      write'User after editing:'.
      display_Customer( instance = <fs_customer_instance> ).

    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_cusomter
  "* Purpose: Delete customer
  "********************************************************************************
  METHOD delete_cusomter.
    DATA(TITLE) = 'DELETE 1 form Customer where id = ' && ID.
    WRITE: TITLE COLOR 2.
    READ TABLE Customer INTO customer_instance WITH KEY customer_id = ID.
    IF sy-subrc = 0.
      WRITE:/ 'Found one customer:'.
      display_Customer( instance = customer_instance ).
      DELETE Customer WHERE customer_id = ID.
    ELSE.
      WRITE: 'No Cusomer exists with ID ' && ID.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_all_flagged
  "* Purpose: Display all the flagged Customer
  "********************************************************************************
  METHOD find_all_flagged.
    DATA(TITLE) = 'SELECT * from Customer where flag = true'.
    WRITE: TITLE COLOR 2.
    DATA(counter) = 0.
    LOOP AT Customer INTO customer_instance WHERE flag = abap_true.
      display_Customer( instance = customer_instance ).
      counter = counter + 1.
    ENDLOOP.
    DATA(total_in_string) = '=> Total reconds:' && counter.
    WRITE: total_in_string COLOR 5.
  ENDMETHOD.
  "********************************************************************************
  "* Method: find_users_by_name
  "* Purpose: Find users by name
  "********************************************************************************
  METHOD find_users_by_name.
    DATA(TITLE) = 'SELECT * from Customer where name LIKE' && name.
    WRITE: TITLE COLOR 2.
    DATA(counter) = 0.
    LOOP AT Customer INTO customer_instance WHERE name CS name.
      display_Customer( instance = customer_instance ).
      counter = counter + 1.
    ENDLOOP.
    DATA(total_in_string) = '=> Total reconds:' && counter.
    WRITE: total_in_string COLOR 5.
  ENDMETHOD.
  "********************************************************************************
  "* Method: display_Customer_table
  "* Purpose: Display Customer table
  "********************************************************************************
  METHOD display_Customer_table.
    WRITE:/ '------------------------------------------Customer Table----------------------------------------------------------------------------------------------' COLOR 2.
    WRITE: /       |Customer ID    |, 15 |Name          |, 35 |email           |,
    65 |Password         | , 85 |premium         | , 100 |Flag         | , 110 |Flag-Comments         |.
    WRITE:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    LOOP AT Customer INTO customer_instance.
      display_Customer( instance = customer_instance ).
    ENDLOOP.
    DATA(total_records) =  LINES( Customer ).
    DATA(total_in_string) = '=> Total reconds:' && total_records.
    WRITE: total_in_string COLOR 5.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_Customer
  "* Purpose: Display single Customer
  "********************************************************************************
  METHOD display_Customer.
    DATA: is_permium TYPE string,
          is_flagged TYPE string,
          color1     TYPE sy-linct,
          color2     TYPE sy-linct,
          color3     TYPE sy-linct.

    color1 = 4.
    color2 = 4.
    color3 = 4.
    is_permium = 'Regular user'.
    IF instance-premium <> abap_false.
      color2 = 7.
      is_permium = 'Permium user'.
    ENDIF.

    IF instance-flag = abap_true.
      color3 = 6.
      is_flagged = 'Flagged!'.
    ENDIF.
    WRITE: / |{ instance-customer_id WIDTH = 15 }| COLOR = color1,
    15 |{ instance-name WIDTH = 45 }| COLOR = color1,
    35 |{ instance-email  WIDTH = 40 }| COLOR = color1,
    70 |{ instance-password WIDTH = 20 }| COLOR = color1,
    85 |{ is_permium WIDTH = 20  }| COLOR = color2,
    100 |{ is_flagged WIDTH = 15  }| COLOR = color3,
    110 |{ instance-flag_comments WIDTH = 55  }| COLOR = color1.
    WRITE:/.
  ENDMETHOD.
ENDCLASS.