*&---------------------------------------------------------------------*
*&  Include  z_review
*&---------------------------------------------------------------------*
CLASS Review DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: review_id_counter TYPE I VALUE 1.
    METHODS: add_review IMPORTING review_id TYPE I customer_id TYPE I product_id TYPE I review TYPE string,
      update_review IMPORTING review_id TYPE I review TYPE string,
      delete_review IMPORTING review_id TYPE I,
      cal_average_rating,
      display_review_table.
  PRIVATE SECTION.
    METHODS: display_review IMPORTING instance TYPE Review_struct.
ENDCLASS.
"********************************************************************************
"* Class: Review
"* Purpose: implementation for Review Class
"********************************************************************************
CLASS Review IMPLEMENTATION.
  "********************************************************************************
  "* Method: add_review
  "* Purpose: Add a review to a product
  "********************************************************************************
  METHOD add_review.
    READ TABLE Customer_cart INTO cart_instance WITH KEY product_id = product_id
                                                         customer_id = customer_id
                                                         history_flag = abap_true.
    IF sy-subrc = 0.
      review_instance = VALUE #( review_id = review_id customer_id = customer_id product_id = product_id
                                 review = review ).
      INSERT review_instance INTO TABLE Customer_review.
      review_id_counter = review_id_counter + 1.
    ELSE.
      READ TABLE Inventory into product_instance with key product_id = product_id.
      IF sy-subrc = 0.
        WRITE:/ 'You need to purchese' && product_instance-name && ' before reviewing it!'.
      endif.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_review
  "* Purpose: Update a review to a product
  "********************************************************************************
  METHOD update_review.
    DATA(TITLE) = 'UPDATE Customer_review set ... where id = ' && review_id.
    WRITE:/ TITLE COLOR 2.
    READ TABLE Customer_review INTO review_instance WITH KEY review_id = review_id.
    IF sy-subrc = 0.
      write: 'Before the edit!'.
      display_review( review_instance ).
      review_instance-review = review.
      write: 'After the edit!'.
      display_review( review_instance ).
    ELSE.
      WRITE:/ 'No item has been found!'.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_review
  "* Purpose: Delete a review
  "********************************************************************************
  METHOD delete_review.
    READ TABLE Customer_review INTO review_instance WITH KEY review_id = review_id.
    DATA(TITLE) = 'DELETE 1 form Customer_reivew where product_id = '
                   && review_instance-product_id && ' and customer_id = ' && review_instance-customer_id.
    WRITE:/ TITLE COLOR 2.
    IF sy-subrc = 0.
      WRITE:/ 'Found one review:'.
      display_review( instance = review_instance ).
      DELETE Customer_review WHERE product_id = review_instance-product_id AND customer_id = review_instance-customer_id.
    ELSE.
      WRITE:/ 'No item has been found!'.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: cal_average_rating
  "* Purpose: Calculate the average rating
  "********************************************************************************
  method cal_average_rating.

  endmethod.
  "********************************************************************************
  "* Method: display_review_table
  "* Purpose: Display Customer Review table
  "********************************************************************************
  METHOD display_review_table.
    WRITE:/ '------------------------------------------Customer Review Table----------------------------------------------------------------------------------------------' COLOR 2.
    WRITE: /       |ID    |, 15 |Customer ID           |, 35 |Product ID            |, 55 |Review            |.
    WRITE:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.

    LOOP AT Customer_review INTO review_instance.
      display_review( instance = review_instance ).
    ENDLOOP.
    ULINE.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_review
  "* Purpose: Display single Customer review
  "********************************************************************************
  METHOD display_review.
    WRITE: / |{ instance-review_id WIDTH = 15 }| COLOR 4,
    15 |{ instance-customer_id WIDTH = 45 }| COLOR 4,
    35 |{ instance-product_id  WIDTH = 40 }| COLOR 4,
    55 |{ instance-review  WIDTH = 40 }| COLOR 4.
    WRITE:/.
  ENDMETHOD.
ENDCLASS.