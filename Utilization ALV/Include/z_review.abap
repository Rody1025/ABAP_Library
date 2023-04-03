*&---------------------------------------------------------------------*
*&  Include  z_review
*&---------------------------------------------------------------------*
CLASS Review DEFINITION inheriting from Operations.
  PUBLIC SECTION.
    CLASS-DATA: review_id_counter TYPE I VALUE 1.
    METHODS: add_review IMPORTING
                          review_id   TYPE I
                          customer_id TYPE I
                          product_id  TYPE I
                          review      TYPE string
                          rating      type I,

      update_review IMPORTING
                      review_id TYPE I
                      review    TYPE string
                      rating    type I,

      delete_review IMPORTING
                      review_id TYPE I,
      cal_average_rating,
      init_ALV_columns redefinition,
      free_tables redefinition,
      display_table redefinition.
  PRIVATE SECTION.
    DATA: col_header_table TYPE SLIS_T_FIELDCAT_ALV,
          column           TYPE SLIS_FIELDCAT_ALV.
    METHODS:
      display_review IMPORTING
                               instance      TYPE Review_struct
                     returning value(result) type string.
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
                                 review = review rating = rating ).
      INSERT review_instance INTO TABLE Customer_review.
      review_id_counter = review_id_counter + 1.
    ELSE.
      READ TABLE Inventory into product_instance with key product_id = product_id.
      IF sy-subrc = 0.
        APPEND LINES OF append_comment_italic( 'cant add :' && product_instance-name && ' You need to purchese before reviewing it!' ) to comments_table.
        APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
      endif.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: init_ALV_columns
  "* Purpose: Initialize the table headers
  "********************************************************************************
  method init_ALV_columns.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'review_id' col_name = 'Id' len = 2  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 2 header = 'product_id' col_name = 'Product Id' len = 15 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 3 header = 'customer_id' col_name = 'customer Id' len = 15  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 5 header = 'rating' col_name = 'Rating' len = 5 ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 4 header = 'review' col_name = 'Review' len =  60 ) to col_header_table.
  endmethod.
  "********************************************************************************
  "* Method: display_review_table
  "* Purpose: Display Customer Review table
  "********************************************************************************
  METHOD display_table.
    if is_inital = abap_true.
      APPEND LINES OF append_comment_header( 'Review Table' ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Total Review :' value = '' && get_table_length( Customer_review ) ) to comments_table.
      APPEND LINES OF append_comment_pair( key = 'Date :' value = '' && date ) to comments_table.
      APPEND LINES OF append_comment_italic( 'Look at the table and check the item.' ) to comments_table.
      APPEND LINES OF append_comment_italic( 'After pressing ESC, we will process/manipulate the data.' ) to comments_table.
      DATA(tmp_tabel) = Customer_review.
    else.
      tmp_tabel = table.
    endif.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = tmp_tabel.
    " Freeing the table, while the first element is kept.
    LOOP AT comments_table FROM 2 INTO DATA(ls_header).
      DELETE comments_table INDEX 2.
    ENDLOOP.
  ENDMETHOD.
  "********************************************************************************
  "* Private Method: display_review
  "* Purpose: Display single Customer review
  "********************************************************************************
  METHOD display_review.
    DATA(id) = '' && instance-review_id.
    DATA(customer_id) = '' && instance-customer_id .
    DATA(product_id) = '' && instance-product_id .
    CONCATENATE id customer_id product_id instance-review into result separated by ' '.
  ENDMETHOD.
  "********************************************************************************
  "* Method: update_review
  "* Purpose: Update a review to a product
  "********************************************************************************
  METHOD update_review.
    APPEND LINES OF append_comment_italic( 'UPDATE Customer_review set ... where id = ' && review_id ) to comments_table.
    READ TABLE Customer_review INTO review_instance WITH KEY review_id = review_id.
    IF sy-subrc = 0.
      DATA(res) = display_review( review_instance ).
      APPEND LINES OF append_comment_italic( res ) to comments_table.
      review_instance-review = review.
      review_instance-rating = rating.
    ELSE.
      APPEND LINES OF append_comment_italic( 'No item has been found!' ) to comments_table.
    ENDIF.
  ENDMETHOD.
  "********************************************************************************
  "* Method: delete_review
  "* Purpose: Delete a review
  "********************************************************************************
  METHOD delete_review.
    READ TABLE Customer_review INTO review_instance WITH KEY review_id = review_id.
    APPEND LINES OF append_comment_italic( 'DELETE 1 form Customer_reivew where product_id = ' ) to comments_table.
    APPEND LINES OF append_comment_italic( review_instance-product_id && ' and customer_id = ' && review_instance-customer_id ) to comments_table.
    IF sy-subrc = 0.
      DELETE Customer_review WHERE product_id = review_instance-product_id AND customer_id = review_instance-customer_id.
    ELSE.
      APPEND LINES OF append_comment_italic( 'No item has been found!' ) to comments_table.
    ENDIF.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    APPEND LINES OF append_comment_italic( ' ' ) to comments_table.
    APPEND LINES OF append_comment_italic( 'Table after processing' ) to comments_table.

    display_table( is_inital = abap_false table = Customer_review ).
  ENDMETHOD.
  "********************************************************************************
  "* Method: cal_average_rating
  "* Purpose: Calculate the average rating
  "********************************************************************************
  method cal_average_rating.
    free col_header_table.

    APPEND LINES OF init_ALV_column( pos = 1 header = 'product_id' col_name = 'Id' len = 2  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'counter' col_name = 'Number of item' len = 20  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'total_rating' col_name = 'Total rating' len = 15  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'average' col_name = 'Averge' len = 10  ) to col_header_table.

    APPEND LINES OF append_comment_italic( 'Calculating Average' ) to comments_table.

    TYPES: BEGIN OF Rating_struct,
             product_id   TYPE I,
             counter      TYPE I,
             total_rating TYPE I,
             average      TYPE P length 10 decimals 2,
           END OF Rating_struct.

    DATA: Ratings             TYPE STANDARD TABLE OF Rating_struct,
          rating_instance     TYPE Rating_struct,
          ref_rating_instance TYPE ref to Rating_struct.

    LOOP AT Customer_review INTO review_instance.
      Read Table Ratings reference into ref_rating_instance with key product_id = review_instance-product_id.
      IF sy-subrc = 0.
        ref_rating_instance->counter = ref_rating_instance->counter + 1.
        ref_rating_instance->total_rating = ref_rating_instance->total_rating + review_instance-rating.
      ELSE.
        rating_instance = VALUE #( product_id = review_instance-product_id counter = 1 total_rating = review_instance-rating average = 0 ).
        APPEND rating_instance TO Ratings.
      ENDIF.
    ENDLOOP.

    DATA(len) = LINES( Ratings ).
    LOOP AT Ratings reference into ref_rating_instance where counter > 0.
      ref_rating_instance->average = ref_rating_instance->total_rating / ref_rating_instance->counter.
    ENDLOOP.
    Sort Ratings by product_id.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = Ratings.

*******************************Product Rating***************************************
    free col_header_table.

    APPEND LINES OF init_ALV_column( pos = 1 header = 'product_id' col_name = 'Id' len = 2  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'average' col_name = 'Number of item' len = 15  ) to col_header_table.
    APPEND LINES OF init_ALV_column( pos = 1 header = 'name' col_name = 'Total rating' len = 50  ) to col_header_table.

    TYPES: BEGIN OF Product_rating_struct,
             product_id TYPE I,
             average    TYPE P length 10 decimals 2,
             name       TYPE String,
           END OF Product_rating_struct.

    DATA: Product_rating          TYPE STANDARD TABLE OF Product_rating_struct,
          product_rating_instance TYPE Product_rating_struct.

    LOOP AT Ratings INTO rating_instance.
      READ TABLE Inventory into product_instance with key product_id = rating_instance-product_id.
      if sy-subrc = 0.
        product_rating_instance-product_id = rating_instance-product_id.
        product_rating_instance-average = rating_instance-average.
        product_rating_instance-name = product_instance-name.
      endif.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        it_fieldcat            = col_header_table
      TABLES
        t_outtab               = Product_rating.

  endmethod.
  "********************************************************************************
  "* Private Method: free_tables
  "* Purpose: Freeing the tables before next process begin
  "********************************************************************************
  method free_tables.
    FREE comments_table.
    FREE col_header_table.
  endmethod.
ENDCLASS.