*&---------------------------------------------------------------------*
*&  Include  z_store
*&---------------------------------------------------------------------*
"********************************************************************************
"* Class: Store
"* Purpose: DEFINITION for Store Class
"********************************************************************************
CLASS Store DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: product_id_counter TYPE I VALUE 1.

    METHODS:
      constructor,
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
      display_inventory,
      generate_report.
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
  "* Method: constructor
  "* Purpose: Class constructor, called when creating the class
  "********************************************************************************
  method constructor.
    WRITE:/ 'Color Indications' Color 1.
    WRITE: /       |Color    |, 12 |Meaning   |.
    WRITE:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.
    WRITE: / |{ 'Grau' WIDTH = 15 }| COLOR 1, 12 |{ 'Processing something' WIDTH = 30 }|.
    WRITE: / |{ 'RED' WIDTH = 15 }| COLOR 6, 12 |{ 'Item is not available/flagged' WIDTH = 30 }|.
    WRITE: / |{ 'GREEN' WIDTH = 15 }| COLOR 5, 12 |{ 'Total counter/price' WIDTH = 30 }|.
    WRITE: / |{ 'Orange' WIDTH = 15 }| COLOR 7, 12 |{ 'Premium' WIDTH = 30 }|.
    WRITE:/.
  endmethod.
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
  "* Method: generate_report
  "* Purpose: Generate a report for product, displaying total number of P, number of
  "* P in each category, average price of products...
  "********************************************************************************
  method generate_report.
    WRITE: 'Generate Report' COLOR 2.
    TYPES: begin of report_struct,
             category                type string,
             num_product_in_category type i,
             average_price           type P length 10 decimals 2,
           end of report_struct.

    DATA: Report              type table of report_struct,
          report_instance     type report_struct,
          ref_report_instance type ref to report_struct.

    DATA counter type i value 0.
    Loop at Inventory into product_instance.
      Read table Report reference into ref_report_instance with key category = product_instance-category.
      if sy-subrc = 0.
        ref_report_instance->num_product_in_category = ref_report_instance->num_product_in_category + 1.
        ref_report_instance->average_price = ref_report_instance->average_price + product_instance-price.
      else.
        report_instance = value #( category = product_instance-category
                                   num_product_in_category = 1 average_price = product_instance-price ).
        append report_instance to Report.
      endif.
      counter = counter + 1.
    endloop.
    loop at Report reference into ref_report_instance.
      ref_report_instance->average_price =  ref_report_instance->average_price / ref_report_instance->num_product_in_category.
    endloop.
    WRITE:/ '----------------------------------------------------------Generated Report-----------------------------------------------------------------' COLOR 2.
    WRITE: /       |Category    |, 15 |Number of Product           |, 40 |Average price           |.
    WRITE:/ '-------------------------------------------------------------------------------------------------------------------------------------------------------'.
    LOOP AT Report INTO report_instance.
      WRITE: / |{ report_instance-category WIDTH = 15 }| COLOR 4,
            15 |{ report_instance-num_product_in_category WIDTH = 35 }| COLOR 4,
            40 |{ report_instance-average_price WIDTH = 20 }| COLOR 4.
      WRITE:/.
    ENDLOOP.
    DATA(total_in_string) = '=> Total reconds:' && counter.
    WRITE: total_in_string COLOR 5.
  endmethod.
  "********************************************************************************
  "* Method: display_inventory
  "* Purpose: Display the Inventory table in a structured manners
  "********************************************************************************
  METHOD display_inventory.
    WRITE:/ '-------------------------------------------------------------------Product Table-----------------------------------------------------------------------' color 1.
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
