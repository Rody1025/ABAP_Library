*&---------------------------------------------------------------------*
*&  Include  z_decleration
*&---------------------------------------------------------------------*
TYPES: BEGIN OF Product,
         product_id   TYPE I,
         name         TYPE string,
         desc         TYPE string,
         category     TYPE string,
         price        TYPE p length 10 DECIMALS 2,
         is_available TYPE abap_bool,  " Availability
       END OF Product.

DATA: Inventory            TYPE TABLE OF Product,
      product_instance     TYPE Product,
      ref_product_instnace TYPE REF TO  Product.
FIELD-symbols: <fs_instance_product> TYPE Product.

TYPES: BEGIN OF Inventory_readable,
         product_id   TYPE I,
         name         TYPE string,
         desc         TYPE string,
         category     TYPE string,
         price        TYPE p length 10 DECIMALS 2,
         is_available TYPE string,
       END OF Inventory_readable.
DATA: tmp_table          TYPE TABLE OF Inventory_readable,
      tmp_table_instance TYPE Inventory_readable.
**********************************************************************
TYPES: BEGIN OF User_struct,
         customer_id   TYPE I,
         name          TYPE string,
         email         TYPE string,
         password      TYPE string,
         premium       TYPE abap_bool,
         flag          TYPE abap_bool,
         flag_comments TYPE string,
       END OF User_struct.

DATA: Customer              TYPE TABLE OF User_struct,
      customer_instance     TYPE User_struct,
      ref_customer_instance TYPE REF TO  User_struct.
**********************************************************************
TYPES: BEGIN OF Cart_struct,
         cart_id      TYPE I,
         customer_id  TYPE I,
         product_id   TYPE I,
         history_flag TYPE abap_bool, " have been bought or not!
       END OF Cart_struct.

DATA: Customer_cart TYPE TABLE OF Cart_struct,
      cart_instance TYPE Cart_struct.
FIELD-symbols: <fs_cart_instance> TYPE Cart_struct.
**********************************************************************
TYPES: BEGIN OF Review_struct,
         review_id   TYPE I,
         product_id  TYPE I,
         customer_id TYPE I,
         review      TYPE string,
         rating      TYPE I,
       END OF Review_struct.

DATA: Customer_review     TYPE TABLE OF Review_struct,
      review_instance     TYPE Review_struct,
      ref_review_instance TYPE REF TO  Review_struct.
DATA(date) =  sy-datum+6(2) && '/'  && sy-datum+4(2) && '/' && sy-datum(4).