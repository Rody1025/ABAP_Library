*&---------------------------------------------------------------------*
*&  Include  z_decleration
*&---------------------------------------------------------------------*
Types: begin of User_struct,
         customer_id   type i,
         name          type string,
         email         type string,
         password      type string,
         premium       type abap_bool,
         flag          type abap_bool,
         flag_comments type string,
       end of User_struct.

DATA: Customer              type table of User_struct,
      customer_instance     type User_struct,
      ref_customer_instance type ref to  User_struct.

Types: begin of Cart_struct,
         cart_id     type i,
         customer_id type i,
         product_id  type i,
       end of Cart_struct.

DATA: Customer_cart type table of Cart_struct,
      cart_instance type Cart_struct.

TYPES: BEGIN OF Product,
         product_id   TYPE I,
         name         TYPE string,
         desc         TYPE string,
         category     TYPE string,
         price        TYPE p length 10 decimals 2,
         is_available TYPE abap_bool,  " Availability
       END OF Product.

DATA: Inventory            TYPE TABLE OF Product,
      product_instance     TYPE Product,
      ref_product_instnace type ref to  Product.