*&---------------------------------------------------------------------*
*&  Include  z_parameters
*&---------------------------------------------------------------------*
PARAMETERS: prodID   TYPE i,
            customID TYPE i.

SELECTION-screen begin of block b_header with FRAME title b_header.

SELECTION-screen begin of block b_prodct with FRAME title b_prodct.

SELECTION-SCREEN BEGIN OF LINE.
parameters: import1 as checkbox.
SELECTION-SCREEN COMMENT 4(50) _import1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _ID.
parameters: ID type Product-product_id default Store=>product_id_counter.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _name.
parameters: name type Product-name.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _desc.
parameters: desc type Product-desc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _catgory.
parameters: category type Product-category.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _price.
parameters: price type Product-price.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _date.
parameters: pod_date type Product-production_date.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _ava.
parameters: is_ava type Product-is_available as checkbox.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN PUSHBUTTON /1(10) insrt_bt USER-COMMAND pushbutton1.
SELECTION-SCREEN PUSHBUTTON /1(10) clear_bt USER-COMMAND pushbutton2.

SELECTION-screen end of block b_prodct.

SELECTION-screen begin of block b_custm with FRAME title b_custm.

SELECTION-SCREEN BEGIN OF LINE.
parameters: import2 as checkbox.
SELECTION-SCREEN COMMENT 4(50) _import2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _ID2.
parameters: id2 type User_struct-customer_id default User=>customer_id_counter.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _name2.
parameters: name2 type User_struct-name.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _email.
parameters: email type User_struct-email.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _pass.
parameters: password type User_struct-password.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _premium.
parameters: premium type User_struct-premium as checkbox.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _flag.
parameters: flag type User_struct-flag as checkbox.
SELECTION-SCREEN END OF LINE..

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(12) _comment.
parameters: comment type User_struct-flag_comments.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN PUSHBUTTON /1(10) insrt_t2 USER-COMMAND pushbutton3.
SELECTION-SCREEN PUSHBUTTON /1(10) clear_t2 USER-COMMAND pushbutton4.

SELECTION-screen end of block b_custm.

SELECTION-screen begin of block extern with FRAME title extern.

SELECTION-screen begin of block b_first with FRAME title b_first.
SELECTION-SCREEN BEGIN OF LINE.
parameters: ch_store as checkbox.
SELECTION-SCREEN COMMENT 4(60) _store.
SELECTION-SCREEN END OF LINE.
SELECTION-screen end of block b_first.

SELECTION-screen begin of block b_secon with FRAME title b_secon.
SELECTION-SCREEN BEGIN OF LINE.
parameters: ch_cust as checkbox.
SELECTION-SCREEN COMMENT 4(60) _cust.
SELECTION-SCREEN END OF LINE.
SELECTION-screen end of block b_secon.

SELECTION-screen begin of block b_third with FRAME title b_third.
SELECTION-SCREEN BEGIN OF LINE.
parameters: ch_cart as checkbox.
SELECTION-SCREEN COMMENT 4(60) _cart.
SELECTION-SCREEN END OF LINE.
SELECTION-screen end of block b_third.

SELECTION-screen begin of block b_forth with FRAME title b_forth.
SELECTION-SCREEN BEGIN OF LINE.
parameters: ch_reviw as checkbox.
SELECTION-SCREEN COMMENT 4(60) _review .
SELECTION-SCREEN END OF LINE.
SELECTION-screen end of block b_forth.
SELECTION-screen end of block extern.

SELECTION-screen end of block b_header.