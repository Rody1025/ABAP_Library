*&---------------------------------------------------------------------*
*&  Include  z_reservation
*&---------------------------------------------------------------------*
 """""""""""""""""""""""""""""""Reservation """"""""""""""""""""""""""""""""""""""
 TYPES: begin of reservation,
          book_id     type i,
          member_id   type i,
          is_reserved type abap_bool,
        end of reservation.

 DATA: Reservation_queue type table of Reservation,
       res_instance      type Reservation,
       ref_res_instance  type ref to Reservation.
 FIELD-SYMBOLS: <fs_res_instance> type Reservation.


 "********************************************************************************
 "* Function Module: make_a_reservation
 "* Purpose: Make a reservation for a book
 "********************************************************************************
 form make_a_reservation using member_id type i book_id type i.
   write:/ 'insert' color 2. write 'into from Reservation_queue value ( member_id = '. write member_id. write: ', book_id ='.write: book_id && ' ) | Processing ... |'.
   READ table Reservation_queue into res_instance with key member_id = member_id book_id = book_id.
   if sy-subrc <> 0.
     res_instance = value #( book_id = book_id member_id = member_id is_reserved = abap_true ).
     insert res_instance into table Reservation_queue.
     perform reservation_body using res_instance.
   else.
     write:/ 'Something went wrong!'.
   endif.
 endform.

 "********************************************************************************
 "* Function Module: remove_a_reservation
 "* Purpose: Remove a reservation for a book
 "********************************************************************************
 form remove_a_reservation using member_id type i book_id type i.
   write:/ 'Remove a Reservation' color 1.
   write:/ 'Delete' color 2. write 'from Reservation_queue where  member_id = '. write member_id. write: 'and book_id = '.
   write: book_id && '| Processing ... |'.
   READ table Reservation_queue into res_instance with key member_id = member_id book_id = book_id.
   if sy-subrc <> 0.
     write:/ 'The following will be removed!'.
     perform reservation_body using res_instance.
     delete Reservation_queue where member_id = member_id and book_id = book_id.
   endif.
 endform.

 "********************************************************************************
 "* Function Module: display_reservations
 "* Purpose: Display The Reservation table in a structured manners
 "********************************************************************************
 FORM display_reservations.
   write:/ '------------------------------------------Reservations Table--------------------------------------------------------------------------------------------------'.
   WRITE: /       |Member ID    |,15 |Book ID           |, 25 |Reserved       |.
   write:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
   Data(totalRecords) =  lines( Reservation_queue ).
   Loop AT Reservation_queue into res_instance.
     perform reservation_body using res_instance.
   endloop.
   write: '=> Total reconds: ' && totalRecords.
 endform.

 "********************************************************************************
 "* Function Module: display_reservations
 "* Purpose: Display The Reservation table in a structured manners
 "********************************************************************************
 FORM reservation_body using res_instance type Reservation.
   DATA(reserved) = 'Not Reserved'.
   if res_instance-is_reserved = abap_true.
     reserved = 'Reserved'.
   endif.

   WRITE: / |{ res_instance-member_id WIDTH = 15 }| color 4, 15 |{ res_instance-book_id WIDTH = 20 }|  color 4,
          25 |{ reserved  WIDTH = 20 }|  color 4.
   write:/.
 endform.