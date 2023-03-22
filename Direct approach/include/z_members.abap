*&---------------------------------------------------------------------*
*&  Include  z_members
*&---------------------------------------------------------------------*
 TYPES: begin of Member,
          member_id TYPE i,
          name      TYPE string,
          email     TYPE string,
        end of Member,

        BEGIN OF LoanBook,
          member_id TYPE i,
          book_id   TYPE i,
        END OF LoanBook.

 DATA: Members             type table of Member,
       member_instance     type Member,
       ref_member_instance type ref to Member.
 field-symbols: <fs_member_instance> type Member.

 Data: Loan              type table of LoanBook,
       loan_instance     type LoanBook,
       ref_loan_instance type ref to LoanBook.

 field-symbols: <fs_loan_instance> type LoanBook.

 "********************************************************************************
 "* Function Module: add_member
 "* Purpose: Add member to the Members structure
 "********************************************************************************
 Form add_member using
  id type i
  name type string
  email type string.
   member_instance = value #( member_id = id name = name email = email ).
   insert member_instance into table Members.
 endform.

 "********************************************************************************
 "* Function Module: update_member_by_ID
 "* Purpose: Update a member based on its ID
 "********************************************************************************
 Form update_member_by_ID using
    id type i
    name type string
    email type string.
   write:/ '---------------------------------------------------Updating Member by ID---------------------------------------------------------'.
   write:/ 'UPDATE' color 2. write 'Member set name ='. write name. write: 'and email = '. write email && ' where id = '. write: id.
   loop at Members assigning <fs_member_instance>.
     if <fs_member_instance>-member_id = id.
       <fs_member_instance>-name = name.
       <fs_member_instance>-email = email.
       perform member_body using <fs_member_instance>.
     endif.
   endloop.
 endform.

 "********************************************************************************
 "* Function Module: delete_member_by_ID
 "* Purpose: delete a member based on its ID
 "********************************************************************************
 Form delete_member_by_ID using id type i.
   write:/ 'Delete' color 2. write '* from Member where id ='.
   write: id && ' | Processing ... |'.
   READ TABLE Members into member_instance with key member_id = id.
   DELETE Members where member_id = id.
   if SY-subrc = 0.
     write: 'Found a Member with Id ('. write: id && ' ) | showing its attribute:', /.
     perform member_body using member_instance.
   endif.
 endform.

 "********************************************************************************
 "* Function Module: delete_member_by_name
 "* Purpose: delete a member based on its name
 "********************************************************************************
 Form delete_member_by_name using name type string.
   write:/ 'Delete' color 2. write '* from Member where name ='.write: name && ' | Processing ... |'.
   READ TABLE Members into member_instance with key name = name.
   DELETE Members where name = name.
   if SY-subrc = 0.
     write: ' Found a Member with Member ('. write: name && ' ) | showing its attribute:', /.
     perform member_body using member_instance.
   endif.
 endform.

 "********************************************************************************
 "* Function Module: display_member_table
 "* Purpose: Display The Members table in a structured manners
 "********************************************************************************
 FORM display_member_table.
   write:/ '------------------------------------------Members Table--------------------------------------------------------------------------------------------------'.
   WRITE: /       |ID    |,5 |Name           |, 25 |Email       |.
   write:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
   Data(totalRecords) =  lines( Members ).
   Loop AT Members into member_instance.
     perform member_body using member_instance.
   endloop.
   write: '=> Total reconds: ' && totalRecords.
 endform.

 "********************************************************************************
 "* Function Module: member_body
 "* Purpose: Display The Members table in a structured manners
 "********************************************************************************
 FORM member_body using member_instance type Member.
   WRITE: / |{ member_instance-member_id WIDTH = 15 }| color 4, 5 |{ member_instance-name WIDTH = 20 }|  color 4,
          25 |{ member_instance-email  WIDTH = 30 }|  color 4.
   write:/.
 endform.

 "********************************************************************************
 "* Function Module: display_loan_table_by_name
 "* Purpose: Display The Loan table in a structured manners
 "********************************************************************************
 FORM display_loan_table_by_name using member_id.
   READ table Members into member_instance with key member_id = member_id.
   if sy-subrc = 0.
     Data(name) = member_instance-name.
   endif.
   write:/ 'All Loaned books to' color 2. write name .
   WRITE: /       |Member ID    |,15 |Book ID           |.
   write:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
   Loop AT Loan into loan_instance where member_id = member_id.
     WRITE: / |{ loan_instance-member_id  WIDTH = 20 }|  color 4, 15 |{ loan_instance-book_id  WIDTH = 20 }|  color 4.
     write:/.
   endloop.
 endform.

 "********************************************************************************
 "* Function Module: display_loan_table_by_name
 "* Purpose: Display The Loan and Members table in a structured manners
 "********************************************************************************
 FORM display_member_loan_table.
   write:/ '------------------------------------------Member Loan Table--------------------------------------------------------------------------------------------------'.
   WRITE: /       |ID    |,5 |Name           |, 25 |Email       |, 60 |Loan book ID       |, 75 |Member ID       |.
   write:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
   Data(totalRecords) =  lines( Members ).
   Loop AT Members into member_instance.
     perform member_loan_body using member_instance loan_instance.
   endloop.
   write: '=> Total reconds: ' && totalRecords.
   write:/.
 endform.

 "********************************************************************************
 "* Function Module: member_loan_body
 "* Purpose: Display The Loan and Members table in a structured manners
 "********************************************************************************
 FORM member_loan_body using member_instance type Member loan_instance type LoanBook.
   WRITE: / |{ member_instance-member_id WIDTH = 15 }| color 4, 5 |{ member_instance-name WIDTH = 20 }|  color 4,
        25 |{ member_instance-email  WIDTH = 70 }|  color 4.
   loop at Loan into loan_instance.
     if loan_instance-member_id = member_instance-member_id.
       WRITE: 60 |{ loan_instance-book_id  WIDTH = 20 }|  color 4, 75 |{ loan_instance-member_id  WIDTH = 10 }|  color 4.
     endif.
   endloop.
   write:/.
 endform.
