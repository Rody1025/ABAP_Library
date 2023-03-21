*&---------------------------------------------------------------------*
*& Report  z_11_internaltable_advance
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

" Scenario:
" A small Library wants to manage its Book inventory with the help of a simple software
" that can perform basic operations like adding, deleting, sorting, and data
" processing and manipulation.

" Step 1: Creating the Internal Table structure
" To start, we need to create an internal table structure called "Book" to
"  hold the necessary data fields: Book ID, Title, Author, Publication Year, and Availability.

" Step 2: Adding Books to the Internal Table
" Library staff can add Books to the internal table using the 'add' functionality.
" This action will insert a new row with the required data fields.

" Step 3: Deleting Books from the Internal Table
" Books can be removed from the internal table using the 'delete' functionality.
" This action will delete a row based on a unique identifier, such as the Book ID.

" Step 4: Sorting Books in the Internal Table
" The Library staff can sort the Books in the internal table using the 'sort' functionality.
" They can choose to sort by one or more data fields (e.g., by Author and Publication Year).

" Step 5: Data processing and manipulation
" The Library staff can process and manipulate the data in the internal table using
" various functions, such as finding all Books by a specific author, searching for Books
" published within a certain date range, or updating the availability of a Book.

" Extend:
" 1) Implement a loan and return system:
"     Add a loan duration and due date to the Book structure.
"     Add a new form to loan a Book: set the Book's availability to false,
"        and calculate the due date based on the loan duration.
"     Add a form to return a Book: update the Book's availability to true and reset the due date.
"    Add a form to list all overdue Books.

" 2) Add a new structure for library members and a table to store member data:
"     Create a Member structure with fields like Member ID, Name, Email, and Loaned Books.
"     Implement forms to add, update, and delete members.
"     Modify the loan and return book forms to associate a book with a member when loaned and remove the association when returned.
"     Add a form to display all books loaned by a specific member.

" 3) Add a search functionality:
"     Implement a form to search for books by keywords in the title or author fields.
"     Implement a form to search for books by a range of publication dates.

" 4) Implement a basic reservation system:
"     Add a reservation queue to the Book structure.
"     Create forms to add and remove members from a book's reservation queue.
"     When a book is returned, check if there's a reservation and automatically loan it to the next member in the queue.

" 5) Add basic statistics and reporting:
"     Create a form to display the total number of books, total available books, and total loaned books.

REPORT z_11_internaltable_advance.

"********************************************************************************
"* Include
"* z_members: Contain the implementation of the member class
"* z_reservation: Contain the implementation of the reservation class
"********************************************************************************
INCLUDE z_members.
INCLUDE z_reservation.

"********************************************************************************
"* Define Types and Data, make the use of reference type and field symbols
"********************************************************************************
TYPES:
  begin of Book,
    ID                 type i,
    title              type String,
    author             type String,
    publication_date   type d,
    availability       type abap_bool,
    copy               type i,
    loan_duration_days type i,
    rent_date          type d,
    is_due             type string,
  end of Book.

start-of-selection.
  DATA: Library           type table of Book,
        book_instance     type Book,
        ref_book_instance type ref to Book.
  FIELD-SYMBOLS: <fs_book_instance> type Book.

  Data inital_date type d value '00000000'.

  "********************************************************************************
  "* Separation of the work and the implementation
  "* Calling subroutines
  "********************************************************************************
  perform populate_tables.
  perform check_due_time.
  write:/ 'Display the original Tables'.
  perform display_table.
  perform display_member_table.
  write:/ 'Cleaning the table: {Library}' color 1.
  perform del_books_if_not_available using abap_false.
  perform del_book_by_ID using 002.
  perform inc_book_copy_by_id using 007.
  write:/ 'Cleaning the table: {Members}' color 1.
  perform update_member_by_ID using 012 'John Wick' 'john.wick@gmail.com'.
  perform delete_member_by_ID using 011.
  perform delete_member_by_name using 'Amy Chen'.
  write:/ 'Display the Tables after cleaning' color 1.
  perform display_table.
  perform display_member_table.

  write:/ 'Lending books' color 1.
  perform loan_a_member using 001 000 'To Kill a Mockingbird'.
  perform loan_a_member using 001 002 'NA'.
  perform loan_a_member using 002 003 'NA'.
  perform loan_a_member using 003 015 'NA'.
  perform loan_a_member using 004 011 'NA'.
  perform loan_a_member using 005 007 'NA'.
  perform loan_a_member using 007 010 'NA'.
  perform loan_a_member using 007 012 'NA'.
  uline.
  write:/ 'Make Reservations' color 1.
  perform make_a_reservation using 012 003.
  perform make_a_reservation using 008 015.
  perform make_a_reservation using 009 001.
  perform make_a_reservation using 010 012.
  perform make_a_reservation using 011 014.
  perform make_a_reservation using 008 005.
  perform display_reservations.
  perform display_loan_table.
  perform display_member_loan_table.
  perform display_loan_table_by_name using 001.

  write:/ 'Returning books' color 1.
  perform return_Book using 001 001 ''.
  perform return_Book using 002 003 'NA'.
  perform return_Book using 003 015 'NA'.
  perform return_Book using 007 012 'NA'.
  perform display_loan_table.
  perform display_member_loan_table.
  perform display_reservations.
  perform remove_a_reservation using 008 005.
  perform display_reservations.

  write:/ 'Searching books by keywords' color 1.
  perform search_by_title_or_author using 'Mockingbird'.
  perform search_by_title_or_author using 'Lord'.
  perform search_by_title_or_author using 'Steinbeck'.
  perform find_by_ID using '001'.
  perform find_by_author using 'Gabriel Garcia Marquez'.
  perform find_by_year_range using '19000101' '20100101'.

  write:/ 'Listing Loan after returning' color 1.
  perform display_loan_table.
  perform display_member_loan_table.
  perform list_all_overdue.

  perform sort using 'ID' .
  perform display_table.

  perform sort using 'Publication Date'.
  perform display_table.
  perform sort using 'Author'.
  perform display_table.

  perform add_books USING 010 'Magic' 'tom cris' '19500101' abap_false 2 7 '20230308' ''.
  perform add_books USING 011 'How to raise a child Part2' 'NA' '19120101' abap_false 6 7 '20230203' ''.

  perform update_book_availability using 'Magic'.
  perform update_book_availability using 'How to raise a child Part2'.

  "********************************************************************************
  "* Function Module: add_books
  "* Purpose: Add books to the Library structure
  "********************************************************************************
FORM add_books using
  ID       type i
  title           type String
  author          type String
  publication_date type d
  availability    type abap_bool
  copy     type i
  loan_duration_days    type i
  rent_date    type d
  is_due    type string.

  book_instance = value #( id = ID title = title author = author publication_date = publication_date
                           availability = availability copy = copy loan_duration_days = loan_duration_days
                           rent_date = rent_date is_due = is_due ).
  INSERT book_instance INTO table Library.
endform.

"********************************************************************************
"* Function Module: inc_book_copy_by_id
"* Purpose: increase book copy by 1
"********************************************************************************
form inc_book_copy_by_id using book_id type i.
  write:/ 'Update' color 1. write: 'Library set copy = copy + 1 where id = '. write: book_id &&  ' | Processing ... |'.
  loop at Library assigning <fs_book_instance>.
    if <fs_book_instance>-id = book_id.
      write: 'Showing its attribute ' ,/.
      <fs_book_instance>-copy = <fs_book_instance>-copy + 1.
      perform check_due_time.
      perform check_copy_and_availability.
      perform body using <fs_book_instance>.
    endif.
  endloop.
endform.
"********************************************************************************
"* Function Module: del_books_if_not_available
"* Purpose: Delete unavailable books from the Library structure
"********************************************************************************
FORM del_books_if_not_available using
  is_available    type abap_bool.
  write:/ 'Delete' color 2. write '* from Library where availability = Not available | Processing ... |'.
  DATA: temp_Library like Library.
  temp_Library = Library.
  delete Library where availability = is_available.
  if SY-subcs EQ 'A' .
    write: 'Showing list of the unavailable item(s):' ,/.
    LOOP AT temp_Library INTO book_instance WHERE availability = is_available.
      perform body using book_instance.
    endloop.
  else.
    write: ' There were no Unavailable Items', /.
  endif.
endform.

"********************************************************************************
"* Function Module: del_book_by_ID
"* Purpose: Delete a book by its ID
"********************************************************************************
FORM del_book_by_ID using
  input_ID    type i.
  write:/ 'Delete' color 2.write ' * from Library where ID = '.write: input_ID && ' | Processing ... |'.
  Read Table Library into book_instance WITH key id = input_ID.
  delete Library where ID = input_ID.
  if SY-subcs EQ 'A' .
    write: ' Found a Book with Id ='.write: input_ID && ' | showing its attribute:', /.
    perform body using book_instance.
  else.
    write: 'No book with the ID ('. write: input_id && 'was found', /.
  endif.
endform.

"********************************************************************************
"* Function Module: Sort
"* Purpose: Sorts the library based on the provided sort type
"********************************************************************************
FORM sort using
  sortType type string.
  write:/ 'Sort by' color 2. write: sortType.
  case sortType.
    when 'Author'.
      SORT Library by author.
    when 'Publication Date'.
      SORT Library by publication_date.
    when 'ID'.
      SORT Library by ID.
    when others.
      SORT Library by availability.
  endcase.
endform.

"********************************************************************************
"* Function Module: find_by_ID
"* Purpose: Find a book by its ID
"********************************************************************************
FORM find_by_ID using input_ID type i.
  write:/ 'Select' color 2. write: '* from Library where id = '. write: input_ID.
  Read TABLE Library TRANSPORTING NO FIELDS with key id = input_ID.
  if SY-SUBRC = 0.
    perform body using book_instance.
  else.
    write:/ 'There were no records with ID = ' && input_ID.

  endif.
endForm.

"********************************************************************************
"* Function Module: find_by_author
"* Purpose: Find books by its Author
"********************************************************************************
FORM find_by_author using author_input type string.
  write:/ 'Select' color 2. write: '* from Library where Author = '. write: author_input.
  Read TABLE Library into book_instance with key author = author_input .
  if SY-SUBRC = 0.
    perform body using book_instance.
  else.
    write:/ 'There were no records with Author name = ' && author_input.
  endif.
endForm.

"********************************************************************************
"* Function Module: find_by_year_range
"* Purpose: Find books by its year range
"* Parameters: Start and end date of the search parameter
"********************************************************************************
FORM find_by_year_range using start_date type d end_date type d.
  write:/ 'Select' color 2. write: '* from Library where data between ( '. write: start_date && ' - '. write: end_date && ' )' .
  DATA: lib   LIKE Library.
  LOOP AT Library INTO book_instance WHERE publication_date BETWEEN start_date AND end_date.
    if SY-SUBRC = 0.
      perform body using book_instance.
    else.
      write:/ 'There were no records with Date in ( = ' && start_date && ' - ' && end_date && ' )' .
    endif.
  endloop.

endform.

"********************************************************************************
"* Function Module: update_book_availability
"* Purpose: Update book availability by its title
"********************************************************************************
form update_book_availability using title type string.
  write:/ 'UPDATE' color 2. write: 'libray set availability = Available where title = : "'. write title. write '"'.
  loop at Library reference into ref_book_instance.
    if ref_book_instance->title EQ title AND ref_book_instance->availability = abap_false.
      ref_book_instance->availability = abap_true.
    endif.
  endloop.
endform.


"********************************************************************************
"* Function Module: check_due_time
"* Purpose: calculate the due date
"********************************************************************************
form check_due_time.
  DATA: left_time    type i.
  loop at Library reference into ref_book_instance.
    if ref_book_instance->rent_date <> inital_date.
      perform cal_date_diff using ref_book_instance changing left_time.
      if left_time < 0.
        ref_book_instance->is_due = 'Overdue for ' && left_time && ' day(s)'.
      else.
        ref_book_instance->is_due = left_time && ' day(s) left'.
      endif.
    endif.
  endloop.
endform.

"********************************************************************************
"* Function Module: check_copy_and_availability
"* Purpose: Change availability of the books based on number of the copies left
"********************************************************************************
form check_copy_and_availability.
  data:  left_time type i.
  loop at Library reference into ref_book_instance.
    perform cal_date_diff using ref_book_instance changing left_time.
    if ref_book_instance->copy < 1 and left_time < 0.
      ref_book_instance->availability = abap_false.
    endif.
  endloop.
endform.

"********************************************************************************
"* Function Module: loan_a_member
"* Purpose: Loan a book to a member
"********************************************************************************
form loan_a_member using
               member_id type i
               book_id type i
               title type string.
  READ table Members into member_instance with key member_id = member_id.
  if sy-subrc = 0.
    DATA(name) = member_instance-name.
    DATA(id) = member_instance-member_id.
  endif.
  write:/ 'Update' color 2. write 'Library set copy = copy - 1 where'.
  if book_id <> 0.
    write: 'id = '. write book_id.
  else.
    write: 'title = '. write title.
  endif.

  loop at Library assigning <fs_book_instance>.
    if ( <fs_book_instance>-id = book_id or <fs_book_instance>-title = title ).
      if <fs_book_instance>-copy - 1 >= 0.
        loop at Members assigning <fs_member_instance>.
          if <fs_member_instance>-member_id = member_id.
            write: 'You have'. write: <fs_book_instance>-copy && ' copies. Loaning one | Processing ...| to '. write: name. write id.
            <fs_book_instance>-copy = <fs_book_instance>-copy - 1.
            <fs_book_instance>-rent_date = sy-datum.

            DATA: days type i, date type d.
            days = <fs_book_instance>-loan_duration_days.
            date = <fs_book_instance>-rent_date.
            ADD days TO date.
            DATA(left_time) = date - SY-datum .
            <fs_book_instance>-loan_duration_days = left_time.
            <fs_book_instance>-is_due = 'Rented today'.

            loan_instance = value #( member_id = <fs_member_instance>-member_id book_id = <fs_book_instance>-id ).
            insert loan_instance into table Loan.
            perform body using <fs_book_instance>.
          endif.
        endloop.
      else.
        write:/ 'Cant loan more copy, you have only 0 copy'.
      endif.
    endif.
  endloop.
endform.

"********************************************************************************
"* Function Module: return_Book
"* Purpose: Return a book from a member, and check its that book is reserved or not
"********************************************************************************
form return_Book using
               member_id type i
               book_id type i
               title type string.
  if member_id <> 0.
    write:/ 'Update' color 2. write 'Library set copy = copy + 1 where id = '. write member_id.
  else.
    write:/ 'Update' color 2. write 'Library set copy = copy + 1 where title = '. write title.
  endif.

  loop at Loan reference into ref_loan_instance.
    if ref_loan_instance->member_id = member_id and ref_loan_instance->book_id = book_id.
      delete Loan where member_id = member_id and book_id = book_id.
      if SY-subrc = 0.
        write: 'Found a Member with Id ('. write: member_id && ' ) that lended the book with the id (' . write: book_id.
        loop at Library assigning <fs_book_instance> where ID = book_id.
          write:/ 'You had'. write: <fs_book_instance>-copy && ' copies. Returning one | Processing ...| | showing its attribute:'.
          <fs_book_instance>-copy = <fs_book_instance>-copy + 1.
          <fs_book_instance>-is_due = 'Returned today'.
          perform body using <fs_book_instance>.

          write: 'Checking if the book '. write: <fs_book_instance>-title && ' was reserved. | Processing ...| '.

          " member | table | res
          " 001    |  001  | no
          " 002    |  003  | yes

          " member | table |
          " 003    |  001  |
          " 006    |  003  |

          Loop at Reservation_queue reference into ref_res_instance where  book_id = book_id.
            read table Members into member_instance with key member_id = ref_res_instance->member_id.
            write:/ 'One reservation has been found for this book for: ', member_instance-name.
            write:/ 'Loaning the book to: ', member_instance-name. write 'and deleting the reservation'.
            ref_res_instance->is_reserved = abap_false.
            loan_instance = value #( member_id = ref_res_instance->member_id book_id = ref_res_instance->book_id ).
            insert loan_instance into table Loan.
            delete Reservation_queue where is_reserved = abap_false and book_id = book_id.
          endloop.
        endloop.
      endif.
    endif.
  endloop.
  uline.
endform.

"********************************************************************************
"* Function Module: list_all_overdue
"* Purpose: Print a list of all overdue books
"********************************************************************************
form list_all_overdue.
  write:/ 'Listing all overdue Books' color 2.
  loop at Library into book_instance where is_due CS 'Overdue'.
     perform body using book_instance.

  endloop.
endform.

"********************************************************************************
"* Function Module: search_by_title_or_author
"* Purpose: Search books by keywords
"********************************************************************************
FORM search_by_title_or_author using keyword type string.
  write:/ 'Searching for the keyword' color 2. write keyword.
  Loop at Library into book_instance.
    if book_instance-title CS keyword.
      write:/ 'Found a book with the title:'.
      perform body using book_instance.
    elseif book_instance-author CS keyword.
      write:/ 'Found a book with the author name:'.
      perform body using book_instance.
    endif.
  endloop.
endform.

"********************************************************************************
"* Function Module: display_table
"* Purpose: Display The library table in a structured manners
"********************************************************************************
FORM display_table.
  perform check_copy_and_availability.

  WRITE: /       |ID    |,5 |Title           |, 45 |Author       |,
              70 |Publication Date       |, 90 |Availability       |,  110 |Copies       |,
               120 |Loan Duration       |, 135 |Loan Date       |, 150 |Due comments        |.
  write:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
  Data(totalRecords) =  lines( Library ).

  Loop AT Library assigning <fs_book_instance>.
    perform body using <fs_book_instance>.
  endloop.
  write: '=> Total reconds: ' && totalRecords.
  write:/.
endform.

"********************************************************************************
"* Function Module: body
"* Purpose: Display The library table in a structured manners
"********************************************************************************
FORM body using getInstance type Book.
  DATA: is_available   type string, published_date type string, rent_date type string, left_time type i.

  perform date_to_YYYYMMDD using getInstance-publication_date changing published_date.
  perform date_to_YYYYMMDD using getInstance-rent_date changing rent_date.
  perform cal_date_diff_instance using getInstance changing left_time.

  if getInstance-availability = abap_true.
    is_available = 'Available'.
  else.
    is_available = 'Not Available'.
  endif.
  "  color 4 color blue | color 5 green

  if getInstance-availability = abap_false.
    WRITE: / |{ getInstance-id WIDTH = 15 }|, 5 |{ getInstance-title WIDTH = 40 }|, 45 |{ getInstance-author  WIDTH = 40 }|,
    70 |{ published_date WIDTH = 20 }|,  90 |{ is_available WIDTH = 20  }|,  110 |{ getInstance-copy WIDTH = 20 }|,
     120 |{ getInstance-loan_duration_days WIDTH = 20 }|, 135 |{ rent_date WIDTH = 10 }| ,
      150 |{ getInstance-is_due WIDTH = 25 }|.

  elseif left_time <= 0.
    if getInstance-is_due = 'Rented today' or  getInstance-is_due =  'Returned today'.
      WRITE: / |{ getInstance-id WIDTH = 15 }| color 4, 5 |{ getInstance-title WIDTH = 40 }| color 4,
     45 |{ getInstance-author  WIDTH = 40 }| color 4, 70 |{ published_date WIDTH = 20 }| color 4,
      90 |{ is_available WIDTH = 20  }| color 4,  110 |{ getInstance-copy WIDTH = 20 }| color 4,
         120 |{ getInstance-loan_duration_days WIDTH = 20 }| color 4, 135 |{ rent_date WIDTH = 20 }| color 4,
             150 |{ getInstance-is_due WIDTH = 25 }| color 5. "Green
    else.
      WRITE: / |{ getInstance-id WIDTH = 15 }| color 4, 5 |{ getInstance-title WIDTH = 40 }| color 4,
     45 |{ getInstance-author  WIDTH = 40 }| color 4, 70 |{ published_date WIDTH = 20 }| color 4,
      90 |{ is_available WIDTH = 20  }| color 4,  110 |{ getInstance-copy WIDTH = 20 }| color 4,
         120 |{ getInstance-loan_duration_days WIDTH = 20 }| color 4, 135 |{ rent_date WIDTH = 20 }| color 4,
             150 |{ getInstance-is_due WIDTH = 25 }| color 6. "RED
    endif.
  else.
    WRITE: / |{ getInstance-id WIDTH = 15 }| color 4, 5 |{ getInstance-title WIDTH = 40 }|  color 4,
    45 |{ getInstance-author  WIDTH = 40 }|  color 4, 70 |{ published_date WIDTH = 20 }|  color 4,
         90 |{ is_available WIDTH = 20  }|  color 4,  110 |{ getInstance-copy WIDTH = 20 }|  color 4,
            120 |{ getInstance-loan_duration_days WIDTH = 20 }|  color 4, 135 |{ rent_date WIDTH = 20 }|  color 4,
              150 |{ getInstance-is_due WIDTH = 25 }|  color 4.
  endif.
  write:/.
endform.

"********************************************************************************
"* Function Module: display_loan_table
"* Purpose: Display The Loan table in a structured manners
"********************************************************************************
FORM display_loan_table.
  write:/ '------------------------------------------Loan Table--------------------------------------------------------------------------------------------------'.
  WRITE: /       |Member ID    |,15 |Book ID           |, 25 |Book Title       |.
  write:/ '-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
  Data(totalRecords) =  lines( Loan ).
  Loop AT Loan into loan_instance.
    READ table Library into book_instance with key id =  loan_instance-book_id.
    WRITE: / |{ loan_instance-member_id  WIDTH = 20 }|  color 4, 15 |{ loan_instance-book_id  WIDTH = 20 }|  color 4,
          25 |{ book_instance-title  WIDTH = 20 }|  color 4.
    write:/.
  endloop.
  write:'=> Total reconds: ' && totalRecords.
endform.

"********************************************************************************
"* Function Module: cal_date_diff
"* Purpose: Calculate the data different to check the due date
"********************************************************************************
form cal_date_diff using ref_instance type ref to Book changing value(left_time) type i.
  DATA: days type i, date type d.
  days = ref_instance->loan_duration_days.
  date = ref_instance->rent_date.
  ADD days TO date.
  left_time = SY-datum - date.
endform.

"********************************************************************************
"* Function Module: cal_date_diff_instance
"* Purpose: Calculate the data different to check the due date
"********************************************************************************
form cal_date_diff_instance using instance type Book changing value(left_time) type i.
  DATA: days type i, date type d.
  days = instance-loan_duration_days. date = instance-rent_date. ADD days TO date.
  left_time = SY-datum - date.
endform.

"********************************************************************************
"* Function Module: date_to_YYYYMMDD
"* Purpose: Cast date to String form the form YYYY/MM/DD
"********************************************************************************
form date_to_YYYYMMDD using getInstance type d changing value(formatted_due_date) type string.
  formatted_due_date = getInstance(4)
 && '/' && getInstance+4(2)
 && '/' &&  getInstance+6(2).
endform.

"********************************************************************************
"* Function Module: populate_tables
"* Purpose: populate Library and Members table
"********************************************************************************
form populate_tables.
  perform  add_books USING 001 'To Kill a Mockingbird' 'Harper Lee' '19600611' abap_true 5 14 '20230301' ''.
  perform  add_books USING 002 'The Great Gatsby' 'F. Scott Fitzgerald' '19250410' abap_true 4 0 '00000000' ''.
  perform  add_books USING 003 '1984' 'George Orwell' '19490608' abap_true 2 21 '20230308' ''.
  perform  add_books USING 004 'Pride and Prejudice' 'Jane Austen' '18130128' abap_true 3 7 '20230324' ''.
  perform  add_books USING 005 'The Catcher in the Rye' 'J.D. Salinger' '19510103' abap_true 4 10 '20230327' ''.
  perform  add_books USING 006 'The Hobbit' 'J.R.R. Tolkien' '19370921' abap_false 0 0 '00000000' ''.
  perform  add_books USING 007 'The Lord of the Rings' 'J.R.R. Tolkien' '19540729' abap_true 3 14 '20230220' ''.
  perform  add_books USING 008 'One Hundred Years of Solitude' 'Gabriel Garcia Marquez' '19670605' abap_true 8 21 '20230205' ''.
  perform  add_books USING 009 'Brave New World' 'Aldous Huxley' '19320517' abap_true 9 14 '20230131' ''.
  perform  add_books USING 010 'Moby-Dick' 'Herman Melville' '18511018' abap_false 0 0 '00000000' ''.
  perform  add_books USING 011 'The Adventures of Huckleberry Finn' 'Mark Twain' '18841210' abap_true 1 7 '20230324' ''.
  perform  add_books USING 012 'The Grapes of Wrath' 'John Steinbeck' '19390414' abap_true 1 30 '20230101' ''.
  perform  add_books USING 013 'The Sound and the Fury' 'William Faulkner' '19291007' abap_true 0 0 '00000000' ''.
  perform  add_books USING 014 'Wuthering Heights' 'Emily Bronte' '18471219' abap_true 4 7 '20230305' ''.
  perform  add_books USING 015 'The Picture of Dorian Gray' 'Oscar Wilde' '18900701' abap_true 5 30 '20230310' ''.
  perform  add_books USING 040 'To be deleted' 'To be deleted' '10001010' abap_false 0 0 '00000000' ''.

  perform add_member using 001 'John Doe' 'john.doe@example.com'.
  perform add_member using 002 'Emily Smith' 'emily.smith@gmail.com'.
  perform add_member using 003 'Michael Johnson' 'michael.johnson@hotmail.com'.
  perform add_member using 004 'Sarah Lee' 'sarah.lee@example.com'.
  perform add_member using 005 'David Kim' 'david.kim@gmail.com'.
  perform add_member using 006 'Amy Chen' 'amy.chen@hotmail.com'.
  perform add_member using 007 'Kevin Patel' 'kevin.patel@example.com'.
  perform add_member using 008 'Rachel Williams' 'rachel.williams@gmail.com'.
  perform add_member using 009 'Alex Turner' 'alex.turner@hotmail.com'.
  perform add_member using 010 'Jessica Brown' 'jessica.brown@example.com'.
  perform add_member using 011 'NA' 'NA'.
  perform add_member using 012 'Rody abd' 'Rody.abd@gmail.com'.
endform.