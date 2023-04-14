*&---------------------------------------------------------------------*
*& Report  z_dossier_table
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

" Customer/Debtor: They are the entity that receive service or goods. When they receive,
"  they are also issuing an invoice. This invoice has a deadline and a specific amount
"  that is claimed by the Creditor.
"  The Debtor can be a person, company, or a legal entity.

" Supplier/Creditor: They are who provides service or goods. When they provide, they keep
"  track of the amounts owed by their debtors in (Debitorenkontokorrent) which is a type of
"  open-item accounting system.
"  If a Debtor does not pay their invoice by the payment deadline, the Creditor may issue
"  payment reminders (Zahlungserinnerung) or dunning notices (Mahnung). A "Mahnung" sets
"   the Debtor in default (Verzug) and triggers the calculation of interest on the overdue amount.
"   If the payment deadline is not specified in the invoice (or in the contract),
"   the date of the Mahnung becomes the reference date for calculating interest.
"   It is also noted that a Betreibung, which refers to the legal process of debt collection,
"   can be initiated even without a Mahnung.

REPORT z_dossier_table.

TABLES: /ASEM/DOSSIER.

DATA: l_dossier_min TYPE /ASEM/DE_DOSSIER_NO,
      l_dossier_max TYPE /ASEM/DE_DOSSIER_NO.

SELECTION-screen begin of block b_header with FRAME title header_.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(35) id_.
parameters: p_id type /ASEM/DE_DOSSIER_NO.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(32) range_.
SELECT-OPTIONS: doss_id FOR /ASEM/DOSSIER-dossier.
l_dossier_min = doss_id-low.
l_dossier_max = doss_id-high.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(35) custm_.
parameters: custm_id type KUNNR.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(35) created_.
parameters: creat_on type i.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(35) intrst_.
parameters: interest type i.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(35) max_.
parameters: max_item type I default 10 obligatory.
SELECTION-SCREEN END OF LINE.

SELECTION-screen end of block b_header.

AT SELECTION-SCREEN OUTPUT.
  id_ = `Search for spesific ID`.
  range_ = 'Search by id range'.
  custm_ = 'Search for spscific Customer'.
  created_ = 'Search by creation (year)'.
  intrst_ = 'Last interest calculation by Years'.
  max_ = 'Maximum N. of hits'.

INITIALIZATION.
  header_ = `Input to search for a specific value in the /ASEM/DOSSIER table`.

  Include z_operations.

start-of-selection.

  DATA: col_header_table TYPE SLIS_T_FIELDCAT_ALV,
        comments_table   TYPE slis_t_listheader,

        op               type ref to Process_table.
  op = new Process_table( ).

  APPEND LINES OF op->append_comment_header( 'DOSSIER table' ) to comments_table.

  append LINES OF op->init_ALV_column( pos =  1
                                       header = 'DOSSIER'
                                       col_name = 'D. Id'
                                       len =  10 ) to col_header_table.
  append LINES OF op->init_ALV_column( pos =  2
                                       header = 'KUNNR'
                                       col_name = 'Customer ID'
                                       len =  10 ) to col_header_table.
  append LINES OF op->init_ALV_column( pos =  3
                                       header = 'DOSS_TYPE'
                                       col_name = 'D. Type'
                                       len =  10 ) to col_header_table.
  append LINES OF op->init_ALV_column( pos =  4
                                       header = 'COP'
                                       col_name = 'COP (Controlling Operating Post)'
                                       len =  20 ) to col_header_table.
  append LINES OF op->init_ALV_column( pos =  5
                                       header = 'BUKRS'
                                       col_name = 'BUKRS (Associated Code)'
                                       len =  15 ) to col_header_table.
  append LINES OF op->init_ALV_column( pos =  6
                                       header = 'CIRC'
                                       col_name = 'CIRC (Controlling Integrated)'
                                       len =  20 ) to col_header_table.
  append LINES OF op->init_ALV_column( pos =  7
                                       header = 'ERFDATE'
                                       col_name = 'Creation Year'
                                       len =  15 ) to col_header_table.

  op->init_columns( col_header_table = col_header_table ).

  if max_item > 0.
    op->set_number_of_rows( row = max_item ).

    IF p_id IS NOT INITIAL.
      APPEND LINES OF op->append_comment_italic( 'SELECT * where DOSSIER = ' && p_id ) to comments_table.
      op->search_by_id( id = p_id ).

    ELSEIF doss_id IS NOT INITIAL.
      APPEND LINES OF op->append_comment_italic( 'SELECT * where DOSSIER between ( '  ) to comments_table.
      APPEND LINES OF op->append_comment_italic( l_dossier_min && ' --- ' && l_dossier_max && ' )' ) to comments_table.

      op->search_by_id_range( low = l_dossier_min
                         high = l_dossier_max ).

    ELSEIF custm_id IS NOT INITIAL.
      APPEND LINES OF op->append_comment_italic( 'SELECT * where DOSSIER =' && custm_id ) to comments_table.

      op->search_by_customer_id( customer_id = custm_id ).

    ELSEIF creat_on IS NOT INITIAL.
      APPEND LINES OF op->append_comment_italic( 'SELECT * where ERFDATE >=' && op->convert_date_to_YYYYMMDD( creat_on ) ) to comments_table.

      op->search_by_creation_date( input_year = creat_on ).

    ELSEIF interest IS NOT INITIAL.
      APPEND LINES OF op->append_comment_italic( 'SELECT * where INT_CALC_DATE >=' && op->convert_date_to_YYYYMMDD( interest ) ) to comments_table.

      append LINES OF op->init_ALV_column( pos =  8
                                         header = 'INT_CALC_DATE'
                                         col_name = 'Last interest calculation'
                                         len =  25 ) to col_header_table.
      append LINES OF op->init_ALV_column( pos =  9
                                         header = 'INTEREST'
                                         col_name = 'Interest'
                                         len =  15 ) to col_header_table.

      op->init_columns( col_header_table = col_header_table ).

      op->search_by_interest_year( input_year = interest ).
    ENDIF.
    if ( p_id IS INITIAL and
       doss_id IS INITIAL and
       custm_id IS INITIAL and
       creat_on IS INITIAL and
       interest IS INITIAL ) and max_item is not initial.
      op->display_rows( ).
    endif.
  endif.

  "********************************************************************************
  "* Subroutine: top_of_page
  "* Purpose: Append the comments to the comments table
  "********************************************************************************
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = comments_table.
ENDFORM.
