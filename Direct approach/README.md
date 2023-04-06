# Inventory Management System for an Online Store

This is a simple software that can manage the product inventory of a small online store. The system allows store staff to perform basic operations like adding, deleting, sorting, and data processing and manipulation.

## Table of Contents
- [Internal Table Structure](#internal-table-Structure)
- [Manipulation Table](#Manipulation-table)
- [Loan and Return System](#Loan-and-Return-System)
- [Members Structure](#Members-Structure)
- [Search Functionality](#Search-Functionality)
- [Reservation System](#search-functionality)
- [Statistics and Reporting](#Statistics-and-Reporting)
    
## Internal Table Structure

To start, we need to create an internal table structure called Book to hold the necessary data fields:

    Book  ID
    Book  Title
    Book  Author
    Book  Publication Year
    Book Availability

## Manipulation Table

#### Adding Products

Library staff can add Books to the internal table using the 'add' functionality.

This action will insert a new row with the required data fields.

#### Deleting Products

Books can be removed from the internal table using the 'delete' functionality.

This action will delete a row based on a unique identifier, such as the Book ID.

#### Sorting Products

The Library staff can sort the Books in the internal table using the 'sort' functionality.
They can choose to sort by one or more data fields (e.g., by Author and Publication Year).

## Data Processing and Manipulation

The Library staff can process and manipulate the data in the internal table using various functions, such as finding all Books by a specific author, searching for Books published within a certain date range, or updating the availability of a Book.

# Extend

The system can be extended to include the following features:

## Loan and Return System

- Add a loan duration and due date to the Book structure.
- Add a new form to loan a Book: set the Book's availability to false, and calculate the due date based on the loan duration.
- Add a form to return a Book: update the Book's availability to true and reset the due date.
- Add a form to list all overdue Books.

## Cart System

- Create a Member structure with fields like Member ID, Name, Email, and Loaned Books.
- Implement forms to add, update, and delete members.
- Modify the loan and return book forms to associate a book with a member when loaned and remove the association when returned.
- Add a form to display all books loaned by a specific member.

### Members Structure

- Implement a form to search for books by keywords in the title or author fields.
- Implement a form to search for books by a range of publication dates.

## Search Functionality

- Implement a form to search for books by keywords in the title or author fields.
- Implement a form to search for books by a range of publication dates.

## Reservation System

- Add a reservation queue to the Book structure.
- Create forms to add and remove members from a book's reservation queue.
- When a book is returned, check if there's a reservation and automatically loan it to the next member in the queue.
    
### Statistics and Reporting

- Create a form to display the total number of books, total available books, and total loaned books.
- Implement a form to display the most popular authors and books, based on the number of times they were loaned.