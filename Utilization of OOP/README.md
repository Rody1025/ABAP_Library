# Inventory Management System for an Online Store

This is a simple software that can manage the product inventory of a small online store. The system allows store staff to perform basic operations like adding, deleting, sorting, and data processing and manipulation.

## Table of Contents
- [Internal Table Structure](#internal-table-Structure)
- [Manipulation Table](#Manipulation-table)
- [Data Processing and Manipulation](#date-processing-and-manipulation)
- [User System](#user-System)
- [Cart System](#cart-system)
- [Search Functionality](#search-functionality)
- [Review System](#review-system)
- [Report Functionality](#report-functionality)
    
## Internal Table Structure

To start, we need to create an internal table structure called Product to hold the necessary data fields:

    Product ID
    Product Name
    Product Description
    Product Category
    Price
    Availability

## Manipulation Table

#### Adding Products

Store staff can add products to the internal table using the add functionality. This action will insert a new row with the required data fields.

#### Deleting Products

Products can be removed from the internal table using the delete functionality. This action will delete a row based on a unique identifier, such as the Product ID.

#### Sorting Products

The store staff can sort the products in the internal table using the sort functionality. They can choose to sort by one or more data fields (e.g., by Product Category and Price).

## Data Processing and Manipulation

The store staff can process and manipulate the data in the internal table using various functions, such as finding all products in a specific category, searching for products within a certain price range, or updating the availability of a product.

# Extend

The system can be extended to include the following features:

## User System

- Create a User structure with fields like User ID, Name, Email, and Password.
- Implement forms to add, update, and delete users.

## Cart System

- Add a Cart structure to hold the products that the customer has added to their cart.
- Create methods to add, update and delete products from the cart.
- Implement a form to display the contents of the cart and the total price.
- Implement a form to checkout and purchase the products in the cart.

### Connect Cart with Customer Table

- Modify the cart system to associate a user with their cart.
- Implement a login system that allows users to log in and view their cart and purchase history.

## Search Functionality

- Implement a form to search for products by keywords in the name or description fields.
- Implement a form to search for products by a range of prices.

## Review System

- Add a Review structure to hold the reviews that customers have left for products.
- Create methods to add and delete reviews.
- Implement a form to display the reviews for a product.
- Calculate and display the average rating for each product based on the reviews.

## Report Functionality

- Change the code to implement ALV to display the tables instead of the Write approach.
