# /ASEM/DOSSIER.

This is a simple demonstration of the /ASEM/DOSSIER table.

## Table of Contents
- [Purpose](#Purpose)
- [Objective](#Objective)
- [Report](#Report)
- [Keywords Highlighted](#Keywords-Highlighted)

## Purpose

The purpose of this exercise is to familiarize yourself with the ASEM/DOSSIER table and its key elements.

## Objective:

- Create a Z-Report for the ASEM/DOSSIER table with selection-screen and ALV display.
- Read up on the key elements of the ASEM/DOSSIER table, and be prepared to discuss them.

## Report

Create a Z-Report with the following features:
a. Selection-screen with the fields highlighted in the table.

| Field                     | key      | initial value | data element                          | data type    | length | decimal | short description |
|---------------------------|----------|---------------|---------------------------------------|--------------|--------|---------|-------------------|
| **DOSSIER**               | &#x2611; | &#x2611;      | /ASEM/DE_DOSSIER_NO                   | CHAR         | 10     |         |                   |
| **KUNNR**                 |          |               | KUNNR                                 | CHAR         | 10     |         |                   |
| **DOSS_TYPE**             |          |               | /ASEM/DE_DOSSIER_TYPE                 | CHAR         | 4      |         |                   |
| **COP**                   |          |               | /ASEM/DE_COP                          | CHAR         | 10     |         |                   |
| **BUKRS**                 |          |               | /ASEM/DE_BUKRS                        | CHAR         | 4      |         |                   |
| **CIRC**                  |          |               | /ASEM/DE_CIRC                         | CHAR         | 4      |         |                   |
| **ERFDATE**               |          |               | /ASEM/DE_ERFDATE                      | DATS         | 8      |         |                   |
| **INT_CALC_DATE**         |          |               | /ASEM/DE_INTEREST_CALC_LDATE          | DATS         | 8      |         |                   |
| **INTEREST**              |          |               | /ASEM/DE_INTEREST                     | CURR         | 13     |         |                   |

In which:

### DOSSIER:

This field represents the unique identifier assigned to each dossier in the ASEM system. It is used to track and manage documents within the system.

### KUNNR:

This field represents the customer number associated with each dossier in the ASEM system. It is used to uniquely identify customers and link them with their respective dossiers.

### Doss_type:

This field is used to classify documents based on their type. It is a domain field that has a value table associated with it, which allows for consistent classification of documents across the system. Examples of document types that can be used in this field include contracts, purchase orders, or invoices.

### COP: (Controlling Operating Post)

This field represents the cost object associated with the dossier. The cost object is a component of the controlling module in SAP, which allows for tracking and management of costs associated with a particular activity or project.

### BUKRS:

This field represents the company code associated with the dossier. The company code is a key component of the finance module in SAP, which allows for tracking and management of financial transactions associated with a particular company.

### CIRC: (Controlling Integrated Reporting Code)

This field represents the circuit associated with the dossier. The circuit is a component of the logistics module in SAP, which allows for tracking and management of inventory and supply chain activities.

### INT_CALC_DATE: 

This field represents the interest calculation date associated with each dossier in the ASEM system. It is used to determine the date on which interest calculations should be performed for the dossier

### INTEREST:

This field represents the interest amount associated with each dossier in the ASEM system. It is used to track the interest accrued on the dossier over time, and is calculated based on the interest rate and calculation date associated with the dossier

## Keywords Highlighted

Use the keywords highlighted in the document to discuss the following key elements of the ASEM/DOSSIER table:
- Nummernkreisobjekt (number range object)
- Buchungskreis (company code)
- Domäne mit Wertebereich bzw. Wertetabelle (domain with value range or value table)

**Customer/Debtor**: They are the entity that receive service or goods. When they receive, they are also issuing an invoice. This invoice has a deadline and a specific amount that is claimed by the Creditor.
The Debtor can be a person, company, or a legal entity.

**Supplier/Creditor**: They are who provides service or goods. When they provide, they keep track of the amounts owed by their debtors in (Debitorenkontokorrent) which is a type of open-item accounting system.
 If a Debtor does not pay their invoice by the payment deadline, the Creditor may issue payment reminders (Zahlungserinnerung) or dunning notices (Mahnung). A "Mahnung" sets the Debtor in default (Verzug) and triggers the calculation of interest on the overdue amount. If the payment deadline is not specified in the invoice (or in the contract), the date of the Mahnung becomes the reference date for calculating interest. It is also noted that a Betreibung, which refers to the legal process of debt collection, can be initiated even without a Mahnung.
