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

| Field         | key      | initial value | data element             | data type | length | decimal | short description                             |
|---------------|----------|---------------|--------------------------|-----------|--------|---------|-----------------------------------------------|
| **DOSSIER**   | &#x2611; | &#x2611;      | /ASEM/DE_DOSSIER_NO      | CHAR      | 10     |         |                                               |
| **DOSS_TYPE** |          |               | /ASEM/DE_DOSSIER_TYPE    | CHAR      | 4      |         |                                               |
| **COP**       |          |               | /ASEM/DE_COP             | CHAR      | 10     |         |                                               |
| **BUKRS**     |          |               | /ASEM/DE_BUKRS           | CHAR      | 4      |         |                                               |
| **CIRC**      |          |               | /ASEM/DE_CIRC            | CHAR      | 4      |         |                                               |

In which:

### Dossier Number:

The dossier number is assigned by a number range object. It is a unique identifier used to track and manage documents within the ASEM system.

### Buchungskreis:

The Buchungskreis, which is an element from financial accounting, is also stored in the ASEM/DOSSIER table. It is used to assign a document to a particular company code.

### Status:

The status field in the ASEM/DOSSIER table is a domain with a value range. It indicates the current status of the document, such as whether it is in draft, awaiting approval, or has been finalized.

### Doss_type:

The doss_type field is a domain with a value table. It is used to classify documents according to their type, such as contracts, purchase orders, or invoices.

## Keywords Highlighted

Use the keywords highlighted in the document to discuss the following key elements of the ASEM/DOSSIER table:
- Nummernkreisobjekt (number range object)
- Buchungskreis (company code)
- Domäne mit Wertebereich bzw. Wertetabelle (domain with value range or value table)
