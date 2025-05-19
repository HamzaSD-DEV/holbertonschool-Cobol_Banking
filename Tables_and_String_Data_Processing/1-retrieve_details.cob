       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETRIEVE-DETAILS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  IDX                      PIC 9(1).

       LINKAGE SECTION.
       01  LK-CUSTOMER-TABLE.
           05  LK-CUSTOMER-DATA OCCURS 5 TIMES.
               10  LK-CUST-ID        PIC 9(5).
               10  LK-CUST-FNAME     PIC X(10).
               10  LK-CUST-LNAME     PIC X(10).
               10  LK-CUST-EMAIL     PIC X(25).
               10  LK-CUST-BAL       PIC 9(5)V99.

       01  LK-CHOICE                PIC 9.

       PROCEDURE DIVISION USING LK-CUSTOMER-TABLE, LK-CHOICE.

       DISPLAY-CUSTOMER-DETAILS.
           MOVE LK-CHOICE TO IDX

           DISPLAY " "
           DISPLAY "CUSTOMER DETAILS"
           DISPLAY "----------------"
           DISPLAY "Customer ID   : " LK-CUST-ID(IDX)
           DISPLAY "First Name    : " LK-CUST-FNAME(IDX)
           DISPLAY "Last Name     : " LK-CUST-LNAME(IDX)
           DISPLAY "Email         : " LK-CUST-EMAIL(IDX)
           DISPLAY "Balance       : " LK-CUST-BAL(IDX) "$"

           EXIT PROGRAM.
