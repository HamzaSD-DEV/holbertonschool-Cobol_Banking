       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMAT-MASTER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 MAX-CUSTOMERS         PIC 9 VALUE 3.
       01 I                     PIC 9 VALUE 1.

       01 HEADERS               PIC X(65)
          VALUE "| CUSTOMER ID | CUSTOMER NAME |   BALANCE   |  RATE  | 
      -    " STATUS  |".

       01 DIVIDER               PIC X(65)
          VALUE "|-------------+---------------+-------------+--------+-
      -    "---------|".

       01 CUSTOMER-DATA.
          05 CUSTOMER-INFO OCCURS 3 TIMES INDEXED BY IDX.
             10 CUST-ID     PIC X(10).
             10 CUST-NAME   PIC X(13).
             10 CUST-BAL    PIC 9(6)V99.
             10 CUST-RATE   PIC 9(2)V99.
             10 CUST-STATUS PIC X(9).

       01 DISPLAY-BAL     PIC ZZZ,ZZ9.99.
       01 DISPLAY-RATE    PIC Z9.99.

       01 OUT-ID          PIC X(10).
       01 OUT-NAME        PIC X(13).
       01 OUT-BAL         PIC ZZZ,ZZ9.99.
       01 OUT-RATE        PIC Z9.99.
       01 OUT-STATUS      PIC X(9).

       PROCEDURE DIVISION.
       BEGIN.
           PERFORM READ-CUSTOMERS VARYING I FROM 1 BY 1 UNTIL I > 
           MAX-CUSTOMERS
           DISPLAY HEADERS
           DISPLAY DIVIDER
           PERFORM PRINT-CUSTOMERS VARYING I FROM 1 BY 1 UNTIL I > 
           MAX-CUSTOMERS
           STOP RUN.

       READ-CUSTOMERS.
           DISPLAY "Enter Customer #" I
           DISPLAY "Customer ID: "
           ACCEPT CUST-ID (I)
           DISPLAY "Customer Name: "
           ACCEPT CUST-NAME (I)
           DISPLAY "Customer Balance : "
           ACCEPT CUST-BAL (I)
           DISPLAY "Interest Rate : "
           ACCEPT CUST-RATE (I)
           DISPLAY "Status: "
           ACCEPT CUST-STATUS (I)
           DISPLAY SPACE.

       PRINT-CUSTOMERS.
           MOVE CUST-ID (I)     TO OUT-ID
           MOVE CUST-NAME (I)   TO OUT-NAME
           MOVE CUST-BAL (I)    TO DISPLAY-BAL
           MOVE DISPLAY-BAL     TO OUT-BAL
           MOVE CUST-RATE (I)   TO DISPLAY-RATE
           MOVE DISPLAY-RATE    TO OUT-RATE
           MOVE CUST-STATUS (I) TO OUT-STATUS

           DISPLAY "|  " OUT-ID " | " OUT-NAME " | " OUT-BAL
               "  | " OUT-RATE "%" " | " OUT-STATUS "|".
