       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-CUSTOMER-FILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUST-ID        PIC 9(5).
           05 CUST-FNAME     PIC A(10).
           05 CUST-LNAME     PIC A(10).
           05 CUST-BALANCE   PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS     PIC XX.
       01 IDX                PIC 9 VALUE 1.

       01 CUSTOMER-TABLE.
           05 CUSTOMER-ENTRY OCCURS 7 TIMES INDEXED BY T-INDEX.
               10 CUST-TABLE-ID    PIC 9(5).
               10 CUST-TABLE-FNAME PIC A(10).
               10 CUST-TABLE-LNAME PIC A(10).
               10 CUST-TABLE-BAL   PIC 9(5)V99.

       PROCEDURE DIVISION.
       BEGIN.
           MOVE 00001 TO CUST-TABLE-ID (1)
           MOVE "John     " TO CUST-TABLE-FNAME (1)
           MOVE "Smith    " TO CUST-TABLE-LNAME (1)
           MOVE 123.45 TO CUST-TABLE-BAL (1)

           MOVE 00002 TO CUST-TABLE-ID (2)
           MOVE "Alice    " TO CUST-TABLE-FNAME (2)
           MOVE "Brown    " TO CUST-TABLE-LNAME (2)
           MOVE 5432 TO CUST-TABLE-BAL (2)

           MOVE 00003 TO CUST-TABLE-ID (3)
           MOVE "Bob      " TO CUST-TABLE-FNAME (3)
           MOVE "White    " TO CUST-TABLE-LNAME (3)
           MOVE 0 TO CUST-TABLE-BAL (3)

           MOVE 00004 TO CUST-TABLE-ID (4)
           MOVE "David    " TO CUST-TABLE-FNAME (4)
           MOVE "Green    " TO CUST-TABLE-LNAME (4)
           MOVE 4500.25 TO CUST-TABLE-BAL (4)

           MOVE 00005 TO CUST-TABLE-ID (5)
           MOVE "Maria    " TO CUST-TABLE-FNAME (5)
           MOVE "Lopez    " TO CUST-TABLE-LNAME (5)
           MOVE 320.00 TO CUST-TABLE-BAL (5)

           MOVE 00006 TO CUST-TABLE-ID (6)
           MOVE "James    " TO CUST-TABLE-FNAME (6)
           MOVE "Bond     " TO CUST-TABLE-LNAME (6)
           MOVE 700.07 TO CUST-TABLE-BAL (6)

           MOVE 00007 TO CUST-TABLE-ID (7)
           MOVE "Lily     " TO CUST-TABLE-FNAME (7)
           MOVE "Adams    " TO CUST-TABLE-LNAME (7)
           MOVE 1000.1 TO CUST-TABLE-BAL (7)

      *    MOVE 10101 TO CUST-TABLE-ID (7)
      *    MOVE "Hamza    " TO CUST-TABLE-FNAME (7)
      *    MOVE "Saoud    " TO CUST-TABLE-LNAME (7)
      *    MOVE 111.1 TO CUST-TABLE-BAL (7)

           OPEN OUTPUT CUSTOMER-FILE

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 7
               MOVE CUST-TABLE-ID (IDX)      TO CUST-ID
               MOVE CUST-TABLE-FNAME (IDX)   TO CUST-FNAME
               MOVE CUST-TABLE-LNAME (IDX)   TO CUST-LNAME
               MOVE CUST-TABLE-BAL (IDX)     TO CUST-BALANCE
               WRITE CUSTOMER-RECORD
           END-PERFORM

           CLOSE CUSTOMER-FILE

           DISPLAY "CUSTOMERS.DAT created successfully!"
           STOP RUN.
