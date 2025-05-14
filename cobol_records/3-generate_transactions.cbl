       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERATE-TRANSACTIONS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TRANS-ID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANS-ID         PIC 9(5).
           05 CUSTOMER-NAME    PIC X(20).
           05 AMOUNT           PIC S9(5)V99 SIGN LEADING SEPARATE.

       WORKING-STORAGE SECTION.
       77  WS-FS      PIC XX.

       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT TRANSACTION-FILE

           *> Record 1
           MOVE "00010" TO TRANS-ID
           MOVE "John Doe           " TO CUSTOMER-NAME
           MOVE "-1050.00" TO AMOUNT
           WRITE TRANSACTION-RECORD

           *> Record 2
           MOVE "00020" TO TRANS-ID
           MOVE "                    " TO CUSTOMER-NAME
           MOVE "+0100.50" TO AMOUNT
           WRITE TRANSACTION-RECORD

           *> Record 3
           MOVE "00030" TO TRANS-ID
           MOVE "Alice Johnson      " TO CUSTOMER-NAME
           MOVE "+0000.01" TO AMOUNT
           WRITE TRANSACTION-RECORD

           *> Record 4
           MOVE "00040" TO TRANS-ID
           MOVE "Robert Smith       " TO CUSTOMER-NAME
           MOVE "-0750.75" TO AMOUNT
           WRITE TRANSACTION-RECORD

           *> Record 5
           MOVE "00050" TO TRANS-ID
           MOVE "kane                " TO CUSTOMER-NAME
           MOVE "+1020.00" TO AMOUNT
           WRITE TRANSACTION-RECORD

           
           *> Record 6
           MOVE "01000" TO TRANS-ID
           MOVE "Lane                " TO CUSTOMER-NAME
           MOVE "+0012.00" TO AMOUNT
           WRITE TRANSACTION-RECORD
                      
           *> Record 6
           MOVE "01010" TO TRANS-ID
           MOVE "Line                " TO CUSTOMER-NAME
           MOVE "+0000.00" TO AMOUNT
           WRITE TRANSACTION-RECORD

           CLOSE TRANSACTION-FILE
           DISPLAY "transactions.dat (indexed) with 5 records created."
           STOP RUN.
