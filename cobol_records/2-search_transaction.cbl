       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-TRANSACTION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TR-ID
               FILE STATUS IS FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TR-ID           PIC X(8).         *> TXN1025
           05  TR-DATE         PIC X(10).        *> YYYY-MM-DD
           05  TR-CUST-ID      PIC 9(5).
           05  TR-AMOUNT       PIC 9(7)V99.
           05  TR-STATUS       PIC X(10).

       WORKING-STORAGE SECTION.
       01  USER-INPUT-ID       PIC X(8).
       01  FS                  PIC XX.
       01  DISPLAY-AMOUNT      PIC 999.99.
       01  WS-DIVIDER          PIC X(26) VALUE ALL "-".

       01  PROMPT-MSG          PIC X(30) VALUE "Enter transaction ID: ".
       01  NOT-FOUND-MSG       PIC X(25) VALUE "Transaction not found.".

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY PROMPT-MSG WITH NO ADVANCING
           ACCEPT USER-INPUT-ID

           OPEN INPUT TRANSACTION-FILE

           MOVE USER-INPUT-ID TO TR-ID

           READ TRANSACTION-FILE
               KEY IS TR-ID
               INVALID KEY
                   DISPLAY NOT-FOUND-MSG
               NOT INVALID KEY
                   DISPLAY "Transaction Found:"
                   DISPLAY WS-DIVIDER
                   DISPLAY "Transaction ID   : " TR-ID
                   DISPLAY "Date             : " TR-DATE
                   DISPLAY "Customer ID      : " TR-CUST-ID
                   MOVE TR-AMOUNT TO DISPLAY-AMOUNT
                   DISPLAY "Amount           : " DISPLAY-AMOUNT
                   DISPLAY "Status           : " TR-STATUS
           END-READ

           CLOSE TRANSACTION-FILE
           STOP RUN.
