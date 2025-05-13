       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERATE-TRANSACTIONS-STATIC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TR-ID
               FILE STATUS IS FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TR-ID           PIC X(8).
           05  TR-DATE         PIC X(10).
           05  TR-CUST-ID      PIC 9(5).
           05  TR-AMOUNT       PIC 9(7)V99.
           05  TR-STATUS       PIC X(10).

       WORKING-STORAGE SECTION.
       77  FS                  PIC XX.

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN OUTPUT TRANSACTION-FILE

           *> Record 1
           MOVE "TXN1001" TO TR-ID
           MOVE "2025-04-10" TO TR-DATE
           MOVE 00001       TO TR-CUST-ID
           MOVE  001250.00  TO TR-AMOUNT
           MOVE "Completed" TO TR-STATUS
           WRITE TRANSACTION-RECORD

           *> Record 2
           MOVE "TXN1002" TO TR-ID
           MOVE "2025-04-11" TO TR-DATE
           MOVE 00002       TO TR-CUST-ID
           MOVE  000850.25  TO TR-AMOUNT
           MOVE "Pending"   TO TR-STATUS
           WRITE TRANSACTION-RECORD

           *> Record 3
           MOVE "TXN1003" TO TR-ID
           MOVE "2025-04-12" TO TR-DATE
           MOVE 00003       TO TR-CUST-ID
           MOVE  000625.50  TO TR-AMOUNT
           MOVE "Completed" TO TR-STATUS
           WRITE TRANSACTION-RECORD

           *> Record 4
           MOVE "TXN1025" TO TR-ID
           MOVE "2025-04-20" TO TR-DATE
           MOVE 00042       TO TR-CUST-ID
           MOVE  000350.75  TO TR-AMOUNT
           MOVE "Completed" TO TR-STATUS
           WRITE TRANSACTION-RECORD

           *> Record 5
           MOVE "TXN1044" TO TR-ID
           MOVE "2025-04-22" TO TR-DATE
           MOVE 00021       TO TR-CUST-ID
           MOVE  000100.00  TO TR-AMOUNT
           MOVE "Failed"    TO TR-STATUS
           WRITE TRANSACTION-RECORD

                      *> Record 6
           MOVE "TXN1916" TO TR-ID
           MOVE "2025-05-13" TO TR-DATE
           MOVE 10101       TO TR-CUST-ID
           MOVE  000101.10  TO TR-AMOUNT
           MOVE "Failed"    TO TR-STATUS
           WRITE TRANSACTION-RECORD

           CLOSE TRANSACTION-FILE
           DISPLAY "✅ transactions.idx with 6 records created."
           STOP RUN.
