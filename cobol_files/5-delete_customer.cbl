       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETE-CUSTOMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUST-ID         PIC 9(5).
           05 CUST-FNAME      PIC X(10).
           05 CUST-LNAME      PIC X(10).
           05 CUST-BALANCE    PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS       PIC XX.
       01 WS-ACCOUNT-ID   PIC X(5).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Enter account number to delete: "
           ACCEPT WS-ACCOUNT-ID

           MOVE WS-ACCOUNT-ID TO CUST-ID

           OPEN I-O CUSTOMER-FILE

           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY "Account not found."
                   CLOSE CUSTOMER-FILE
                   STOP RUN
           END-READ

           DELETE CUSTOMER-FILE RECORD
           DISPLAY "Account " WS-ACCOUNT-ID 
           " has been successfully deleted."

           CLOSE CUSTOMER-FILE
           STOP RUN.
