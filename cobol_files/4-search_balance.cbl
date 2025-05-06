       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-CUSTOMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZZCUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ZZCUST-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ZZCUSTOMER-FILE.
       01 ZZCUSTOMER-RECORD.
           05 ZZCUST-ID          PIC 9(5).
           05 ZZCUST-FNAME       PIC X(10).
           05 ZZCUST-LNAME       PIC X(10).
           05 ZZCUST-BALANCE     PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS       PIC XX.
       01 ARG-ID               PIC X(5).
       
       PROCEDURE DIVISION.
       BEGIN.
           ACCEPT ARG-ID FROM ARGUMENT-VALUE

           OPEN INPUT ZZCUSTOMER-FILE

           MOVE ARG-ID TO ZZCUST-ID
           READ ZZCUSTOMER-FILE
               INVALID KEY
                   DISPLAY "Sorry, Account not found!"
               NOT INVALID KEY
                   DISPLAY "Balance: " ZZCUST-BALANCE "$"
           END-READ

           CLOSE ZZCUSTOMER-FILE
           STOP RUN.
