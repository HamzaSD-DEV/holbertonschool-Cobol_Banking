       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE-CUSTOMER.

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
       01 WS-STATUS              PIC XX.
       01 WS-ID-INPUT            PIC X(5).
       01 WS-AMOUNT-INPUT        PIC 9(5)V99.
       01 WS-OPERATION           PIC X.
       01  FULL-NAME           PIC X(21).


       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Enter Account ID:"
           ACCEPT WS-ID-INPUT

           MOVE WS-ID-INPUT TO CUST-ID

           OPEN I-O CUSTOMER-FILE

           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY "Sorry, Account not found!"
                   CLOSE CUSTOMER-FILE
                   STOP RUN
           END-READ
           STRING 
               FUNCTION TRIM(CUST-FNAME) DELIMITED BY SIZE
               " "
               FUNCTION TRIM(CUST-LNAME) DELIMITED BY SIZE
               INTO FULL-NAME
           END-STRING          
           DISPLAY "Account found!"
           DISPLAY "Customer: " FULL-NAME
           DISPLAY "Current Balance: $" CUST-BALANCE

           DISPLAY 
           "Enter operation type (D for deposit, W for withdraw):"
           ACCEPT WS-OPERATION

           IF WS-OPERATION NOT = "D" AND WS-OPERATION NOT = "W"
               DISPLAY "Invalid operation type. Use D or W."
               CLOSE CUSTOMER-FILE
               STOP RUN
           END-IF

           DISPLAY "Enter transaction amount:"
           ACCEPT WS-AMOUNT-INPUT

           IF WS-OPERATION = "D"
               ADD WS-AMOUNT-INPUT TO CUST-BALANCE
           ELSE
               IF WS-AMOUNT-INPUT > CUST-BALANCE
                   DISPLAY "Insufficient funds for withdrawal."
                   CLOSE CUSTOMER-FILE
                   STOP RUN
               END-IF
               SUBTRACT WS-AMOUNT-INPUT FROM CUST-BALANCE
           END-IF

           REWRITE CUSTOMER-RECORD
           DISPLAY "Balance updated successfully!"

           CLOSE CUSTOMER-FILE
           STOP RUN.
