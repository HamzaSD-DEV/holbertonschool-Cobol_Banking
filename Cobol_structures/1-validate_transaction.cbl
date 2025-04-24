       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE-TRANSACTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TRANSACTION-AMOUNT   PIC 9(5).

       PROCEDURE DIVISION.
           ACCEPT TRANSACTION-AMOUNT FROM COMMAND-LINE

           IF TRANSACTION-AMOUNT > 0 AND TRANSACTION-AMOUNT <= 10000
               DISPLAY "Transaction is valid."
           ELSE
               DISPLAY "Invalid transaction amount!"
           END-IF

           STOP RUN.
