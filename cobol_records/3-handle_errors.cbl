       IDENTIFICATION DIVISION.
       PROGRAM-ID. HandleErrors.
       AUTHOR.     COBOL Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TRANS-ID.

       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANS-ID         PIC 9(5).
           05 CUSTOMER-NAME    PIC X(20).
           05 AMOUNT           PIC S9(5)V99 SIGN LEADING SEPARATE.

       WORKING-STORAGE SECTION.
       01 WS-EOF-FLAG          PIC X VALUE 'N'.
           88 WS-EOF           VALUE 'Y'.
           88 WS-NOT-EOF       VALUE 'N'.
       01 VALID-RECORD-FLAG    PIC X VALUE 'Y'.
           88 VALID-RECORD     VALUE 'Y'.
           88 INVALID-RECORD   VALUE 'N'.
       01 VALID-COUNT          PIC 9(3) VALUE 0.
       01 INVALID-COUNT        PIC 9(3) VALUE 0.
       01 TOTAL-COUNT          PIC 9(3) VALUE 0.
       01 DISPLAY-AMOUNT       PIC +ZZZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Reading transactions...".
           OPEN INPUT TRANSACTION-FILE.
           PERFORM UNTIL WS-EOF
               READ TRANSACTION-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO TOTAL-COUNT
                       PERFORM VALIDATE-RECORD
                       IF VALID-RECORD
                           ADD 1 TO VALID-COUNT
                           MOVE AMOUNT TO DISPLAY-AMOUNT
                           DISPLAY "Transaction ID: " TRANS-ID 
                                   " | Customer: " FUNCTION 
                                   TRIM(CUSTOMER-NAME)
                                   " | Amount: " DISPLAY-AMOUNT
                       ELSE
                           ADD 1 TO INVALID-COUNT
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE TRANSACTION-FILE.
           DISPLAY "Done. Processed " TOTAL-COUNT " records, " 
                   INVALID-COUNT " invalid.".
           STOP RUN.

       VALIDATE-RECORD.
           SET VALID-RECORD TO TRUE.

           *> Validate Transaction ID
           IF TRANS-ID = ZERO
               DISPLAY "Invalid record: Invalid Transaction ID. Skipping
      -         "..."
               SET INVALID-RECORD TO TRUE
           END-IF.

           *> Validate Customer Name
           IF VALID-RECORD AND CUSTOMER-NAME = SPACES
               DISPLAY "Invalid record: Invalid customer name. Skipping.
      -         ".."
               SET INVALID-RECORD TO TRUE
           END-IF.

           *> Validate Amount
           IF VALID-RECORD 
               IF AMOUNT NOT NUMERIC
                   DISPLAY "Invalid record: Invalid amount. Skipping..."
                   SET INVALID-RECORD TO TRUE
               ELSE IF AMOUNT <= 0
                   DISPLAY "Invalid record: Invalid amount. Skipping..."
                   SET INVALID-RECORD TO TRUE
               END-IF
           END-IF.
