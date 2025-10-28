       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO "TRANSIN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TRANS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE
           RECORDING MODE F
           DATA RECORD IS TRANS-RECORD.
       01  TRANS-RECORD           PIC X(80).

       WORKING-STORAGE SECTION.
       01  FS-TRANS               PIC XX.
       01  WS-EOF                 PIC X VALUE 'N'.
       01  WS-TOTAL               PIC 9(5) VALUE 0.
       01  WS-VALID               PIC 9(5) VALUE 0.
       01  WS-INVALID             PIC 9(5) VALUE 0.

       01  WS-TX-FIELDS.
           05  WS-TXN-ID          PIC X(6).
           05  FILLER             PIC X VALUE ','.
           05  WS-TXN-TYPE        PIC X(10).
           05  FILLER             PIC X VALUE ','.
           05  WS-ACC-ID          PIC X(5).
           05  FILLER             PIC X VALUE ','.
           05  WS-AMOUNT          PIC X(8).
           05  FILLER             PIC X VALUE ','.
           05  WS-DATE            PIC X(8).

       01  DISPLAY-COUNTERS.
           05  DISP-TOTAL         PIC 9(5).
           05  DISP-VALID         PIC 9(5).
           05  DISP-INVALID       PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "BATCH-VALIDATOR: Starting transaction validation..."
           
           OPEN INPUT TRANS-FILE
           IF FS-TRANS NOT = "00"
               DISPLAY "ERROR: Cannot open input file. Status: " 
                       FS-TRANS
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRANS-FILE
                   AT END 
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-TOTAL
                       UNSTRING TRANS-RECORD 
                           DELIMITED BY ',' INTO
                           WS-TXN-ID
                           WS-TXN-TYPE
                           WS-ACC-ID
                           WS-AMOUNT
                           WS-DATE
                       END-UNSTRING
                       PERFORM VALIDATE-TXN
               END-READ
           END-PERFORM
           
           CLOSE TRANS-FILE
           
           MOVE WS-TOTAL TO DISP-TOTAL
           MOVE WS-VALID TO DISP-VALID
           MOVE WS-INVALID TO DISP-INVALID
           
           DISPLAY "BATCH-VALIDATOR: Validation completed"
           DISPLAY "BATCH-VALIDATOR: Total transactions: " 
                   DISP-TOTAL
           DISPLAY "BATCH-VALIDATOR: Valid transactions: " 
                   DISP-VALID
           DISPLAY "BATCH-VALIDATOR: Invalid transactions: " 
                   DISP-INVALID
           
           IF WS-INVALID > 0
               DISPLAY "BATCH-VALIDATOR: Invalid transactions found!"
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           
           STOP RUN.

       VALIDATE-TXN.
           EVALUATE WS-TXN-TYPE
               WHEN "DEPOSIT   "
               WHEN "WITHDRAWAL"
               WHEN "TRANSFER  "
                   ADD 1 TO WS-VALID
                   DISPLAY "✓ VALID: " TRANS-RECORD
               WHEN OTHER
                   ADD 1 TO WS-INVALID
                   DISPLAY "✗ INVALID: " TRANS-RECORD
           END-EVALUATE.