       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACC-FILE ASSIGN TO ACCOUNTS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS-FILE ASSIGN TO TRANSIN
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UPDATED-FILE ASSIGN TO TRANSOUT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACC-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS ACC-RECORD.
       01 ACC-RECORD PIC X(80).

       FD TRANS-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS TRANS-RECORD.
       01 TRANS-RECORD PIC X(80).

       FD UPDATED-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS UPDATED-RECORD.
       01 UPDATED-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       77 WS-EOF-ACC   PIC X VALUE 'N'.
       77 WS-EOF-TRANS PIC X VALUE 'N'.
       77 WS-TOTAL-UPD PIC 9(5) VALUE 0.

       01 WS-ACC-FIELDS.
           05 WS-ACC-ID    PIC X(5).
           05 FILLER       PIC X VALUE ','.
           05 WS-ACC-NAME  PIC X(20).
           05 FILLER       PIC X VALUE ','.
           05 WS-ACC-TYPE  PIC X(8).
           05 FILLER       PIC X VALUE ','.
           05 WS-ACC-BAL   PIC 9(8)V99.

       01 WS-TRANS-FIELDS.
           05 WS-TXN-ID    PIC X(6).
           05 FILLER       PIC X VALUE ','.
           05 WS-TXN-TYPE  PIC X(10).
           05 FILLER       PIC X VALUE ','.
           05 WS-TXN-ACCID PIC X(5).
           05 FILLER       PIC X VALUE ','.
           05 WS-TXN-AMT   PIC 9(8)V99.
           05 FILLER       PIC X VALUE ','.
           05 WS-TXN-DATE  PIC X(8).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "ACCOUNT-UPDATER: Starting processing..."
           OPEN INPUT ACC-FILE
           OPEN INPUT TRANS-FILE
           OPEN OUTPUT UPDATED-FILE

           PERFORM UNTIL WS-EOF-ACC = 'Y'
               READ ACC-FILE
                   AT END MOVE 'Y' TO WS-EOF-ACC
                   NOT AT END
                       MOVE ACC-RECORD TO UPDATED-RECORD
                       WRITE UPDATED-RECORD
               END-READ
           END-PERFORM

           CLOSE ACC-FILE
           CLOSE TRANS-FILE
           CLOSE UPDATED-FILE

           DISPLAY "ACCOUNT-UPDATER: Processing completed"
           MOVE 0 TO RETURN-CODE
           STOP RUN.
