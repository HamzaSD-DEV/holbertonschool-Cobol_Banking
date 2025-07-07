       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL-WITHDRAWAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS ACC-STATUS.

           SELECT AUTH-FILE ASSIGN TO "AUTHORIZED_USERS.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS AUTH-STATUS.

           SELECT LOG-FILE ASSIGN TO "WITHDRAWAL_ERRORS.LOG"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD ACCOUNTS-FILE.
       01 ACCOUNT-REC.
           05 ACC-ID         PIC X(6).
           05 ACC-NAME       PIC X(20).
           05 ACC-BAL-TX     PIC X(8).

       FD AUTH-FILE.
       01 AUTH-REC.
           05 AUTH-ID        PIC X(6).

       FD LOG-FILE.
       01 LOG-REC           PIC X(200).

       WORKING-STORAGE SECTION.
       01 ACC-STATUS        PIC XX.
       01 AUTH-STATUS       PIC XX.
       01 LOG-STATUS        PIC XX.

       01 WS-INPUT-ID       PIC X(6).
       01 WS-AMOUNT-TX      PIC X(10).
       01 WS-AMOUNT-NUM     PIC 9(5)V99.
       01 WS-BAL-NUM        PIC 9(5)V99.
       01 WS-NEW-BAL-NUM    PIC 9(5)V99.
       01 WS-NEW-BAL-TX     PIC 9(5).99.
       01 WS-NEW-BAL-DISP   PIC Z(5)9.99.

       01 WS-FOUND-FLAG     PIC X VALUE 'N'.
       01 WS-AUTH-FLAG      PIC X VALUE 'N'.
       01 WS-END-FLAG       PIC X VALUE 'N'.

       01 WS-DATE       PIC 9(8) VALUE ZEROS.
       01 WS-TIME       PIC 9(8) VALUE ZEROS.
       01 WS-TIMESTAMP  PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Account ID: "
           ACCEPT WS-INPUT-ID

           DISPLAY "Enter Withdrawal Amount: "
           ACCEPT WS-AMOUNT-TX

           *> Validate amount
           IF FUNCTION NUMVAL(WS-AMOUNT-TX) = 0
               PERFORM LOG-INVALID-AMOUNT
               DISPLAY "Error: Invalid amount."
               GO TO END-PARA
           END-IF
           COMPUTE WS-AMOUNT-NUM = FUNCTION NUMVAL(WS-AMOUNT-TX)

           *> Check if authorized
           OPEN INPUT AUTH-FILE
           PERFORM UNTIL AUTH-STATUS = "10"
               READ AUTH-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF AUTH-ID = WS-INPUT-ID
                           MOVE "Y" TO WS-AUTH-FLAG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AUTH-FILE

           IF WS-AUTH-FLAG NOT = "Y"
               PERFORM LOG-UNAUTH
               DISPLAY "Error: Unauthorized access."
               GO TO END-PARA
           END-IF

           *> Search in accounts
           OPEN I-O ACCOUNTS-FILE
           PERFORM UNTIL WS-END-FLAG = 'Y' OR WS-FOUND-FLAG = 'Y'
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 'Y' TO WS-END-FLAG
                   NOT AT END
                       IF ACC-ID = WS-INPUT-ID
                           MOVE 'Y' TO WS-FOUND-FLAG
                           COMPUTE WS-BAL-NUM = FUNCTION 
                           NUMVAL(ACC-BAL-TX)
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND-FLAG = 'Y'
               IF WS-AMOUNT-NUM > WS-BAL-NUM
                   PERFORM LOG-INSUFFICIENT
                   DISPLAY "Error: Insufficient funds."
               ELSE
                   COMPUTE WS-NEW-BAL-NUM = WS-BAL-NUM - WS-AMOUNT-NUM
                       ON SIZE ERROR
                           PERFORM LOG-OVERFLOW
                           DISPLAY "Error: Overflow during withdrawal."
                       NOT ON SIZE ERROR
                           MOVE WS-NEW-BAL-NUM TO WS-NEW-BAL-TX
                           MOVE WS-NEW-BAL-NUM TO WS-NEW-BAL-DISP
                           MOVE WS-NEW-BAL-TX TO ACC-BAL-TX
                           REWRITE ACCOUNT-REC
                           DISPLAY "New balance for " FUNCTION 
                           TRIM(ACC-NAME)
                                   ": " FUNCTION TRIM(WS-NEW-BAL-DISP)
                   END-COMPUTE
               END-IF
           ELSE
               PERFORM LOG-NOT-FOUND
               DISPLAY "Error: Account ID not found."
           END-IF

           CLOSE ACCOUNTS-FILE.
       END-PARA.
           STOP RUN.

       *> LOGGING PARAGRAPHS USING CLEAN FORMAT

       LOG-INVALID-AMOUNT.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-REC
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Invalid amount for Account ID "
               WS-INPUT-ID DELIMITED BY SIZE
               INTO LOG-REC
           END-STRING
           PERFORM WRITE-LOG.

       LOG-UNAUTH.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-REC
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Unauthorized access attempt for Account ID "
               WS-INPUT-ID DELIMITED BY SIZE
               INTO LOG-REC
           END-STRING
           PERFORM WRITE-LOG.

       LOG-NOT-FOUND.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-REC
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Account ID not found: "
               WS-INPUT-ID DELIMITED BY SIZE
               INTO LOG-REC
           END-STRING
           PERFORM WRITE-LOG.

       LOG-INSUFFICIENT.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-REC
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Insufficient funds for Account ID "
               WS-INPUT-ID DELIMITED BY SIZE
               INTO LOG-REC
           END-STRING
           PERFORM WRITE-LOG.

       LOG-OVERFLOW.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-REC
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Overflow error for Account ID "
               WS-INPUT-ID DELIMITED BY SIZE
               INTO LOG-REC
           END-STRING
           PERFORM WRITE-LOG.

       WRITE-LOG.
           OPEN EXTEND LOG-FILE
           IF LOG-STATUS = "35"
               CLOSE LOG-FILE
               OPEN OUTPUT LOG-FILE
               CLOSE LOG-FILE
               OPEN EXTEND LOG-FILE
           END-IF
           WRITE LOG-REC AFTER ADVANCING 1 LINES
           CLOSE LOG-FILE.

       GET-TIMESTAMP.
           ACCEPT WS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           STRING
               WS-DATE(1:4) "-" WS-DATE(5:2) "-" WS-DATE(7:2) " "
               WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
               DELIMITED BY SIZE
               INTO WS-TIMESTAMP
           END-STRING.
