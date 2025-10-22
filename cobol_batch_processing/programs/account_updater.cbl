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
       FD ACC-FILE.
       01 ACC-RECORD PIC X(80).

       FD TRANS-FILE.
       01 TRANS-RECORD PIC X(80).

       FD UPDATED-FILE.
       01 UPDATED-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       77 WS-EOF-ACC     PIC X VALUE 'N'.
       77 WS-EOF-TRANS   PIC X VALUE 'N'.
       77 WS-ACC-COUNT   PIC 9(3) VALUE 0.
       77 WS-TRANS-COUNT PIC 9(3) VALUE 0.
       77 WS-UPD-COUNT   PIC 9(3) VALUE 0.
       77 WS-I           PIC 9(3).
       77 WS-J           PIC 9(3).

       01 WS-ACCOUNTS-TABLE.
          05 WS-ACCOUNT-ENTRY OCCURS 100.
             10 WS-ACC-ID     PIC X(5).
             10 WS-ACC-NAME   PIC X(20).
             10 WS-ACC-TYPE   PIC X(8).
             10 WS-ACC-BAL    PIC 9(6)V99.

       01 WS-ACC-DATA.
          05 WS-ACC-ID-F     PIC X(5).
          05 FILLER          PIC X.
          05 WS-ACC-NAME-F   PIC X(20).
          05 FILLER          PIC X.
          05 WS-ACC-TYPE-F   PIC X(8).
          05 FILLER          PIC X.
          05 WS-ACC-BAL-F    PIC 9(6)V99.

       01 WS-TRANS-DATA.
          05 WS-TXN-ID-F     PIC X(6).
          05 FILLER          PIC X.
          05 WS-TXN-TYPE-F   PIC X(10).
          05 FILLER          PIC X.
          05 WS-TXN-ACCID-F  PIC X(5).
          05 FILLER          PIC X.
          05 WS-TXN-AMT-F    PIC 9(6)V99.
          05 FILLER          PIC X.
          05 WS-TXN-DATE-F   PIC X(8).

       01 WS-OUTPUT-RECORD.
          05 WS-OUT-ACC-ID   PIC X(5).
          05 FILLER          PIC X VALUE ','.
          05 WS-OUT-NAME     PIC X(20).
          05 FILLER          PIC X VALUE ','.
          05 WS-OUT-TYPE     PIC X(8).
          05 FILLER          PIC X VALUE ','.
          05 WS-OUT-BAL      PIC Z(5)9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "ACCOUNT-UPDATER: Starting processing..."
           
           PERFORM INITIALIZE-PROGRAM
           PERFORM LOAD-ACCOUNTS
           PERFORM PROCESS-TRANSACTIONS
           PERFORM WRITE-UPDATED-ACCOUNTS
           PERFORM DISPLAY-STATISTICS
           
           DISPLAY "ACCOUNT-UPDATER: Processing completed"
           MOVE 0 TO RETURN-CODE
           STOP RUN.

       INITIALIZE-PROGRAM.
           OPEN INPUT ACC-FILE
           OPEN INPUT TRANS-FILE
           OPEN OUTPUT UPDATED-FILE.

       LOAD-ACCOUNTS.
           MOVE 0 TO WS-ACC-COUNT
           MOVE 'N' TO WS-EOF-ACC
           
           PERFORM UNTIL WS-EOF-ACC = 'Y'
               READ ACC-FILE INTO ACC-RECORD
                   AT END 
                      MOVE 'Y' TO WS-EOF-ACC
                   NOT AT END
                      ADD 1 TO WS-ACC-COUNT
                      MOVE ACC-RECORD TO WS-ACC-DATA
                      MOVE WS-ACC-ID-F TO WS-ACC-ID(WS-ACC-COUNT)
                      MOVE WS-ACC-NAME-F TO WS-ACC-NAME(WS-ACC-COUNT)
                      MOVE WS-ACC-TYPE-F TO WS-ACC-TYPE(WS-ACC-COUNT)
                      MOVE WS-ACC-BAL-F TO WS-ACC-BAL(WS-ACC-COUNT)
               END-READ
           END-PERFORM
           
           CLOSE ACC-FILE
           DISPLAY "ACCOUNT-UPDATER: Loaded " WS-ACC-COUNT 
                   " accounts".

       PROCESS-TRANSACTIONS.
           MOVE 'N' TO WS-EOF-TRANS
           MOVE 0 TO WS-TRANS-COUNT
           MOVE 0 TO WS-UPD-COUNT
           
           PERFORM UNTIL WS-EOF-TRANS = 'Y'
               READ TRANS-FILE INTO TRANS-RECORD
                   AT END 
                      MOVE 'Y' TO WS-EOF-TRANS
                   NOT AT END
                      ADD 1 TO WS-TRANS-COUNT
                      MOVE TRANS-RECORD TO WS-TRANS-DATA
                      PERFORM PROCESS-SINGLE-TRANSACTION
               END-READ
           END-PERFORM
           
           CLOSE TRANS-FILE.

       PROCESS-SINGLE-TRANSACTION.
           PERFORM VARYING WS-I FROM 1 BY 1 
             UNTIL WS-I > WS-ACC-COUNT
               IF WS-TXN-ACCID-F = WS-ACC-ID(WS-I)
                   EVALUATE WS-TXN-TYPE-F
                       WHEN 'DEPOSIT'
                       WHEN 'DEPOSIT   '
                           COMPUTE WS-ACC-BAL(WS-I) = 
                               WS-ACC-BAL(WS-I) + WS-TXN-AMT-F
                           ADD 1 TO WS-UPD-COUNT
                           DISPLAY "ACCOUNT-UPDATER: Deposit " 
                                   WS-TXN-AMT-F " to account " 
                                   WS-TXN-ACCID-F
                       WHEN 'WITHDRAWAL'
                       WHEN 'WITHDRAWAL '
                           COMPUTE WS-ACC-BAL(WS-I) = 
                               WS-ACC-BAL(WS-I) - WS-TXN-AMT-F
                           ADD 1 TO WS-UPD-COUNT
                           DISPLAY "ACCOUNT-UPDATER: Withdrawal " 
                                   WS-TXN-AMT-F " from account " 
                                   WS-TXN-ACCID-F
                       WHEN 'TRANSFER'
                       WHEN 'TRANSFER  '
                           COMPUTE WS-ACC-BAL(WS-I) = 
                               WS-ACC-BAL(WS-I) - WS-TXN-AMT-F
                           ADD 1 TO WS-UPD-COUNT
                           DISPLAY "ACCOUNT-UPDATER: Transfer " 
                                   WS-TXN-AMT-F " from account " 
                                   WS-TXN-ACCID-F
                       WHEN OTHER
                           DISPLAY "ACCOUNT-UPDATER: ERROR - " 
                                   "Invalid transaction type: " 
                                   WS-TXN-TYPE-F
                   END-EVALUATE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       WRITE-UPDATED-ACCOUNTS.
           PERFORM VARYING WS-I FROM 1 BY 1 
             UNTIL WS-I > WS-ACC-COUNT
               MOVE WS-ACC-ID(WS-I) TO WS-OUT-ACC-ID
               MOVE WS-ACC-NAME(WS-I) TO WS-OUT-NAME
               MOVE WS-ACC-TYPE(WS-I) TO WS-OUT-TYPE
               MOVE WS-ACC-BAL(WS-I) TO WS-OUT-BAL
               
               MOVE SPACES TO UPDATED-RECORD
               STRING
                   WS-OUT-ACC-ID ',' 
                   WS-OUT-NAME ',' 
                   WS-OUT-TYPE ',' 
                   WS-OUT-BAL 
                   DELIMITED BY SIZE
                   INTO UPDATED-RECORD
               WRITE UPDATED-RECORD
           END-PERFORM
           
           CLOSE UPDATED-FILE.

       DISPLAY-STATISTICS.
           DISPLAY "ACCOUNT-UPDATER: =========================="
           DISPLAY "ACCOUNT-UPDATER: Processing Statistics:"
           DISPLAY "ACCOUNT-UPDATER: Total accounts: " WS-ACC-COUNT
           DISPLAY "ACCOUNT-UPDATER: Total transactions: " 
                   WS-TRANS-COUNT
           DISPLAY "ACCOUNT-UPDATER: Successful updates: " 
                   WS-UPD-COUNT
           DISPLAY "ACCOUNT-UPDATER: ==========================".