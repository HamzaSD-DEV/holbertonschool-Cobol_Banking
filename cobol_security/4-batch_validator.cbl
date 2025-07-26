       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHVALIDATOR.
       AUTHOR. COBOL PROGRAMMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO "TRANSACTIONS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE.
       01 ACCOUNT-RECORD.
           05 ACC-ID     PIC X(10).
           05 ACC-STAT   PIC X(6).
           05 ACC-BALTXT PIC X(12).
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TR-TYPE        PIC X.
           05 TR-SOURCE      PIC X(10).
           05 TR-DEST        PIC X(10).
           05 TR-AMOUNT-STR  PIC X(12).
           05 TR-DESCRIPTION PIC X(30).


       WORKING-STORAGE SECTION.
       77 ACCT-EOF        PIC X VALUE 'N'.
       77 TRANS-EOF       PIC X VALUE 'N'.
       77 WS-TRANS-COUNT  PIC 9(3) VALUE 0.
       77 WS-APPROVED     PIC 9(3) VALUE 0.
       77 WS-REJECTED     PIC 9(3) VALUE 0.
       77 WS-AMT          PIC 9(9)V99.
       77 REASON          PIC X(40).
       77 WS-I            PIC 9(2).

       01 WS-SECURITY-FLAG PIC 9.
          88 SEC-FAIL VALUE 1.
          88 SEC-OK   VALUE 0.

       01 ACCOUNT-TABLE.
           05 ACC-ENTRY OCCURS 100 TIMES.
               10 ACC-T-ID    PIC X(10).
               10 ACC-T-STAT  PIC X(6).
               10 ACC-T-BAL   PIC 9(9)V99.

       77 ACC-COUNT       PIC 9(3) VALUE 0.
       77 SRC-IDX         PIC 9(3).
       77 DEST-IDX        PIC 9(3).

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "SECURE BATCH TRANSACTION VALIDATOR"
           PERFORM LOAD-ACCOUNTS
           PERFORM PROCESS-TXNS
           PERFORM SHOW-SUMMARY
           STOP RUN.

       LOAD-ACCOUNTS.
           OPEN INPUT ACCOUNT-FILE
           PERFORM UNTIL ACCT-EOF = 'Y'
               READ ACCOUNT-FILE
                   AT END MOVE 'Y' TO ACCT-EOF
                   NOT AT END
                       ADD 1 TO ACC-COUNT
                       MOVE ACC-ID TO ACC-T-ID(ACC-COUNT)
                       MOVE ACC-STAT TO ACC-T-STAT(ACC-COUNT)
                       MOVE FUNCTION NUMVAL(ACC-BALTXT)
                            TO ACC-T-BAL(ACC-COUNT)
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           DISPLAY ACC-COUNT " accounts loaded".

       PROCESS-TXNS.
           OPEN INPUT TRANSACTION-FILE
           PERFORM UNTIL TRANS-EOF = 'Y'
               READ TRANSACTION-FILE
                   AT END MOVE 'Y' TO TRANS-EOF
                   NOT AT END
                       IF TRANSACTION-RECORD NOT = SPACES
                           ADD 1 TO WS-TRANS-COUNT
                           PERFORM HANDLE-TRANSACTION
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRANSACTION-FILE.

       HANDLE-TRANSACTION.
           MOVE SPACES TO REASON
           MOVE FUNCTION NUMVAL(TR-AMOUNT-STR) TO WS-AMT
           MOVE 0 TO WS-SECURITY-FLAG

           *> Security check
           PERFORM CHECK-SECURITY

           *> Find account indexes
           PERFORM FIND-SOURCE
           PERFORM FIND-DEST

           DISPLAY " "
           DISPLAY "Txn " WS-TRANS-COUNT ":"

           IF SEC-FAIL
               MOVE "Suspicious description" TO REASON
           ELSE
               EVALUATE TR-TYPE
                   WHEN 'T'
                       IF SRC-IDX = 0
                           MOVE "Source account not found" TO REASON
                       ELSE IF ACC-T-STAT(SRC-IDX) NOT = "ACTIVE"
                           MOVE "Source account locked" TO REASON
                       ELSE IF ACC-T-BAL(SRC-IDX) < WS-AMT
                           MOVE "Insufficient funds" TO REASON
                       ELSE IF DEST-IDX = 0
                           MOVE "Destination not found" TO REASON
                       ELSE
                           SUBTRACT WS-AMT FROM ACC-T-BAL(SRC-IDX)
                           ADD WS-AMT TO ACC-T-BAL(DEST-IDX)
                   WHEN 'W'
                       IF SRC-IDX = 0
                           MOVE "Source account not found" TO REASON
                       ELSE IF ACC-T-STAT(SRC-IDX) NOT = "ACTIVE"
                           MOVE "Source account locked" TO REASON
                       ELSE IF ACC-T-BAL(SRC-IDX) < WS-AMT
                           MOVE "Insufficient funds" TO REASON
                       ELSE
                           SUBTRACT WS-AMT FROM ACC-T-BAL(SRC-IDX)
                   WHEN 'D'
                       IF DEST-IDX = 0
                           MOVE "Destination not found" TO REASON
                       ELSE IF ACC-T-STAT(DEST-IDX) NOT = "ACTIVE"
                           MOVE "Destination account locked" TO REASON
                       ELSE
                           ADD WS-AMT TO ACC-T-BAL(DEST-IDX)
                   WHEN OTHER
                       MOVE "Unknown transaction type" TO REASON
               END-EVALUATE
           END-IF

           IF REASON = SPACES
               DISPLAY "STATUS: APPROVED"
               ADD 1 TO WS-APPROVED
           ELSE
               DISPLAY "REJECTED - " REASON
               ADD 1 TO WS-REJECTED
           END-IF.

       CHECK-SECURITY.
           MOVE 0 TO WS-SECURITY-FLAG
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 30
               IF TR-DESCRIPTION(WS-I:1) = "'" OR
                  TR-DESCRIPTION(WS-I:1) = ";"
                   MOVE 1 TO WS-SECURITY-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-SECURITY-FLAG = 0
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 29
                   IF TR-DESCRIPTION(WS-I:2) = "--"
                       MOVE 1 TO WS-SECURITY-FLAG
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF.

       FIND-SOURCE.
           MOVE 0 TO SRC-IDX
           PERFORM VARYING SRC-IDX FROM 1 BY 1 UNTIL SRC-IDX > ACC-COUNT
               IF ACC-T-ID(SRC-IDX) = TR-SOURCE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF SRC-IDX > ACC-COUNT MOVE 0 TO SRC-IDX.

       FIND-DEST.
           MOVE 0 TO DEST-IDX
           PERFORM VARYING DEST-IDX FROM 1 BY 1 UNTIL DEST-IDX > 
           ACC-COUNT
               IF ACC-T-ID(DEST-IDX) = TR-DEST
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF DEST-IDX > ACC-COUNT MOVE 0 TO DEST-IDX.

       SHOW-SUMMARY.
           DISPLAY " "
           DISPLAY "SUMMARY: Processed:" WS-TRANS-COUNT
                   "  Approved:" WS-APPROVED
                   "  Rejected:" WS-REJECTED.
