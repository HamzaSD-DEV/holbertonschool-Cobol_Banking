       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATER.
       AUTHOR. STUDENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS ASSIGN TO "ACCOUNTS"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSIN ASSIGN TO "TRANSIN"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSOUT ASSIGN TO "TRANSOUT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS.
       01  ACCOUNT-RECORD          PIC X(80).
       
       FD  TRANSIN.
       01  TRANSACTION-RECORD      PIC X(80).
       
       FD  TRANSOUT.
       01  UPDATED-ACCOUNT-RECORD  PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-TRANS-COUNT          PIC 9(5) VALUE 0.
       01  WS-SUCCESS-COUNT        PIC 9(5) VALUE 0.
       01  WS-FAILED-COUNT         PIC 9(5) VALUE 0.
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
       01  WS-ACCT-EOF-FLAG        PIC X VALUE 'N'.
       01  WS-ACCOUNTS-TABLE.
          05  WS-ACCT-ENTRY        OCCURS 100 TIMES.
              10  WS-ACCT-ID       PIC X(5).
              10  WS-ACCT-NAME     PIC X(20).
              10  WS-ACCT-TYPE     PIC X(10).
              10  WS-ACCT-BALANCE  PIC 9(7)V99.
       01  WS-ACCT-COUNT           PIC 9(3) VALUE 0.
       01  WS-CURRENT-ACCT         PIC X(5).
       01  WS-FOUND-FLAG           PIC X VALUE 'N'.
       01  WS-ACCT-EXISTS-FLAG     PIC X VALUE 'N'.
       01  WS-I                    PIC 9(3).
       01  WS-TXN-ID               PIC X(10).
       01  WS-TXN-TYPE             PIC X(15).
       01  WS-AMT                  PIC X(10).
       01  WS-DT                   PIC X(10).
       01  WS-NUMERIC-AMT          PIC 9(7)V99.
       01  WS-BALANCE-STR          PIC X(10).
       01  WS-UPDATED-RECORD       PIC X(80).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ACCOUNT-UPDATER: Starting account updates..."
           
           PERFORM LOAD-ACCOUNTS-INTO-MEMORY
           PERFORM PROCESS-ALL-TRANSACTIONS
           PERFORM WRITE-ACCOUNTS-BACK-TO-FILE
           
           DISPLAY "ACCOUNT-UPDATER: Updates completed"
           DISPLAY "ACCOUNT-UPDATER: Transactions processed: " 
                   WS-TRANS-COUNT
           DISPLAY "ACCOUNT-UPDATER: Successful transactions: " 
                   WS-SUCCESS-COUNT
           DISPLAY "ACCOUNT-UPDATER: Failed transactions: " 
                   WS-FAILED-COUNT
           
           STOP RUN.
       
       LOAD-ACCOUNTS-INTO-MEMORY.
           OPEN INPUT ACCOUNTS.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 0 TO WS-ACCT-COUNT.
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNTS
                   AT END MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-ACCT-COUNT
                       UNSTRING ACCOUNT-RECORD DELIMITED BY ','
                           INTO WS-ACCT-ID(WS-ACCT-COUNT),
                                WS-ACCT-NAME(WS-ACCT-COUNT),
                                WS-ACCT-TYPE(WS-ACCT-COUNT),
                                WS-BALANCE-STR
                       END-UNSTRING
                       MOVE FUNCTION NUMVAL(WS-BALANCE-STR) 
                            TO WS-ACCT-BALANCE(WS-ACCT-COUNT)
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNTS.
           
       PROCESS-ALL-TRANSACTIONS.
           OPEN INPUT TRANSIN.
           MOVE 'N' TO WS-EOF-FLAG.
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ TRANSIN
                   AT END MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-TRANS-COUNT
                       PERFORM PROCESS-SINGLE-TRANSACTION
               END-READ
           END-PERFORM.
           
           CLOSE TRANSIN.
           
       PROCESS-SINGLE-TRANSACTION.
           UNSTRING TRANSACTION-RECORD DELIMITED BY ','
               INTO WS-TXN-ID, WS-TXN-TYPE, WS-CURRENT-ACCT, WS-AMT, 
               WS-DT
           END-UNSTRING
           
           MOVE FUNCTION NUMVAL(WS-AMT) TO WS-NUMERIC-AMT
           PERFORM FIND-ACCOUNT-IN-MEMORY
           
           IF WS-FOUND-FLAG = 'Y'
               PERFORM UPDATE-ACCOUNT-IN-MEMORY
               ADD 1 TO WS-SUCCESS-COUNT
               DISPLAY "UPDATED: " TRANSACTION-RECORD
               DISPLAY " -> Account: " WS-CURRENT-ACCT 
               " Balance updated"
           ELSE
               ADD 1 TO WS-FAILED-COUNT
               DISPLAY "FAILED: " TRANSACTION-RECORD
               DISPLAY " -> Reason: Account " WS-CURRENT-ACCT 
                       " not found in master file"
           END-IF.
           
       FIND-ACCOUNT-IN-MEMORY.
           MOVE 'N' TO WS-FOUND-FLAG.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
                                                 OR WS-FOUND-FLAG = 'Y'
               IF WS-ACCT-ID(WS-I) = WS-CURRENT-ACCT
                   MOVE 'Y' TO WS-FOUND-FLAG
               END-IF
           END-PERFORM.
           
       UPDATE-ACCOUNT-IN-MEMORY.
           EVALUATE WS-TXN-TYPE
               WHEN 'DEPOSIT   '
                   ADD WS-NUMERIC-AMT TO WS-ACCT-BALANCE(WS-I)
               WHEN 'WITHDRAWAL'
                   SUBTRACT WS-NUMERIC-AMT FROM WS-ACCT-BALANCE(WS-I)
               WHEN 'TRANSFER  '
                   SUBTRACT WS-NUMERIC-AMT FROM WS-ACCT-BALANCE(WS-I)
           END-EVALUATE.
           
       WRITE-ACCOUNTS-BACK-TO-FILE.
           OPEN OUTPUT TRANSOUT.
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ACCT-COUNT
               STRING WS-ACCT-ID(WS-I) DELIMITED BY SPACE
                      ',' DELIMITED BY SIZE
                      WS-ACCT-NAME(WS-I) DELIMITED BY SPACE
                      ',' DELIMITED BY SIZE
                      WS-ACCT-TYPE(WS-I) DELIMITED BY SPACE
                      ',' DELIMITED BY SIZE
                      WS-ACCT-BALANCE(WS-I) DELIMITED BY SIZE
                   INTO WS-UPDATED-RECORD
               END-STRING
               MOVE WS-UPDATED-RECORD TO UPDATED-ACCOUNT-RECORD
               WRITE UPDATED-ACCOUNT-RECORD
           END-PERFORM.
           
           CLOSE TRANSOUT.
