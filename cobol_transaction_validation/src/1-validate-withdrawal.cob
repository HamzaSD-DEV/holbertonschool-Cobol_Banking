IDENTIFICATION DIVISION.
       PROGRAM-ID. validate-withdrawal.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TX-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TX-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  TX-FILE.
       01  TX-RECORD            PIC X(200).
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200) 
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L PIC 9(4) VALUE 0.
       01  TX-FILE-STATUS PIC XX.
       01  TX-DATA.
           05 TX-ACTION         PIC X(8).
           05 TX-ACCOUNT-ID     PIC X(4).
           05 TX-AMOUNT         PIC X(10).
       01  CURRENT-BALANCE      PIC S9(8)V99.
       01  WITHDRAWAL-AMOUNT    PIC S9(8)V99.
       01  BALANCE-STR          PIC X(20).
       01  BAL-INTEGER          PIC X(10).
       01  BAL-DECIMAL          PIC X(10).
       01  WS-POS               PIC 9(4).
       01  DECIMAL-FOUND        PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN STOP RUN.

           OPEN INPUT TX-FILE.
           PERFORM PROCESS-WITHDRAWALS UNTIL TX-FILE-STATUS NOT = "00".
           CLOSE TX-FILE.

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
           GOBACK.

       PROCESS-WITHDRAWALS.
           READ TX-FILE AT END MOVE "10" TO TX-FILE-STATUS.
           IF TX-FILE-STATUS = "00" THEN
               UNSTRING TX-RECORD DELIMITED BY ","
                   INTO TX-ACTION, TX-ACCOUNT-ID, TX-AMOUNT
               *> Only process WITHDRAW actions
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(TX-ACTION)) = "WITHDRAW"
                   PERFORM VALIDATE-AND-PROCESS
               END-IF
           END-IF.

       VALIDATE-AND-PROCESS.
           MOVE SPACES TO SQL-COMMAND.
           MOVE SPACES TO SINGLE-RESULT-BUFFER.
           STRING 
               "SELECT balance FROM accounts WHERE account_id = '"
               FUNCTION TRIM(TX-ACCOUNT-ID) 
               "'"
               DELIMITED BY SIZE 
               INTO SQL-COMMAND
           END-STRING.
           MOVE X"00" TO SQL-COMMAND(100:1).

           CALL STATIC "DB_QUERY_SINGLE"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND,
                     BY REFERENCE SINGLE-RESULT-BUFFER
               RETURNING RC.

           IF RC = 0 THEN
               *> Improved manual parsing with better decimal handling
               MOVE 0 TO CURRENT-BALANCE
               MOVE FUNCTION TRIM(SINGLE-RESULT-BUFFER) TO BALANCE-STR
               DISPLAY "Raw balance string: '" BALANCE-STR "'"
        
               *> Find the decimal point position
               MOVE 1 TO WS-POS
               MOVE 0 TO DECIMAL-FOUND
               PERFORM UNTIL WS-POS > FUNCTION LENGTH(BALANCE-STR)
                              OR DECIMAL-FOUND = 1
                   IF BALANCE-STR(WS-POS:1) = "."
                       MOVE 1 TO DECIMAL-FOUND
                   ELSE
                       ADD 1 TO WS-POS
                   END-IF
               END-PERFORM
        
               IF DECIMAL-FOUND = 1
                   *> Extract integer and decimal parts
                   MOVE BALANCE-STR(1:WS-POS - 1) TO BAL-INTEGER
                   MOVE BALANCE-STR(WS-POS + 1:2) TO BAL-DECIMAL
                   COMPUTE CURRENT-BALANCE = 
                       FUNCTION NUMVAL(FUNCTION TRIM(BAL-INTEGER)) + 
                       (FUNCTION NUMVAL(FUNCTION TRIM(BAL-DECIMAL)) / 100)
               ELSE
                   *> No decimal point found
                   MOVE FUNCTION NUMVAL(FUNCTION TRIM(BALANCE-STR)) 
                     TO CURRENT-BALANCE
               END-IF

               *> Convert withdrawal amount
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(TX-AMOUNT)) 
                 TO WITHDRAWAL-AMOUNT

               DISPLAY "Debug: Current balance: " CURRENT-BALANCE
                       " Withdrawal amount: " WITHDRAWAL-AMOUNT

               IF CURRENT-BALANCE >= WITHDRAWAL-AMOUNT THEN
                   PERFORM EXECUTE-UPDATE
               ELSE
                   DISPLAY "Validation FAILED: Insufficient funds for account "
                           FUNCTION TRIM(TX-ACCOUNT-ID)
               END-IF
           ELSE
               DISPLAY "ERROR: Could not find account " 
                       FUNCTION TRIM(TX-ACCOUNT-ID)
           END-IF.

       EXECUTE-UPDATE.
           MOVE SPACES TO SQL-COMMAND.
           *> Build UPDATE query with quotes around account_id
           STRING 
               "UPDATE accounts SET balance = balance - "
               FUNCTION TRIM(TX-AMOUNT) 
               " WHERE account_id = '"
               FUNCTION TRIM(TX-ACCOUNT-ID) 
               "'"
               DELIMITED BY SIZE 
               INTO SQL-COMMAND
           END-STRING.
           *> Null-terminate
           MOVE X"00" TO SQL-COMMAND(100:1).

           CALL STATIC "DB_EXEC"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
               RETURNING RC.

           IF RC = 0 THEN
               DISPLAY "Validation PASSED: Withdrawal of " 
                       FUNCTION TRIM(TX-AMOUNT)
                       " from account " FUNCTION TRIM(TX-ACCOUNT-ID) 
                       " successful."
           ELSE
               DISPLAY "ERROR: Update failed for account " 
                       FUNCTION TRIM(TX-ACCOUNT-ID)
           END-IF.
