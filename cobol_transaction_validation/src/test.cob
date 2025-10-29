IDENTIFICATION DIVISION.
       PROGRAM-ID. initial-report.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  SQL-LIT  PIC X(200).
       01  SQL-LIT2 PIC X(200).
       01  L PIC 9(4) VALUE 0.
       01  AMOUNT-VAL PIC X(30).
       01  TIMESTAMP-VAL PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN STOP RUN.

           *> Get basic transaction info
           MOVE "SELECT log_id, account_id, tx_type FROM tx_log "
           & "ORDER BY tx_timestamp" 
             TO SQL-LIT.
           
           MOVE SPACES TO SQL-COMMAND.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(SQL-LIT)).
           MOVE SQL-LIT(1:L) TO SQL-COMMAND(1:L).
           MOVE X"00" TO SQL-COMMAND(L + 1:1).

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
               RETURNING STMT.

           IF STMT NOT = NULL-PTR THEN
               DISPLAY "=== TRANSACTION LOG ==="
               PERFORM FETCH-TRANSACTION-LOOP UNTIL RC NOT = 0
           ELSE
               DISPLAY "No transactions found"
           END-IF.

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
           GOBACK.

       FETCH-TRANSACTION-LOOP.
           MOVE SPACES TO C1, C2, C3.
           CALL STATIC "DB_FETCH"
               USING BY VALUE STMT, BY REFERENCE C1, C2, C3
               RETURNING RC.
           IF RC = 0 THEN
               *> Get amount for this transaction
               MOVE SPACES TO SQL-COMMAND
               MOVE SPACES TO SQL-LIT2
               STRING
                 "SELECT amount FROM tx_log WHERE log_id = "
                 FUNCTION TRIM(C1)
                 INTO SQL-LIT2
               END-STRING
               COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(SQL-LIT2))
               MOVE SQL-LIT2(1:L) TO SQL-COMMAND(1:L)
               MOVE X"00" TO SQL-COMMAND(L + 1:1)

               CALL "DB_QUERY_SINGLE"
                    USING BY VALUE DBH
                          BY REFERENCE SQL-COMMAND
                          BY REFERENCE SINGLE-RESULT-BUFFER
                    RETURNING RC
               END-CALL
               IF RC = 0 THEN
                 MOVE SINGLE-RESULT-BUFFER TO AMOUNT-VAL
               ELSE
                 MOVE "N/A" TO AMOUNT-VAL
               END-IF

               DISPLAY "ID: " FUNCTION TRIM(C1) 
                       " | Account: " FUNCTION TRIM(C2)
                       " | Type: " FUNCTION TRIM(C3)
                       " | Amount: " FUNCTION TRIM(AMOUNT-VAL)
           END-IF.
           