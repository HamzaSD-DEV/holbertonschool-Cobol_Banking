IDENTIFICATION DIVISION.
       PROGRAM-ID. initial-report.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  SQL-LIT  PIC X(200) VALUE
           "SELECT c.name, a.balance FROM customers c "
           & "JOIN accounts a ON c.customer_id = a.customer_id "
           & "ORDER BY c.customer_id".
       01  L PIC 9(4) VALUE 0.
       01  CUST-NAME PIC X(100).
       01  BALANCE-VAL PIC X(30).
       01  I PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN STOP RUN.

           MOVE SPACES TO SQL-COMMAND.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(SQL-LIT)).
           MOVE SQL-LIT(1:L) TO SQL-COMMAND(1:L).
           MOVE X"00" TO SQL-COMMAND(L + 1:1).

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
               RETURNING STMT.

           IF STMT NOT = NULL-PTR THEN
               DISPLAY "--- INITIAL BALANCE REPORT ---"
               PERFORM FETCH-LOOP UNTIL RC NOT = 0
           END-IF.

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
           DISPLAY "--- End of Test 2 ---".
           GOBACK.

       FETCH-LOOP.
           MOVE SPACES TO C1, C2, C3.
           CALL STATIC "DB_FETCH"
               USING BY VALUE STMT, BY REFERENCE C1, C2, C3
               RETURNING RC.
           IF RC = 0 THEN
               *> Remove null characters from C1 and C2
               PERFORM REMOVE-NULLS-FROM-C1
               PERFORM REMOVE-NULLS-FROM-C2
               DISPLAY "Customer: " FUNCTION TRIM(CUST-NAME) 
                       ", Balance: " FUNCTION TRIM(BALANCE-VAL)
           END-IF.

       REMOVE-NULLS-FROM-C1.
           MOVE SPACES TO CUST-NAME
           MOVE 1 TO I
           PERFORM UNTIL I > 100 OR C1(I:1) = X"00"
               MOVE C1(I:1) TO CUST-NAME(I:1)
               ADD 1 TO I
           END-PERFORM.

       REMOVE-NULLS-FROM-C2.
           MOVE SPACES TO BALANCE-VAL
           MOVE 1 TO I
           PERFORM UNTIL I > 30 OR C2(I:1) = X"00"
               MOVE C2(I:1) TO BALANCE-VAL(I:1)
               ADD 1 TO I
           END-PERFORM.
           