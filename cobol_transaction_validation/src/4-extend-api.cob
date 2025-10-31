IDENTIFICATION DIVISION.
       PROGRAM-ID. extend-ap.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200) VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L PIC 9(4) VALUE 0.
       01  NUM-ROWS-DIP PIC Z(9).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN STOP RUN.

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH,
               BY CONTENT Z"SELECT * FROM customers"
               RETURNING STMT.

           IF STMT NOT = NULL-PTR THEN
               CALL STATIC "DB_NUM_ROWS"
                   USING BY VALUE STMT
                   RETURNING NUM-ROWS
               MOVE NUM-ROWS TO NUM-ROWS-DIP
               DISPLAY "SUCCESS: Query returned " FUNCTION  TRIM(NUM-ROWS-DIP) " rows."
           END-IF.

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
           GOBACK.
           