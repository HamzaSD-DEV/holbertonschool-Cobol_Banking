       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-WITH-JQ.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TODO-FILE ASSIGN TO "todos.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TODO-FILE.
       01  TODO-RECORD         PIC X(256).

       WORKING-STORAGE SECTION.
       01 CURL-JQ-COMMAND      PIC X(200) VALUE
           "curl -s ""https://jsonplaceholder.typicode.com/todos?_limit=10"" | jq -r '.[].title' > todos.txt".
       01 SYSTEM-STATUS        PIC S9(9) BINARY.

       01 WS-FILE-STATUS       PIC X.
           88 EOF-REACHED      VALUE 'Y' FALSE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Fetching and processing to-do list...".
           
           CALL "SYSTEM" USING CURL-JQ-COMMAND
                         RETURNING SYSTEM-STATUS.

           IF SYSTEM-STATUS = 0
               DISPLAY "API call successful. Displaying titles:"
               PERFORM READ-TITLES-FILE
           ELSE
               DISPLAY "Error: Command failed with status: "
                       SYSTEM-STATUS
           END-IF.

           DISPLAY "Done.".
           STOP RUN.

       READ-TITLES-FILE.
           SET EOF-REACHED TO FALSE.
           OPEN INPUT TODO-FILE.
           PERFORM UNTIL EOF-REACHED
               READ TODO-FILE
                   AT END
                       SET EOF-REACHED TO TRUE
                   NOT AT END
                       DISPLAY FUNCTION TRIM(TODO-RECORD)
               END-READ
           END-PERFORM.
           CLOSE TODO-FILE.
