       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETE-POST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESP-FILE ASSIGN TO "delete_response.json"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RESP-FILE.
       01  RESP-RECORD         PIC X(100).

       WORKING-STORAGE SECTION.
       01 API-URL              PIC X(55)
           VALUE "https://jsonplaceholder.typicode.com/posts/1".
       01 CURL-COMMAND         PIC X(200).
       01 SYSTEM-STATUS        PIC S9(9) BINARY.

       01 WS-FILE-STATUS       PIC X.
           88 EOF-REACHED      VALUE 'Y' FALSE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Sending DELETE request to JSONPlaceholder...".

           STRING "curl -s -X DELETE " API-URL
                  " -o delete_response.json"
               DELIMITED BY SIZE INTO CURL-COMMAND.

           CALL "SYSTEM" USING CURL-COMMAND
                         RETURNING SYSTEM-STATUS.

           IF SYSTEM-STATUS = 0
               DISPLAY "API call command executed successfully."
               PERFORM READ-RESPONSE-FILE
               DISPLAY "Post 1 has been marked for deletion on the server."
           ELSE
               DISPLAY "Error: API call command failed with status: "
                       SYSTEM-STATUS
           END-IF.

           DISPLAY "Done.".
           STOP RUN.

       READ-RESPONSE-FILE.
           SET EOF-REACHED TO FALSE.
           OPEN INPUT RESP-FILE.
           PERFORM UNTIL EOF-REACHED
               READ RESP-FILE
                   AT END
                       SET EOF-REACHED TO TRUE
                   NOT AT END
                       DISPLAY "Response from server: "
                               FUNCTION TRIM(RESP-RECORD)
               END-READ
           END-PERFORM.
           CLOSE RESP-FILE.
           