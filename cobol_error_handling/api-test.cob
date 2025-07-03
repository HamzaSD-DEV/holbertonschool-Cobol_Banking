       IDENTIFICATION DIVISION.
       PROGRAM-ID. APICALL.
       AUTHOR.     COBOL-API-EXAMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CMD-STRING     PIC X(200).
       01 RETURN-CODES    PIC 9(4) VALUE ZERO.
       01 WS-RESPONSE    PIC X(10000).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 'curl -s "https://jsonplaceholder.typicode.com/todos/1"' 
             TO CMD-STRING
           CALL "SYSTEM" USING CMD-STRING RETURNING RETURN-CODES
           
           IF RETURN-CODES NOT = 0
               DISPLAY "ERROR: API CALL FAILED WITH CODE " RETURN-CODES
           ELSE
               DISPLAY "API CALL SUCCESSFUL"
               DISPLAY "RESPONSE:"
               DISPLAY CMD-STRING
           END-IF
           
           STOP RUN.
           