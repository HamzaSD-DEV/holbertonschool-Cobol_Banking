       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-VALIDATION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SEARCH-TYPE       PIC X.
       01 WS-CUSTOMER-ID       PIC X(10).
       01 WS-CUSTOMER-NAME     PIC X(20).
       01 WS-FIELD-TO-CHECK    PIC X(30).
       01 WS-INVALID-CHARS     PIC X(50) VALUE SPACES.
       01 WS-VALID-FLAG        PIC X VALUE 'Y'.

       01 IDX                  PIC 99.
       01 CHAR                 PIC X.
       01 INVALID-CHAR-FLAG    PIC X VALUE 'N'.

       01 NUMERIC-FLAG         PIC X VALUE 'Y'.

       01 SQL-INJECTION-CHARS.
           05 CH1              PIC X VALUE "'".
           05 CH2              PIC X VALUE ";".
           05 CH3              PIC X VALUE "-".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Search Type (I=ID, N=Name): " WITH NO 
           ADVANCING
           ACCEPT WS-SEARCH-TYPE

           IF WS-SEARCH-TYPE = 'I' OR WS-SEARCH-TYPE = 'i'
               DISPLAY "Enter Customer ID: " WITH NO ADVANCING
               ACCEPT WS-CUSTOMER-ID
               PERFORM VALIDATE-ID
           ELSE
               IF WS-SEARCH-TYPE = 'N' OR WS-SEARCH-TYPE = 'n'
                   DISPLAY "Enter Customer Name: " WITH NO ADVANCING
                   ACCEPT WS-CUSTOMER-NAME
                   PERFORM VALIDATE-NAME
               ELSE
                   DISPLAY "Invalid Search Type. Exiting..."
                   STOP RUN
               END-IF
           END-IF

           IF WS-VALID-FLAG = 'Y'
               DISPLAY "Input validation passed!"
           ELSE
               DISPLAY "Invalid character detected: " FUNCTION 
               TRIM(WS-INVALID-CHARS)
           END-IF

           STOP RUN.

       VALIDATE-ID.
           *> Check numeric only
           MOVE 'Y' TO NUMERIC-FLAG
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION 
           LENGTH(WS-CUSTOMER-ID)
               MOVE WS-CUSTOMER-ID(IDX:1) TO CHAR
               IF CHAR NOT = SPACE AND (CHAR < "0" OR CHAR > "9")
                   MOVE 'N' TO NUMERIC-FLAG
               END-IF
           END-PERFORM

           IF NUMERIC-FLAG = 'N'
               MOVE 'N' TO WS-VALID-FLAG
               STRING "Non-numeric ID" DELIMITED BY SIZE
                   INTO WS-INVALID-CHARS
           ELSE
               MOVE WS-CUSTOMER-ID TO WS-FIELD-TO-CHECK
               PERFORM SQL-INJECTION-CHECK
           END-IF
           .

       VALIDATE-NAME.
           *> Allow letters, spaces, . and -
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION 
           LENGTH(WS-CUSTOMER-NAME)
               MOVE WS-CUSTOMER-NAME(IDX:1) TO CHAR
               IF CHAR NOT = SPACE AND CHAR NOT = "." AND CHAR NOT = "-"
                   AND (CHAR < "A" OR (CHAR > "Z" AND CHAR < "a") OR 
                   CHAR > "z")
                       MOVE 'N' TO WS-VALID-FLAG
               END-IF
           END-PERFORM

           MOVE WS-CUSTOMER-NAME TO WS-FIELD-TO-CHECK
           PERFORM SQL-INJECTION-CHECK
           .

       SQL-INJECTION-CHECK.
           MOVE 'N' TO INVALID-CHAR-FLAG
           
           *> Reset before checking each character
           MOVE ZERO TO IDX
           INSPECT WS-FIELD-TO-CHECK TALLYING IDX FOR ALL CH1
           IF IDX > 0
               STRING "'" DELIMITED BY SIZE INTO WS-INVALID-CHARS
               MOVE 'Y' TO INVALID-CHAR-FLAG
           END-IF

           MOVE ZERO TO IDX
           INSPECT WS-FIELD-TO-CHECK TALLYING IDX FOR ALL CH2
           IF IDX > 0
               STRING WS-INVALID-CHARS ";" DELIMITED BY SIZE INTO 
               WS-INVALID-CHARS
               MOVE 'Y' TO INVALID-CHAR-FLAG
           END-IF

           MOVE ZERO TO IDX
           INSPECT WS-FIELD-TO-CHECK TALLYING IDX FOR ALL CH3
           IF IDX > 0
               STRING WS-INVALID-CHARS "- -" DELIMITED BY SIZE INTO 
               WS-INVALID-CHARS
               MOVE 'Y' TO INVALID-CHAR-FLAG
           END-IF

           IF INVALID-CHAR-FLAG = 'Y'
               MOVE 'N' TO WS-VALID-FLAG
           END-IF
           .

