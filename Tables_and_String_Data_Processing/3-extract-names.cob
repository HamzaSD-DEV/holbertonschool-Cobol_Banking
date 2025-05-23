       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRACTNAMES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TRIMMED-NAME      PIC X(50).
       01 WS-TOKEN-TABLE.
          05 WS-TOKEN OCCURS 10 TIMES PIC X(20).
       01 WS-INDEX             PIC 9(2).
       01 WS-CHAR              PIC X.
       01 WS-TOKEN-IDX         PIC 9(2).
       01 WS-TOKEN-POS         PIC 9(2).
       01 WS-LENGTH            PIC 9(3).
       01 WS-IN-TOKEN          PIC X.

       LINKAGE SECTION.
       01 LK-FULL-NAME         PIC X(50).

       PROCEDURE DIVISION USING LK-FULL-NAME.

       *> Reset variables before each call
           MOVE SPACES TO WS-TRIMMED-NAME
           PERFORM VARYING WS-TOKEN-IDX FROM 1 BY 1 UNTIL WS-TOKEN-IDX > 
           10
               MOVE SPACES TO WS-TOKEN(WS-TOKEN-IDX)
           END-PERFORM

           MOVE 1 TO WS-INDEX
           MOVE 1 TO WS-TOKEN-IDX
           MOVE 1 TO WS-TOKEN-POS
           MOVE 'N' TO WS-IN-TOKEN

       *> Trim spaces
           MOVE FUNCTION TRIM(LK-FULL-NAME) TO WS-TRIMMED-NAME
           MOVE FUNCTION LENGTH(WS-TRIMMED-NAME) TO WS-LENGTH

       *> Tokenize the name
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX >
            WS-LENGTH
               MOVE WS-TRIMMED-NAME(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR NOT = SPACE
                   IF WS-IN-TOKEN = 'N'
                       MOVE 'Y' TO WS-IN-TOKEN
                       MOVE 1 TO WS-TOKEN-POS
                       MOVE SPACES TO WS-TOKEN(WS-TOKEN-IDX)
                   END-IF
                   MOVE WS-CHAR TO 
                   WS-TOKEN(WS-TOKEN-IDX)(WS-TOKEN-POS:1)
                   ADD 1 TO WS-TOKEN-POS
               ELSE
                   IF WS-IN-TOKEN = 'Y'
                       MOVE 'N' TO WS-IN-TOKEN
                       ADD 1 TO WS-TOKEN-IDX
                   END-IF
               END-IF
           END-PERFORM

       *> Final check to include last word if still open
           IF WS-IN-TOKEN = 'Y'
               ADD 1 TO WS-TOKEN-IDX
           END-IF

       *> Show results
           IF WS-TOKEN-IDX = 2
               DISPLAY "First Name: " WS-TOKEN(1)
               DISPLAY "Last Name:  " WS-TOKEN(1)
           ELSE
               DISPLAY "First Name: " WS-TOKEN(1)
               DISPLAY "Last Name:  " WS-TOKEN(WS-TOKEN-IDX - 1)
           END-IF

           GOBACK.
           