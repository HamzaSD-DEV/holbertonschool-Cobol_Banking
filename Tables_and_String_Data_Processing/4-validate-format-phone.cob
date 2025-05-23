       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATEPHONE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-CLEAN-NUMBER       PIC X(10) VALUE SPACES.
       01 WS-DIGIT-COUNT        PIC 9(02) VALUE 0.
       01 WS-INDEX              PIC 9(02) VALUE 1.
       01 WS-DIGIT-INDEX        PIC 9(02) VALUE 1.
       01 WS-CURRENT-CHAR       PIC X.
       01 WS-FORMATTED-NUMBER   PIC X(15).

       LINKAGE SECTION.
       01 LK-RAW-PHONE-NUMBER   PIC X(30).

       PROCEDURE DIVISION USING LK-RAW-PHONE-NUMBER.

       *> Clean number
           MOVE SPACES TO WS-CLEAN-NUMBER
           MOVE 0 TO WS-DIGIT-COUNT
           MOVE 1 TO WS-INDEX
           MOVE 1 TO WS-DIGIT-INDEX

           PERFORM UNTIL WS-INDEX > LENGTH OF LK-RAW-PHONE-NUMBER
               MOVE LK-RAW-PHONE-NUMBER(WS-INDEX:1) TO WS-CURRENT-CHAR
               IF WS-CURRENT-CHAR >= "0" AND WS-CURRENT-CHAR <= "9"
                   IF WS-DIGIT-COUNT < 10
                       MOVE WS-CURRENT-CHAR TO 
                       WS-CLEAN-NUMBER(WS-DIGIT-INDEX:1)
                       ADD 1 TO WS-DIGIT-COUNT
                       ADD 1 TO WS-DIGIT-INDEX
                   ELSE
                       ADD 1 TO WS-DIGIT-COUNT *> Track overflow
                   END-IF
               END-IF
               ADD 1 TO WS-INDEX
           END-PERFORM

       *> Validate total digit count = 10
           IF WS-DIGIT-COUNT NOT = 10
               DISPLAY "Invalid phone number: " LK-RAW-PHONE-NUMBER
               GOBACK
           END-IF

       *> Format: (0X) XXXX XXXX
           STRING
               "(" DELIMITED BY SIZE
               WS-CLEAN-NUMBER(1:2) DELIMITED BY SIZE
               ") " DELIMITED BY SIZE
               WS-CLEAN-NUMBER(3:4) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-CLEAN-NUMBER(7:4) DELIMITED BY SIZE
               INTO WS-FORMATTED-NUMBER
           END-STRING

           DISPLAY "Formatted Phone: " WS-FORMATTED-NUMBER
           GOBACK.
