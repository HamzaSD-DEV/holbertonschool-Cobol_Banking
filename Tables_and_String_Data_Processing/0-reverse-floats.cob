       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-FLOATS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Array to hold 100 float values
       01  FLOAT-ARRAY.
           05  FLOAT-ENTRY     OCCURS 100 TIMES PIC 9(4)V99.

       *> Loop indices
       77  WS-INDEX           PIC 9(3) VALUE 1.
       77  WS-REV-INDEX       PIC 9(3).
       *> Raw user input
       77  WS-USER-INPUT      PIC X(10).

       PROCEDURE DIVISION.
       BEGIN.

           *> Read exactly 100 floats, no validation
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 100
               DISPLAY "Enter float #" WS-INDEX ": " WITH NO ADVANCING
               ACCEPT WS-USER-INPUT
               MOVE FUNCTION NUMVAL(WS-USER-INPUT) 
                   TO FLOAT-ENTRY(WS-INDEX)
           END-PERFORM

           DISPLAY "Values in reverse order:"
           PERFORM VARYING WS-REV-INDEX FROM 100 BY -1 UNTIL
            WS-REV-INDEX = 0
               DISPLAY "Float #" WS-REV-INDEX ": " 
               FLOAT-ENTRY(WS-REV-INDEX)
           END-PERFORM

           STOP RUN.
