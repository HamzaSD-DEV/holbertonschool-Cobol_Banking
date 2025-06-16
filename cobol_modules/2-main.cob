       IDENTIFICATION DIVISION.
       PROGRAM-ID. TASK2-MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-SCORES.
           05  WS-SCORE OCCURS 3 TIMES PIC 9(3).

       01  WS-AVERAGE   PIC 9(3)V9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=============================="
           DISPLAY "  STUDENT SCORE PROCESSOR"
           DISPLAY "=============================="

           CALL 'READ-SCORES' USING WS-SCORES
           CALL 'CALCULATE-AVERAGE' USING WS-SCORES, WS-AVERAGE
           CALL 'DISPLAY-RESULTS' USING WS-SCORES, WS-AVERAGE

           DISPLAY "Processing Complete. Goodbye!"
           STOP RUN.
