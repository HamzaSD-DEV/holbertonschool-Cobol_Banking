       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-SCORES.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  IDX     PIC 9 VALUE 1.

       LINKAGE SECTION.
       01  L-SCORES.
           05  L-SCORE OCCURS 3 TIMES PIC 9(3).



       PROCEDURE DIVISION USING L-SCORES.
       READ-SCORES-PARA.
           DISPLAY "Enter 3 exam scores (0-100):"
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               DISPLAY "Score " IDX ": "
               ACCEPT L-SCORE(IDX)
           END-PERFORM
           EXIT PROGRAM.
