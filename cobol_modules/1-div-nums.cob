       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIV-NUMS.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  L-NUM1     PIC S9(5)V99.
       01  L-NUM2     PIC S9(5)V99.
       01  L-RESULT   PIC S9(6)V99.
       
       PROCEDURE DIVISION USING L-NUM1, L-NUM2, L-RESULT.
           IF L-NUM2 = 0
               MOVE 0 TO L-RESULT
           ELSE
               COMPUTE L-RESULT = L-NUM1 / L-NUM2
           END-IF
           EXIT PROGRAM.
       