       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-NUMS.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  L-NUM1     PIC S9(5)V99.
       01  L-NUM2     PIC S9(5)V99.
       01  L-RESULT   PIC S9(6)V99.
       
       PROCEDURE DIVISION USING L-NUM1, L-NUM2, L-RESULT.
           COMPUTE L-RESULT = L-NUM1 + L-NUM2
           EXIT PROGRAM.
       