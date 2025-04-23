       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-INTEREST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       *> Inputs
       01  PRINCIPAL             PIC 9(5)V99 VALUE 1000.00.
       01  RATE                  PIC 99V99   VALUE 05.00.
       01  TIME-YEARS            PIC 9       VALUE 3.

       *> Intermediate values for compound calculation
       01  TEMP-RESULT           PIC 9(3)V9(6) VALUE 1.000000.
       01  TEMP-MULTIPLIER       PIC 9(1)V9(6) VALUE 1.050000.
       01  COUNTER               PIC 9         VALUE 0.

       *> Result fields
       01  SIMPLE-INTEREST       PIC 9(5)V99 VALUE 0.
       01  COMPOUND-INTEREST     PIC 9(5)V99 VALUE 0.

       *> Display fields
       01  DISPLAY-SIMPLE        PIC 9(5).99.
       01  DISPLAY-COMPOUND      PIC 9(5).99.

       PROCEDURE DIVISION.
       BEGIN.

           *> Calculate Simple Interest
           COMPUTE SIMPLE-INTEREST ROUNDED =
               (PRINCIPAL * RATE * TIME-YEARS) / 100

           *> Calculate compound factor using a loop
           PERFORM VARYING COUNTER FROM 1 BY 1 
           UNTIL COUNTER > TIME-YEARS
               COMPUTE TEMP-RESULT ROUNDED = 
               TEMP-RESULT * TEMP-MULTIPLIER
           END-PERFORM

           *> Apply compound interest formula
           COMPUTE COMPOUND-INTEREST ROUNDED =
               (PRINCIPAL * TEMP-RESULT) - PRINCIPAL

           *> Prepare values for DISPLAY
           MOVE SIMPLE-INTEREST TO DISPLAY-SIMPLE
           MOVE COMPOUND-INTEREST TO DISPLAY-COMPOUND

           *> Output
           DISPLAY "Simple Interest is     : " DISPLAY-SIMPLE
           DISPLAY "Compound Interest is   : " DISPLAY-COMPOUND

           STOP RUN.
