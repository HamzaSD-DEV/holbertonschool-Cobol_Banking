       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORMAT-OUTPUT-PIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 Customer-ID        PIC X(9)     VALUE "CUST00123".
       01 Account-Balance    PIC 9(5)V99  VALUE 1234.56.
       01 Interest-Rate      PIC 9V99     VALUE 5.75.
       
       01 Account-Balance-DISPLAY  PIC 9(5).99.
       01 Interest-Rate-DISPLAY    PIC 9.99.

       PROCEDURE DIVISION.
       BEGIN.
           MOVE Account-Balance TO Account-Balance-DISPLAY
           MOVE Interest-Rate TO Interest-Rate-DISPLAY

           DISPLAY "Customer ID : " Customer-ID
           DISPLAY "Account Balance : " Account-Balance-DISPLAY
           DISPLAY "Interest Rate : " Interest-Rate-DISPLAY "%"
           STOP RUN.
