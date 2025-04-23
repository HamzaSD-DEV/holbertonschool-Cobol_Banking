       IDENTIFICATION DIVISION.
       PROGRAM-ID. LoanPaymentInput.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 LOAN-AMOUNT           PIC 9(5)      VALUE ZEROS.
       01 INTEREST-RATE         PIC 9(2)      VALUE 5.
       01 DURATION-YEARS        PIC 9(2)         VALUE ZEROS.
       01 SIMPLE-INTEREST       PIC 9(5)V99   VALUE ZEROS.
       01 TOTAL-AMOUNT          PIC 9(5)V99   VALUE ZEROS.
       01 TEMP1                 PIC 9(7)V99   VALUE ZEROS.

       PROCEDURE DIVISION.
           DISPLAY "Please enter Loan Amount: "
           ACCEPT LOAN-AMOUNT
           DISPLAY "Please enter Duration in years: "
           ACCEPT DURATION-YEARS

           MULTIPLY LOAN-AMOUNT BY INTEREST-RATE GIVING TEMP1
           MULTIPLY TEMP1 BY DURATION-YEARS GIVING TEMP1
           DIVIDE TEMP1 BY 100 GIVING SIMPLE-INTEREST

           ADD LOAN-AMOUNT TO SIMPLE-INTEREST GIVING TOTAL-AMOUNT

           DISPLAY " "
           DISPLAY "Loan Amount: " LOAN-AMOUNT "$"
           DISPLAY "Interest Rate: " INTEREST-RATE "%"
           DISPLAY "Duration: " DURATION-YEARS " years"
           DISPLAY " "
           DISPLAY "Simple Interest: " SIMPLE-INTEREST "$"
           DISPLAY "Total Amount to be Repaid: " TOTAL-AMOUNT "$"
           STOP RUN.
