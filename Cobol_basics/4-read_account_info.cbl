       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-ACCOUNT-INFO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACCOUNT-NUMBER       PIC 9(6).
       01 ACCOUNT-BALANCE      PIC 9(6)V99.
       

       PROCEDURE DIVISION.
           DISPLAY "Please enter your account number: "
           ACCEPT ACCOUNT-NUMBER

           DISPLAY "Please enter your current balance: "
           ACCEPT ACCOUNT-BALANCE


           DISPLAY "Account " ACCOUNT-NUMBER
               " has a balance of : " ACCOUNT-BALANCE "$"

           STOP RUN.
