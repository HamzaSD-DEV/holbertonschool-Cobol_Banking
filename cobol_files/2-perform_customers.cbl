       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM-CUSTOMERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD   PIC X(33).

       WORKING-STORAGE SECTION.
       01  CUST-ID           PIC 9(5).
       01  CUST-FNAME        PIC X(10).
       01  CUST-LNAME        PIC X(10).
       01  BALANCE           PIC 9(5).99.
       01  CUST-BALANCE-STRING PIC X(8).
       01  CONTINUE-FLAG     PIC X VALUE "Y".

       PROCEDURE DIVISION.
       BEGIN.
           OPEN EXTEND CUSTOMER-FILE

           PERFORM UNTIL CONTINUE-FLAG NOT = "Y"
               DISPLAY "Enter Customer ID (5 digits): "
               ACCEPT CUST-ID

               DISPLAY "Enter First Name (max 10 characters): "
               
               ACCEPT CUST-FNAME

               DISPLAY "Enter Last Name (max 10 characters): "
               ACCEPT CUST-LNAME

               DISPLAY "Enter Balance (less than 99999.99): "
               ACCEPT BALANCE
               MOVE BALANCE TO CUST-BALANCE-STRING
               STRING
                   CUST-ID 
                   CUST-FNAME 
                   CUST-LNAME 
                   CUST-BALANCE-STRING
               DELIMITED BY SIZE 
           INTO CUSTOMER-RECORD
           WRITE CUSTOMER-RECORD

               DISPLAY "Record written. Add another? (Y/N): "
               
               ACCEPT CONTINUE-FLAG
               MOVE FUNCTION UPPER-CASE(CONTINUE-FLAG) TO CONTINUE-FLAG
           END-PERFORM

           CLOSE CUSTOMER-FILE
           DISPLAY "All records saved. Goodbye!"
           STOP RUN.
