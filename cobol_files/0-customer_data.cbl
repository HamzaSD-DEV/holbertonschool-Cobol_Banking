       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-DATA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
             ASSIGN TO "CUSTOMERS.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD            PIC X(33).
       01  CUSTOMER-FIELDS REDEFINES CUSTOMER-RECORD.
           05  CUST-ID-FIELD         PIC X(5).
           05  CUST-FNAME-FIELD      PIC X(10).
           05  CUST-LNAME-FIELD      PIC X(10).
           05  CUST-BAL-FIELD        PIC X(8).   

       WORKING-STORAGE SECTION.
       01  FILE-STATUS                PIC XX.
       01  WS-END-FILE                PIC X VALUE "N".

       01  CUST-BAL-NUMERIC           PIC 9(5)V99.
       01  DISPLAY-BALANCE            PIC ZZZZ9.99.

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT CUSTOMER-FILE

           PERFORM UNTIL WS-END-FILE = "Y"
               READ CUSTOMER-FILE
                   AT END
                       MOVE "Y" TO WS-END-FILE
                   NOT AT END
                       
                       COMPUTE CUST-BAL-NUMERIC = FUNCTION NUMVAL 
                       (CUST-BAL-FIELD)

                       MOVE CUST-BAL-NUMERIC TO DISPLAY-BALANCE
                       DISPLAY "Customer ID: " CUST-ID-FIELD
                           ", First Name: " FUNCTION 
                           TRIM(CUST-FNAME-FIELD TRAILING)
                           ", Last Name: " FUNCTION 
                           TRIM(CUST-LNAME-FIELD TRAILING)
                           ", Balance: " DISPLAY-BALANCE
               END-READ
           END-PERFORM

           CLOSE CUSTOMER-FILE
           STOP RUN.
