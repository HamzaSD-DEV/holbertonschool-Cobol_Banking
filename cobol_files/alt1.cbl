       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-DATA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMERS.DAT'
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID            PIC 9(5).
           05  CUST-FIRST-NAME    PIC X(10).
           05  CUST-LAST-NAME     PIC X(10).
           05  CUST-BALANCE       PIC X(8).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG           PIC X(1) VALUE 'N'.
           88  END-OF-FILE        VALUE 'Y'.
           88  NOT-END-OF-FILE    VALUE 'N'.
       
       01  WS-DISPLAY-LINE.
           05  FILLER            PIC X(15) VALUE 'Customer ID: '.
           05  DISPLAY-ID        PIC 9(5).

           05  FILLER            PIC X(8)  VALUE ', Name: '.
           05  DISPLAY-FNAME     PIC X(10).

           05  FILLER            PIC X(1)  VALUE ' '.
           05  DISPLAY-LNAME     PIC X(10).

           05  FILLER            PIC X(11) VALUE ', Balance: '.
           05  DISPLAY-BALANCE   PIC ZZZZ9.99.

       PROCEDURE DIVISION.
           OPEN INPUT CUSTOMER-FILE
           PERFORM UNTIL END-OF-FILE
               READ CUSTOMER-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       MOVE CUST-ID TO DISPLAY-ID
                       MOVE CUST-FIRST-NAME TO DISPLAY-FNAME
                       MOVE CUST-LAST-NAME TO DISPLAY-LNAME
                       MOVE CUST-BALANCE TO DISPLAY-BALANCE
                       DISPLAY WS-DISPLAY-LINE
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE
           STOP RUN.
