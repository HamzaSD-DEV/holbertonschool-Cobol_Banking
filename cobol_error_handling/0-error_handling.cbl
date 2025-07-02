       IDENTIFICATION DIVISION.
       PROGRAM-ID. ERROR-HANDLING-TASK0.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID     PIC 9(5).
           05 CUSTOMER-NAME   PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-INPUT-ID         PIC 9(5).
       01 WS-FOUND-FLAG       PIC X VALUE 'N'.
       01 WS-END-FLAG         PIC X VALUE 'N'.
       01 FILE-STATUS         PIC XX.
       01 WS-DISPLAY-NAME     PIC X(20).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter Customer ID: "
           ACCEPT WS-INPUT-ID

           OPEN INPUT CUSTOMER-FILE
           
           PERFORM UNTIL WS-FOUND-FLAG = 'Y' OR WS-END-FLAG = 'Y'
               READ CUSTOMER-FILE
                   AT END
                       MOVE 'Y' TO WS-END-FLAG
                   NOT AT END
                       IF CUSTOMER-ID = WS-INPUT-ID
                           MOVE 'Y' TO WS-FOUND-FLAG
                           MOVE CUSTOMER-NAME TO WS-DISPLAY-NAME
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND-FLAG = 'Y'
               DISPLAY "Customer found: " WS-DISPLAY-NAME
           ELSE
               DISPLAY "Error: Customer ID not found"
           END-IF

           CLOSE CUSTOMER-FILE

           STOP RUN.
