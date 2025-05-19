       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CUSTOMER-TABLE.
           05  CUSTOMER-DATA OCCURS 5 TIMES INDEXED BY CUST-IDX.
               10  CUST-ID        PIC 9(5).
               10  CUST-FNAME     PIC X(10).
               10  CUST-LNAME     PIC X(10).
               10  CUST-EMAIL     PIC X(25).
               10  CUST-BAL       PIC 9(5)V99.

       01  WS-USER-CHO            PIC 9.
       01  WS-VALID-INPUT         PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INITIALIZE-DATA
           PERFORM GET-VALID-INPUT
           IF WS-VALID-INPUT = 'Y'
               CALL 'RETRIEVE-DETAILS' USING CUSTOMER-TABLE, WS-USER-CHO
           ELSE
               DISPLAY "No valid selection made."
           END-IF
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 10010 TO CUST-ID(1)
           MOVE 10020 TO CUST-ID(2)
           MOVE 10030 TO CUST-ID(3)
           MOVE 10040 TO CUST-ID(4)
           MOVE 10050 TO CUST-ID(5)

           MOVE "Alex      " TO CUST-FNAME(1)
           MOVE "Pop       " TO CUST-FNAME(2)
           MOVE "Hamza     " TO CUST-FNAME(3)
           MOVE "Qiana     " TO CUST-FNAME(4)
           MOVE "Ali       " TO CUST-FNAME(5)

           MOVE "Smich     " TO CUST-LNAME(1)
           MOVE "Jonie     " TO CUST-LNAME(2)
           MOVE "Saoud     " TO CUST-LNAME(3)
           MOVE "Stona     " TO CUST-LNAME(4)
           MOVE "Ping      " TO CUST-LNAME(5)

           MOVE "alex@example.com        " TO CUST-EMAIL(1)
           MOVE "pop@example.com         " TO CUST-EMAIL(2)
           MOVE "hamza@example.com       " TO CUST-EMAIL(3)
           MOVE "qiana@example.com       " TO CUST-EMAIL(4)
           MOVE "ali@example.com         " TO CUST-EMAIL(5)

           MOVE 100050 TO CUST-BAL(1)
           MOVE 25000 TO CUST-BAL(2)
           MOVE 50075 TO CUST-BAL(3)
           MOVE 0 TO CUST-BAL(4)
           MOVE 12010 TO CUST-BAL(5).

       GET-VALID-INPUT.
           PERFORM UNTIL WS-VALID-INPUT = 'Y'
               DISPLAY "Enter a customer number (1-5): "
               ACCEPT WS-USER-CHO
               IF WS-USER-CHO >= 1 AND WS-USER-CHO <= 5
                   MOVE 'Y' TO WS-VALID-INPUT
               ELSE
                   DISPLAY "Invalid customer number. Please try again."
               END-IF
           END-PERFORM.
           