       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-MAIN.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DISC-PRICE        PIC 9(4)V99.
       01 WS-DISP-TOTAL        PIC ZZZZZ9.99.
       01 WS-TOTAL             PIC 9(6)V99 VALUE 0.
       01 WS-CHOICE-STR        PIC X.
       01 WS-CHOICE            PIC X.

       PROCEDURE DIVISION.
       MAIN-PARA.
           *> Initialize total to zero
           MOVE 0 TO WS-TOTAL
           *> Start loop: default 'Y' to enter at least once
           MOVE 'Y' TO WS-CHOICE

           PERFORM UNTIL WS-CHOICE NOT = 'Y'
               *> Call PROCESS-ORDER to handle one item
               CALL 'PROCESS-ORDER' USING WS-DISC-PRICE

               *> Accumulate returned discounted price
               ADD WS-DISC-PRICE TO WS-TOTAL

               *> Ask to continue or exit
               DISPLAY "Another item? (Y/N): " WITH NO ADVANCING
               ACCEPT WS-CHOICE-STR
               MOVE WS-CHOICE-STR(1:1) TO WS-CHOICE
               *> If WS-CHOICE = 'Y', loop continues; otherwise exit
           END-PERFORM

           *> Display total amount
           MOVE WS-TOTAL TO WS-DISP-TOTAL
           DISPLAY "Total amount: " WS-DISP-TOTAL

           STOP RUN.
