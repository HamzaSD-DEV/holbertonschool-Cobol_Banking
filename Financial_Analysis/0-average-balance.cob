       IDENTIFICATION DIVISION.
       PROGRAM-ID. AVERAGEBALANCE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BALANCE-FILE ASSIGN TO 'balances.idx'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD BALANCE-FILE.
       01 BALANCE-RECORD.
           05 CUSTOMER-ID         PIC X(7).
           05 FILLER              PIC X.
           05 BALANCE-AMOUNT      PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 WS-TOTAL-BALANCE        PIC 9(7)V99 VALUE 0.
       01 WS-CUSTOMER-COUNT       PIC 9(4) VALUE 0.
       01 WS-AVERAGE              PIC 9(5)V99 VALUE 0.
       01 WS-DISPLAY-TOTAL        PIC Z(6)9.99.
       01 WS-DISPLAY-AVERAGE      PIC Z(5)9.99.
       01 EOF-FLAG                PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT BALANCE-FILE

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BALANCE-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-CUSTOMER-COUNT
                       ADD BALANCE-AMOUNT TO WS-TOTAL-BALANCE
               END-READ
           END-PERFORM

           IF WS-CUSTOMER-COUNT > 0
               COMPUTE WS-AVERAGE = WS-TOTAL-BALANCE / WS-CUSTOMER-COUNT
           END-IF

           MOVE WS-TOTAL-BALANCE TO WS-DISPLAY-TOTAL
           MOVE WS-AVERAGE TO WS-DISPLAY-AVERAGE

           DISPLAY "Total Customers: " WS-CUSTOMER-COUNT
           DISPLAY "Total Balance: " WS-DISPLAY-TOTAL
           DISPLAY "Average Balance: " WS-DISPLAY-AVERAGE

           CLOSE BALANCE-FILE
           STOP RUN.
