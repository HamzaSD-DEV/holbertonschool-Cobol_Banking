       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDENTIFY-NEGATIVE-BALANCES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BALANCE-FILE ASSIGN TO "balances.idx"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD BALANCE-FILE.
       01 BALANCE-RECORD.
           05 ACCOUNT-ID         PIC X(9).
           05 BALANCE-STRING     PIC X(10).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG              PIC X VALUE "N".
           88 END-OF-FILE       VALUE "Y".
           88 NOT-EOF           VALUE "N".

       01 BALANCE-NUMERIC       PIC S9(5)V99 COMP-3.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT BALANCE-FILE

           PERFORM UNTIL END-OF-FILE
               READ BALANCE-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE FUNCTION NUMVAL(BALANCE-STRING) TO 
                       BALANCE-NUMERIC
                       IF BALANCE-NUMERIC < 0
                           DISPLAY "Account " ACCOUNT-ID " - Balance: " 
                           BALANCE-STRING
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BALANCE-FILE
           STOP RUN.
