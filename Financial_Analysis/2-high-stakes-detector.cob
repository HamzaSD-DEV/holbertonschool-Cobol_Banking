       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUD-DETECTION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.idx"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANS-ID         PIC X(6).
           05 FILLER           PIC X(1).
           05 ACC-NUM          PIC X(9).
           05 FILLER           PIC X(1).
           05 TRANS-DATE       PIC X(8).
           05 FILLER           PIC X(1).
           05 TRANS-TYPE       PIC X(1).
           05 FILLER           PIC X(1).
           05 TRANS-AMOUNT     PIC 9(10).

       WORKING-STORAGE SECTION.
       01 AMOUNT-NUMERIC      PIC 9(7)V99 COMP-3.
       01 AMOUNT-DISPLAY      PIC ZZ,ZZZ,ZZ9.99.
       01 EOF-FLAG            PIC X VALUE "N".
           88 END-OF-FILE     VALUE "Y".
           88 NOT-EOF         VALUE "N".
       01 STATUS-MSG          PIC X(10).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT TRANSACTION-FILE

           PERFORM UNTIL END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       COMPUTE AMOUNT-NUMERIC = TRANS-AMOUNT / 100
                       MOVE AMOUNT-NUMERIC TO AMOUNT-DISPLAY

                       IF AMOUNT-NUMERIC > 10000.00
                           MOVE "SUSPICIOUS" TO STATUS-MSG
                       ELSE
                           MOVE "OK" TO STATUS-MSG
                       END-IF

                       DISPLAY TRANS-ID " " ACC-NUM " " TRANS-DATE " "
                               TRANS-TYPE " " AMOUNT-DISPLAY " " 
                               STATUS-MSG
               END-READ
           END-PERFORM

           CLOSE TRANSACTION-FILE
           STOP RUN.
