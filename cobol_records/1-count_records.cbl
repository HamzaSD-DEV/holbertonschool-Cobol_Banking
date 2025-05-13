       IDENTIFICATION DIVISION.
       PROGRAM-ID. CountIndexedRecords.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCOUNT-KEY
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE.
       01  ACCOUNT-RECORD.
           05 ACCOUNT-KEY       PIC 9(5).
           05 FNAME             PIC X(10).
           05 LNAME             PIC X(10).
           05 BALANCE           PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS         PIC XX.
       01  RECORD-COUNT        PIC 9(4) VALUE ZERO.
       01  RECORD-COUNT-DISP        PIC ZZZ9.
       01  EOF-FLAG            PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT ACCOUNTS-FILE

           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACCOUNTS-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO RECORD-COUNT
               END-READ
           END-PERFORM

           CLOSE ACCOUNTS-FILE
           MOVE RECORD-COUNT TO RECORD-COUNT-DISP

           DISPLAY "Total number of customer records: " 
           RECORD-COUNT-DISP

           STOP RUN.
