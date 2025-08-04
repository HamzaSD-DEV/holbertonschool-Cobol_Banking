       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUMP-LOGIN.
       AUTHOR. ChatGPT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOGIN-FILE
             ASSIGN TO "users-login.idx"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS L-USERID
             FILE STATUS IS WS-FS-LOGIN.

       DATA DIVISION.
       FILE SECTION.
       FD  LOGIN-FILE.
       01  LOGIN-REC.
           05  L-USERID       PIC X(8).
           05  L-USERNAME     PIC X(15).
           05  L-PASS-ENC     PIC X(15).
           05  SAVED-TOKEN    PIC X(16).
           05  TOKEN-CR-TIME  PIC X(6).

       WORKING-STORAGE SECTION.
       77  WS-FS-LOGIN     PIC XX.
       77  WS-EOF          PIC X    VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT LOGIN-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ LOGIN-FILE NEXT
                 AT END
                    MOVE 'Y' TO WS-EOF
                 NOT AT END
                    IF L-USERID = "USER0030"
                       DISPLAY SAVED-TOKEN
                    END-IF
               END-READ
           END-PERFORM
           CLOSE LOGIN-FILE
           STOP RUN.
