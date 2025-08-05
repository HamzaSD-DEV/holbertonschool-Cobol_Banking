       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECURE-TOKEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOGIN-FILE
             ASSIGN TO "users-login.idx"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS L-USERID
             FILE STATUS IS WS-FS-LOGIN.
           SELECT DETAIL-FILE
             ASSIGN TO "users-details.dat"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LOGIN-FILE.
       01  LOGIN-REC.
           05  L-USERID       PIC X(8).
           05  L-USERNAME     PIC X(15).
           05  L-PASS-ENC     PIC X(15).
           05  SAVED-TOKEN    PIC X(16).
           05  TOKEN-CR-TIME  PIC X(6).

       FD  DETAIL-FILE.
       01  DETAIL-REC.
           05  D-USERID       PIC X(8).
           05  D-FIRST        PIC X(15).
           05  D-LAST         PIC X(15).
           05  D-DOB          PIC X(8).
           05  D-BAL-TXT      PIC X(12).
           05  D-STATUS       PIC X(9).

       WORKING-STORAGE SECTION.
       77  WS-FS-LOGIN     PIC XX.
       77  ACCESS-CHOICE   PIC X.
       77  WS-INPUT-USER   PIC X(15).
       77  WS-INPUT-PASS   PIC X(15).
       77  WS-ENC-PASS     PIC X(15).
       77  WS-FOUND        PIC X VALUE 'N'.
       77  WS-LOGIN-ID     PIC X(8).
       77  SESSION-TOKEN   PIC X(16).
       77  RANDOM-PART     PIC X(8).
       77  IDX             PIC 9(2).
       77  POSI            PIC 9(2).
       77  LEN             PIC 9(2).
       77  DUMMY-INT       PIC 9(2).
       77  WS-TIME         PIC X(6).
       77  TIME-VAL        PIC 9(6).
       77  WS-CMD          PIC X.
       77  WS-TOK-ENTER    PIC X(16).
       77  TOKEN-SECONDS   PIC 9(7).
       77  CURRENT-SECONDS PIC 9(7).
       77  WS-TOK-AGE      PIC 9(6).
       77  WS-TIK-ISVALID  PIC X VALUE 'N'.
       77  EOF-DETAIL      PIC X VALUE 'N'.
    

       01  UPPER-TABLE.
           05 U-CHARS     PIC X(36)
             VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Option (L=Login, T=Access-with-token): " WITH NO 
           ADVANCING
           ACCEPT ACCESS-CHOICE
           EVALUATE FUNCTION UPPER-CASE(ACCESS-CHOICE)
             WHEN 'L'
               OPEN I-O LOGIN-FILE
               PERFORM LOGIN-PHASE
               CLOSE LOGIN-FILE
               IF WS-FOUND = 'Y'
                  DISPLAY "Your token is: " SESSION-TOKEN
               END-IF
             WHEN 'T'
               OPEN INPUT LOGIN-FILE
               PERFORM ACCESS-PHASE
               CLOSE LOGIN-FILE
             WHEN OTHER
               DISPLAY "Invalid choice"
           END-EVALUATE
           STOP RUN.

       *>──────────────────────────────────────────────────────
       LOGIN-PHASE.
           DISPLAY "Username: " WITH NO ADVANCING
           ACCEPT WS-INPUT-USER
           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT WS-INPUT-PASS

           *> Encrypt password
           MOVE SPACES TO WS-ENC-PASS
           COMPUTE LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-PASS))
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LEN
             MOVE 0 TO POSI
             PERFORM VARYING POSI FROM 1 BY 1 UNTIL POSI > 36
               IF U-CHARS(POSI:1) = WS-INPUT-PASS(IDX:1)
                  EXIT PERFORM
               END-IF
             END-PERFORM
             IF POSI = 0
               MOVE WS-INPUT-PASS(IDX:1) TO WS-ENC-PASS(IDX:1)
             ELSE
               ADD IDX TO POSI
               IF POSI > 36
                 SUBTRACT 36 FROM POSI
               END-IF
               MOVE U-CHARS(POSI:1) TO WS-ENC-PASS(IDX:1)
             END-IF
           END-PERFORM
           *> Sequential scan for username match
           MOVE 'N' TO WS-FOUND
           READ LOGIN-FILE
           END-READ
           PERFORM UNTIL WS-FOUND = 'Y' OR WS-FS-LOGIN = "Y"
             READ LOGIN-FILE
               AT END
                 MOVE "Y" TO WS-FS-LOGIN
                 IF WS-FOUND = "N"
                   DISPLAY  "Invalid credentials."
                 END-IF
               NOT AT END
                 IF FUNCTION TRIM(L-USERNAME) = FUNCTION 
                 TRIM(WS-INPUT-USER)
                    IF FUNCTION TRIM(L-PASS-ENC) = FUNCTION 
                    TRIM(WS-ENC-PASS)
                       MOVE 'Y' TO WS-FOUND
                       MOVE L-USERID TO WS-LOGIN-ID
                       *> generate & store
                       PERFORM GENERATE-TOKEN
                       MOVE SESSION-TOKEN TO SAVED-TOKEN
                       ACCEPT WS-TIME FROM TIME
                       MOVE WS-TIME TO TOKEN-CR-TIME
                       REWRITE LOGIN-REC
                       DISPLAY "Login successful."
                    END-IF
                 END-IF
             END-READ
           END-PERFORM.


       *>──────────────────────────────────────────────────────
       GENERATE-TOKEN.
           *> Build random 8‐char suffix from clock
           ACCEPT WS-TIME FROM TIME
           MOVE FUNCTION NUMVAL(WS-TIME) TO TIME-VAL
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8
             COMPUTE POSI = TIME-VAL + (IDX * 7)
             DIVIDE POSI BY 36 GIVING DUMMY-INT REMAINDER POSI
             IF POSI = 0
               MOVE U-CHARS(36:1) TO RANDOM-PART(IDX:1)
             ELSE
               MOVE U-CHARS(POSI:1) TO RANDOM-PART(IDX:1)
             END-IF
           END-PERFORM
           MOVE WS-LOGIN-ID TO SESSION-TOKEN(1:8)
           MOVE RANDOM-PART  TO SESSION-TOKEN(9:8).

       *>──────────────────────────────────────────────────────
       ACCESS-PHASE.
           DISPLAY "Option (U=User, A=Account, E=Exit): " WITH NO 
           ADVANCING
           ACCEPT WS-CMD
           IF FUNCTION UPPER-CASE(WS-CMD) = 'E'
             STOP RUN
           END-IF

           DISPLAY "Enter token: " WITH NO ADVANCING
           ACCEPT WS-TOK-ENTER

           *> normalize token and extract key
           MOVE WS-TOK-ENTER(1:8) TO L-USERID

           *> lookup by USERID key
           READ LOGIN-FILE KEY IS L-USERID
             INVALID KEY
               DISPLAY "Security warning: invalid token."
               STOP RUN
             NOT INVALID KEY
               IF WS-TOK-ENTER = SAVED-TOKEN
                  *> check age
                  
                  COMPUTE TOKEN-SECONDS = 
                         FUNCTION NUMVAL(TOKEN-CR-TIME(1:2)) * 3600 +
                         FUNCTION NUMVAL(TOKEN-CR-TIME(3:2)) * 60 +
                         FUNCTION NUMVAL(TOKEN-CR-TIME(5:2))
                  ACCEPT WS-TIME FROM TIME
                  COMPUTE CURRENT-SECONDS = 
                         FUNCTION NUMVAL(WS-TIME(1:2)) * 3600 +
                         FUNCTION NUMVAL(WS-TIME(3:2)) * 60 +
                         FUNCTION NUMVAL(WS-TIME(5:2))
                  
                  COMPUTE WS-TOK-AGE = CURRENT-SECONDS - TOKEN-SECONDS
                  IF WS-TOK-AGE < 5
                     MOVE L-USERID TO WS-LOGIN-ID
                     IF FUNCTION UPPER-CASE(WS-CMD) = 'U'
                       DISPLAY "shoing user : "
                       PERFORM SHOW-USER
                     ELSE
                       PERFORM SHOW-ACCOUNT
                     END-IF
                  ELSE
                    DISPLAY "Security warning: token expired."
                  END-IF
               ELSE
                 DISPLAY "Security warning: invalid token."
               END-IF
           END-READ.

       *>──────────────────────────────────────────────────────
       SHOW-USER.
           OPEN INPUT DETAIL-FILE
           MOVE 'N' TO EOF-DETAIL
           PERFORM UNTIL EOF-DETAIL = 'Y'
             READ DETAIL-FILE
               AT END MOVE 'Y' TO EOF-DETAIL
               NOT AT END
                 IF FUNCTION TRIM(D-USERID) = WS-LOGIN-ID
                    DISPLAY "Name: " FUNCTION TRIM(D-FIRST)
                            " " FUNCTION TRIM(D-LAST)
                    DISPLAY "DOB : " D-DOB(7:2) "/" D-DOB(5:2) "/" 
                    D-DOB(1:4)
                    EXIT PERFORM
                 END-IF
             END-READ
           END-PERFORM
           CLOSE DETAIL-FILE.

       SHOW-ACCOUNT.
           OPEN INPUT DETAIL-FILE
           MOVE 'N' TO EOF-DETAIL
           PERFORM UNTIL EOF-DETAIL = 'Y'
             READ DETAIL-FILE
               AT END MOVE 'Y' TO EOF-DETAIL
               NOT AT END
                 IF FUNCTION TRIM(D-USERID) = WS-LOGIN-ID
                    DISPLAY "Balance: $" D-BAL-TXT
                    DISPLAY "Status : " FUNCTION TRIM(D-STATUS)
                    EXIT PERFORM
                 END-IF
             END-READ
           END-PERFORM
           CLOSE DETAIL-FILE.

       END PROGRAM SECURE-TOKEN.
