       IDENTIFICATION DIVISION.
       PROGRAM-ID. MASKED-LOGIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD USER-FILE.
       01 USER-RECORD.
           05 FILE-USER-ID      PIC X(8).
           05 FILE-USER-NAME    PIC X(20).
           05 FILE-PASSWORD     PIC X(8).
           05 FILE-ACCESS-LEVEL PIC 9.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS        PIC XX.
       01 WS-USER-ID            PIC X(8).
       01 WS-PASSWORD           PIC X(8).
       01 WS-ATTEMPTS-LEFT      PIC 9 VALUE 3.
       01 WS-FOUND              PIC X VALUE 'N'.
       01 WS-PASSWORD-OK        PIC X VALUE 'N'.
       01 WS-DUMMY              PIC X.

       01 WS-EXIT-FLAG          PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "BANKING LOGIN SYSTEM"
           DISPLAY "===================="

           OPEN INPUT USER-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "User file not found. Program ending."
               STOP RUN
           END-IF

           PERFORM UNTIL WS-ATTEMPTS-LEFT = 0 OR WS-PASSWORD-OK = 'Y'
               DISPLAY "Enter User ID: " WITH NO ADVANCING
               ACCEPT WS-USER-ID

               DISPLAY "Enter Password: " WITH NO ADVANCING
               ACCEPT WS-PASSWORD

               PERFORM SEARCH-USER

               IF WS-FOUND = 'Y'
                   IF WS-PASSWORD-OK = 'Y'
                       DISPLAY ""
                       DISPLAY "Login successful!"
                       DISPLAY "Welcome, " FILE-USER-NAME
                       DISPLAY "Access level: " FILE-ACCESS-LEVEL
                       MOVE 'Y' TO WS-EXIT-FLAG
                       EXIT PERFORM
                   ELSE
                       DISPLAY ""
                       DISPLAY "Attempts remaining: " WS-ATTEMPTS-LEFT
                       DISPLAY 
                       "Invalid password. Authentication failed."
                       SUBTRACT 1 FROM WS-ATTEMPTS-LEFT
                       DISPLAY "Press Enter to continue..." WITH NO 
                       ADVANCING
                       ACCEPT WS-DUMMY
                   END-IF
               ELSE
                   DISPLAY ""
                   DISPLAY "Attempts remaining: " WS-ATTEMPTS-LEFT
                   DISPLAY "User not found."
                   SUBTRACT 1 FROM WS-ATTEMPTS-LEFT
                   DISPLAY "Press Enter to continue..." WITH NO 
                   ADVANCING
                   ACCEPT WS-DUMMY
               END-IF
           END-PERFORM

           CLOSE USER-FILE
           STOP RUN.

       SEARCH-USER.
           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-PASSWORD-OK
           OPEN INPUT USER-FILE
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ USER-FILE
                   AT END
                       EXIT PERFORM
               END-READ
               IF FILE-USER-ID = WS-USER-ID
                   MOVE 'Y' TO WS-FOUND
                   IF FILE-PASSWORD = WS-PASSWORD
                       MOVE 'Y' TO WS-PASSWORD-OK
                   END-IF
                   EXIT PERFORM
               END-IF
           END-PERFORM.
