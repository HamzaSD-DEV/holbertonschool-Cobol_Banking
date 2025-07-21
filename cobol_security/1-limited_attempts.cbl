       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIMITED-ATTEMPTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "USERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD USER-FILE.
       01 USER-RECORD.
           05 USER-ID          PIC X(7).
           05 USER-NAME        PIC X(20).
           05 USER-PASSWORD    PIC X(7).
           05 USER-ACCESS-LEVEL PIC 9.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS      PIC XX.
       01 WS-EOF              PIC X VALUE 'N'.

       01 WS-INPUT-ID         PIC X(7).
       01 WS-INPUT-PASS       PIC X(7).

       01 WS-FOUND-FLAG       PIC X VALUE 'N'.
       01 WS-PASS-FLAG        PIC X VALUE 'N'.

       01 WS-ATTEMPTS         PIC 9 VALUE 3.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-ATTEMPTS = 0 OR WS-PASS-FLAG = 'Y'
               *> Display header
               DISPLAY "BANKING LOGIN SYSTEM"
               DISPLAY "====================="

               *> Read credentials
               DISPLAY "Enter User ID: " WITH NO ADVANCING
               ACCEPT WS-INPUT-ID
               DISPLAY "Enter Password: " WITH NO ADVANCING
               ACCEPT WS-INPUT-PASS

               *> Reset flags and EOF
               MOVE 'N' TO WS-FOUND-FLAG
               MOVE 'N' TO WS-PASS-FLAG
               MOVE 'N' TO WS-EOF

               *> Open and search
               OPEN INPUT USER-FILE
               IF WS-FILE-STATUS NOT = "00"
                   DISPLAY "Error: USERS.DAT not found."
                   STOP RUN
               END-IF

               PERFORM UNTIL WS-EOF = 'Y'
                   READ USER-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF USER-ID = WS-INPUT-ID
                               MOVE 'Y' TO WS-FOUND-FLAG
                               IF USER-PASSWORD = WS-INPUT-PASS
                                   MOVE 'Y' TO WS-PASS-FLAG
                                   DISPLAY "Login successful!"
                                   DISPLAY "Welcome, " FUNCTION 
                                   TRIM(USER-NAME)
                                   DISPLAY "Access level: " 
                                   USER-ACCESS-LEVEL
                               ELSE
                                   
                                   DISPLAY "Invalid password. Authentica
      -                            "tion failed."
                               END-IF
                               MOVE 'Y' TO WS-EOF
                           END-IF
                   END-READ
               END-PERFORM

               CLOSE USER-FILE

               *> If not successful, decrement attempts
               IF WS-PASS-FLAG NOT = 'Y'
                   SUBTRACT 1 FROM WS-ATTEMPTS
                   IF WS-FOUND-FLAG = 'N'
                       
                       DISPLAY "User not found."
                   END-IF
                   IF WS-ATTEMPTS > 0
                       DISPLAY "Attempts remaining: " WS-ATTEMPTS
                       DISPLAY "Press Enter to continue..." WITH NO 
                       ADVANCING
                       ACCEPT WS-INPUT-ID
                   ELSE
                       
                       DISPLAY "Too many failed attempts. Exiting..."
                   END-IF
               END-IF

               
           END-PERFORM

           STOP RUN.
