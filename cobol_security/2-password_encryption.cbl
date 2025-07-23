       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENCRYPTED-LOGIN.

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
           05 USER-ID           PIC X(7).
           05 USER-NAME         PIC X(20).
           05 USER-PASSWORD     PIC X(7).
           05 USER-ACCESS-LEVEL PIC 9.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS       PIC XX.
       01 WS-EOF               PIC X VALUE 'N'.

       01 INPUT-USER-ID        PIC X(7).
       01 INPUT-PASSWORD       PIC X(7).
       01 ENCRYPTED-PASSWORD   PIC X(7).

       01 ATTEMPTS-LEFT        PIC 9 VALUE 3.
       01 FOUND-FLAG           PIC X VALUE 'N'.

       01 I                    PIC 9(2).
       01 CHAR                 PIC X.
       01 ASCII-VALUE          PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL ATTEMPTS-LEFT = 0 OR FOUND-FLAG = 'Y'
               DISPLAY "BANKING LOGIN SYSTEM"
               DISPLAY "====================="
               DISPLAY "Enter User ID: "
               ACCEPT INPUT-USER-ID
               DISPLAY "Enter Password: "
               ACCEPT INPUT-PASSWORD

               *> Encrypt the entered password
               MOVE SPACES TO ENCRYPTED-PASSWORD
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7
                   MOVE INPUT-PASSWORD(I:1) TO CHAR
                   IF CHAR NOT = SPACE
                       COMPUTE ASCII-VALUE = FUNCTION ORD(CHAR) + 3
                       IF ASCII-VALUE > 90 AND ASCII-VALUE < 97
                           SUBTRACT 26 FROM ASCII-VALUE
                       ELSE
                           IF ASCII-VALUE > 122
                               SUBTRACT 26 FROM ASCII-VALUE
                           END-IF
                       END-IF
                       MOVE FUNCTION CHAR(ASCII-VALUE)
                            TO ENCRYPTED-PASSWORD(I:1)
                   END-IF
               END-PERFORM

               *> Open and search file
               OPEN INPUT USER-FILE
               MOVE 'N' TO WS-EOF
               MOVE 'N' TO FOUND-FLAG

               PERFORM UNTIL WS-EOF = 'Y' OR FOUND-FLAG = 'Y'
                   READ USER-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF USER-ID = INPUT-USER-ID
                               IF USER-PASSWORD = ENCRYPTED-PASSWORD
                                   DISPLAY "Login successful!"
                                   DISPLAY "Welcome, " FUNCTION 
                                   TRIM(USER-NAME)
                                   DISPLAY "Access level: " 
                                   USER-ACCESS-LEVEL
                                   MOVE 'Y' TO FOUND-FLAG
                               ELSE
                                   DISPLAY "Invalid password."
                               END-IF
                               MOVE 'Y' TO WS-EOF
                           END-IF
                   END-READ
               END-PERFORM

               CLOSE USER-FILE

               IF FOUND-FLAG = 'N'
                   DISPLAY "User not found."
                   SUBTRACT 1 FROM ATTEMPTS-LEFT
                   IF ATTEMPTS-LEFT > 0
                       DISPLAY "Attempts remaining: " ATTEMPTS-LEFT
                       DISPLAY "Press Enter to continue..."
                       ACCEPT OMITTED
                   ELSE
                       DISPLAY "Too many failed attempts. Exiting..."
                   END-IF
               END-IF
           END-PERFORM

           STOP RUN.
