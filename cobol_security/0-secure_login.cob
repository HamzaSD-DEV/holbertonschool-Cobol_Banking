       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECURE-LOGIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "BASIC_USERS.DAT"
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

       01 INPUT-USER-ID       PIC X(7).
       01 INPUT-PASSWORD      PIC X(7).

       01 FOUND-FLAG          PIC X VALUE 'N'.

       PROCEDURE DIVISION.

            DISPLAY "BASIC BANKING LOGIN SYSTEM"
            DISPLAY "=========================="
     
            DISPLAY "Enter User ID: " WITH NO ADVANCING
            ACCEPT INPUT-USER-ID
     
            DISPLAY "Enter Password: " WITH NO ADVANCING
            ACCEPT INPUT-PASSWORD
     
            OPEN INPUT USER-FILE
     
            IF WS-FILE-STATUS NOT = "00"
                DISPLAY "Error: BASIC_USERS.DAT not found."
                STOP RUN
            END-IF
     
            PERFORM UNTIL WS-EOF = 'Y'
                READ USER-FILE
                    AT END
                        MOVE 'Y' TO WS-EOF
                    NOT AT END
                        IF USER-ID = INPUT-USER-ID
                            MOVE 'Y' TO FOUND-FLAG
                            IF USER-PASSWORD = INPUT-PASSWORD
                                DISPLAY "Welcome, " FUNCTION 
                                TRIM(USER-NAME)
                                ". Access level: " USER-ACCESS-LEVEL

                            ELSE
                                DISPLAY "Invalid password. Authenticatio
      -                         "n failed."
                            END-IF
                            MOVE 'Y' TO WS-EOF
                        END-IF
                END-READ
            END-PERFORM
     
            IF FOUND-FLAG = 'N'
                DISPLAY "User not found."
            END-IF
     
            CLOSE USER-FILE
     
            STOP RUN.
     