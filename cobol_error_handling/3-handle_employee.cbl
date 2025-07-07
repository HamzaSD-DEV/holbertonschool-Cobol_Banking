       IDENTIFICATION DIVISION.
       PROGRAM-ID. HANDLE-EMPLOYEE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEES.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS EMP-STATUS.
           SELECT LOG-FILE ASSIGN TO "ERRORS.LOG"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID         PIC X(5).
           05 EMP-NAME       PIC X(20).
           05 EMP-SALARY-TX  PIC X(8).

       FD LOG-FILE.
       01 LOG-RECORD        PIC X(100).

       WORKING-STORAGE SECTION.
       01 EMP-STATUS        PIC XX.
       01 LOG-STATUS        PIC XX.
       01 WS-EMP-ID         PIC X(5).
       01 WS-BONUS-TX       PIC X(10).
       01 WS-FOUND-FLAG     PIC X VALUE 'N'.
       01 WS-END-FLAG       PIC X VALUE 'N'.
       01 WS-SALARY-NUM     PIC 9(5)V99.
       01 WS-BONUS-NUM      PIC 9(5)V99.
       01 WS-NEW-SALARY-DSP PIC Z(5)9.99.
       01 WS-NEW-SALARY-TX  PIC 9(5).99.
       01 WS-DATE           PIC 9(8).
       01 WS-TIME           PIC 9(8).
       01 WS-TIMESTAMP      PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter Employee ID: "
           ACCEPT WS-EMP-ID
           DISPLAY "[DEBUG] Searching for Employee ID: " WS-EMP-ID

           *> Open EMPLOYEES.DAT for read/write
           OPEN I-O EMPLOYEE-FILE

           *> Open LOG-FILE for append (create if missing)
           OPEN EXTEND LOG-FILE
           IF LOG-STATUS = "35"
               CLOSE LOG-FILE
               OPEN OUTPUT LOG-FILE
               CLOSE LOG-FILE
               OPEN EXTEND LOG-FILE
           END-IF

           *> Search for the record
           PERFORM UNTIL WS-END-FLAG = 'Y' OR WS-FOUND-FLAG = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-END-FLAG
                   NOT AT END
                       IF EMP-ID = WS-EMP-ID
                           MOVE 'Y' TO WS-FOUND-FLAG
                           COMPUTE WS-SALARY-NUM = FUNCTION 
                           NUMVAL(EMP-SALARY-TX)
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND-FLAG = 'Y'
               DISPLAY "[DEBUG] Found: " FUNCTION TRIM(EMP-NAME)
                       " with salary " EMP-SALARY-TX
               DISPLAY "Enter Bonus Amount: "
               ACCEPT WS-BONUS-TX
               COMPUTE WS-BONUS-NUM = FUNCTION NUMVAL(WS-BONUS-TX)

               COMPUTE WS-SALARY-NUM = WS-SALARY-NUM + WS-BONUS-NUM
                   ON SIZE ERROR
                       PERFORM LOG-OVERFLOW
                       DISPLAY "Error: Bonus too large. Salary update fa
      -                "iled due to overflow."
                   NOT ON SIZE ERROR
                       MOVE WS-SALARY-NUM TO WS-NEW-SALARY-DSP
                       DISPLAY "Updated Salary for " FUNCTION 
                       TRIM(EMP-NAME)
                               ": " FUNCTION TRIM(WS-NEW-SALARY-DSP)
                       MOVE WS-SALARY-NUM TO WS-NEW-SALARY-TX
                       MOVE WS-NEW-SALARY-TX TO EMP-SALARY-TX
                       REWRITE EMPLOYEE-RECORD
               END-COMPUTE
           ELSE
               DISPLAY "[DEBUG] Employee ID not found: " WS-EMP-ID
               PERFORM LOG-NOT-FOUND
               DISPLAY "Error: Employee ID not found."
           END-IF

           CLOSE EMPLOYEE-FILE
           CLOSE LOG-FILE
           STOP RUN.

       LOG-OVERFLOW.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-RECORD
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Bonus too large for Employee ID "
               WS-EMP-ID
               ". Salary update failed due to overflow."
               DELIMITED BY SIZE
               INTO LOG-RECORD
           END-STRING
           WRITE LOG-RECORD AFTER ADVANCING 1 LINES
           .

       LOG-NOT-FOUND.
           PERFORM GET-TIMESTAMP
           MOVE SPACES TO LOG-RECORD
           STRING
               WS-TIMESTAMP DELIMITED BY SIZE
               " - ERROR: Employee ID "
               WS-EMP-ID
               " not found in EMPLOYEES.DAT."
               DELIMITED BY SIZE
               INTO LOG-RECORD
           END-STRING
           WRITE LOG-RECORD AFTER ADVANCING 1 LINES
           .

       GET-TIMESTAMP.
           ACCEPT WS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           STRING
               WS-DATE(1:4) "-" WS-DATE(5:2) "-" WS-DATE(7:2) " "
               WS-TIME(1:2) ":" WS-TIME(3:2) ":" WS-TIME(5:2)
               DELIMITED BY SIZE
               INTO WS-TIMESTAMP
           END-STRING
           .
