       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERFLOW-HANDLING-TASK1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID-TEXT       PIC X(5).    *> "10002"
           05 EMP-NAME          PIC X(20).   *> "Alice Johnson      "
           05 EMP-SALARY-TEXT   PIC X(8).    *> "09999.99"

       WORKING-STORAGE SECTION.
       01 WS-INPUT-ID-TEXT       PIC X(5).
       01 WS-INPUT-BONUS-TEXT    PIC X(10).
       01 WS-FOUND-FLAG          PIC X VALUE 'N'.
       01 WS-END-FLAG            PIC X VALUE 'N'.
       01 FILE-STATUS            PIC XX.
       *> Now only 4 integer digits + 2 decimals
       01 WS-SALARY-NUM       PIC 9(5)V99.
       01 WS-BONUS-NUM        PIC 9(5)V99.
       01 WS-NEW-SALARY-DSP   PIC Z(5)9.99.


       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> 1) Read inputs
           DISPLAY "Enter Employee ID (5 digits): "
           ACCEPT WS-INPUT-ID-TEXT

           DISPLAY "Enter Bonus Amount (e.g. 500.00): "
           ACCEPT WS-INPUT-BONUS-TEXT

           *> 2) Convert bonus to 4V99 numeric
           COMPUTE WS-BONUS-NUM = FUNCTION NUMVAL(WS-INPUT-BONUS-TEXT)

           OPEN INPUT EMPLOYEE-FILE

           *> 3) Search
           PERFORM UNTIL WS-FOUND-FLAG = 'Y' OR WS-END-FLAG = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-END-FLAG
                   NOT AT END
                       IF EMP-ID-TEXT = WS-INPUT-ID-TEXT
                           MOVE 'Y' TO WS-FOUND-FLAG
                           COMPUTE WS-SALARY-NUM = FUNCTION 
                           NUMVAL(EMP-SALARY-TEXT)
                       END-IF
               END-READ
           END-PERFORM

           *> 4) Compute + check overflow
           IF WS-FOUND-FLAG = 'Y'
               COMPUTE WS-SALARY-NUM = WS-SALARY-NUM + WS-BONUS-NUM
                   ON SIZE ERROR
                       DISPLAY "Error: Bonus too large. Salary update fa
      -                 "iled due to overflow."
                   NOT ON SIZE ERROR
                       MOVE WS-SALARY-NUM TO WS-NEW-SALARY-DSP
                       DISPLAY "Updated Salary for " EMP-NAME 
                                WS-NEW-SALARY-DSP ": $"
               END-COMPUTE
           ELSE
               DISPLAY "Error: Employee ID not found."
           END-IF

           CLOSE EMPLOYEE-FILE
           STOP RUN.
