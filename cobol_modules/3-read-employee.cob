       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-EMPLOYEE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IDX  PIC 9 VALUE 1.

       *> Temporary strings for numeric input
       01  WS-BASIC-STR    PIC X(12).  
       01  WS-ALLOW-STR    PIC X(12).
       01  WS-DEDUCT-STR   PIC X(12).

       LINKAGE SECTION.
       01  EMP-NAME       PIC X(20).
       01  BASIC-SALARY   PIC 9(5)V99.
       01  ALLOWANCE      PIC 9(4)V99.
       01  DEDUCTION      PIC 9(4)V99.

       PROCEDURE DIVISION 
       USING EMP-NAME, BASIC-SALARY, ALLOWANCE, DEDUCTION.
       READ-EMP-PARA.
           *> Read Employee Name
           DISPLAY "Enter Employee Name: " WITH NO ADVANCING
           ACCEPT EMP-NAME

           *> Read Basic Salary as string, then convert via NUMVAL
           DISPLAY "Enter Basic Salary: " WITH NO ADVANCING
           ACCEPT WS-BASIC-STR
           *> Strip leading/trailing spaces: NUMVAL ignores leading spaces
           IF FUNCTION NUMVAL(WS-BASIC-STR) >= 0
               MOVE FUNCTION NUMVAL(WS-BASIC-STR) TO BASIC-SALARY
           ELSE
               DISPLAY 
               "  >> Warning: invalid Basic Salary, defaulting to 0"
               MOVE 0 TO BASIC-SALARY
           END-IF

           *> Read Allowance as string, then convert
           DISPLAY "Enter Allowance: " WITH NO ADVANCING
           ACCEPT WS-ALLOW-STR
           IF FUNCTION NUMVAL(WS-ALLOW-STR) >= 0
               MOVE FUNCTION NUMVAL(WS-ALLOW-STR) TO ALLOWANCE
           ELSE
               DISPLAY 
               "  >> Warning: invalid Allowance, defaulting to 0"
               MOVE 0 TO ALLOWANCE
           END-IF

           *> Read Deductions as string, then convert
           DISPLAY "Enter Deductions: " WITH NO ADVANCING
           ACCEPT WS-DEDUCT-STR
           IF FUNCTION NUMVAL(WS-DEDUCT-STR) >= 0
               MOVE FUNCTION NUMVAL(WS-DEDUCT-STR) TO DEDUCTION
           ELSE
               DISPLAY 
               "  >> Warning: invalid Deductions, defaulting to 0"
               MOVE 0 TO DEDUCTION
           END-IF

           GOBACK.
