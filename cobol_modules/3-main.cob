       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME       PIC X(20).
       01  WS-BASIC      PIC 9(5)V99.
       01  WS-ALLOW      PIC 9(4)V99.
       01  WS-DEDUCT     PIC 9(4)V99.
       01  WS-GROSS      PIC 9(6)V99.
       01  WS-NET        PIC 9(6)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "===== MODULAR SALARY SLIP GENERATOR ====="

           CALL 'READ-EMPLOYEE'
               USING WS-NAME, WS-BASIC, WS-ALLOW, WS-DEDUCT

           CALL 'CALC-GROSS'
               USING WS-BASIC, WS-ALLOW, WS-GROSS

           CALL 'CALC-NET'
               USING WS-GROSS, WS-DEDUCT, WS-NET

           CALL 'DISPLAY-SLIP'
               USING WS-NAME, WS-BASIC, WS-ALLOW, WS-DEDUCT, WS-GROSS, 
               WS-NET

           STOP RUN.
