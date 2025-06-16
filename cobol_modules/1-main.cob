       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE        PIC 9.
       01  WS-NUM1          PIC S9(5)V99.
       01  WS-NUM2          PIC S9(5)V99.
       01  WS-RESULT        PIC S9(6)V99.
       01  WS-VALID         PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-CHOICE = 5
               DISPLAY "========== SIMPLE CALCULATOR =========="
               DISPLAY "1. Addition"
               DISPLAY "2. Subtraction"
               DISPLAY "3. Multiplication"
               DISPLAY "4. Division"
               DISPLAY "5. Exit"
               DISPLAY "Enter your choice (1-5): "
               ACCEPT WS-CHOICE
       
               IF WS-CHOICE >= 1 AND WS-CHOICE <= 4
                   DISPLAY "Enter first number: "
                   ACCEPT WS-NUM1
                   DISPLAY "Enter second number: "
                   ACCEPT WS-NUM2
       
                   EVALUATE WS-CHOICE
                       WHEN 1
                           CALL 'ADD-NUMS' USING WS-NUM1, WS-NUM2, 
                           WS-RESULT
                           DISPLAY "Result: " WS-NUM1 " + " WS-NUM2 
                           " = " WS-RESULT
                       WHEN 2
                           CALL 'SUB-NUMS' USING WS-NUM1, WS-NUM2, 
                           WS-RESULT
                           DISPLAY "Result: " WS-NUM1 " - " WS-NUM2 
                           " = " WS-RESULT
                       WHEN 3
                           CALL 'MULT-NUMS' USING WS-NUM1, WS-NUM2, 
                           WS-RESULT
                           DISPLAY "Result: " WS-NUM1 " * " WS-NUM2 
                           " = " WS-RESULT
                       WHEN 4
                           IF WS-NUM2 = 0
                               DISPLAY "Error: Cannot divide by zero."
                           ELSE
                               CALL 'DIV-NUMS' USING WS-NUM1, WS-NUM2, 
                               WS-RESULT
                               DISPLAY "Result: " WS-NUM1 " / " WS-NUM2 
                               " = " WS-RESULT
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM.
           STOP RUN.
       