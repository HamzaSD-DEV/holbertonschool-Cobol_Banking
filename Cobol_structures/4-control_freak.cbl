       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTROL-FREAK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 BALANCE         PIC 9(5)V99 VALUE 1000.00.
       01 AMOUNT          PIC 9(5)V99.
       01 DISPLAY-BAL     PIC Z,ZZZ.99.
       01 DISPLAY-AMOUNT  PIC Z,ZZZ.99.

       01 USER-CHOICE     PIC X.
       01 EXIT-FLAG       PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-LOOP.
           PERFORM UNTIL EXIT-FLAG = "Y"
               DISPLAY "Enter operation [D=Deposit, W=Withdraw, B=Balanc
      -        "e, Q=Quit]: "
               ACCEPT USER-CHOICE

               EVALUATE USER-CHOICE
                   WHEN "D"
                       DISPLAY "Enter deposit amount: "
                       ACCEPT AMOUNT
                       ADD AMOUNT TO BALANCE
                       DISPLAY "Deposit successful."
                   WHEN "W"
                       DISPLAY "Enter withdrawal amount: "
                       ACCEPT AMOUNT
                       IF AMOUNT > BALANCE
                           DISPLAY "Insufficient funds."
                       ELSE
                           SUBTRACT AMOUNT FROM BALANCE
                           DISPLAY "Withdrawal successful."
                       END-IF
                   WHEN "B"
                       MOVE BALANCE TO DISPLAY-BAL
                       DISPLAY "Current Balance: " DISPLAY-BAL
                   WHEN "Q"
                       DISPLAY "Goodbye!"
                       MOVE "Y" TO EXIT-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid option. Please try again."
               END-EVALUATE

               DISPLAY SPACE
           END-PERFORM

           STOP RUN.
