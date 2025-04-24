       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTION-HISTORY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-ID         PIC X(9).
       01 HEAD-LINE           PIC X(52) VALUE ALL "-".

       01 TRANSACTIONS.
          05 TX-DATE        OCCURS 5 TIMES PIC X(10).
          05 TX-DESC        OCCURS 5 TIMES PIC X(20).
          05 TX-AMOUNT      OCCURS 5 TIMES PIC S9(5)V99 COMP-3.
          05 TX-BALANCE     OCCURS 5 TIMES PIC S9(5)V99 COMP-3.

       01 DISPLAY-AMOUNT     PIC +Z,ZZZ.99.
       01 DISPLAY-BALANCE    PIC Z,ZZZ.99.

       01 I                 PIC 9 VALUE 1.
       01 CURRENT-BALANCE   PIC S9(5)V99 COMP-3 VALUE 0.
       01 USER-AMOUNT       PIC S9(5)V99.
       01 TEMP-AMOUNT       PIC S9(5)V99 COMP-3.

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Enter Customer ID (9 characters): "
           ACCEPT CUSTOMER-ID

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY "Enter transaction " I " date (DD/MM/YYYY): "
               ACCEPT TX-DATE(I)

               DISPLAY "Enter transaction " I " description: "
               ACCEPT TX-DESC(I)

               DISPLAY "Enter transaction " I " amount: "
               ACCEPT USER-AMOUNT
               MOVE USER-AMOUNT TO TEMP-AMOUNT
               MOVE TEMP-AMOUNT TO TX-AMOUNT(I)
           END-PERFORM

           PERFORM CALCULATE-BALANCES

           DISPLAY HEAD-LINE
           DISPLAY "      CUSTOMER TRANSACTION HISTORY"
           DISPLAY "      Customer ID : " CUSTOMER-ID(1:8)
           DISPLAY HEAD-LINE
           DISPLAY 
           "Date       Description          Amount       Balance"
           DISPLAY HEAD-LINE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE TX-AMOUNT(I) TO DISPLAY-AMOUNT
               MOVE TX-BALANCE(I) TO DISPLAY-BALANCE
               DISPLAY TX-DATE(I) SPACE
                       TX-DESC(I) SPACE
                       DISPLAY-AMOUNT SPACE
                       DISPLAY-BALANCE
           END-PERFORM

           DISPLAY HEAD-LINE
           DISPLAY "End of Report"
           STOP RUN.

       CALCULATE-BALANCES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               ADD TX-AMOUNT(I) TO CURRENT-BALANCE
               MOVE CURRENT-BALANCE TO TX-BALANCE(I)
           END-PERFORM.
