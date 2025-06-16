       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-ORDER.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRICE-STR        PIC X(20).
       01 WS-PRICE-NUM        PIC 9(4)V99.
       01 WS-CODE             PIC X.
       01 WS-DISC-PRICE-LOC   PIC 9(4)V99.
       01 WS-DISP-PRICE       PIC ZZZ9.99.

       LINKAGE SECTION.
       01 LNK-DISC-PRICE      PIC 9(4)V99.  *> returning discounted price

       PROCEDURE DIVISION USING LNK-DISC-PRICE.
       PROCESS-ORDER-PARA.
           *> Prompt for product price
           DISPLAY "Enter product price (format 9999.99): " WITH NO 
           ADVANCING
           ACCEPT WS-PRICE-STR
           *> Convert to numeric (assume correct format)
           COMPUTE WS-PRICE-NUM = FUNCTION NUMVAL(WS-PRICE-STR)

           *> Prompt for discount code
           DISPLAY "Enter discount code (A/B): " WITH NO ADVANCING
           ACCEPT WS-CODE

           *> Call CALC-DISCOUNT
           CALL 'CALC-DISCOUNT' USING
                 WS-PRICE-NUM
                 WS-CODE
                 WS-DISC-PRICE-LOC

           *> Move result to linkage to return
           MOVE WS-DISC-PRICE-LOC TO LNK-DISC-PRICE

           *> Display discounted price
           MOVE WS-DISC-PRICE-LOC TO WS-DISP-PRICE
           DISPLAY "Discounted price: " WS-DISP-PRICE

           GOBACK.
