       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-DISCOUNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRICE          PIC 9(4)V99.
       01 WS-CODE           PIC X.
       01 WS-RESULT         PIC 9(4)V99.
       
       LINKAGE SECTION.
       01 LNK-ORIG-PRICE    PIC 9(4)V99.
       01 LNK-DISCOUNT-CODE PIC X.
       01 LNK-DISC-PRICE    PIC 9(4)V99.

       PROCEDURE DIVISION USING LNK-ORIG-PRICE
                             LNK-DISCOUNT-CODE
                             LNK-DISC-PRICE.
       CALC-DISCOUNT-PARA.
           *> Move inputs to locals
           MOVE LNK-ORIG-PRICE TO WS-PRICE
           MOVE LNK-DISCOUNT-CODE TO WS-CODE

           EVALUATE TRUE
             WHEN WS-CODE = 'A' OR WS-CODE = 'a'
                 *> 10% off: price * 0.90
                 COMPUTE WS-RESULT = WS-PRICE * 0.90
             WHEN WS-CODE = 'B' OR WS-CODE = 'b'
                 *> 20% off: price * 0.80
                 COMPUTE WS-RESULT = WS-PRICE * 0.80
             WHEN OTHER
                 MOVE WS-PRICE TO WS-RESULT
           END-EVALUATE

           *> Move result back to caller
           MOVE WS-RESULT TO LNK-DISC-PRICE
           GOBACK.
