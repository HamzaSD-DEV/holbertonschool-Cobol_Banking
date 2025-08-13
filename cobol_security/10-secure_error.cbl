       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECURE-ERROR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-ACCOUNT    PIC X(12).
       77  WS-TYPE       PIC X.
       77  WS-AMT-STR    PIC X(12).
       77  WS-AMT-V      PIC S9(10)V99.
       77  WS-PIN        PIC X(4).
       77  ERR-CODE      PIC X(7).
       77  LEN           PIC 9(4).
       77  CNT-D         PIC 9(4).
       77  ND-CNT        PIC 9(4).
       77  I             PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PARA.
           *> 1) Read inputs
           DISPLAY "Enter Account Number: " WITH NO ADVANCING
           ACCEPT WS-ACCOUNT
           DISPLAY "Enter Transaction Type (D/W/T): " WITH NO ADVANCING
           ACCEPT WS-TYPE
           DISPLAY "Enter Amount: " WITH NO ADVANCING
           ACCEPT WS-AMT-STR
           DISPLAY "Enter PIN: " WITH NO ADVANCING
           ACCEPT WS-PIN

           *> 2) Required fields missing?
           IF FUNCTION TRIM(WS-ACCOUNT) = " "
              OR FUNCTION TRIM(WS-TYPE)    = " "
              OR FUNCTION TRIM(WS-AMT-STR) = " "
              OR FUNCTION TRIM(WS-PIN)     = " "
              MOVE "VAL-001" TO ERR-CODE
              GO TO DISPLAY-VAL-001
           END-IF

           *> 3) Validate formats:
           *>   Account: ≤12 digits only
           COMPUTE LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-ACCOUNT))
           MOVE 0 TO CNT-D
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
              IF WS-ACCOUNT(I:1) >= "0" AND WS-ACCOUNT(I:1) <= "9"
                 ADD 1 TO CNT-D
              END-IF
           END-PERFORM
           COMPUTE ND-CNT = LEN - CNT-D
           IF LEN > 12 OR ND-CNT > 0
              MOVE "VAL-002" TO ERR-CODE
              GO TO DISPLAY-VAL-002
           END-IF

           *>   Type: one of D, W, T
           IF WS-TYPE NOT = "D" AND WS-TYPE NOT = "W" AND WS-TYPE NOT = 
           "T"
              MOVE "VAL-002" TO ERR-CODE
              GO TO DISPLAY-VAL-002
           END-IF

           *>   Amount: numeric > 0
           IF WS-AMT-STR(1:1) = "-"
              MOVE "VAL-002" TO ERR-CODE
              GO TO DISPLAY-VAL-002
           END-IF
           COMPUTE WS-AMT-V = FUNCTION NUMVAL(WS-AMT-STR)
           IF WS-AMT-V <= 0
              MOVE "VAL-002" TO ERR-CODE
              GO TO DISPLAY-VAL-002
           END-IF

           *>   PIN: exactly 4 digits
           COMPUTE LEN = FUNCTION LENGTH(FUNCTION TRIM(WS-PIN))
           MOVE 0 TO CNT-D
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
              IF WS-PIN(I:1) >= "0" AND WS-PIN(I:1) <= "9"
                 ADD 1 TO CNT-D
              END-IF
           END-PERFORM
           COMPUTE ND-CNT = LEN - CNT-D
           IF LEN NOT = 4 OR ND-CNT > 0
              MOVE "VAL-002" TO ERR-CODE
              GO TO DISPLAY-VAL-002
           END-IF

           *> 4) Business rule: withdrawal over 1000 ⇒ TXN-001
           IF WS-TYPE = "W" AND WS-AMT-V > 1000.00
              MOVE "TXN-001" TO ERR-CODE
              GO TO DISPLAY-TXN-001
           END-IF

           *> No errors
           STOP RUN.

       *>————————————————————————
       DISPLAY-VAL-001.
           DISPLAY "ERROR: Required fields missing. Error Code: VAL-001"
           DISPLAY "All fields must be completed."
           STOP RUN.

       DISPLAY-VAL-002.
           DISPLAY "ERROR: Invalid input detected. Error Code: VAL-002"
           DISPLAY "Please verify your information and try again."
           STOP RUN.

       DISPLAY-TXN-001.
           DISPLAY 
           "ERROR: Transaction cannot be completed. Error Code: TXN-001"
           DISPLAY 
           "Please contact customer support if the problem persists."
           STOP RUN.

       END PROGRAM SECURE-ERROR.
