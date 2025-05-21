       IDENTIFICATION DIVISION.
       PROGRAM-ID. ValidateIBAN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-IBAN-IN              PIC X(50).
       01  WS-IBAN-TRIMMED         PIC X(22).
       01  WS-CHECK-STATUS         PIC X VALUE 'Y'.
       01  WS-IDX                  PIC 99.
       01  WS-CHAR                 PIC X.

       01  WS-COUNTRY-CODE         PIC XX.
       01  WS-CHECK-DIGITS         PIC XX.
       01  WS-BANK-CODE            PIC X(4).
       01  WS-SORT-CODE            PIC X(6).
       01  WS-ACCOUNT-NUMBER       PIC X(8).
       01  WS-COUNT            PIC 9(2).
       01  WS-ACTUAL-LENGTH    PIC 9(2).

       LINKAGE SECTION.
       01  LK-IBAN                 PIC X(50).

       PROCEDURE DIVISION USING LK-IBAN.
       
           MOVE FUNCTION TRIM(LK-IBAN) TO WS-IBAN-IN
           MOVE 0 TO WS-COUNT
           MOVE 0 TO WS-ACTUAL-LENGTH

           INSPECT FUNCTION REVERSE(WS-IBAN-IN) TALLYING WS-COUNT 
           FOR LEADING SPACE   
           COMPUTE WS-ACTUAL-LENGTH = 50 - WS-COUNT 

           IF WS-ACTUAL-LENGTH NOT = 22
      *        DISPLAY "Invalid IBAN: Incorrect length. " WS-IBAN-IN
               DISPLAY "IBAN is invalid: " WS-IBAN-IN
               GOBACK
           END-IF

           MOVE WS-IBAN-IN TO WS-IBAN-TRIMMED

           *> Split into components
           MOVE WS-IBAN-TRIMMED(1:2)   TO WS-COUNTRY-CODE
           MOVE WS-IBAN-TRIMMED(3:2)   TO WS-CHECK-DIGITS
           MOVE WS-IBAN-TRIMMED(5:4)   TO WS-BANK-CODE
           MOVE WS-IBAN-TRIMMED(9:6)   TO WS-SORT-CODE
           MOVE WS-IBAN-TRIMMED(15:8)  TO WS-ACCOUNT-NUMBER

           *> Country code must be GB
           IF WS-COUNTRY-CODE NOT = "GB"
      *        DISPLAY "Invalid IBAN: Country code must be GB. " 
      *        WS-IBAN-IN
               DISPLAY "IBAN is invalid: " WS-IBAN-IN
               GOBACK
           END-IF

           *> Check digits must be digits
           IF WS-CHECK-DIGITS NOT NUMERIC
      *        DISPLAY "Invalid IBAN: Check digits must be numeric. " 
      *        WS-IBAN-IN
               DISPLAY "IBAN is invalid: " WS-IBAN-IN
               GOBACK
           END-IF

           *> Bank code must be all letters
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 4
               MOVE WS-BANK-CODE(WS-IDX:1) TO WS-CHAR
               IF WS-CHAR < "A" OR WS-CHAR > "Z"
      *            DISPLAY 
      *            "Invalid IBAN: Bank code must be letters only. " 
      *            WS-IBAN-IN
                   DISPLAY "IBAN is invalid: " WS-IBAN-IN
                   GOBACK
               END-IF
           END-PERFORM

           *> Sort code must be digits only
           IF WS-SORT-CODE NOT NUMERIC
      *        DISPLAY "Invalid IBAN: Sort code must be numeric. " 
      *        WS-IBAN-IN
               DISPLAY "IBAN is invalid: " WS-IBAN-IN
               GOBACK
           END-IF

           *> Account number must be digits only
           IF WS-ACCOUNT-NUMBER NOT NUMERIC
      *        DISPLAY "Invalid IBAN: Account number must be numeric. " 
      *        WS-IBAN-IN
               DISPLAY "IBAN is invalid: " WS-IBAN-IN
               GOBACK
           END-IF

           *> Check for special characters or spaces
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 22
               MOVE WS-IBAN-TRIMMED(WS-IDX:1) TO WS-CHAR
               IF WS-CHAR < "0" OR
                  (WS-CHAR > "9" AND WS-CHAR < "A") OR
                  (WS-CHAR > "Z")
      *            DISPLAY 
      *           "Invalid IBAN: Must contain only letters and digits. " 
      *           WS-IBAN-IN
                   DISPLAY "IBAN is invalid: " WS-IBAN-IN
                   GOBACK
               END-IF
           END-PERFORM

           DISPLAY "IBAN is valid: " WS-IBAN-IN

           GOBACK.
