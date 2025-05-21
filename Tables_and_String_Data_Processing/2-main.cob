       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  IBAN-INDEX             PIC 9(2) VALUE 1.

       01  TEST-IBAN-LIST.
           05  IBAN-VALUE OCCURS 10 TIMES INDEXED BY IDX.
               10 IBAN-ENTRY      PIC X(50).

       PROCEDURE DIVISION.

           *> Valid IBAN
           MOVE " GB29NWBK60161331936819 "         TO IBAN-ENTRY(1)

           *> Too short (invalid length)
           MOVE "GB29NWBK60316135"                 TO IBAN-ENTRY(2)

           *> Too long (invalid length)
           MOVE "GB29NWBK60161331921900000"    TO IBAN-ENTRY(3)

           *> Wrong country code (not GB)
           MOVE "TN29NWBK60161331926819"         TO IBAN-ENTRY(4)

           *> Check digits not numeric
           MOVE "GBXXNWBK60161331989819"         TO IBAN-ENTRY(5)

           *> Bank code includes a digit
           MOVE "GB29N1BK60161331926419"         TO IBAN-ENTRY(6)

           *> Sort code contains letters
           MOVE "GB29NWBK606S1331924119"         TO IBAN-ENTRY(7)

           *> Account number contains letters
           MOVE "GB29NWBK601613310HJ119"             TO IBAN-ENTRY(8)

           *> Contains special character
           MOVE "GB29NW&K60161331916819"         TO IBAN-ENTRY(9)

           *> Contains space
           MOVE "GB29WBK 60161331926819"        TO IBAN-ENTRY(10)

           PERFORM VARYING IBAN-INDEX FROM 1 BY 1 UNTIL IBAN-INDEX > 10
               DISPLAY "Testing IBAN: " IBAN-ENTRY(IBAN-INDEX)
               CALL 'ValidateIBAN' USING IBAN-ENTRY (IBAN-INDEX)
           END-PERFORM

           STOP RUN.
