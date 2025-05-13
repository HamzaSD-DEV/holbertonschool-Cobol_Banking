       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-NAMES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS    IS FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD  PIC X(33).  *> 5 + 10 + 10 + 8 = 33 chars

       WORKING-STORAGE SECTION.
       77  FS                   PIC XX.
       77  EOF-FLAG             PIC X VALUE "N".
       77  CUSTOMER-COUNT       PIC 9(3) VALUE 0.
       77  I                    PIC 9(3).
       77  J                    PIC 9(3).
       77  TEMP-INDEX           PIC 9(3).

       *> Parsed text fields
       77  ACC-TXT              PIC X(5).
       77  FIRST-TXT            PIC X(10).
       77  LAST-TXT             PIC X(10).
       77  BAL-TXT              PIC X(8).

       *> In-memory table
       01  CUSTOMER-TABLE.
           05  CUSTOMER-ENTRY OCCURS 100 TIMES INDEXED BY IDX.
               10  C-ACC-NUM   PIC 9(5).
               10  C-FIRST     PIC X(10).
               10  C-LAST      PIC X(10).
               10  C-BALANCE   PIC 9(5)V99.

       *> Temp for swap
       01  TEMP-ACC-NUM         PIC 9(5).
       01  TEMP-FIRST           PIC X(10).
       01  TEMP-LAST            PIC X(10).
       01  TEMP-BAL             PIC 9(5)V99.

       *> Helpers
       77  NAME1                PIC X(20).
       77  NAME2                PIC X(20).
       77  DISP-ACC             PIC 9(5).
       77  DISP-BAL             PIC Z(5).99.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT CUSTOMER-FILE

           PERFORM UNTIL EOF-FLAG = "Y"
               READ CUSTOMER-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO CUSTOMER-COUNT
                       *> parse fixed columns
                       MOVE CUSTOMER-RECORD(1:5)   TO ACC-TXT
                       MOVE CUSTOMER-RECORD(6:10)  TO FIRST-TXT
                       MOVE CUSTOMER-RECORD(16:10) TO LAST-TXT
                       MOVE CUSTOMER-RECORD(26:8)  TO BAL-TXT

                       *> store into table
                       MOVE FUNCTION NUMVAL(ACC-TXT)
                           TO C-ACC-NUM(CUSTOMER-COUNT)
                       MOVE FIRST-TXT TO C-FIRST(CUSTOMER-COUNT)
                       MOVE LAST-TXT  TO C-LAST(CUSTOMER-COUNT)
                       MOVE FUNCTION NUMVAL(BAL-TXT)
                           TO C-BALANCE(CUSTOMER-COUNT)
               END-READ
           END-PERFORM

           CLOSE CUSTOMER-FILE

           *> sort by full name
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= CUSTOMER-COUNT
               COMPUTE TEMP-INDEX = I + 1
               PERFORM VARYING J FROM TEMP-INDEX BY 1
                       UNTIL J > CUSTOMER-COUNT
                   STRING C-FIRST(I) DELIMITED BY SPACE
                          C-LAST(I)  DELIMITED BY SPACE
                       INTO NAME1
                   STRING C-FIRST(J) DELIMITED BY SPACE
                          C-LAST(J)  DELIMITED BY SPACE
                       INTO NAME2

                   IF NAME1 > NAME2
                       PERFORM SWAP
                   END-IF
               END-PERFORM
           END-PERFORM

           *> display sorted
           DISPLAY "Sorted Customer List:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CUSTOMER-COUNT
               MOVE C-ACC-NUM(I) TO DISP-ACC
               MOVE C-BALANCE(I) TO DISP-BAL
               DISPLAY "Account " DISP-ACC
                       " - " C-FIRST(I) " " C-LAST(I)
                       " - Balance: " DISP-BAL
           END-PERFORM

           STOP RUN.

       SWAP.
           MOVE C-ACC-NUM(I) TO TEMP-ACC-NUM
           MOVE C-FIRST(I)   TO TEMP-FIRST
           MOVE C-LAST(I)    TO TEMP-LAST
           MOVE C-BALANCE(I) TO TEMP-BAL

           MOVE C-ACC-NUM(J) TO C-ACC-NUM(I)
           MOVE C-FIRST(J)   TO C-FIRST(I)
           MOVE C-LAST(J)    TO C-LAST(I)
           MOVE C-BALANCE(J) TO C-BALANCE(I)

           MOVE TEMP-ACC-NUM TO C-ACC-NUM(J)
           MOVE TEMP-FIRST   TO C-FIRST(J)
           MOVE TEMP-LAST    TO C-LAST(J)
           MOVE TEMP-BAL     TO C-BALANCE(J).
