       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETECT-DUPLICATES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO "transactions.idx"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRANS-FILE.
       01 TRANS-RECORD.
           05 TR-ID        PIC X(6).
           05 TR-ACC       PIC X(9).
           05 TR-DATE      PIC X(8).
           05 TR-TYPE      PIC X(1).
           05 TR-AMOUNT    PIC 9(10).  *> Full 10 digits

       WORKING-STORAGE SECTION.
       01 EOF                PIC X VALUE 'N'.
       01 NUM-RECORDS        PIC 9(4) VALUE 0.

       01 I                  PIC 9(4).
       01 J                  PIC 9(4).
       01 NEXT-J             PIC 9(4).
       01 DECIMAL-AMOUNT     PIC 9(7)V99.
       01 TEMP-AMOUNT        PIC 9(10).
       01 DOLLARS            PIC 9(7).
       01 CENTS              PIC 99.

       01 TRANS-TABLE.
           05 TRANS-ENTRY OCCURS 100 TIMES.
               10 TE-USED     PIC X VALUE 'N'.
               10 TE-ID       PIC X(6).
               10 TE-ACC      PIC X(9).
               10 TE-DATE     PIC X(8).
               10 TE-TYPE     PIC X(1).
               10 TE-AMOUNT   PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT TRANS-FILE

           PERFORM UNTIL EOF = 'Y'
               READ TRANS-FILE
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       ADD 1 TO NUM-RECORDS
                       MOVE TR-ID     TO TE-ID(NUM-RECORDS)
                       MOVE TR-ACC    TO TE-ACC(NUM-RECORDS)
                       MOVE TR-DATE   TO TE-DATE(NUM-RECORDS)
                       MOVE TR-TYPE   TO TE-TYPE(NUM-RECORDS)
                       MOVE TR-AMOUNT TO TE-AMOUNT(NUM-RECORDS)
               END-READ
           END-PERFORM

           CLOSE TRANS-FILE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-RECORDS
               IF TE-USED(I) = 'N'
                   COMPUTE NEXT-J = I + 1
                   PERFORM VARYING J FROM NEXT-J BY 1 UNTIL J > 
                   NUM-RECORDS
                       IF TE-USED(J) = 'N'
                          AND TE-ACC(I) = TE-ACC(J)
                          AND TE-DATE(I) = TE-DATE(J)
                          AND TE-TYPE(I) = TE-TYPE(J)
                          AND TE-AMOUNT(I) = TE-AMOUNT(J)
                           MOVE 'Y' TO TE-USED(I)
                           MOVE 'Y' TO TE-USED(J)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

           DISPLAY "DUPLICATE TRANSACTIONS:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-RECORDS
               IF TE-USED(I) = 'Y'
                   MOVE TE-AMOUNT(I) TO TEMP-AMOUNT
                   DIVIDE TEMP-AMOUNT BY 100
                       GIVING DOLLARS
                       REMAINDER CENTS
                   DISPLAY "DUPLICATE: "
                           TE-ID(I) " "
                           TE-ACC(I) " "
                           TE-DATE(I) " "
                           TE-TYPE(I) " "
                           DOLLARS "." CENTS
                   MOVE 'N' TO TE-USED(I) *> Mark it as printed
               END-IF
           END-PERFORM

           STOP RUN.
