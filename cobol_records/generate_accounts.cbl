       IDENTIFICATION DIVISION.
       PROGRAM-ID. GenerateIndexedAccounts76.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "accounts.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCOUNT-KEY
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE.
       01  ACCOUNT-RECORD.
           05 ACCOUNT-KEY       PIC 9(5).          *> Primary key
           05 FNAME             PIC X(10).
           05 LNAME             PIC X(10).
           05 BALANCE           PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01  I                   PIC 99 VALUE 1.
       01  FILE-STATUS         PIC XX.
       01  FNAME-IDX           PIC 99.
       01  LNAME-IDX           PIC 99.
       01  BAL-INT             PIC 9(4) VALUE 500.
       01  BAL-FRAC            PIC 99 VALUE 00.
       01  DISP-BALANCE        PIC Z(6).99.

       01  FIRST-NAME-TBL.
           05 FNAMES OCCURS 10 TIMES PIC X(10) VALUE SPACES.
       01  LAST-NAME-TBL.
           05 LNAMES OCCURS 10 TIMES PIC X(10) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "Alice" TO FNAMES(1)
           MOVE "Bob"   TO FNAMES(2)
           MOVE "Clara" TO FNAMES(3)
           MOVE "David" TO FNAMES(4)
           MOVE "Emma"  TO FNAMES(5)
           MOVE "Frank" TO FNAMES(6)
           MOVE "Grace" TO FNAMES(7)
           MOVE "Henry" TO FNAMES(8)
           MOVE "Irene" TO FNAMES(9)
           MOVE "Jack"  TO FNAMES(10)

           MOVE "Smith" TO LNAMES(1)
           MOVE "White" TO LNAMES(2)
           MOVE "Jones" TO LNAMES(3)
           MOVE "Brown" TO LNAMES(4)
           MOVE "Clark" TO LNAMES(5)
           MOVE "Young" TO LNAMES(6)
           MOVE "Stone" TO LNAMES(7)
           MOVE "King"  TO LNAMES(8)
           MOVE "Scott" TO LNAMES(9)
           MOVE "Black" TO LNAMES(10)

           OPEN OUTPUT ACCOUNTS-FILE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 76
               MOVE I TO ACCOUNT-KEY

               COMPUTE FNAME-IDX = FUNCTION MOD(I, 10) + 1
               COMPUTE LNAME-IDX = FUNCTION MOD(I + 5, 10) + 1

               MOVE FNAMES(FNAME-IDX) TO FNAME
               MOVE LNAMES(LNAME-IDX) TO LNAME

               COMPUTE BAL-INT = 500 + FUNCTION MOD(I * 37, 1200)
               COMPUTE BAL-FRAC = FUNCTION MOD(I * 71, 100)
               COMPUTE BALANCE = BAL-INT + (BAL-FRAC / 100)

               WRITE ACCOUNT-RECORD
                   INVALID KEY
                       DISPLAY "ERROR: Duplicate or invalid key: " 
                       ACCOUNT-KEY
               END-WRITE
           END-PERFORM

           CLOSE ACCOUNTS-FILE

           DISPLAY 
           "Indexed file accounts.idx generated with 76 records."
           STOP RUN.
