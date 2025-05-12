       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE-AND-APPEND.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
             ASSIGN TO "CUSTOMERS.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS CUST-ID
             FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUST-ID         PIC 9(5).
           05 CUST-FNAME      PIC A(10).
           05 CUST-LNAME      PIC A(10).
           05 CUST-BALANCE    PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01  WS-STATUS            PIC XX.
       01  WS-ACC-ID            PIC X(6).
       01  WS-ACC-ID-NUM            PIC 9(6).
       01  WS-FNAME-IN          PIC X(20).
       01  WS-LNAME-IN          PIC X(20).
       01  WS-BAL-TEXT          PIC X(9).
       01  WS-BAL-NUM          PIC 9(8).
       01  BEFORE-DECIMAL      PIC X(8).
       01  AFTER-DECIMAL       PIC X(7).
       01  BEFORE-DECIMAL-NUM      PIC 9(8).
       01  AFTER-DECIMAL-NUM       PIC 9(7).
       01  INVALID-FLOAT       PIC X VALUE "Y".
       01  WS-RETRY-FLAG        PIC X VALUE "Y".
       01  WS-DECIMAL-POS       PIC 9.
       01  WS-COUNT            PIC 9(2).
       01  WS-ACTUAL-LENGTH    PIC 9(2).
       77  IDX             PIC 9       VALUE 1.
       77  CHARC            PIC X.
       77  IS-NUMERIC      PIC X VALUE "Y".
       77  CHAR-CODE       PIC 9(3).
       
       
      * Helpers for trimming/padding
       01  WS-FNAME-TMP         PIC X(10).
       01  WS-LNAME-TMP         PIC X(10).

       PROCEDURE DIVISION.
       MAIN.
           OPEN I-O CUSTOMER-FILE

           *> --- Prompt & validate Account ID ---
           PERFORM UNTIL WS-RETRY-FLAG = "N" AND 
           WS-ACC-ID  NOT = SPACE AND LOW-VALUE
               DISPLAY "Enter Account Number (5 digits):"
               ACCEPT WS-ACC-ID
               MOVE WS-ACC-ID TO WS-ACC-ID-NUM
               MOVE 0 to WS-DECIMAL-POS
               INSPECT WS-ACC-ID TALLYING WS-DECIMAL-POS FOR ALL "."
               IF WS-ACC-ID-NUM  > 0 AND WS-ACC-ID-NUM  < 100000 
               AND WS-DECIMAL-POS= 0
                   *> Check for duplicates
                   MOVE WS-ACC-ID TO  CUST-ID 
                   READ CUSTOMER-FILE
                       INVALID KEY
                           MOVE "N" TO WS-RETRY-FLAG
                       NOT INVALID KEY
                           DISPLAY "  >> Account " CUST-ID 
                           " already exists."
                   END-READ
               ELSE
                   DISPLAY
                   "  >> Invalid account number."
               END-IF
           END-PERFORM


           *> --- Prompt & validate First Name ---
           MOVE "Y" TO WS-RETRY-FLAG
           PERFORM UNTIL 
           WS-RETRY-FLAG = "N" AND 
           (CUST-FNAME  NOT = SPACE AND LOW-VALUE)
               DISPLAY "Enter First Name (1-10 letters):"
               ACCEPT WS-FNAME-IN
               MOVE 0 TO WS-COUNT
               MOVE 0 TO WS-ACTUAL-LENGTH
               INSPECT FUNCTION REVERSE(WS-FNAME-IN) TALLYING WS-COUNT 
               FOR LEADING SPACE   
               COMPUTE WS-ACTUAL-LENGTH = 20 - WS-COUNT 
               IF WS-ACTUAL-LENGTH > 0 AND
                  WS-ACTUAL-LENGTH <= 10 AND WS-FNAME-IN IS
                  ALPHABETIC AND WS-FNAME-IN NOT = SPACE AND LOW-VALUE
                   MOVE "N" TO WS-RETRY-FLAG
                   MOVE WS-FNAME-IN TO CUST-FNAME (1:10)
               ELSE
                   DISPLAY "  >> Invalid First Name."
               END-IF
           END-PERFORM

           *> --- Prompt & validate Last Name ---
           MOVE "Y" TO WS-RETRY-FLAG
           PERFORM UNTIL 
           WS-RETRY-FLAG = "N" AND 
           WS-LNAME-IN NOT = SPACE AND LOW-VALUE
               DISPLAY "Enter Last Name (1-10 letters): "
               ACCEPT WS-LNAME-IN
               MOVE 0 TO WS-COUNT
               MOVE 0 TO WS-ACTUAL-LENGTH

               INSPECT FUNCTION REVERSE(WS-LNAME-IN) TALLYING WS-COUNT 
               FOR LEADING SPACE   
               COMPUTE WS-ACTUAL-LENGTH = 20 - WS-COUNT 

               IF WS-ACTUAL-LENGTH > 0 AND
                  WS-ACTUAL-LENGTH <= 10 AND WS-LNAME-IN IS
                  ALPHABETIC AND WS-LNAME-IN NOT = SPACE AND LOW-VALUE
                   MOVE WS-LNAME-IN  TO CUST-LNAME (1:10)
                   MOVE "N" TO WS-RETRY-FLAG
               ELSE
                   DISPLAY "  >> Invalid Last Name."
               END-IF
           END-PERFORM

           *> --- Prompt & validate Balance ---
           MOVE "Y" TO WS-RETRY-FLAG
           PERFORM UNTIL 
           WS-RETRY-FLAG = "N"
               MOVE 0 to WS-DECIMAL-POS
               MOVE "Y" to IS-NUMERIC
               DISPLAY "Enter Starting Balance (0 <= Balance < 100000):"
               ACCEPT WS-BAL-TEXT
               MOVE 0 TO WS-COUNT
               INSPECT WS-BAL-TEXT TALLYING WS-DECIMAL-POS FOR ALL "."
               IF WS-DECIMAL-POS NOT = 1 AND WS-DECIMAL-POS NOT = 0 
                   MOVE "Y" TO INVALID-FLOAT
               ELSE
                   *> Split string into parts before and after decimal
                   IF WS-DECIMAL-POS = 1
                       UNSTRING WS-BAL-TEXT DELIMITED BY "."
                           INTO BEFORE-DECIMAL, AFTER-DECIMAL
                   ELSE
                       MOVE WS-BAL-TEXT TO WS-BAL-NUM
                       IF WS-BAL-NUM < 100000 
                           MOVE WS-BAL-TEXT TO BEFORE-DECIMAL
                           MOVE "0" TO AFTER-DECIMAL
                       ELSE
                           MOVE "Y" TO INVALID-FLOAT
                           MOVE "N" TO BEFORE-DECIMAL
                           MOVE "N" TO AFTER-DECIMAL
                       END-IF
                   END-IF

                   *> Check both parts are numeric

                   PERFORM VARYING IDX FROM 1 BY 1
                       UNTIL IDX > FUNCTION LENGTH(BEFORE-DECIMAL)
                       OR IS-NUMERIC = "N"
                       MOVE BEFORE-DECIMAL(IDX:1) TO CHARC
                       IF (CHARC >= "0" AND CHARC <= "9") OR CHARC = " "
                          CONTINUE
                       ELSE
                           MOVE "N" TO IS-NUMERIC
                       END-IF
                   END-PERFORM
                   PERFORM VARYING IDX FROM 1 BY 1
                       UNTIL IDX > FUNCTION LENGTH(AFTER-DECIMAL)
                       OR IS-NUMERIC = "N"
                       MOVE AFTER-DECIMAL(IDX:1) TO CHARC
                       IF (CHARC >= "0" AND CHARC <= "9") OR CHARC = " "
                           CONTINUE
                       ELSE
                           MOVE "N" TO IS-NUMERIC
                       END-IF
                   END-PERFORM

                   MOVE BEFORE-DECIMAL TO BEFORE-DECIMAL-NUM
                   MOVE AFTER-DECIMAL TO AFTER-DECIMAL-NUM
                   IF BEFORE-DECIMAL-NUM < 100000 AND 
                   AFTER-DECIMAL-NUM < 100 AND IS-NUMERIC = "Y"
                       MOVE "N" TO INVALID-FLOAT
                   ELSE
                       MOVE "Y" TO INVALID-FLOAT

                   END-IF
               END-IF
               
               MOVE WS-BAL-TEXT TO CUST-BALANCE
               IF CUST-BALANCE >= 0 AND CUST-BALANCE < 100000 AND
               INVALID-FLOAT = "N" AND  
               WS-BAL-TEXT NOT = SPACE AND LOW-VALUE
                   MOVE "N" TO WS-RETRY-FLAG
               ELSE
                   DISPLAY "  >> Invalid Balance."
               END-IF
           END-PERFORM

           *> --- All inputs valid, write record ---
           WRITE CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY "  >> File error: Unable to add record."
                   CLOSE CUSTOMER-FILE
                   STOP RUN
           END-WRITE

           DISPLAY "Account " CUST-ID " has been successfully added."

           CLOSE CUSTOMER-FILE
           STOP RUN.
