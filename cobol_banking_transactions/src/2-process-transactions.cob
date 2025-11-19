IDENTIFICATION DIVISION.
PROGRAM-ID. process-transactions.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT TX-FILE ASSIGN TO "transactions.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS TX-FILE-STATUS.
DATA DIVISION.
FILE SECTION.
FD  TX-FILE.
01  TX-RECORD            PIC X(200).
WORKING-STORAGE SECTION.
COPY "dbapi.cpy".
01  CONN-LIT PIC X(200)
    VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
01  L PIC 9(4) VALUE 0.
01  TX-FILE-STATUS PIC XX.
01  TX-FIELDS.
    05 TX-ACTION         PIC X(8).
    05 FILLER            PIC X.
    05 TX-FIELD-2        PIC X(10).
    05 FILLER            PIC X.
    05 TX-FIELD-3        PIC X(10).
    05 FILLER            PIC X.
    05 TX-FIELD-4        PIC X(10).
    05 FILLER            PIC X.
    05 TX-FIELD-5        PIC X(10).

PROCEDURE DIVISION.
MAIN-PROCEDURE.
    MOVE SPACES TO DB-CONNSTR.
    COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
    MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
    MOVE X"00" TO DB-CONNSTR(L + 1:1).

    CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
    IF DBH = NULL-PTR THEN STOP RUN.

    OPEN INPUT TX-FILE.
    PERFORM PROCESS-TRANSACTIONS UNTIL TX-FILE-STATUS NOT = "00".
    CLOSE TX-FILE.

    CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
    GOBACK.

PROCESS-TRANSACTIONS.
    READ TX-FILE AT END MOVE "10" TO TX-FILE-STATUS.
    IF TX-FILE-STATUS = "00" THEN
        MOVE SPACES TO TX-FIELDS
        UNSTRING TX-RECORD DELIMITED BY ","
            INTO TX-ACTION, TX-FIELD-2, TX-FIELD-3, 
                 TX-FIELD-4, TX-FIELD-5
        END-UNSTRING
        
        EVALUATE FUNCTION UPPER-CASE(FUNCTION TRIM(TX-ACTION))
            WHEN "INSERT"   PERFORM HANDLE-INSERT
            WHEN "UPDATE"   PERFORM HANDLE-UPDATE
        END-EVALUATE
    END-IF.

HANDLE-INSERT.
    *> Insert into customers table
    MOVE SPACES TO SQL-COMMAND.
    STRING 
        "INSERT INTO customers (customer_id, name) VALUES ("
        FUNCTION TRIM(TX-FIELD-2) ", '"
        FUNCTION TRIM(TX-FIELD-3) "')"
        DELIMITED BY SIZE INTO SQL-COMMAND
    END-STRING.
    MOVE X"00" TO SQL-COMMAND(100:1).

    CALL STATIC "DB_EXEC"
        USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING RC.
    IF RC <> 0 THEN
        DISPLAY "DB_EXEC failed for customers insert: " RC
        EXIT PARAGRAPH
    END-IF.

    *> Insert into accounts table
    MOVE SPACES TO SQL-COMMAND.
    STRING 
        "INSERT INTO accounts (account_id, customer_id, balance) VALUES ("
        FUNCTION TRIM(TX-FIELD-4) ", "
        FUNCTION TRIM(TX-FIELD-2) ", "
        FUNCTION TRIM(TX-FIELD-5) ")"
        DELIMITED BY SIZE INTO SQL-COMMAND
    END-STRING.
    MOVE X"00" TO SQL-COMMAND(100:1).

    CALL STATIC "DB_EXEC"
        USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING RC.
    IF RC <> 0 THEN
        DISPLAY "DB_EXEC failed for accounts insert: " RC
    ELSE
        DISPLAY "Processed INSERT for " 
                FUNCTION TRIM(TX-FIELD-3)
    END-IF.

HANDLE-UPDATE.
    IF FUNCTION UPPER-CASE(FUNCTION TRIM(TX-FIELD-3)) = "DEPOSIT"
        *> Update account balance for deposit
        MOVE SPACES TO SQL-COMMAND
        STRING 
            "UPDATE accounts SET balance = balance + "
            FUNCTION TRIM(TX-FIELD-4)
            " WHERE account_id = "
            FUNCTION TRIM(TX-FIELD-2)
            DELIMITED BY SIZE INTO SQL-COMMAND
        END-STRING
        MOVE X"00" TO SQL-COMMAND(100:1)

        CALL STATIC "DB_EXEC"
            USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING RC
        IF RC <> 0 THEN
            DISPLAY "DB_EXEC failed for deposit: " RC
        ELSE
            DISPLAY "Processed DEPOSIT for account " 
                    FUNCTION TRIM(TX-FIELD-2)
        END-IF
    ELSE
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(TX-FIELD-3)) = "WITHDRAW"
            *> Update account balance for withdrawal
            MOVE SPACES TO SQL-COMMAND
            STRING 
                "UPDATE accounts SET balance = balance - "
                FUNCTION TRIM(TX-FIELD-4)
                " WHERE account_id = "
                FUNCTION TRIM(TX-FIELD-2)
                DELIMITED BY SIZE INTO SQL-COMMAND
            END-STRING
            MOVE X"00" TO SQL-COMMAND(100:1)

            CALL STATIC "DB_EXEC"
                USING BY VALUE DBH, BY REFERENCE SQL-COMMAND RETURNING RC
            IF RC <> 0 THEN
                DISPLAY "DB_EXEC failed for withdraw: " RC
            ELSE
                DISPLAY "Processed WITHDRAW for account " 
                        FUNCTION TRIM(TX-FIELD-2)
            END-IF
        END-IF
    END-IF.
    