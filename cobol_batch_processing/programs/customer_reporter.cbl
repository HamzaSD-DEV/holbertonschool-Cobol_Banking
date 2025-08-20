       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTER.
       AUTHOR. STUDENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTMAST ASSIGN TO "CUSTMAST"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTMAST.
       01  CUSTOMER-RECORD         PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-COUNT       PIC 9(5) VALUE 0.
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
       
       01  WS-CUSTOMER-FIELDS.
           05  WS-CUST-ID          PIC X(5).
           05  WS-CUST-NAME        PIC X(20).
           05  WS-CUST-ADDR        PIC X(15).
           05  WS-CUST-CITY        PIC X(10).
           05  WS-CUST-STATE       PIC X(2).
           05  WS-CUST-ZIP         PIC X(5).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "CUSTOMER-REPORTER" 
           ": Starting customer report generation..."
           DISPLAY "CUSTOMER-REPORTER" 
           ": =================================="
           
           OPEN INPUT CUSTMAST
           
           PERFORM PROCESS-CUSTOMERS UNTIL WS-EOF-FLAG = 'Y'

           PERFORM DISPLAY-SUMMARY
           
           CLOSE CUSTMAST
           
           
           STOP RUN.
       
       PROCESS-CUSTOMERS.
           READ CUSTMAST INTO CUSTOMER-RECORD
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-CUSTOMER-COUNT
                   MOVE CUSTOMER-RECORD TO WS-CUSTOMER-FIELDS
                   PERFORM DISPLAY-CUSTOMER-INFO
           END-READ.
       
       DISPLAY-CUSTOMER-INFO.
           DISPLAY "Customer #" WS-CUSTOMER-COUNT ":"
           DISPLAY "  ID: " WS-CUST-ID
           DISPLAY "  Name: " FUNCTION TRIM (WS-CUST-NAME)
           DISPLAY "  Address: " FUNCTION TRIM (WS-CUST-ADDR) " " 
                  FUNCTION TRIM (WS-CUST-CITY) " " 
                  FUNCTION TRIM (WS-CUST-STATE) " " WS-CUST-ZIP
           DISPLAY "  ----------------------------------".
       
       DISPLAY-SUMMARY.
           DISPLAY "CUSTOMER-REPORTER" 
           ": =================================="
           DISPLAY "CUSTOMER-REPORTER" ": Report generation completed"
           DISPLAY "CUSTOMER-REPORTER" ": Total customers processed: " 
                   WS-CUSTOMER-COUNT
           DISPLAY "CUSTOMER-REPORTER" 
           ": Report ready for management review".