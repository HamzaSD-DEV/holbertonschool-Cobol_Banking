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
           05  FILLER              PIC X VALUE ','.
           05  WS-CUST-NAME        PIC X(20).
           05  FILLER              PIC X VALUE ','.
           05  WS-CUST-ADDR        PIC X(20).
           05  FILLER              PIC X VALUE ','.
           05  WS-CUST-CITY        PIC X(15).
           05  FILLER              PIC X VALUE ','.
           05  WS-CUST-STATE       PIC X(2).
           05  FILLER              PIC X VALUE ','.
           05  WS-CUST-ZIP         PIC X(5).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "CUSTOMER-REPORTER" 
           ": Starting customer report generation..."
           DISPLAY "CUSTOMER-REPORTER" 
           ": =================================="
           
           OPEN INPUT CUSTMAST
           
           PERFORM PROCESS-CUSTOMERS UNTIL WS-EOF-FLAG = 'Y'
           
           CLOSE CUSTMAST
           
           PERFORM DISPLAY-SUMMARY
           
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
           DISPLAY "  Name: " WS-CUST-NAME
           DISPLAY "  Address: " WS-CUST-ADDR ", " WS-CUST-CITY ", " 
                  WS-CUST-STATE " " WS-CUST-ZIP
           DISPLAY "  ----------------------------------".
       
       DISPLAY-SUMMARY.
           DISPLAY "CUSTOMER-REPORTER" 
           ": =================================="
           DISPLAY "CUSTOMER-REPORTER" ": Report generation completed"
           DISPLAY "CUSTOMER-REPORTER" ": Total customers processed: " 
                   WS-CUSTOMER-COUNT
           DISPLAY "CUSTOMER-REPORTER" 
           ": Report ready for management review".