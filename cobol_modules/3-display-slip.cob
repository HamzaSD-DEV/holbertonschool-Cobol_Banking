       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-SLIP.
       DATA DIVISION. 
       LINKAGE SECTION.
       01  DS-NAME      PIC X(20).
       01  DS-BASIC     PIC 9(5)V99.
       01  DS-ALLOW     PIC 9(4)V99.
       01  DS-DEDUCT    PIC 9(4)V99.
       01  DS-GROSS     PIC 9(6)V99.
       01  DS-NET       PIC 9(6)V99.

       PROCEDURE DIVISION 
       USING DS-NAME, DS-BASIC, DS-ALLOW, DS-DEDUCT, DS-GROSS, DS-NET.
       DISP-SLIP-PARA.
           DISPLAY "==========================="
           DISPLAY "       SALARY SLIP"
           DISPLAY "---------------------------"
           DISPLAY "Employee Name : " DS-NAME
           DISPLAY "Basic Salary  : " DS-BASIC
           DISPLAY "Allowance     : " DS-ALLOW
           DISPLAY "Deductions    : " DS-DEDUCT
           DISPLAY "Gross Salary  : " DS-GROSS
           DISPLAY "Net Salary    : " DS-NET
           DISPLAY "==========================="
           GOBACK.
