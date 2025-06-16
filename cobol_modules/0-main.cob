       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-NAME          PIC X(20).

       PROCEDURE DIVISION.
           DISPLAY "Enter your name: "
           ACCEPT USER-NAME
           CALL 'WELCOME' USING USER-NAME
           DISPLAY "You've entered the COBOL modular programming."
           STOP RUN.
