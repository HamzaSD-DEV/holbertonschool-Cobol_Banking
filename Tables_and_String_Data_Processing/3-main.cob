       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FULL-NAME-VALUE   PIC X(50).

       PROCEDURE DIVISION.

           *> Test case 1: Simple Name
           MOVE " Jahn Toe " TO FULL-NAME-VALUE.
           CALL 'EXTRACTNAMES' USING FULL-NAME-VALUE.
           DISPLAY SPACES.

           *> Test case 2: Full name with middle name
           MOVE "  Cary Inn Snith  " TO FULL-NAME-VALUE.
           CALL 'EXTRACTNAMES' USING FULL-NAME-VALUE.
           DISPLAY SPACES.

           *> Test case 3: One word name
           MOVE "  Cherlo  " TO FULL-NAME-VALUE.
           CALL 'EXTRACTNAMES' USING FULL-NAME-VALUE.

           STOP RUN.
