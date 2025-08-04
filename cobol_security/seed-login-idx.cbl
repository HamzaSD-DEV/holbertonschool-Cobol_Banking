       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEED-LOGIN-IDX.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE
             ASSIGN TO "users-login.idx"
             ORGANIZATION IS INDEXED
             RECORD KEY IS I-USERID
             FILE STATUS IS FS-IDX.

       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           05 I-USERID       PIC X(8).
           05 I-USERNAME     PIC X(15).
           05 I-PASS-ENC     PIC X(15).
           05 I-TOKEN        PIC X(16).
           05 I-TIME         PIC X(6).

       WORKING-STORAGE SECTION.
       77  FS-IDX          PIC X(2).

       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT IDX-FILE

           *> Record 1
           MOVE "USER0010"             TO I-USERID
           MOVE "JOHNYSMOOTH    "       TO I-USERNAME
           MOVE "QCVW68A8       "       TO I-PASS-ENC
           MOVE " "     TO I-TOKEN
           MOVE " "               TO I-TIME
           WRITE IDX-REC
             INVALID KEY
               DISPLAY "Duplicate key on USER0010" 
               DISPLAY "Status: " FS-IDX
           END-WRITE

           *> Record 2
           MOVE "USER0020"             TO I-USERID
           MOVE "MARIASNOW      "       TO I-USERNAME
           MOVE "IGOPT87AEA     "       TO I-PASS-ENC
           MOVE " "     TO I-TOKEN
           MOVE " "               TO I-TIME
           WRITE IDX-REC
             INVALID KEY
               DISPLAY "Duplicate key on USER0020" 
               DISPLAY "Status: " FS-IDX
           END-WRITE

           *> Record 3
           MOVE "USER0030"             TO I-USERID
           MOVE "BOBBYRAY       "       TO I-USERNAME
           MOVE "DQESQX1TN2B    "       TO I-PASS-ENC
           MOVE " "     TO I-TOKEN
           MOVE " "               TO I-TIME
           WRITE IDX-REC
             INVALID KEY
               DISPLAY "Duplicate key on USER0030" 
               DISPLAY "Status: " FS-IDX
           END-WRITE

           CLOSE IDX-FILE
           DISPLAY "Indexed file users-login.idx seeded successfully."
           STOP RUN.
