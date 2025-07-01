
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RBAC-SECURITY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Input variables
       01 WS-USER-ID              PIC X(8).
       01 WS-ROLE-CODE            PIC X(1).
       01 WS-OPERATION            PIC X(2).
       01 WS-RESOURCE-LEVEL       PIC 9(1).

      * Validation variables
       01 WS-VALID-FLAG           PIC X(1) VALUE 'Y'.
       01 WS-ERROR-MESSAGE        PIC X(100).
       01 WS-INVALID-CHARS        PIC X(50).
       01 WS-CHAR-COUNT           PIC 9(2) VALUE ZERO.
       01 WS-ACCESS-GRANTED       PIC X(1) VALUE 'N'.

      * Loop counters and temporary variables
       01 WS-INDEX                PIC 9(2).
       01 WS-CURRENT-CHAR         PIC X(1).

      * Role descriptions for output
       01 WS-ROLE-DESC            PIC X(10).
       01 WS-OP-DESC              PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM GET-USER-INPUT
           PERFORM VALIDATE-INPUT

           IF WS-VALID-FLAG = 'Y'
               PERFORM CHECK-PERMISSIONS
               PERFORM DISPLAY-RESULT
           ELSE
               PERFORM DISPLAY-ERROR
           END-IF

           STOP RUN.

       GET-USER-INPUT.
           DISPLAY "Enter User ID: " WITH NO ADVANCING
           ACCEPT WS-USER-ID

           DISPLAY "Enter Role Code (A/M/U/G): " WITH NO ADVANCING
           ACCEPT WS-ROLE-CODE

           DISPLAY "Enter Operation (RD/WR/DL/AU): " WITH NO ADVANCING
           ACCEPT WS-OPERATION

           DISPLAY "Enter Resource Level (1-5): " WITH NO ADVANCING
           ACCEPT WS-RESOURCE-LEVEL.

       VALIDATE-INPUT.
           PERFORM VALIDATE-USER-ID
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-ROLE-CODE
           END-IF
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-OPERATION
           END-IF
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-RESOURCE-LEVEL
           END-IF.

       VALIDATE-USER-ID.
           MOVE SPACES TO WS-INVALID-CHARS
           MOVE ZERO TO WS-CHAR-COUNT

           PERFORM VARYING WS-INDEX FROM 1 BY 1 
                   UNTIL WS-INDEX > 8
               MOVE WS-USER-ID(WS-INDEX:1) TO WS-CURRENT-CHAR

               IF WS-CURRENT-CHAR = SPACE
                   EXIT PERFORM
               END-IF

               IF WS-CURRENT-CHAR = "'" OR
                  WS-CURRENT-CHAR = ";" OR
                  WS-CURRENT-CHAR = "-" OR
                  WS-CURRENT-CHAR = "/" OR
                  WS-CURRENT-CHAR = "*" OR
                  WS-CURRENT-CHAR = "%" OR
                  WS-CURRENT-CHAR = "=" OR
                  WS-CURRENT-CHAR = "<" OR
                  WS-CURRENT-CHAR = ">" OR
                  WS-CURRENT-CHAR = "(" OR
                  WS-CURRENT-CHAR = ")"
                   PERFORM HANDLE-INVALID-CHAR
               ELSE
                   IF WS-CURRENT-CHAR >= 'A'
                       IF WS-CURRENT-CHAR <= 'Z'
                           CONTINUE
                       ELSE
                           PERFORM HANDLE-INVALID-CHAR
                       END-IF
                   ELSE
                       IF WS-CURRENT-CHAR >= 'a'
                           IF WS-CURRENT-CHAR <= 'z'
                               CONTINUE
                           ELSE
                               PERFORM HANDLE-INVALID-CHAR
                           END-IF
                       ELSE
                           IF WS-CURRENT-CHAR >= '0'
                               IF WS-CURRENT-CHAR <= '9'
                                   CONTINUE
                               ELSE
                                   PERFORM HANDLE-INVALID-CHAR
                               END-IF
                           ELSE
                               IF WS-CURRENT-CHAR = SPACE
                                   CONTINUE
                               ELSE
                                   PERFORM HANDLE-INVALID-CHAR
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-CHAR-COUNT > 0
               MOVE 'N' TO WS-VALID-FLAG
               STRING "VALIDATION ERROR: User ID contains invalid "
                      "characters!" DELIMITED BY SIZE
                      INTO WS-ERROR-MESSAGE
               END-STRING
           END-IF.

       HANDLE-INVALID-CHAR.
           ADD 1 TO WS-CHAR-COUNT
           IF WS-CHAR-COUNT = 1
               MOVE WS-CURRENT-CHAR TO WS-INVALID-CHARS(1:1)
           ELSE
               STRING WS-INVALID-CHARS DELIMITED BY SPACE
                      " " DELIMITED BY SIZE
                      WS-CURRENT-CHAR DELIMITED BY SIZE
                      INTO WS-INVALID-CHARS
               END-STRING
           END-IF.

       VALIDATE-ROLE-CODE.
           IF WS-ROLE-CODE NOT = 'A' AND
              WS-ROLE-CODE NOT = 'M' AND
              WS-ROLE-CODE NOT = 'U' AND
              WS-ROLE-CODE NOT = 'G'
               MOVE 'N' TO WS-VALID-FLAG
               MOVE "VALIDATION ERROR: Invalid role code!" 
                    TO WS-ERROR-MESSAGE
           END-IF.

       VALIDATE-OPERATION.
           IF WS-OPERATION NOT = 'RD' AND
              WS-OPERATION NOT = 'WR' AND
              WS-OPERATION NOT = 'DL' AND
              WS-OPERATION NOT = 'AU'
               MOVE 'N' TO WS-VALID-FLAG
               MOVE "VALIDATION ERROR: Invalid operation code!" 
                    TO WS-ERROR-MESSAGE
           END-IF.

       VALIDATE-RESOURCE-LEVEL.
           IF WS-RESOURCE-LEVEL < 1 OR WS-RESOURCE-LEVEL > 5
               MOVE 'N' TO WS-VALID-FLAG
               MOVE "VALIDATION ERROR: Resource level must be 1-5!" 
                    TO WS-ERROR-MESSAGE
           END-IF.

       CHECK-PERMISSIONS.
           MOVE 'N' TO WS-ACCESS-GRANTED

           EVALUATE WS-ROLE-CODE
               WHEN 'A' 
                   MOVE 'Y' TO WS-ACCESS-GRANTED
                   MOVE "Admin" TO WS-ROLE-DESC

               WHEN 'M'
                   MOVE "Manager" TO WS-ROLE-DESC
                   IF WS-RESOURCE-LEVEL <= 4
                       MOVE 'Y' TO WS-ACCESS-GRANTED
                   ELSE
                       IF WS-OPERATION = 'RD' OR WS-OPERATION = 'AU'
                           MOVE 'Y' TO WS-ACCESS-GRANTED
                       END-IF
                   END-IF

               WHEN 'U'
                   MOVE "User" TO WS-ROLE-DESC
                   IF WS-RESOURCE-LEVEL <= 3
                       IF WS-OPERATION = 'RD' OR WS-OPERATION = 'WR'
                           MOVE 'Y' TO WS-ACCESS-GRANTED
                       END-IF
                   ELSE
                       IF WS-RESOURCE-LEVEL = 4 AND WS-OPERATION = 'RD'
                           MOVE 'Y' TO WS-ACCESS-GRANTED
                       END-IF
                   END-IF

               WHEN 'G'
                   MOVE "Guest" TO WS-ROLE-DESC
                   IF WS-RESOURCE-LEVEL = 1 AND WS-OPERATION = 'RD'
                       MOVE 'Y' TO WS-ACCESS-GRANTED
                   END-IF
           END-EVALUATE

           EVALUATE WS-OPERATION
               WHEN 'RD' MOVE "READ" TO WS-OP-DESC
               WHEN 'WR' MOVE "WRITE" TO WS-OP-DESC
               WHEN 'DL' MOVE "DELETE" TO WS-OP-DESC
               WHEN 'AU' MOVE "AUDIT" TO WS-OP-DESC
           END-EVALUATE.

       DISPLAY-RESULT.
           IF WS-ACCESS-GRANTED = 'Y'
               DISPLAY "ACCESS GRANTED: User " FUNCTION 
                                TRIM(WS-USER-ID) " (" 
                       FUNCTION TRIM(WS-ROLE-DESC) ") authorized for " 
                       FUNCTION 
                                TRIM(WS-OP-DESC) 
                       " on Level " FUNCTION 
                                TRIM(WS-RESOURCE-LEVEL) " resource"
           ELSE
               DISPLAY "ACCESS DENIED: Insufficient privileges"
               DISPLAY "User role '" FUNCTION 
                                TRIM(WS-ROLE-DESC) 
                       "' not authorized for " FUNCTION 
                                TRIM(WS-OP-DESC) 
                       " operations on Level " FUNCTION 
                                TRIM(WS-RESOURCE-LEVEL) 
                       " resources"
           END-IF.

       DISPLAY-ERROR.
           DISPLAY WS-ERROR-MESSAGE
           IF WS-CHAR-COUNT > 0
               DISPLAY "Invalid characters detected: " FUNCTION 
                                TRIM(WS-INVALID-CHARS)
               DISPLAY "Security violation logged for attempted " 
                       "injection attack"
           END-IF.

