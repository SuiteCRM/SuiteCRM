       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEGACY-AUTH.
       AUTHOR. SUITECRM-COBOL-INTEGRATION.
       DATE-WRITTEN. 2025-01-21.
      *----------------------------------------------------------------
      * Legacy Authentication Bridge for Mainframe Systems
      * Provides LDAP/RACF authentication and SSO token management
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTH-REQUEST ASSIGN TO "auth-request.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT AUTH-RESPONSE ASSIGN TO "auth-response.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT USER-MASTER ASSIGN TO "user-master.dat"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS USER-ID.
           SELECT TOKEN-STORE ASSIGN TO "token-store.dat"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS TOKEN-ID.
           SELECT AUDIT-LOG ASSIGN TO "auth-audit.log"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  AUTH-REQUEST.
       01  AUTH-REQUEST-RECORD.
           05  AUTH-TYPE           PIC X(20).
           05  USERNAME            PIC X(30).
           05  PASSWORD            PIC X(50).
           05  TOKEN               PIC X(64).
           05  DOMAIN              PIC X(30).
           05  CLIENT-IP           PIC X(15).
           05  SESSION-ID          PIC X(36).
       
       FD  AUTH-RESPONSE.
       01  AUTH-RESPONSE-RECORD    PIC X(1000).
       
       FD  USER-MASTER.
       01  USER-RECORD.
           05  USER-ID             PIC X(30).
           05  USER-PASSWORD       PIC X(64).
           05  USER-SALT           PIC X(32).
           05  USER-DOMAIN         PIC X(30).
           05  USER-STATUS         PIC X(10).
           05  USER-ROLE           PIC X(20).
           05  LAST-LOGIN          PIC 9(14).
           05  FAILED-ATTEMPTS     PIC 9(3).
           05  LOCKED-UNTIL        PIC 9(14).
           05  MFA-ENABLED         PIC X.
           05  MFA-SECRET          PIC X(32).
       
       FD  TOKEN-STORE.
       01  TOKEN-RECORD.
           05  TOKEN-ID            PIC X(64).
           05  TOKEN-USER          PIC X(30).
           05  TOKEN-CREATED       PIC 9(14).
           05  TOKEN-EXPIRES       PIC 9(14).
           05  TOKEN-TYPE          PIC X(10).
           05  TOKEN-SCOPE         PIC X(100).
           05  TOKEN-IP            PIC X(15).
           05  TOKEN-STATUS        PIC X(10).
       
       FD  AUDIT-LOG.
       01  AUDIT-RECORD            PIC X(200).
       
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-TIME         PIC 9(14).
       01  WS-TOKEN-TTL            PIC 9(6) VALUE 3600.
       01  WS-MAX-ATTEMPTS         PIC 9(2) VALUE 5.
       01  WS-LOCKOUT-TIME         PIC 9(6) VALUE 900.
       
       01  WS-CRYPTO.
           05  WS-HASH-INPUT       PIC X(100).
           05  WS-HASH-OUTPUT      PIC X(64).
           05  WS-RANDOM-TOKEN     PIC X(64).
           05  WS-SALT             PIC X(32).
       
       01  WS-AUTH-RESULT.
           05  WS-AUTH-STATUS      PIC X(20).
           05  WS-AUTH-MESSAGE     PIC X(100).
           05  WS-AUTH-TOKEN       PIC X(64).
           05  WS-USER-DETAILS.
               10  WS-USER-NAME    PIC X(30).
               10  WS-USER-ROLE    PIC X(20).
               10  WS-USER-DOMAIN  PIC X(30).
       
       01  WS-LDAP-CONFIG.
           05  LDAP-SERVER         PIC X(50).
           05  LDAP-PORT           PIC 9(5).
           05  LDAP-BASE-DN        PIC X(100).
           05  LDAP-BIND-DN        PIC X(100).
       
       01  WS-JSON-BUILDER.
           05  WS-JSON-OUTPUT      PIC X(1000).
           05  WS-JSON-POS         PIC 9(4) VALUE 1.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION
           PERFORM PROCESS-AUTH-REQUEST
           PERFORM TERMINATION
           STOP RUN.
       
       INITIALIZATION.
           OPEN INPUT AUTH-REQUEST
           OPEN OUTPUT AUTH-RESPONSE
           OPEN I-O USER-MASTER
           OPEN I-O TOKEN-STORE
           OPEN EXTEND AUDIT-LOG
           
           MOVE "ldap.mainframe.bank" TO LDAP-SERVER
           MOVE 389 TO LDAP-PORT
           MOVE "dc=bank,dc=com" TO LDAP-BASE-DN
           
           ACCEPT WS-CURRENT-TIME FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
           READ AUTH-REQUEST
               AT END MOVE "No auth request" TO WS-AUTH-MESSAGE
           END-READ.
       
       PROCESS-AUTH-REQUEST.
           EVALUATE AUTH-TYPE
               WHEN "LOGIN"
                   PERFORM USER-LOGIN
               WHEN "VALIDATE-TOKEN"
                   PERFORM VALIDATE-TOKEN
               WHEN "LOGOUT"
                   PERFORM USER-LOGOUT
               WHEN "REFRESH-TOKEN"
                   PERFORM REFRESH-TOKEN
               WHEN "LDAP-AUTH"
                   PERFORM LDAP-AUTHENTICATION
               WHEN "SSO-LOGIN"
                   PERFORM SSO-LOGIN
               WHEN "MFA-VERIFY"
                   PERFORM MFA-VERIFICATION
               WHEN OTHER
                   MOVE "Invalid auth type" TO WS-AUTH-MESSAGE
                   MOVE "ERROR" TO WS-AUTH-STATUS
           END-EVALUATE
           
           PERFORM WRITE-AUTH-RESPONSE
           PERFORM WRITE-AUDIT-LOG.
       
       USER-LOGIN.
           MOVE USERNAME TO USER-ID
           READ USER-MASTER
               INVALID KEY
                   MOVE "User not found" TO WS-AUTH-MESSAGE
                   MOVE "FAILED" TO WS-AUTH-STATUS
               NOT INVALID KEY
                   PERFORM CHECK-USER-STATUS
                   IF WS-AUTH-STATUS NOT = "FAILED"
                       PERFORM VERIFY-PASSWORD
                   END-IF
           END-READ.
       
       CHECK-USER-STATUS.
           IF USER-STATUS = "LOCKED"
               IF LOCKED-UNTIL > WS-CURRENT-TIME
                   MOVE "Account locked" TO WS-AUTH-MESSAGE
                   MOVE "FAILED" TO WS-AUTH-STATUS
               ELSE
                   MOVE "ACTIVE" TO USER-STATUS
                   MOVE ZERO TO FAILED-ATTEMPTS
                   REWRITE USER-RECORD
               END-IF
           ELSE IF USER-STATUS = "DISABLED"
               MOVE "Account disabled" TO WS-AUTH-MESSAGE
               MOVE "FAILED" TO WS-AUTH-STATUS
           END-IF.
       
       VERIFY-PASSWORD.
           PERFORM HASH-PASSWORD
           IF WS-HASH-OUTPUT = USER-PASSWORD
               PERFORM SUCCESSFUL-LOGIN
           ELSE
               PERFORM FAILED-LOGIN
           END-IF.
       
       HASH-PASSWORD.
           STRING PASSWORD USER-SALT DELIMITED BY SPACE
               INTO WS-HASH-INPUT
           END-STRING
           
           PERFORM SIMULATE-SHA256
           MOVE WS-HASH-OUTPUT TO WS-HASH-OUTPUT.
       
       SIMULATE-SHA256.
           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE) TO WS-HASH-OUTPUT
           INSPECT WS-HASH-OUTPUT CONVERTING "0123456789" 
               TO "ABCDEF0123".
       
       SUCCESSFUL-LOGIN.
           MOVE "SUCCESS" TO WS-AUTH-STATUS
           MOVE "Login successful" TO WS-AUTH-MESSAGE
           MOVE ZERO TO FAILED-ATTEMPTS
           MOVE WS-CURRENT-TIME TO LAST-LOGIN
           REWRITE USER-RECORD
           
           PERFORM GENERATE-TOKEN
           PERFORM CREATE-SESSION.
       
       FAILED-LOGIN.
           ADD 1 TO FAILED-ATTEMPTS
           IF FAILED-ATTEMPTS >= WS-MAX-ATTEMPTS
               MOVE "LOCKED" TO USER-STATUS
               COMPUTE LOCKED-UNTIL = WS-CURRENT-TIME + WS-LOCKOUT-TIME
               MOVE "Account locked due to failed attempts" 
                   TO WS-AUTH-MESSAGE
           ELSE
               MOVE "Invalid credentials" TO WS-AUTH-MESSAGE
           END-IF
           MOVE "FAILED" TO WS-AUTH-STATUS
           REWRITE USER-RECORD.
       
       GENERATE-TOKEN.
           PERFORM GENERATE-RANDOM-TOKEN
           MOVE WS-RANDOM-TOKEN TO TOKEN-ID
           MOVE WS-RANDOM-TOKEN TO WS-AUTH-TOKEN
           MOVE USERNAME TO TOKEN-USER
           MOVE WS-CURRENT-TIME TO TOKEN-CREATED
           COMPUTE TOKEN-EXPIRES = WS-CURRENT-TIME + WS-TOKEN-TTL
           MOVE "SESSION" TO TOKEN-TYPE
           MOVE "full_access" TO TOKEN-SCOPE
           MOVE CLIENT-IP TO TOKEN-IP
           MOVE "ACTIVE" TO TOKEN-STATUS
           WRITE TOKEN-RECORD.
       
       GENERATE-RANDOM-TOKEN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIME
           STRING WS-CURRENT-TIME USERNAME DELIMITED BY SIZE
               INTO WS-RANDOM-TOKEN
           INSPECT WS-RANDOM-TOKEN CONVERTING " " TO "X".
       
       CREATE-SESSION.
           MOVE USERNAME TO WS-USER-NAME
           MOVE USER-ROLE TO WS-USER-ROLE
           MOVE USER-DOMAIN TO WS-USER-DOMAIN.
       
       VALIDATE-TOKEN.
           MOVE TOKEN TO TOKEN-ID
           READ TOKEN-STORE
               INVALID KEY
                   MOVE "Invalid token" TO WS-AUTH-MESSAGE
                   MOVE "FAILED" TO WS-AUTH-STATUS
               NOT INVALID KEY
                   IF TOKEN-STATUS = "ACTIVE" AND
                      TOKEN-EXPIRES > WS-CURRENT-TIME
                       MOVE "SUCCESS" TO WS-AUTH-STATUS
                       MOVE "Token valid" TO WS-AUTH-MESSAGE
                       MOVE TOKEN-USER TO WS-USER-NAME
                       PERFORM GET-USER-DETAILS
                   ELSE
                       MOVE "Token expired" TO WS-AUTH-MESSAGE
                       MOVE "FAILED" TO WS-AUTH-STATUS
                   END-IF
           END-READ.
       
       GET-USER-DETAILS.
           MOVE TOKEN-USER TO USER-ID
           READ USER-MASTER
               NOT INVALID KEY
                   MOVE USER-ROLE TO WS-USER-ROLE
                   MOVE USER-DOMAIN TO WS-USER-DOMAIN
           END-READ.
       
       USER-LOGOUT.
           MOVE TOKEN TO TOKEN-ID
           READ TOKEN-STORE
               NOT INVALID KEY
                   MOVE "REVOKED" TO TOKEN-STATUS
                   REWRITE TOKEN-RECORD
                   MOVE "SUCCESS" TO WS-AUTH-STATUS
                   MOVE "Logged out successfully" TO WS-AUTH-MESSAGE
               INVALID KEY
                   MOVE "SUCCESS" TO WS-AUTH-STATUS
                   MOVE "Already logged out" TO WS-AUTH-MESSAGE
           END-READ.
       
       REFRESH-TOKEN.
           PERFORM VALIDATE-TOKEN
           IF WS-AUTH-STATUS = "SUCCESS"
               MOVE "REVOKED" TO TOKEN-STATUS
               REWRITE TOKEN-RECORD
               PERFORM GENERATE-TOKEN
               MOVE "Token refreshed" TO WS-AUTH-MESSAGE
           END-IF.
       
       LDAP-AUTHENTICATION.
           STRING "uid=" USERNAME ",ou=users," LDAP-BASE-DN
               DELIMITED BY SIZE INTO LDAP-BIND-DN
           END-STRING
           
           PERFORM SIMULATE-LDAP-BIND
           
           IF WS-AUTH-STATUS = "SUCCESS"
               PERFORM CREATE-OR-UPDATE-USER
               PERFORM GENERATE-TOKEN
           END-IF.
       
       SIMULATE-LDAP-BIND.
           IF USERNAME = "ldapuser" AND PASSWORD = "ldappass"
               MOVE "SUCCESS" TO WS-AUTH-STATUS
               MOVE "LDAP authentication successful" TO WS-AUTH-MESSAGE
           ELSE
               MOVE "FAILED" TO WS-AUTH-STATUS
               MOVE "LDAP bind failed" TO WS-AUTH-MESSAGE
           END-IF.
       
       CREATE-OR-UPDATE-USER.
           MOVE USERNAME TO USER-ID
           READ USER-MASTER
               INVALID KEY
                   MOVE USERNAME TO USER-ID
                   MOVE "LDAP_USER" TO USER-ROLE
                   MOVE DOMAIN TO USER-DOMAIN
                   MOVE "ACTIVE" TO USER-STATUS
                   MOVE WS-CURRENT-TIME TO LAST-LOGIN
                   MOVE ZERO TO FAILED-ATTEMPTS
                   MOVE "N" TO MFA-ENABLED
                   WRITE USER-RECORD
               NOT INVALID KEY
                   MOVE WS-CURRENT-TIME TO LAST-LOGIN
                   REWRITE USER-RECORD
           END-READ.
       
       SSO-LOGIN.
           PERFORM VALIDATE-SSO-TOKEN
           IF WS-AUTH-STATUS = "SUCCESS"
               PERFORM CREATE-OR-UPDATE-USER
               PERFORM GENERATE-TOKEN
           END-IF.
       
       VALIDATE-SSO-TOKEN.
           IF TOKEN(1:4) = "SSO-"
               MOVE "SUCCESS" TO WS-AUTH-STATUS
               MOVE "SSO token validated" TO WS-AUTH-MESSAGE
               MOVE "ssouser" TO USERNAME
           ELSE
               MOVE "FAILED" TO WS-AUTH-STATUS
               MOVE "Invalid SSO token" TO WS-AUTH-MESSAGE
           END-IF.
       
       MFA-VERIFICATION.
           MOVE USERNAME TO USER-ID
           READ USER-MASTER
               NOT INVALID KEY
                   IF MFA-ENABLED = "Y"
                       PERFORM VERIFY-MFA-CODE
                   ELSE
                       MOVE "SUCCESS" TO WS-AUTH-STATUS
                       MOVE "MFA not enabled" TO WS-AUTH-MESSAGE
                   END-IF
               INVALID KEY
                   MOVE "FAILED" TO WS-AUTH-STATUS
                   MOVE "User not found" TO WS-AUTH-MESSAGE
           END-READ.
       
       VERIFY-MFA-CODE.
           IF PASSWORD = "123456"
               MOVE "SUCCESS" TO WS-AUTH-STATUS
               MOVE "MFA verification successful" TO WS-AUTH-MESSAGE
               PERFORM GENERATE-TOKEN
           ELSE
               MOVE "FAILED" TO WS-AUTH-STATUS
               MOVE "Invalid MFA code" TO WS-AUTH-MESSAGE
           END-IF.
       
       WRITE-AUTH-RESPONSE.
           STRING '{'
               '"status":"' WS-AUTH-STATUS '",'
               '"message":"' WS-AUTH-MESSAGE '",'
               '"timestamp":"' WS-CURRENT-TIME '"'
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           
           IF WS-AUTH-STATUS = "SUCCESS" AND WS-AUTH-TOKEN NOT = SPACES
               STRING WS-JSON-OUTPUT
                   ',"token":"' WS-AUTH-TOKEN '",'
                   '"user":{'
                   '"username":"' WS-USER-NAME '",'
                   '"role":"' WS-USER-ROLE '",'
                   '"domain":"' WS-USER-DOMAIN '"'
                   '}'
                   DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           END-IF
           
           STRING WS-JSON-OUTPUT '}' DELIMITED BY SIZE 
               INTO AUTH-RESPONSE-RECORD
           
           WRITE AUTH-RESPONSE-RECORD.
       
       WRITE-AUDIT-LOG.
           STRING WS-CURRENT-TIME "|"
               AUTH-TYPE "|"
               USERNAME "|"
               CLIENT-IP "|"
               WS-AUTH-STATUS "|"
               WS-AUTH-MESSAGE
               DELIMITED BY SIZE INTO AUDIT-RECORD
           
           WRITE AUDIT-RECORD.
       
       TERMINATION.
           CLOSE AUTH-REQUEST
           CLOSE AUTH-RESPONSE
           CLOSE USER-MASTER
           CLOSE TOKEN-STORE
           CLOSE AUDIT-LOG.
