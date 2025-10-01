       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINFRAME-SYNC.
       AUTHOR. SUITECRM-COBOL-INTEGRATION.
       DATE-WRITTEN. 2025-01-21.
      *----------------------------------------------------------------
      * Mainframe Synchronization Service for Banking Core Systems
      * Simulates integration with Fiserv DNA, Jack Henry, FIS
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYNC-REQUEST ASSIGN TO "sync-request.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SYNC-RESPONSE ASSIGN TO "sync-response.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-MASTER ASSIGN TO "account-master.dat"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER.
           SELECT TRANSACTION-LOG ASSIGN TO "transaction-log.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SYNC-REQUEST.
       01  SYNC-REQUEST-RECORD.
           05  SYNC-TYPE           PIC X(20).
           05  CUSTOMER-ID         PIC X(10).
           05  ACCOUNT-NUMBER      PIC X(20).
           05  START-DATE          PIC 9(8).
           05  END-DATE            PIC 9(8).
           05  SYNC-DIRECTION      PIC X(10).
       
       FD  SYNC-RESPONSE.
       01  SYNC-RESPONSE-RECORD    PIC X(500).
       
       FD  ACCOUNT-MASTER.
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER         PIC X(20).
           05  ACCT-TYPE           PIC X(10).
           05  CUSTOMER-ID         PIC X(10).
           05  BALANCE             PIC S9(12)V99 COMP-3.
           05  AVAILABLE-BALANCE   PIC S9(12)V99 COMP-3.
           05  INTEREST-RATE       PIC 9(2)V9(4) COMP-3.
           05  OPEN-DATE           PIC 9(8).
           05  LAST-ACTIVITY       PIC 9(8).
           05  ACCT-STATUS         PIC X(10).
       
       FD  TRANSACTION-LOG.
       01  TRANSACTION-RECORD.
           05  TRANS-ID            PIC X(20).
           05  TRANS-ACCT          PIC X(20).
           05  TRANS-DATE          PIC 9(8).
           05  TRANS-TIME          PIC 9(6).
           05  TRANS-TYPE          PIC X(10).
           05  TRANS-AMOUNT        PIC S9(10)V99 COMP-3.
           05  TRANS-BALANCE       PIC S9(12)V99 COMP-3.
           05  TRANS-DESC          PIC X(50).
       
       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(6) VALUE ZERO.
           05  WS-ERROR-COUNT      PIC 9(6) VALUE ZERO.
           05  WS-SUCCESS-COUNT    PIC 9(6) VALUE ZERO.
       
       01  WS-SYNC-STATUS.
           05  WS-SYNC-ID          PIC X(36).
           05  WS-START-TIME       PIC 9(14).
           05  WS-END-TIME         PIC 9(14).
           05  WS-STATUS           PIC X(20).
           05  WS-ERROR-MSG        PIC X(100).
       
       01  WS-JSON-BUILDER.
           05  WS-JSON-OUTPUT      PIC X(5000).
           05  WS-JSON-POSITION    PIC 9(4) VALUE 1.
       
       01  WS-ACCOUNT-TOTALS.
           05  WS-TOTAL-BALANCE    PIC S9(14)V99 COMP-3.
           05  WS-AVG-BALANCE      PIC S9(12)V99 COMP-3.
           05  WS-ACCOUNT-COUNT    PIC 9(6).
       
       01  WS-DISPLAY-FIELDS.
           05  DSP-BALANCE         PIC $$$,$$$,$$$,$$9.99-.
           05  DSP-AMOUNT          PIC $$$,$$$,$$9.99-.
           05  DSP-COUNT           PIC ZZZ,ZZ9.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION
           PERFORM PROCESS-SYNC-REQUEST
           PERFORM TERMINATION
           STOP RUN.
       
       INITIALIZATION.
           OPEN INPUT SYNC-REQUEST
           OPEN OUTPUT SYNC-RESPONSE
           OPEN I-O ACCOUNT-MASTER
           OPEN INPUT TRANSACTION-LOG
           
           ACCEPT WS-START-TIME FROM DATE YYYYMMDD
           ACCEPT WS-SYNC-ID FROM TIME
           
           READ SYNC-REQUEST
               AT END MOVE "No sync request" TO WS-ERROR-MSG
           END-READ.
       
       PROCESS-SYNC-REQUEST.
           EVALUATE SYNC-TYPE
               WHEN "ACCOUNT-SYNC"
                   PERFORM SYNC-ACCOUNTS
               WHEN "TRANSACTION-SYNC"
                   PERFORM SYNC-TRANSACTIONS
               WHEN "BALANCE-INQUIRY"
                   PERFORM BALANCE-INQUIRY
               WHEN "CUSTOMER-ACCOUNTS"
                   PERFORM GET-CUSTOMER-ACCOUNTS
               WHEN "ACCOUNT-HISTORY"
                   PERFORM GET-ACCOUNT-HISTORY
               WHEN "BATCH-UPDATE"
                   PERFORM BATCH-ACCOUNT-UPDATE
               WHEN OTHER
                   MOVE "Invalid sync type" TO WS-ERROR-MSG
                   MOVE "ERROR" TO WS-STATUS
           END-EVALUATE.
       
       SYNC-ACCOUNTS.
           MOVE "PROCESSING" TO WS-STATUS
           MOVE ZERO TO WS-RECORD-COUNT
           MOVE ZERO TO WS-TOTAL-BALANCE
           
           MOVE ACCOUNT-NUMBER TO ACCT-NUMBER
           START ACCOUNT-MASTER KEY >= ACCT-NUMBER
               INVALID KEY MOVE "Account not found" TO WS-ERROR-MSG
           END-START
           
           PERFORM UNTIL WS-RECORD-COUNT > 1000
               READ ACCOUNT-MASTER NEXT
                   AT END EXIT PERFORM
               END-READ
               
               IF CUSTOMER-ID OF ACCOUNT-RECORD = 
                  CUSTOMER-ID OF SYNC-REQUEST-RECORD
                   ADD 1 TO WS-RECORD-COUNT
                   ADD BALANCE TO WS-TOTAL-BALANCE
                   PERFORM BUILD-ACCOUNT-JSON
               END-IF
           END-PERFORM
           
           MOVE WS-RECORD-COUNT TO WS-ACCOUNT-COUNT
           IF WS-ACCOUNT-COUNT > 0
               COMPUTE WS-AVG-BALANCE = WS-TOTAL-BALANCE / 
                                        WS-ACCOUNT-COUNT
           END-IF
           
           PERFORM FINALIZE-JSON-RESPONSE
           MOVE "SUCCESS" TO WS-STATUS.
       
       SYNC-TRANSACTIONS.
           MOVE "PROCESSING" TO WS-STATUS
           MOVE ZERO TO WS-RECORD-COUNT
           
           PERFORM READ-TRANSACTION-LOG
               UNTIL WS-RECORD-COUNT > 500.
       
       READ-TRANSACTION-LOG.
           READ TRANSACTION-LOG
               AT END EXIT PARAGRAPH
           END-READ
           
           IF TRANS-ACCT = ACCOUNT-NUMBER AND
              TRANS-DATE >= START-DATE AND
              TRANS-DATE <= END-DATE
               ADD 1 TO WS-RECORD-COUNT
               PERFORM BUILD-TRANSACTION-JSON
           END-IF.
       
       BALANCE-INQUIRY.
           MOVE ACCOUNT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-MASTER
               INVALID KEY 
                   MOVE "Account not found" TO WS-ERROR-MSG
                   MOVE "ERROR" TO WS-STATUS
               NOT INVALID KEY
                   PERFORM BUILD-BALANCE-JSON
                   MOVE "SUCCESS" TO WS-STATUS
           END-READ.
       
       GET-CUSTOMER-ACCOUNTS.
           MOVE "PROCESSING" TO WS-STATUS
           MOVE ZERO TO WS-RECORD-COUNT
           MOVE SPACES TO ACCT-NUMBER
           
           START ACCOUNT-MASTER KEY >= ACCT-NUMBER
           END-START
           
           STRING '{"customerAccounts":[' 
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           
           PERFORM UNTIL WS-RECORD-COUNT > 100
               READ ACCOUNT-MASTER NEXT
                   AT END EXIT PERFORM
               END-READ
               
               IF CUSTOMER-ID OF ACCOUNT-RECORD = 
                  CUSTOMER-ID OF SYNC-REQUEST-RECORD
                   IF WS-RECORD-COUNT > 0
                       STRING ',' DELIMITED BY SIZE 
                           INTO WS-JSON-OUTPUT
                           WITH POINTER WS-JSON-POSITION
                   END-IF
                   ADD 1 TO WS-RECORD-COUNT
                   PERFORM BUILD-ACCOUNT-JSON
               END-IF
           END-PERFORM
           
           STRING '],' DELIMITED BY SIZE 
               INTO WS-JSON-OUTPUT
               WITH POINTER WS-JSON-POSITION
           
           MOVE WS-RECORD-COUNT TO DSP-COUNT
           STRING '"totalAccounts":' DSP-COUNT ',' DELIMITED BY SIZE
               INTO WS-JSON-OUTPUT
               WITH POINTER WS-JSON-POSITION
           
           MOVE WS-TOTAL-BALANCE TO DSP-BALANCE
           STRING '"totalBalance":"' DSP-BALANCE '"}' 
               DELIMITED BY SIZE
               INTO WS-JSON-OUTPUT
               WITH POINTER WS-JSON-POSITION
           
           MOVE WS-JSON-OUTPUT TO SYNC-RESPONSE-RECORD
           WRITE SYNC-RESPONSE-RECORD
           MOVE "SUCCESS" TO WS-STATUS.
       
       GET-ACCOUNT-HISTORY.
           PERFORM SYNC-TRANSACTIONS.
       
       BATCH-ACCOUNT-UPDATE.
           MOVE "Batch update simulated" TO WS-ERROR-MSG
           MOVE "SUCCESS" TO WS-STATUS.
       
       BUILD-ACCOUNT-JSON.
           MOVE BALANCE TO DSP-BALANCE
           MOVE AVAILABLE-BALANCE TO DSP-AMOUNT
           
           STRING '{'
               '"accountNumber":"' ACCT-NUMBER '",'
               '"accountType":"' ACCT-TYPE '",'
               '"balance":"' DSP-BALANCE '",'
               '"availableBalance":"' DSP-AMOUNT '",'
               '"interestRate":' INTEREST-RATE ','
               '"status":"' ACCT-STATUS OF ACCOUNT-RECORD '"'
               '}'
               DELIMITED BY SIZE 
               INTO WS-JSON-OUTPUT
               WITH POINTER WS-JSON-POSITION.
       
       BUILD-TRANSACTION-JSON.
           MOVE TRANS-AMOUNT TO DSP-AMOUNT
           MOVE TRANS-BALANCE TO DSP-BALANCE
           
           STRING '{'
               '"transactionId":"' TRANS-ID '",'
               '"date":"' TRANS-DATE '",'
               '"time":"' TRANS-TIME '",'
               '"type":"' TRANS-TYPE '",'
               '"amount":"' DSP-AMOUNT '",'
               '"balance":"' DSP-BALANCE '",'
               '"description":"' TRANS-DESC '"'
               '}'
               DELIMITED BY SIZE 
               INTO WS-JSON-OUTPUT
               WITH POINTER WS-JSON-POSITION.
       
       BUILD-BALANCE-JSON.
           MOVE BALANCE TO DSP-BALANCE
           MOVE AVAILABLE-BALANCE TO DSP-AMOUNT
           
           STRING '{'
               '"accountNumber":"' ACCT-NUMBER '",'
               '"balance":"' DSP-BALANCE '",'
               '"availableBalance":"' DSP-AMOUNT '",'
               '"lastActivity":"' LAST-ACTIVITY '",'
               '"status":"' ACCT-STATUS OF ACCOUNT-RECORD '"'
               '}'
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           
           MOVE WS-JSON-OUTPUT TO SYNC-RESPONSE-RECORD
           WRITE SYNC-RESPONSE-RECORD.
       
       FINALIZE-JSON-RESPONSE.
           MOVE WS-RECORD-COUNT TO DSP-COUNT
           MOVE WS-TOTAL-BALANCE TO DSP-BALANCE
           
           STRING '{'
               '"syncId":"' WS-SYNC-ID '",'
               '"recordCount":' DSP-COUNT ','
               '"totalBalance":"' DSP-BALANCE '",'
               '"status":"' WS-STATUS '"'
               '}'
               DELIMITED BY SIZE INTO SYNC-RESPONSE-RECORD
           
           WRITE SYNC-RESPONSE-RECORD.
       
       TERMINATION.
           ACCEPT WS-END-TIME FROM DATE YYYYMMDD
           
           CLOSE SYNC-REQUEST
           CLOSE SYNC-RESPONSE
           CLOSE ACCOUNT-MASTER
           CLOSE TRANSACTION-LOG.
