       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACTION-STREAM.
       AUTHOR. SUITECRM-COBOL-INTEGRATION.
       DATE-WRITTEN. 2025-01-21.
      *----------------------------------------------------------------
      * Real-time Transaction Streaming for Banking Operations
      * Provides transaction feed for CRM dashboards
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STREAM-REQUEST ASSIGN TO "stream-request.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STREAM-RESPONSE ASSIGN TO "stream-response.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS TRANS-KEY
               ALTERNATE RECORD KEY IS TRANS-ACCOUNT WITH DUPLICATES
               ALTERNATE RECORD KEY IS TRANS-DATE WITH DUPLICATES.
           SELECT STREAM-BUFFER ASSIGN TO "stream-buffer.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  STREAM-REQUEST.
       01  STREAM-REQUEST-RECORD.
           05  STREAM-TYPE         PIC X(20).
           05  ACCOUNT-FILTER      PIC X(20).
           05  DATE-FROM           PIC 9(8).
           05  DATE-TO             PIC 9(8).
           05  TRANS-TYPE-FILTER   PIC X(20).
           05  AMOUNT-MIN          PIC S9(10)V99.
           05  AMOUNT-MAX          PIC S9(10)V99.
           05  MAX-RECORDS         PIC 9(4).
           05  STREAM-MODE         PIC X(10).
       
       FD  STREAM-RESPONSE.
       01  STREAM-RESPONSE-RECORD  PIC X(2000).
       
       FD  TRANSACTION-FILE.
       01  TRANSACTION-REC.
           05  TRANS-KEY.
               10  TRANS-DATE      PIC 9(8).
               10  TRANS-TIME      PIC 9(6).
               10  TRANS-SEQ       PIC 9(6).
           05  TRANS-ACCOUNT       PIC X(20).
           05  TRANS-TYPE          PIC X(20).
           05  TRANS-AMOUNT        PIC S9(10)V99 COMP-3.
           05  TRANS-BALANCE       PIC S9(12)V99 COMP-3.
           05  TRANS-DESCRIPTION   PIC X(100).
           05  TRANS-MERCHANT      PIC X(50).
           05  TRANS-CATEGORY      PIC X(30).
           05  TRANS-STATUS        PIC X(10).
           05  TRANS-REFERENCE     PIC X(30).
           05  TRANS-CHANNEL       PIC X(20).
       
       FD  STREAM-BUFFER.
       01  BUFFER-RECORD           PIC X(500).
       
       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(6) VALUE ZERO.
           05  WS-MATCH-COUNT      PIC 9(6) VALUE ZERO.
           05  WS-BUFFER-COUNT     PIC 9(4) VALUE ZERO.
       
       01  WS-STREAM-CONTROL.
           05  WS-STREAM-ID        PIC X(36).
           05  WS-STREAM-STATUS    PIC X(10).
           05  WS-LAST-POSITION    PIC 9(9).
           05  WS-CONTINUE-FLAG    PIC X VALUE 'Y'.
       
       01  WS-FILTERS.
           05  WS-APPLY-FILTERS    PIC X VALUE 'N'.
           05  WS-DATE-FILTER      PIC X VALUE 'N'.
           05  WS-TYPE-FILTER      PIC X VALUE 'N'.
           05  WS-AMOUNT-FILTER    PIC X VALUE 'N'.
       
       01  WS-JSON-BUILDER.
           05  WS-JSON-OUTPUT      PIC X(2000).
           05  WS-JSON-POS         PIC 9(4).
       
       01  WS-TRANSACTION-DATA.
           05  WS-PREV-BALANCE     PIC S9(12)V99 COMP-3.
           05  WS-RUNNING-TOTAL    PIC S9(14)V99 COMP-3.
           05  WS-DAILY-TOTAL      PIC S9(12)V99 COMP-3.
           05  WS-CURRENT-DATE     PIC 9(8).
       
       01  WS-DISPLAY-FIELDS.
           05  DSP-AMOUNT          PIC -$$,$$$,$$9.99.
           05  DSP-BALANCE         PIC -$$$,$$$,$$9.99.
           05  DSP-DATE            PIC 9999/99/99.
           05  DSP-TIME            PIC 99B99B99.
       
       01  WS-REALTIME-FLAGS.
           05  WS-NEW-TRANS        PIC X VALUE 'N'.
           05  WS-POLL-INTERVAL    PIC 9(3) VALUE 5.
           05  WS-LAST-CHECK       PIC 9(14).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION
           PERFORM PROCESS-STREAM-REQUEST
           PERFORM TERMINATION
           STOP RUN.
       
       INITIALIZATION.
           OPEN INPUT STREAM-REQUEST
           OPEN OUTPUT STREAM-RESPONSE
           OPEN I-O TRANSACTION-FILE
           OPEN OUTPUT STREAM-BUFFER
           
           ACCEPT WS-STREAM-ID FROM TIME
           MOVE "ACTIVE" TO WS-STREAM-STATUS
           
           READ STREAM-REQUEST
               AT END MOVE "No stream request" TO WS-STREAM-STATUS
           END-READ
           
           PERFORM SETUP-FILTERS.
       
       SETUP-FILTERS.
           IF DATE-FROM NOT = ZERO OR DATE-TO NOT = 99999999
               MOVE 'Y' TO WS-DATE-FILTER
               MOVE 'Y' TO WS-APPLY-FILTERS
           END-IF
           
           IF TRANS-TYPE-FILTER NOT = SPACES AND 
              TRANS-TYPE-FILTER NOT = "ALL"
               MOVE 'Y' TO WS-TYPE-FILTER
               MOVE 'Y' TO WS-APPLY-FILTERS
           END-IF
           
           IF AMOUNT-MIN NOT = ZERO OR AMOUNT-MAX NOT = 999999999.99
               MOVE 'Y' TO WS-AMOUNT-FILTER
               MOVE 'Y' TO WS-APPLY-FILTERS
           END-IF.
       
       PROCESS-STREAM-REQUEST.
           EVALUATE STREAM-TYPE
               WHEN "LIVE-FEED"
                   PERFORM STREAM-LIVE-TRANSACTIONS
               WHEN "HISTORICAL"
                   PERFORM STREAM-HISTORICAL
               WHEN "ACCOUNT-ACTIVITY"
                   PERFORM STREAM-ACCOUNT-ACTIVITY
               WHEN "DAILY-SUMMARY"
                   PERFORM STREAM-DAILY-SUMMARY
               WHEN "SEARCH"
                   PERFORM SEARCH-TRANSACTIONS
               WHEN "ANALYTICS"
                   PERFORM STREAM-ANALYTICS
               WHEN OTHER
                   MOVE "Invalid stream type" TO WS-STREAM-STATUS
           END-EVALUATE.
       
       STREAM-LIVE-TRANSACTIONS.
           MOVE "STREAMING" TO WS-STREAM-STATUS
           PERFORM START-JSON-STREAM
           
           PERFORM UNTIL WS-CONTINUE-FLAG = 'N' OR 
                         WS-MATCH-COUNT >= MAX-RECORDS
               PERFORM CHECK-NEW-TRANSACTIONS
               IF WS-NEW-TRANS = 'Y'
                   PERFORM PROCESS-NEW-TRANSACTIONS
               END-IF
               
               CALL "CBL_OC_NANOSLEEP" USING WS-POLL-INTERVAL
           END-PERFORM
           
           PERFORM END-JSON-STREAM.
       
       CHECK-NEW-TRANSACTIONS.
           ACCEPT WS-LAST-CHECK FROM DATE YYYYMMDD
           ACCEPT WS-LAST-CHECK FROM TIME
           
           MOVE SPACES TO TRANS-KEY
           START TRANSACTION-FILE KEY > TRANS-KEY
           END-START
           
           READ TRANSACTION-FILE NEXT
               AT END MOVE 'N' TO WS-NEW-TRANS
               NOT AT END
                   IF TRANS-DATE = WS-CURRENT-DATE AND
                      TRANS-TIME > WS-LAST-CHECK(9:6)
                       MOVE 'Y' TO WS-NEW-TRANS
                   ELSE
                       MOVE 'N' TO WS-NEW-TRANS
                   END-IF
           END-READ.
       
       PROCESS-NEW-TRANSACTIONS.
           PERFORM UNTIL WS-NEW-TRANS = 'N'
               IF WS-APPLY-FILTERS = 'Y'
                   PERFORM APPLY-TRANSACTION-FILTERS
                   IF WS-CONTINUE-FLAG = 'Y'
                       PERFORM WRITE-TRANSACTION-JSON
                   END-IF
               ELSE
                   PERFORM WRITE-TRANSACTION-JSON
               END-IF
               
               READ TRANSACTION-FILE NEXT
                   AT END MOVE 'N' TO WS-NEW-TRANS
               END-READ
           END-PERFORM.
       
       STREAM-HISTORICAL.
           MOVE "PROCESSING" TO WS-STREAM-STATUS
           PERFORM START-JSON-STREAM
           
           IF ACCOUNT-FILTER NOT = SPACES
               MOVE ACCOUNT-FILTER TO TRANS-ACCOUNT
               START TRANSACTION-FILE KEY >= TRANS-ACCOUNT
           ELSE
               MOVE DATE-FROM TO TRANS-DATE
               START TRANSACTION-FILE KEY >= TRANS-DATE
           END-IF
           
           PERFORM READ-AND-STREAM-TRANSACTIONS
               UNTIL WS-CONTINUE-FLAG = 'N' OR
                     WS-MATCH-COUNT >= MAX-RECORDS
           
           PERFORM END-JSON-STREAM.
       
       READ-AND-STREAM-TRANSACTIONS.
           READ TRANSACTION-FILE NEXT
               AT END MOVE 'N' TO WS-CONTINUE-FLAG
               NOT AT END
                   IF WS-APPLY-FILTERS = 'Y'
                       PERFORM APPLY-TRANSACTION-FILTERS
                   END-IF
                   
                   IF WS-CONTINUE-FLAG = 'Y'
                       PERFORM WRITE-TRANSACTION-JSON
                       ADD 1 TO WS-MATCH-COUNT
                   END-IF
           END-READ.
       
       APPLY-TRANSACTION-FILTERS.
           MOVE 'Y' TO WS-CONTINUE-FLAG
           
           IF WS-DATE-FILTER = 'Y'
               IF TRANS-DATE < DATE-FROM OR TRANS-DATE > DATE-TO
                   MOVE 'N' TO WS-CONTINUE-FLAG
               END-IF
           END-IF
           
           IF WS-TYPE-FILTER = 'Y' AND WS-CONTINUE-FLAG = 'Y'
               IF TRANS-TYPE NOT = TRANS-TYPE-FILTER
                   MOVE 'N' TO WS-CONTINUE-FLAG
               END-IF
           END-IF
           
           IF WS-AMOUNT-FILTER = 'Y' AND WS-CONTINUE-FLAG = 'Y'
               IF TRANS-AMOUNT < AMOUNT-MIN OR 
                  TRANS-AMOUNT > AMOUNT-MAX
                   MOVE 'N' TO WS-CONTINUE-FLAG
               END-IF
           END-IF
           
           IF ACCOUNT-FILTER NOT = SPACES AND WS-CONTINUE-FLAG = 'Y'
               IF TRANS-ACCOUNT NOT = ACCOUNT-FILTER
                   MOVE 'N' TO WS-CONTINUE-FLAG
               END-IF
           END-IF.
       
       STREAM-ACCOUNT-ACTIVITY.
           IF ACCOUNT-FILTER = SPACES
               MOVE "Account required" TO WS-STREAM-STATUS
               PERFORM WRITE-ERROR-RESPONSE
           ELSE
               PERFORM STREAM-HISTORICAL
           END-IF.
       
       STREAM-DAILY-SUMMARY.
           MOVE ZERO TO WS-DAILY-TOTAL
           MOVE ZERO TO WS-RECORD-COUNT
           
           MOVE DATE-FROM TO TRANS-DATE
           START TRANSACTION-FILE KEY >= TRANS-DATE
           
           PERFORM UNTIL TRANS-DATE > DATE-TO OR WS-CONTINUE-FLAG = 'N'
               READ TRANSACTION-FILE NEXT
                   AT END MOVE 'N' TO WS-CONTINUE-FLAG
                   NOT AT END
                       IF TRANS-DATE >= DATE-FROM AND 
                          TRANS-DATE <= DATE-TO
                           ADD TRANS-AMOUNT TO WS-DAILY-TOTAL
                           ADD 1 TO WS-RECORD-COUNT
                       END-IF
               END-READ
           END-PERFORM
           
           PERFORM WRITE-SUMMARY-JSON.
       
       SEARCH-TRANSACTIONS.
           PERFORM START-JSON-STREAM
           
           MOVE SPACES TO TRANS-KEY
           START TRANSACTION-FILE KEY >= TRANS-KEY
           
           PERFORM SEARCH-AND-MATCH
               UNTIL WS-CONTINUE-FLAG = 'N' OR
                     WS-MATCH-COUNT >= MAX-RECORDS
           
           PERFORM END-JSON-STREAM.
       
       SEARCH-AND-MATCH.
           READ TRANSACTION-FILE NEXT
               AT END MOVE 'N' TO WS-CONTINUE-FLAG
               NOT AT END
                   PERFORM APPLY-TRANSACTION-FILTERS
                   IF WS-CONTINUE-FLAG = 'Y'
                       PERFORM WRITE-TRANSACTION-JSON
                       ADD 1 TO WS-MATCH-COUNT
                   END-IF
           END-READ.
       
       STREAM-ANALYTICS.
           PERFORM CALCULATE-ANALYTICS
           PERFORM WRITE-ANALYTICS-JSON.
       
       CALCULATE-ANALYTICS.
           MOVE ZERO TO WS-RUNNING-TOTAL
           MOVE ZERO TO WS-RECORD-COUNT
           
           MOVE ACCOUNT-FILTER TO TRANS-ACCOUNT
           START TRANSACTION-FILE KEY >= TRANS-ACCOUNT
           
           PERFORM UNTIL WS-CONTINUE-FLAG = 'N'
               READ TRANSACTION-FILE NEXT
                   AT END MOVE 'N' TO WS-CONTINUE-FLAG
                   NOT AT END
                       IF TRANS-ACCOUNT = ACCOUNT-FILTER
                           ADD TRANS-AMOUNT TO WS-RUNNING-TOTAL
                           ADD 1 TO WS-RECORD-COUNT
                           MOVE TRANS-BALANCE TO WS-PREV-BALANCE
                       ELSE
                           MOVE 'N' TO WS-CONTINUE-FLAG
                       END-IF
               END-READ
           END-PERFORM.
       
       START-JSON-STREAM.
           STRING '{"streamId":"' WS-STREAM-ID '",'
                  '"status":"' WS-STREAM-STATUS '",'
                  '"transactions":['
                  DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           MOVE 1 TO WS-JSON-POS.
       
       WRITE-TRANSACTION-JSON.
           IF WS-BUFFER-COUNT > 0
               STRING ',' DELIMITED BY SIZE 
                   INTO WS-JSON-OUTPUT
                   WITH POINTER WS-JSON-POS
           END-IF
           
           MOVE TRANS-DATE TO DSP-DATE
           MOVE TRANS-TIME TO DSP-TIME
           MOVE TRANS-AMOUNT TO DSP-AMOUNT
           MOVE TRANS-BALANCE TO DSP-BALANCE
           
           STRING '{'
               '"date":"' DSP-DATE '",'
               '"time":"' DSP-TIME '",'
               '"account":"' TRANS-ACCOUNT '",'
               '"type":"' TRANS-TYPE '",'
               '"amount":"' DSP-AMOUNT '",'
               '"balance":"' DSP-BALANCE '",'
               '"description":"' TRANS-DESCRIPTION '",'
               '"merchant":"' TRANS-MERCHANT '",'
               '"category":"' TRANS-CATEGORY '",'
               '"status":"' TRANS-STATUS '",'
               '"reference":"' TRANS-REFERENCE '",'
               '"channel":"' TRANS-CHANNEL '"'
               '}'
               DELIMITED BY SIZE INTO BUFFER-RECORD
           
           WRITE BUFFER-RECORD
           ADD 1 TO WS-BUFFER-COUNT
           
           IF WS-BUFFER-COUNT >= 10 OR STREAM-MODE = "REALTIME"
               PERFORM FLUSH-BUFFER
           END-IF.
       
       FLUSH-BUFFER.
           CLOSE STREAM-BUFFER
           OPEN INPUT STREAM-BUFFER
           
           PERFORM UNTIL WS-BUFFER-COUNT = 0
               READ STREAM-BUFFER
                   AT END EXIT PERFORM
               END-READ
               
               MOVE BUFFER-RECORD TO STREAM-RESPONSE-RECORD
               WRITE STREAM-RESPONSE-RECORD
               
               SUBTRACT 1 FROM WS-BUFFER-COUNT
           END-PERFORM
           
           CLOSE STREAM-BUFFER
           OPEN OUTPUT STREAM-BUFFER.
       
       END-JSON-STREAM.
           PERFORM FLUSH-BUFFER
           
           STRING '],'
               '"totalRecords":' WS-MATCH-COUNT ','
               '"timestamp":"' WS-LAST-CHECK '"'
               '}'
               DELIMITED BY SIZE INTO STREAM-RESPONSE-RECORD
           
           WRITE STREAM-RESPONSE-RECORD.
       
       WRITE-SUMMARY-JSON.
           MOVE WS-DAILY-TOTAL TO DSP-BALANCE
           
           STRING '{'
               '"streamId":"' WS-STREAM-ID '",'
               '"type":"DAILY_SUMMARY",'
               '"dateRange":{'
                   '"from":"' DATE-FROM '",'
                   '"to":"' DATE-TO '"'
               '},'
               '"summary":{'
                   '"totalAmount":"' DSP-BALANCE '",'
                   '"transactionCount":' WS-RECORD-COUNT ','
                   '"averageAmount":"' DSP-AMOUNT '"'
               '}'
               '}'
               DELIMITED BY SIZE INTO STREAM-RESPONSE-RECORD
           
           WRITE STREAM-RESPONSE-RECORD.
       
       WRITE-ANALYTICS-JSON.
           MOVE WS-RUNNING-TOTAL TO DSP-AMOUNT
           MOVE WS-PREV-BALANCE TO DSP-BALANCE
           
           STRING '{'
               '"streamId":"' WS-STREAM-ID '",'
               '"type":"ANALYTICS",'
               '"account":"' ACCOUNT-FILTER '",'
               '"metrics":{'
                   '"totalVolume":"' DSP-AMOUNT '",'
                   '"transactionCount":' WS-RECORD-COUNT ','
                   '"currentBalance":"' DSP-BALANCE '"'
               '}'
               '}'
               DELIMITED BY SIZE INTO STREAM-RESPONSE-RECORD
           
           WRITE STREAM-RESPONSE-RECORD.
       
       WRITE-ERROR-RESPONSE.
           STRING '{'
               '"streamId":"' WS-STREAM-ID '",'
               '"status":"ERROR",'
               '"message":"' WS-STREAM-STATUS '"'
               '}'
               DELIMITED BY SIZE INTO STREAM-RESPONSE-RECORD
           
           WRITE STREAM-RESPONSE-RECORD.
       
       TERMINATION.
           CLOSE STREAM-REQUEST
           CLOSE STREAM-RESPONSE
           CLOSE TRANSACTION-FILE
           CLOSE STREAM-BUFFER.
